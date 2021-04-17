library(datarobot)
library(tidyverse)
library(yaml)
source('project_helpers.R')
source('bias_functions.R')

config <- read_yaml('config/project_config.yaml')
project <- load_project(config$project_name)

################################################################################
# create separate DR projects that predict the protected features,
# then look at the feature impact, feature effects, and word cloud
################################################################################

merged_data <- tryCatch({
  load_data('merged_data', config$project_name)
}, error = function(e) {
  training_data <- read_csv(config$filename)
  training_predictions <- getStackedPredictions(project, model)
  merged_data <- bind_cols(training_data, training_predictions)
  write_data(merged_data, 'merged_data', config$project_name)
})

# read existing projects if they exist
suppressWarnings(tryCatch({
  indirect_projects <- readRDS(file.path('config', paste(config$project_name, 'indirect_projects.RDS')))
  indirect_protected <- readRDS(file.path('config', paste(config$project_name, 'indirect_protected.RDS')))
  indirect_group <- readRDS(file.path('config', paste(config$project_name, 'indirect_group.RDS')))
}, error = function(e) {
  # if not yet created, start projects for each protected feature
  # start by getting previous featurelist
  best_model <- GetModelRecommendation(project, 'Recommended for Deployment')
  model <- GetModel(best_model$projectId, best_model$modelId)
  training_data <- read_csv(config$filename)
  feature_list = GetFeaturelist(project, model$featurelistId)
  input_features = feature_list$features
  # get the EDA
  eda_summary = ListFeatureInfo(project)
  eda_table = featureinfolist.as.data.frame(eda_summary)
  date_features = unname(unlist(eda_table %>% filter(featureType == 'Date') %>% select(name)))
  text_features = unname(unlist(eda_table %>% filter(featureType == 'Text') %>% select(name)))
  engineered_date_features = bind_rows(lapply(
              c('Year', 'Month', 'Day', 'Hour'),
              function(x) return(tibble(raw = date_features, engineered = paste0(date_features, ' (', x, ')'), period = x)))) %>%
          filter(engineered %in% input_features)
  raw_features = input_features
  for (r in seq_len(nrow(engineered_date_features)))
    raw_features = gsub(engineered_date_features$engineered[r], engineered_date_features$raw[r], raw_features, fixed = TRUE)
  raw_features = unique(raw_features)

  # loop through to generate projects
  indirect_projects <- list()
  indirect_protected <- list()
  indirect_group <- list()
  for (protected_feature in config$protected) {
    group_list <- training_data %>% extract2(protected_feature) %>% unique
    if (length(group_list) == 2) {
      target_group = head(group_list, 1)
      temp_data = merged_data %>%
        select(all_of(c(raw_features, protected_feature)))
      prj = StartProject(dataSource = temp_data,
                         projectName = paste0('Find indirect bias - ', protected_feature),
                         target = protected_feature,
                         positiveClass = target_group,
                         mode = 'quick',
                         workerCount = 'max')
      i = 1 + length(indirect_projects)
      indirect_projects[[i]] = prj
      indirect_protected[[i]] = protected_feature
      indirect_group[[i]] = target_group
    }
    if (length(group_list) > 2) {
      for (target_group in group_list) {
        temp_data = merged_data %>%
          select(all_of(c(raw_features, protected_feature))) %>%
          mutate(target_protected_group = ifelse(!!as.name(protected_feature) == target_group, target_group, 'all_others')) %>%
          select(-all_of(protected_feature))

        prj = StartProject(dataSource = temp_data,
                           projectName = paste0('Find indirect bias - ', protected_feature, ' - ', target_group),
                           target = 'target_protected_group',
                           positiveClass = target_group,
                           mode = 'quick',
                           workerCount = 'max')
        i = 1 + length(indirect_projects)
        indirect_projects[[i]] = prj
        indirect_protected[[i]] = protected_feature
        indirect_group[[i]] = target_group
      }
    }
  }
  for (prj in indirect_projects) WaitForAutopilot(prj)
  saveRDS(indirect_projects, file.path('config', paste(config$project_name, 'indirect_projects.RDS')))
  saveRDS(indirect_protected, file.path('config', paste(config$project_name, 'indirect_protected.RDS')))
  saveRDS(indirect_group, file.path('config', paste(config$project_name, 'indirect_group.RDS')))
}))

# ensure objects exist in memory
indirect_projects <- readRDS(file.path('config', paste(config$project_name, 'indirect_projects.RDS')))
indirect_protected <- readRDS(file.path('config', paste(config$project_name, 'indirect_protected.RDS')))
indirect_group <- readRDS(file.path('config', paste(config$project_name, 'indirect_group.RDS')))

# get insights from the projects we just built
for (i in seq_len(length(indirect_projects))) {
  prj = indirect_projects[[i]]
  protected_feature = indirect_protected[[i]]
  protected_group = indirect_group[[i]]
  cat(paste0('Analysing feature: ', protected_feature, ' and group: ', protected_group, '\n'))

  # get the top model
  indirect_leaderboard = as.data.frame(ListModels(prj))
  indirect_top_model = GetModel(prj, head(indirect_leaderboard$modelId[! grepl('Blender', indirect_leaderboard$modelType)], 1))

  # get the feature impact, identifying possible proxies
  indirect_feature_impact = GetFeatureImpact(indirect_top_model)
  indirect_feature_impact = indirect_feature_impact %>% mutate(featureName = factor(featureName, levels = rev(indirect_feature_impact$featureName)))
  plt = ggplot(data = indirect_feature_impact, aes(x = featureName, y = impactNormalized)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    ggtitle('Feature Impact', subtitle = paste0('Potential proxies for feature: ', protected_feature, ' and group: ', protected_group)) +
    xlab('Feature Name') +
    ylab('Feature Impact')
  print(plt)

  # for each text feature show the wordcloud
  indirect_text_models = indirect_leaderboard %>% filter(grepl('Auto-Tuned Word N-Gram Text Modeler', modelType))
  indirect_text_models = indirect_text_models %>% filter(samplePct == max(indirect_text_models$samplePct))
  positive_class = GetProject(prj$projectId)$positiveClass
  for (modelID in indirect_text_models$modelId) {
    text_model = GetModel(prj, modelID)
    text_feature = gsub('Auto-Tuned Word N-Gram Text Modeler using token occurrences - ', '', text_model$modelType, fixed = TRUE)
    indirect_wordcloud = GetWordCloud(prj, modelID, excludeStopWords = TRUE) %>%
      mutate(weight = abs(coefficient) * count) %>%
      arrange(desc(weight)) %>%
      mutate(effect = ifelse(coefficient < -0.2, 'Negative', ifelse(coefficient > 0.2, 'Positive', 'Moderate'))) %>%
      rename(ngram_count = count) %>%
      mutate(colour_num = ifelse(effect == 'Negative', 1, ifelse(effect == 'Moderate', 2, 3))) %>%
      top_n(80, weight)
    colours = c('blue', 'grey', 'red')[sort(unique(indirect_wordcloud$colour_num))]
    subtitle = paste0('Text feature = ', text_feature)
    if (! is.null(positive_class) & length(positive_class) > 0)
      subtitle = paste0(subtitle, '\nRed colour is ', protected_group)
    set.seed(123)
    plt = ggplot(data = indirect_wordcloud, aes(label = ngram, size = ngram_count, color = effect)) +
      geom_text_wordcloud_area(area_corr_power = 1, shape = 'square', rm_outside = TRUE) +
      scale_size_area(max_size = 24) +
      theme_minimal() +
      scale_colour_manual(values = colours) +
      ggtitle(paste0('Potential text proxies for: ', protected_feature),
              subtitle = subtitle)
    print(plt)
  }
}

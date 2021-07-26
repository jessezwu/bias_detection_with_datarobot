library(datarobot)
library(tidyverse)
library(yaml)
source('project_helpers.R')
source('bias_functions.R')
source('plot_functions.R')

config <- read_yaml('config/project_config.yaml')
project <- load_project(config$project_name)

###########################################################################################
# find indirect bias
###########################################################################################

# get recommended model
best_model <- GetModelRecommendation(project, 'Recommended for Deployment')
model <- GetModel(best_model$projectId, best_model$modelId)

merged_data <- tryCatch({
  load_data('merged_data', config$project_name)
}, error = function(e) {
  training_data <- read_csv(config$filename)
  training_predictions <- getStackedPredictions(project, model)
  merged_data <- bind_cols(training_data, training_predictions)
  write_data(merged_data, 'merged_data', config$project_name)
})

# get a list of the features used in our chosen model
feature_list <- GetFeaturelist(project, model$featurelistId)
input_features <- feature_list$features[! feature_list$features == config$target]

# get the EDA for these features
eda_summary <- ListFeatureInfo(project)
eda_table <- featureinfolist.as.data.frame(eda_summary)

# identify date based features
date_features <- unname(unlist(eda_table %>% filter(featureType == 'Date') %>% select(name)))
text_features <- unname(unlist(eda_table %>% filter(featureType == 'Text') %>% select(name)))
if(length(date_features) > 0) {
  engineered_date_features <- lapply(
      c('Year', 'Month', 'Day', 'Hour'),
      function(x) tibble(
        raw = date_features,
        engineered = paste0(date_features, ' (', x, ')'), period = x
      )
    ) %>%
    bind_rows() %>%
    filter(engineered %in% input_features)
}
raw_features <- input_features
for (r in seq_len(nrow(engineered_date_features)))
  raw_features <- gsub(engineered_date_features$engineered[r], engineered_date_features$raw[r], raw_features, fixed = TRUE)
raw_features <- unique(raw_features)

# get cross-class data disparity for protected features
psi_scores <- bind_rows(lapply(config$protected, function(protected_feature) {
  test_data <- merged_data %>%
    select(all_of(c(raw_features, config$target, protected_feature)))
  for (r in seq_len(nrow(engineered_date_features))) {
    if (engineered_date_features$period[r] == 'Year') test_data[, engineered_date_features$engineered[r]] = year(anydate(unname(unlist(test_data[, engineered_date_features$raw[r]]))))
    if (engineered_date_features$period[r] == 'Month') test_data[, engineered_date_features$engineered[r]] = month(anydate(unname(unlist(test_data[, engineered_date_features$raw[r]]))))
    if (engineered_date_features$period[r] == 'Day') test_data[, engineered_date_features$engineered[r]] = day(anydate(unname(unlist(test_data[, engineered_date_features$raw[r]]))))
  }
  group_levels = test_data %>% extract2(protected_feature) %>% unique()
  psi_scores = bind_rows(lapply(group_levels, function(group) {
    test_data_1 = test_data %>% filter(!!as.name(protected_feature) == group)
    test_data_2 = test_data %>% filter(!!as.name(protected_feature) != group)
    psi_scores = get_psi_all(
                    dat = test_data_1,
                    dat_test = test_data_2,
                    ex_cols = c(text_features, protected_feature),
                    as_table = TRUE
                  ) %>%
                  filter(! is.infinite(PSI_i)) %>%
                  group_by(Feature) %>%
                  summarise(PSI = sum(PSI_i), .groups = 'drop') %>%
                  mutate(group_level = group) %>%
                  relocate(group_level, .before = 1) %>%
                  mutate(protected_feature = protected_feature) %>%
                  relocate(protected_feature, .before = 1)
    return(psi_scores)
  }))
  return(psi_scores)
}))
write_data(psi_scores, 'psi_scores', config$project_name)

# get the feature impact
feature_impact = GetFeatureImpact(model)

# plot the results
plot_data <- psi_scores %>%
  left_join(feature_impact %>% select(featureName, impactNormalized) %>% rename(Feature = featureName), by = 'Feature') %>%
  mutate(impactNormalized = ifelse(Feature == config$target, 1, impactNormalized)) %>%
  filter(! is.na(impactNormalized)) %>%
  mutate(Impact = ifelse(PSI <= 0.1, 'Low', ifelse(PSI <= 0.25, 'Moderate', 'Major'))) %>%
  mutate(Impact = factor(Impact, levels = c('Low', 'Moderate', 'Major')))

for (protected_feature in config$protected) {
  group_levels <- merged_data %>% extract2(protected_feature) %>% unique
  for (protected_group in group_levels) {
    plt <- plot_data %>%
      filter(protected_feature == protected_feature, group_level == protected_group) %>%
      plotPSI(protected_feature, protected_group)
    print(plt)
  }
}

################################################################################

# get the feature association matrix
feature_association = GetFeatureAssociationMatrix(project, associationType = 'association', metric = 'mutualInfo')
#
# show the 5 strongest feature associations for each protected feature
for (protected_feature in config$protected) {
  strengths <- feature_association$strengths %>%
    filter(feature1 == protected_feature | feature2 == protected_feature) %>%
    arrange(desc(statistic))
  strengths %<>%
    top_n(5, statistic)
  associated_features <- append(
    strengths %>% extract2('feature1'),
    strengths %>% extract2('feature2')
  ) %>% unique
  strengths <- feature_association$strengths %>%
    filter(feature1 %in% associated_features & feature2 %in% associated_features) %>%
    mutate(feature1 = factor(feature1, levels = associated_features)) %>%
    mutate(feature2 = factor(feature2, levels = associated_features))
  strengths2 <- strengths %>%
    rename(feature1 = feature2, feature2 = feature1) %>%
    filter(feature1 != feature2)
  strengths <- bind_rows(strengths, strengths2) %>%
    rename(Association = statistic)
  plt <- ggplot(data = strengths, aes(x = feature1, y = feature2, fill = Association)) +
    geom_tile() +
    theme_minimal() +
    scale_fill_gradientn(colours = c('white', 'green', 'yellow', 'red')) +
    ggtitle('Feature Associations', subtitle = paste0('Protected Feature = ', protected_feature)) +
    xlab('Feature Name') +
    ylab('Feature Name')
  print(plt)
}

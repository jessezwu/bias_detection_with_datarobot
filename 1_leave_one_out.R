library(datarobot)
library(tidyverse)
library(yaml)
source('project_helpers.R')
source('datarobot_helpers.R')
source('bias_functions.R')
source('plot_functions.R')

config <- read_yaml('config/project_config.yaml')

################################################################################
# Justification for use of personal attributes
#
# Based on a piece of analysis from the Veritas methodology from Singapore,
# evaluate change in profit as well as bias with the removal of protected
# features one by one.
################################################################################

original_project <- load_project(config$project_name)
featurelists <- ListFeaturelists(original_project)
informative <- keep(featurelists, function(x) grepl('Leakage Removed', x$name))
# if no leakage detected
if(length(informative) == 0) {
  informative <- keep(featurelists, function(x) grepl('Informative', x$name))
}
base_features <- informative %>%
  extract2(1) %>%
  extract2('features')

# run a new project
project <- tryCatch({
  load_project(paste(config$project_name, 'leave one out'))
}, error = function(e) {
  # create a new project, and run autopilot with all features
  project <- SetupProject(config$filename, paste(config$project_name, 'leave one out'))
  features <- CreateFeaturelist(project, 'Base Features', base_features)
  SetTarget(project,
    target = config$target,
    mode = 'quick',
    featurelistId = features$featurelistId
  )
  UpdateProject(project, workerCount = 'max')
  WaitForAutopilot(project)
  project <- GetProject(project$projectId)
  write_project(project)
  project
})

# take the top model using all features
leaderboard <- ListModels(project) %>%
  keep(function(x) !is.null(x$featurelistName)) %>%
  keep(function(x) x$featurelistName == 'Base Features') %>%
  keep(function(x) !is.na(x$metrics$LogLoss$crossValidation))
leaderboard <- leaderboard %>%
  as.data.frame() %>%
  mutate(
    crossValidationMetric = sapply(leaderboard, function(x) x$metrics$LogLoss$crossValidation)
  ) %>%
  arrange(crossValidationMetric)
baseline_model <- GetModel(project, leaderboard %>% head(1) %>% extract2('modelId'))

# download the stacked predictions on the training data
training_predictions <- getStackedPredictions(project, baseline_model)
training_data <- read_csv(config$filename)
baseline_data <- bind_cols(training_data, training_predictions)

# get the profit curve
payoff_matrix <- tryCatch({
    GetPayoffMatrix(project, 'payoff matrix')
  },
  error = function(e) {
    CreatePayoffMatrix(project, 'payoff matrix',
      TP_value = config$tp_value,
      TN_value = config$tn_value,
      FP_value = config$fp_value,
      FN_value = config$fn_value
    )
})
profit_curve <- getProfitCurve(baseline_data, project, payoff_matrix)
baseline_threshold <- profit_curve$threshold[which.max(profit_curve$profit)]
baseline_profit <- max(profit_curve$profit)

################################################################################
# evaluate models built without individual protected features
################################################################################

# summarise fairness as the avg relative disparity to top group
# or equivalently, area overlap compared to perfect relative fairness
# ranging from 1/n (worst) to 1 (best)
summariseFairness <- function(data, colname) {
  stopifnot(data %>% extract2(colname) %>% max() <= 1)
  data %>%
    extract2(colname) %>%
    mean()
}

for (feature in config$protected) {
  data <- getFavorableClassBalance(baseline_data, 'probability', feature, baseline_threshold)
  baseline_fairness <- summariseFairness(data, 'relative_favorable_class_balance')

  new_featurelist <- tryCatch({
    CreateFeaturelist(project, paste('Removed', feature), setdiff(base_features, feature))
  }, error = function(e) {
    ListFeaturelists(project) %>%
      keep(function(x) x$name== paste('Removed', feature)) %>%
      extract2(1)
  })

  model_job <- RequestNewModel(
    project,
    baseline_model,
    featurelist = new_featurelist,
    samplePct = baseline_model$samplePct
  )
  WaitForJobToComplete(project, model_job)
  new_model <- GetModelFromJobId(project, model_job)

  # calculate new profit and fairness
  training_predictions <- getStackedPredictions(project, new_model)
  new_data <- bind_cols(training_data, training_predictions)

  new_profit_curve <- getProfitCurve(new_data, project, payoff_matrix)
  new_threshold <- new_profit_curve$threshold[which.max(new_profit_curve$profit)]
  new_profit <- max(new_profit_curve$profit)

  data <- getFavorableClassBalance(new_data, 'probability', feature, new_threshold)
  new_fairness <- summariseFairness(data, 'relative_favorable_class_balance')

  change <- function(new, old) {
    (new - old) / old
  }
  # plot difference
  plot_data <- tibble(
    Removed = feature,
    Effect = c(change(new_profit, baseline_profit),
               change(new_fairness, baseline_fairness)),
    Measure = c('Profit', 'Equal Opportunity')
  )
  plot <- ggplot(plot_data, aes(x=Removed, y=Effect, fill=Measure)) +
    geom_bar(stat='identity', position='dodge') +
    scale_fill_brewer(palette='Set1') +
    theme_minimal() +
    ylab('Effect of removal (removed - baseline)') +
    ggtitle('Impact of protected attributes')
  print(plot)
}

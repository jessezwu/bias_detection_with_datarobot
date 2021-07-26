library(datarobot)
library(tidyverse)
library(yaml)
library(DT)
source('project_helpers.R')
source('bias_functions.R')
source('per_class_threshold_table.R')

config <- read_yaml('config/project_config.yaml')
project <- load_project(config$project_name)

################################################################################
# remove bias: vary decision threshold separately for each protected class
################################################################################

merged_data <- tryCatch({
  load_data('merged_data', config$project_name)
}, error = function(e) {
  best_model <- GetModelRecommendation(project, 'Recommended for Deployment')
  model <- GetModel(best_model$projectId, best_model$modelId)
  training_data <- read_csv(config$filename)
  training_predictions <- getStackedPredictions(project, model)
  merged_data <- bind_cols(training_data, training_predictions)
  write_data(merged_data, 'merged_data', config$project_name)
})

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
profit_curve <- getProfitCurve(merged_data, project, payoff_matrix)
optimal_threshold_for_profit <- profit_curve$threshold[which.max(profit_curve$profit)]

# find the largest group in a protected feature
# use that cutoff as the base against which everything is compared
# loop one group at a time and get its absolute bias metric to as close to the largest group as possible
threshold_tables <- tryCatch({
    load_data('threshold_tables', config$project_name)
  }, error = function(e) {
  threshold_tables <- list()
  for (feature in config$protected) {
    table <- getPerClassThresholdTable(merged_data, feature, profit_threshold, config) %>%
      mutate(Feature = feature)
    threshold_tables <- bind_rows(threshold_tables, table)
  }
  write_data(threshold_tables, 'threshold_tables', config$project_name)
  threshold_tables
})

datatable(threshold_tables, rownames=FALSE, options=list(dom='t'))


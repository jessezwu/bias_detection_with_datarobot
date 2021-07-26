library(datarobot)
library(tidyverse)
library(yaml)
source('project_helpers.R')
source('bias_functions.R')

config <- read_yaml('config/project_config.yaml')
project <- load_project(config$project_name)

################################################################################
# remove bias: vary the global decision threshold
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

# take at most 100 points for the sake of speed
profit_curve <- profit_curve[seq(1, nrow(profit_curve), ceiling(nrow(profit_curve) / 100)),]

bias_metric_summary <- tryCatch({
    load_data('bias_metric_summary', config$project_name)
  }, error = function(e) {
    bias_metric_summary <- bind_rows(lapply(config$protected, function(protected_feature) {
      getBiasMetricSummary(merged_data, profit_curve, 'probability', protected_feature, config$target, config$preferable_outcome)
    }))
    write_data(bias_metric_summary, 'bias_metric_summary', config$project_name)
    bias_metric_summary
})

# plot trade-offs in profit and bias when varying the global decision threshold
for (feature in config$protected) {
  # show the distribution of predictions
  plt <- merged_data %>%
    getCleanedNames(project) %>%
    ggplot(aes(x=probability, color=get(feature), fill=get(feature))) +
      geom_histogram(position = "identity", bins = 30, alpha = 0.3) +
      theme_minimal() +
      scale_color_discrete(name = 'Class') +
      scale_fill_discrete(name = 'Class') +
      ggtitle(paste('Predictions by', feature))
  print(plt)
  # plot trade off
  plot_data <- bind_rows(list(
    profit_curve %>% mutate(Metric = 'Relative Profit') %>% mutate(y = profit / max(profit_curve$profit)),
    bias_metric_summary %>%
      filter(protectedFeature == feature) %>%
      filter(biasMetric %in% c('relative_proportional_parity', 'relative_favorable_class_balance')) %>%
      rename(y = groupsBelow, Metric = biasMetric)
  )) %>%
    mutate(Metric = gsub('relative_', '', Metric, fixed = TRUE)) %>%
    mutate(Metric = gsub('_', ' ', Metric, fixed = TRUE))
  plt <- ggplot(plot_data, aes(x = threshold, y = y, colour = Metric)) +
    geom_line() +
    theme_minimal() +
    ggtitle(sprintf('Varying Threshold for Controlling Bias in %s', feature)) +
    ylab('% Groups With Bias and % Max Profit') +
    xlab('Threshold')
  print(plt)
}

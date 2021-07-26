library(datarobot)
library(tidyverse)
library(yaml)
source('project_helpers.R')
source('bias_functions.R')
source('plot_functions.R')

config <- read_yaml('config/project_config.yaml')
project <- load_project(config$project_name)

################################################################################
# remove bias: remove indirect bias features
################################################################################

tryCatch({
    psi_scores <- load_data('psi_scores', config$project_name)
  }, error = function(e) {
    stop('PSI scores are required - run the indirect_bias script first!')
})

# rank features by overall psi
ranked_features <- psi_scores %>%
  filter(Feature != config$target) %>%
  group_by(protected_feature, group_level) %>%
  arrange(desc(PSI)) %>%
  mutate(rank = row_number()) %>%
  group_by(Feature) %>%
  summarise(mean_PSI = mean(PSI), mean_Rank = mean(rank), .groups = 'drop') %>%
  arrange(mean_Rank)

# find the feature(s) which is the worst offender overall
worst <- head(ranked_features, 1)
# worst <- ranked_features %>% filter(mean_PSI > 1)

if(worst$mean_PSI > 0.25) {
  # run a new model without feature
  best_model <- GetModelRecommendation(project, 'Recommended for Deployment')
  model <- GetModel(best_model$projectId, best_model$modelId)
  base_features <- GetFeaturelist(project, model$featurelistId)
  removed <- setdiff(base_features$features, worst$Feature)
  flist <- getOrCreateFeaturelist(project, 'Indirect Bias Removed', removed)
  UpdateProject(project, holdoutUnlocked = TRUE)
  jobID <- RequestNewModel(
    project,
    model,
    featurelist = flist,
    samplePct = model$samplePct
  )
  WaitForJobToComplete(project, jobID)
  model_V1 <- GetModelFromJobId(project, jobID)

  # show the effect upon accuracy of removing feature 
  leaderboard <- ListModels(project) %>%
    as.data.frame() %>%
    filter(modelId %in% c(model$modelId, model_V1$modelId))
  leaderboard %<>%
    mutate(
      crossValidationMetric = sapply(leaderboard$modelId, function(x) GetModel(project, x)$metrics$LogLoss$crossValidation),
      holdoutMetric = sapply(leaderboard$modelId, function(x) GetModel(project, x)$metrics$LogLoss$holdout)
  )
  plt <- leaderboard %>%
    select(featurelistName, validationMetric, crossValidationMetric, holdoutMetric) %>%
    pivot_longer(-featurelistName) %>%
    ggplot(aes(x=name, y=value, color=featurelistName)) +
      geom_point() +
      theme_minimal() +
      xlab('Partition') +
      ylab('LogLoss') +
      scale_color_discrete(name = 'Featurelist') +
      ggtitle('Accuracy after Indirect Bias Removed')
  print(plt)


  # show effect on profit curve
  training_data <- read_csv(config$filename)
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
  training_predictions <- getStackedPredictions(project, model)
  merged_data <- bind_cols(training_data, training_predictions)
  profit_curve <- getProfitCurve(merged_data, project, payoff_matrix)
  optimal_threshold_for_profit <- profit_curve$threshold[which.max(profit_curve$profit)]

  training_predictions_V1 <- getStackedPredictions(project, model_V1)
  merged_data_V1 <- bind_cols(training_data, training_predictions_V1)
  profit_curve_V1 <- getProfitCurve(merged_data_V1, project, payoff_matrix)
  optimal_threshold_for_profit_V1 <- profit_curve_V1$threshold[which.max(profit_curve_V1$profit)]
  new_text <- paste('With', worst$Feature, 'removed')
  plotProfitCurveComparison(profit_curve, 'Original', profit_curve_V1, new_text)


  # show the effect of removing zip_code upon unfair bias metrics
  for (feature in config$protected) {
    # calculate and plot proportional parity
    pp1 <- getProportionalParity(merged_data, 'probability', feature, optimal_threshold_for_profit)
    pp2 <- getProportionalParity(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1)
    plotProportionalParityComparison(pp1, 'Original', pp2, new_text)

    # calculate and plot equal parity
    eqp1 <- getEqualParity(merged_data, 'probability', feature, optimal_threshold_for_profit)
    eqp2 <- getEqualParity(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1)
    plotEqualParityComparison(eqp1, 'Original', eqp2, new_text)

    # calculate and plot favorable class balance
    fcb1 <- getFavorableClassBalance(merged_data, 'probability', feature, optimal_threshold_for_profit)
    fcb2 <- getFavorableClassBalance(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1)
    plotFavorableClassBalanceComparison(fcb1, 'Original', fcb2, new_text)

    # calculate and plot unfavorable class balance
    ucb1 <- getUnfavorableClassBalance(merged_data, 'probability', feature, optimal_threshold_for_profit)
    ucb2 <- getUnfavorableClassBalance(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1)
    plotUnfavorableClassBalanceComparison(ucb1, 'Original', ucb2, new_text)

    # calculate and plot favorable rate parity
    frp1 <- getFavorableRateParity(merged_data, 'probability', feature, optimal_threshold_for_profit, config$target, config$preferable_outcome)
    frp2 <- getFavorableRateParity(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1, config$target, config$preferable_outcome)
    plotFavorableRateParityComparison(frp1, 'Original', frp2, new_text)

    # calculate and plot unfavorable rate parity
    urp1 <- getUnfavorableRateParity(merged_data, 'probability', feature, optimal_threshold_for_profit, config$target, config$preferable_outcome)
    urp2 <- getUnfavorableRateParity(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1, config$target, config$preferable_outcome)
    plotUnfavorableRateParityComparison(urp1, 'Original', urp2, new_text)

    # calculate and plot favorable predictive value parity
    fpv1 <- getFavorablePredictiveValueParity(merged_data, 'probability', feature, optimal_threshold_for_profit, config$target, config$preferable_outcome)
    fpv2 <- getFavorablePredictiveValueParity(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1, config$target, config$preferable_outcome)
    plotFavorablePredictiveValueParityComparison(fpv1, 'Original', fpv2, new_text)

    # calculate and plot unfavorable rate parity
    upv1 <- getUnfavorablePredictiveValueParity(merged_data, 'probability', feature, optimal_threshold_for_profit, config$target, config$preferable_outcome)
    upv2 <- getUnfavorablePredictiveValueParity(merged_data_V1, 'probability', feature, optimal_threshold_for_profit_V1, config$target, config$preferable_outcome)
    plotUnfavorablePredictiveValueParityComparison(upv1, 'Original', upv2, new_text)
  }
} else {
  print('No major indirect bias features found using PSI')
}

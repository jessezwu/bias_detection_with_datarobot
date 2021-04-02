##################################################
# 
# Helper functions
# 
##################################################

getStackedPredictions = function(project, model) {
  predictions <- ListTrainingPredictions(project)
  predictionId <- unlist(lapply(predictions, function(x) 
    if(x$modelId == model$modelId && x$dataSubset == 'all') return(x$id) else return(NULL)))
  if (length(predictionId) == 0) {
    jobID = RequestTrainingPredictions(model, dataSubset = DataSubset$All)
    WaitForJobToComplete(project, jobID)
    predictions <- ListTrainingPredictions(project)
    predictionId <- unlist(lapply(predictions, function(x) 
      if(x$modelId == model$modelId && x$dataSubset == 'all') return(x$id) else return(NULL)))
  }
  trainingPredictions <- GetTrainingPredictions(project, predictionId)
  return(trainingPredictions)  
}

# get the profit curve
getProfitCurve = function(merged_data, target, payoff_matrix) {
  thresholds = 0.001 * 0:1000
  profit = sapply(thresholds, function(x) {
    # TO DO: find the positive class for this project and don't hard code it in the calculation
    temp = tibble(target = merged_data[, target], probability = merged_data$class_Yes) %>%
      mutate(payoff = 
               payoff_matrix$truePositiveValue * ifelse(probability > x & target == 'Yes', 1, 0) +
               payoff_matrix$trueNegativeValue * ifelse(probability <= x & target != 'Yes', 1, 0) +
               payoff_matrix$falsePositiveValue * ifelse(probability > x & target != 'Yes', 1, 0) +
               payoff_matrix$falseNegativeValue * ifelse(probability <= x & target == 'Yes', 1, 0)
      )
    return(sum(temp$payoff))
  })
  profitCurve = tibble(threshold = thresholds, profit = profit)
  return(profitCurve)
}

library(datarobot)
library(tidyverse)
library(yaml)
source('project_helpers.R')
source('bias_functions.R')

config <- read_yaml('config/project_config.yaml')
project <- load_project(config$project_name)

###########################################################################################
# measure unfair bias in the base model without protected features
###########################################################################################

best_model <- GetModelRecommendation(project, 'Recommended for Deployment')

# Plot the ROC Curve
roc_curve = GetRocCurve(model, DataPartition$VALIDATION, fallbackToParentInsights = TRUE)
ggplot(roc_curve$rocPoints, aes(x=falsePositiveRate, y=truePositiveRate)) +
  geom_point() +
  geom_abline(intercept = 0) +
  ggtitle('ROC Curve') +
  theme_minimal()

# create a profit curve
payoff_matrix <- CreatePayoffMatrix(project, 'payoff matrix',
  TP_value = config$tp_value,
  TN_value = config$tn_value,
  FP_value = config$fp_value,
  FN_value = config$fn_value
)

# download the stacked predictions on the training data
# we will manually calculate the profit, accuracy, and bias metrics
model <- GetModel(best_model$projectId, best_model$modelId)
training_predictions <- getStackedPredictions(project, model)

# merge the training predictions with the training data
training_data <- read_csv(config$filename)
merged_data <- bind_cols(training_data, training_predictions)

# get the profit curve
project <- GetProject(project$projectId)   # make the project into a full project object
profit_curve <- getProfitCurve(merged_data, project, payoff_matrix)
optimal_threshold_for_profit <- profit_curve$threshold[which.max(profit_curve$profit)]
ggplot(data = profit_curve, aes(x = threshold, y = profit)) +
  geom_line() +
  theme_minimal() +
  ggtitle('Profit Curve') +
  scale_y_continuous(labels=scales::dollar_format())

#new <- merged_data %>%
#  mutate(positiveResult = ifelse(
#    !!as.name(paste0('class_', project$positiveClass)) <= optimal_threshold_for_profit,
#    'Pos',
#    'Neg')
#  )

# get the confusion matrix using the optimal threshold for profit
print('Using the optimal threshold for profit')
confusion_matrix <- getClassificationAccuracy(merged_data, project, optimal_threshold_for_profit)
table(confusion_matrix)

# calculate fairness metrics using the optimal threshold for profit
# https://app.datarobot.com/docs/modeling/investigate/bias/bias-ref.html#proportional-parity

# calculate and plot proportional parity
for (feature in config$protected) {
  pp <- getProportionalParity(merged_data, feature, optimal_threshold_for_profit)
  plotProportionalParity(pp, feature)
}

# calculate and plot equal parity
for (feature in config$protected) {
  eqp <- getEqualParity(merged_data, feature, optimal_threshold_for_profit)
  plotEqualParity(eqp, feature)
}

# calculate and plot favorable class balance
for (feature in config$protected) {
  fcb <- getFavorableClassBalance(merged_data, feature, optimal_threshold_for_profit)
  plotFavorableClassBalance(fcb, feature)
}

# calculate and plot favorable class balance
for (feature in config$protected) {
  ucb <- getUnfavorableClassBalance(merged_data, feature, optimal_threshold_for_profit)
  plotUnfavorableClassBalance(ucb, feature)
}

# calculate and plot true favorable rate parity
for (feature in config$protected) {
  frp <- getFavorableRateParity(merged_data, feature, optimal_threshold_for_profit)
  plotFavorableRateParity(frp, feature)
}

# calculate and plot true unfavorable rate parity
for (feature in config$protected) {
  urp <- getUnfavorableRateParity(merged_data, feature, optimal_threshold_for_profit)
  plotUnfavorableRateParity(urp, feature)
}

# calculate and plot favorable predictive value parity
for (feature in config$protected) {
  pvp <- getFavorablePredictiveValueParity(merged_data, feature, optimal_threshold_for_profit)
  plotFavorablePredictiveValueParity(pvp, feature)
}

# calculate and plot unfavorable predictive value parity
for (feature in config$protected) {
  upvp <- getUnfavorablePredictiveValueParity(merged_data, feature, optimal_threshold_for_profit)
  plotUnfavorablePredictiveValueParity(upvp, feature)
}


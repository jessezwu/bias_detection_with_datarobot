library(datarobot)
library(tidyverse)
library(yaml)
library(DT)
source('project_helpers.R')
source('bias_functions.R')

config <- read_yaml('config/project_config.yaml')
project <- load_project(config$project_name)

################################################################################
# remove bias: reweighting
################################################################################

# We will only explore reweighting for one protected feature
protected <- config$protected[[1]]

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

# step 1: create a contingency table for the outcomes
contingency_table <- merged_data %>%
  group_by(!!as.name(protected), !!as.name(config$target)) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(id_cols = all_of(protected), names_from = all_of(config$target), values_from = Count) %>%
  mutate(nTotal = No + Yes) %>%
  mutate(ProbPositive = !!as.name(config$preferable_outcome) / nTotal)

# step 2: which group is the privileged group?
privileged <- contingency_table %>%
  extract2(protected) %>%
  extract2(which.max(contingency_table$ProbPositive))

# step 3: calculate the weights
weights_table <- merged_data %>%
  group_by(!!as.name(protected), !!as.name(config$target)) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(isPositive = ifelse(!!as.name(config$target) == config$preferable_outcome, 'Yes', 'No')) %>%
  mutate(isPrivileged = ifelse(!!as.name(protected) == privileged, 'Yes', 'No'))
weights_table = weights_table %>%
  mutate(Weights = sapply(seq_len(nrow(weights_table)), function(r) {
      N1 = sum(weights_table$Count[weights_table$isPrivileged == weights_table$isPrivileged[r]])
      N2 = sum(weights_table$Count[weights_table$isPositive == weights_table$isPositive[r]])
      N3 = sum(weights_table$Count)
      N4 = weights_table$Count[r]
      return((N1 * N2) / (N3 * N4))
    }))

# step 4: create a training dataset with these weights for each row
raw_data <- read_csv(config$filename)
weighted_data <- weights_table %>%
  select(protected, config$target, Weights) %>%
  right_join(raw_data, by = all_of(c(protected, config$target)))

# step 5: build a new project with this weighted dataset
project_name <- paste(config$project_name, '- Reweighting')
tryCatch({
    project_reweighted <- load_project(project_name)
  }, error = function(e) {
    best_model <- GetModelRecommendation(project, 'Recommended for Deployment')
    model <- GetModel(best_model$projectId, best_model$modelId)
    feature_list <- GetFeaturelist(project, model$featurelistId)

    project_reweighted <- SetupProject(weighted_data, project_name)
    features_reweighted <- CreateFeaturelist(project_reweighted, 'Protected Removed', feature_list$features)
    # project settings, note bias and fairness is currently only supported through the REST API
    # for documentation, see:
    # https://app.datarobot.com/apidocs/autodoc/api_reference.html#patch--api-v2-projects-(projectId)-aim-
    settings_reweighted <- list(
      target = config$target,
      mode = 'quick',
      featurelistId = features_reweighted$featurelistId,
      protectedFeatures = as.list(config$protected),
      preferableTargetValue = config$preferable_outcome,
      fairnessMetricsSet = config$fairness_metric,
      weights = 'Weights'
    )
    # run project
    response <- datarobot:::DataRobotPATCH(
      datarobot:::UrlJoin("projects", project_reweighted$projectId, "aim"),
      body = settings_reweighted,
      encode = 'json',
      returnRawResponse = TRUE
    )
    UpdateProject(project_reweighted, workerCount = 'max')
    WaitForAutopilot(project_reweighted)
    write_project(project_reweighted)
})

# download the stacked predictions on the weighted training data
best_model <- GetModelRecommendation(project_reweighted, 'Recommended for Deployment')
model <- GetModel(best_model$projectId, best_model$modelId)
training_predictions <- getStackedPredictions(project_reweighted, model)
# merge the training predictions with the training data
merged_data <- bind_cols(weighted_data, training_predictions)

# step 6: show profit curve
# get the profit curve
project_reweighted <- GetProject(project_reweighted$projectId)
payoff_matrix <- tryCatch({
    GetPayoffMatrix(project_reweighted, 'payoff matrix')
  },
  error = function(e) {
    CreatePayoffMatrix(project_reweighted, 'payoff matrix',
      TP_value = config$tp_value,
      TN_value = config$tn_value,
      FP_value = config$fp_value,
      FN_value = config$fn_value
    )
})
profit_curve <- getProfitCurve(merged_data, project_reweighted, payoff_matrix)
optimal_threshold_for_profit <- profit_curve$threshold[which.max(profit_curve$profit)]
plt <- ggplot(data = profit_curve, aes(x = threshold, y = profit)) +
  geom_line() +
  theme_minimal() +
  ggtitle('Profit Curve') +
  scale_y_continuous(labels=scales::dollar_format())
print(plt)

# step 7: show proportional parity
pp <- getProportionalParity(merged_data, protected, optimal_threshold_for_profit)
plotProportionalParity(pp, protected)


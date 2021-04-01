library(datarobot)
library(tidyverse)
library(magrittr)
library(httr)
library(ggplot2)
library(scales)
library(creditmodel)
library(lubridate)
library(anytime)
library(ggrepel)
library(ggwordcloud)

################################################################################
# Settings
################################################################################

filename <- 'data/DR_Demo_LendingClub_Guardrails_Fairness.csv.zip'
target <- 'is_bad'
leakage <- 'loan_status'
preferable_outcome <- 'No'
fairness_metric <- 'predictionBalance'
# One of proportionalParity, equalParity, predictionBalance, trueFavorableAndUnfavorableRateParity or FavorableAndUnfavorablePredictiveValueParity

# load in list of protected features
protected <- read_lines('protected_features.txt')

################################################################################
# detect and remove leakage as well as protected features
################################################################################

# upload dataset to create a project
# data <- read_csv(filename)
project <- SetupProject(filename, 'Bias Demo')

featurelists <- ListFeaturelists(project)
informative <- keep(featurelists, function(x) grepl('Informative', x$name))
# remove protected features, and predefined leakage
no_protected <- informative %>%
  extract2(1) %>%
  extract2('features') %>%
  setdiff(c(target, leakage, protected))
features <- CreateFeaturelist(project, 'Protected Removed', no_protected)

# project settings, note bias and fairness is currently only supported through the REST API
# for documentation, see:
# https://app.datarobot.com/apidocs/autodoc/api_reference.html#patch--api-v2-projects-(projectId)-aim-
settings <- list(
  target = target,
  mode = 'quick',
  featurelistId = features$featurelistId,
  protectedFeatures = as.list(protected),
  preferableTargetValue = preferable_outcome,
  fairnessMetricsSet = fairness_metric
)

# run project
response <- datarobot:::DataRobotPATCH(
  datarobot:::UrlJoin("projects", project$projectId, "aim"),
  body = settings,
  encode = 'json',
  returnRawResponse = TRUE
)
UpdateProject(project, workerCount = 'max')
WaitForAutopilot(project)

###########################################################################################
# measure unfair bias in the base model without protected features
###########################################################################################

# take top model
best_model <- GetModelRecommendation(project, 'Recommended for Deployment')

# write an R function to create a payoff matrix
CreatePayoffMatrix = function (project, matrix_name = 'new payoff matrix', 
                               TP_value = 1, TN_value = 1, FP_value = 1, FN_value = 1) 
{
  projectId <- datarobot:::ValidateProject(project)
  routeString <- datarobot:::UrlJoin("projects", projectId, "payoffMatrices")
  body = list(name = matrix_name, 
                 truePositiveValue = TP_value,
                 trueNegativeValue = TN_value,
                 falsePositiveValue = FP_value,
                 falseNegativeValue = FN_value)
  rawReturn <- datarobot:::DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  payoff_matrix = content(rawReturn)
  return(payoff_matrix)
}

# truePositiveValue = 0        # no bad loan, no loss
# trueNegativeValue = 1500     # good loan written, $1500 profit made
# falsePositiveValue = 0       # no good loan written, opportunity cost $1500 in profit, but net payoff is zero
# falseNegativeValue = -5000   # bad loan written, $10000 in loan write-offs

payoff_matrix = CreatePayoffMatrix(project, 'payoff matrix', 0, 1500, 0, -10000)

# download the stacked predictions on the training data
# we will have to manually calculate the profit, accuracy, and bias metrics

predictions <- ListTrainingPredictions(project)
model <- GetModel(best_model$projectId, best_model$modelId)
predictionId <- sapply(predictions, function(x) 
  if(x$modelId == best_model$modelId && x$dataSubset == 'all') return(x$id) else return(NULL))
if (length(predictionId) == 0) {
  jobID = RequestTrainingPredictions(model, dataSubset = DataSubset$All)
  WaitForJobToComplete(project, jobID)
  predictions <- ListTrainingPredictions(project)
  predictionId <- sapply(predictions, function(x) 
    if(x$modelId == best_model$modelId && x$dataSubset == 'all') return(x$id) else return(NULL))
}
trainingPredictions <- GetTrainingPredictions(project, predictionId)

# merge the training predictions with the training data
trainingData = read_csv(filename)
mergedData = bind_cols(trainingData, trainingPredictions)

# get the profit curve
thresholds = 0.001 * 0:1000
profit = sapply(thresholds, function(x) {
  # TO DO: find the positive class for this project and don't hard code it in the calculation
  temp = tibble(target = mergedData[, target], probability = mergedData$class_Yes) %>%
    mutate(payoff = 
             payoff_matrix$truePositiveValue * ifelse(probability > x & target == 'Yes', 1, 0) +
             payoff_matrix$trueNegativeValue * ifelse(probability <= x & target != 'Yes', 1, 0) +
             payoff_matrix$falsePositiveValue * ifelse(probability > x & target != 'Yes', 1, 0) +
             payoff_matrix$falseNegativeValue * ifelse(probability <= x & target == 'Yes', 1, 0)
             )
    return(sum(temp$payoff))
})
profitCurve = tibble(threshold = thresholds, profit = profit)
optimalThresholdForProfit = profitCurve$threshold[which.max(profitCurve$profit)]
ggplot(data = profitCurve, aes(x = threshold, y = profit)) + 
  geom_line() +
  ggtitle('Profit Curve') +
  scale_y_continuous(labels=scales::dollar_format())
mergedData = mergedData %>%
  mutate(positiveResult = ifelse(class_Yes <= optimalThresholdForProfit, 'Pos', 'Neg'))

# get the accuracy (confusion matrix) using the optimal threshold for profit
rocCurve = GetRocCurve(model, DataPartition$VALIDATION, fallbackToParentInsights = TRUE)

# 

# TO DO: calculate the fairness metrics using the optimal threshold for profit
# https://app.datarobot.com/docs/modeling/investigate/bias/bias-ref.html#proportional-parity

getClassificationAccuracy = function(thresh) {
  temp = tibble(target = mergedData[, target], probability = mergedData$class_Yes) %>%
    mutate(confusion = ifelse(probability > thresh & target == 'Yes', 'TP', '')) %>%
    mutate(confusion = ifelse(probability <= thresh & target != 'Yes', 'TN', confusion)) %>%
    mutate(confusion = ifelse(probability > thresh & target != 'Yes', 'FP', confusion)) %>%
    mutate(confusion = ifelse(probability <= thresh & target == 'Yes', 'FN', confusion))
  return(unlist(temp$confusion))
}

mergedData$confusionQuadrant = getClassificationAccuracy(optimalThresholdForProfit)

# calculate and plot proportional parity
getProportionParity = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows) %>%
    mutate(absolute_proportional_parity = Pos / (Pos + Neg))
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_proportional_parity)) %>%
    mutate(relative_proportional_parity = absolute_proportional_parity / maxRate) %>%
    mutate(nTot = Neg + Pos) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & absolute_proportional_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_proportional_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_proportional_parity, 
                         relative_proportional_parity, fairness))
}
for (featureName in protected) {
  pp = getProportionParity(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_proportional_parity, fill = fairness)) +
            geom_col() + 
            ggtitle('Proportional Parity') +
            xlab(featureName) +
            ylab('Absolute Proportional Parity') +
            scale_fill_manual(values = colours)
  print(plt)
}

# calculate and plot equal parity
getEqualParity = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows) %>%
    mutate(absolute_equal_parity = Pos)
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_equal_parity)) %>%
    mutate(relative_equal_parity = absolute_equal_parity / maxRate) %>%
    mutate(nTot = Neg + Pos) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & absolute_equal_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_equal_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_equal_parity, 
                         relative_equal_parity, fairness))
}
for (featureName in protected) {
  pp = getEqualParity(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_equal_parity, fill = fairness)) +
    geom_col() + 
    ggtitle('Equal Parity') +
    xlab(featureName) +
    ylab('Absolute Equal Parity') +
    scale_fill_manual(values = colours) +
    scale_y_continuous(labels=scales::comma_format())
  print(plt)
}

# calculate and plot favorable class balance
getFavorableClassBalance = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    group_by(!!as.name(featureName)) %>%
    summarise(nTot = n(), aveScore = mean(class_Yes), .groups = 'drop') %>%
    mutate(absolute_favorable_class_balance = aveScore)
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_favorable_class_balance)) %>%
    mutate(relative_favorable_class_balance = absolute_favorable_class_balance / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_favorable_class_balance < 0.1)) %>%
    mutate(fairness = ifelse(relative_favorable_class_balance >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_favorable_class_balance, 
                         relative_favorable_class_balance, fairness))
}
for (featureName in protected) {
  pp = getFavorableClassBalance(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_favorable_class_balance, fill = fairness)) +
    geom_col() + 
    ggtitle('Favorable Class Balance') +
    xlab(featureName) +
    ylab('Absolute Favorable Class Balance') +
    scale_fill_manual(values = colours)
  print(plt)
}

# calculate and plot favorable class balance
getUnfavorableClassBalance = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Neg') %>%
    group_by(!!as.name(featureName)) %>%
    summarise(nTot = n(), aveScore = mean(class_Yes), .groups = 'drop') %>%
    mutate(absolute_unfavorable_class_balance = aveScore)
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_unfavorable_class_balance)) %>%
    mutate(relative_unfavorable_class_balance = absolute_unfavorable_class_balance / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_unfavorable_class_balance < 0.1)) %>%
    mutate(fairness = ifelse(relative_unfavorable_class_balance >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_unfavorable_class_balance, 
                         relative_unfavorable_class_balance, fairness))
}
for (featureName in protected) {
  pp = getUnfavorableClassBalance(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_unfavorable_class_balance, fill = fairness)) +
    geom_col() + 
    ggtitle('Unfavorable Class Balance') +
    xlab(featureName) +
    ylab('Absolute Unfavorable Class Balance') +
    scale_fill_manual(values = colours)
  print(plt)
}

# calculate and plot true favorable rate parity
getFavorableRateParity = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(target) == preferable_outcome) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows) %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_favorable_rate_parity = Pos / nTot)
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_favorable_rate_parity)) %>%
    mutate(relative_favorable_rate_parity = absolute_favorable_rate_parity / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_favorable_rate_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_favorable_rate_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_favorable_rate_parity, 
                         relative_favorable_rate_parity, fairness))
}
for (featureName in protected) {
  pp = getFavorableRateParity(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_favorable_rate_parity, fill = fairness)) +
    geom_col() + 
    ggtitle('Favorable Rate Parity') +
    xlab(featureName) +
    ylab('Absolute Favorable Rate Parity') +
    scale_fill_manual(values = colours)
  print(plt)
}

# calculate and plot true unfavorable rate parity
getUnfavorableRateParity = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(target) != preferable_outcome) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows) %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_unfavorable_rate_parity = Neg / nTot)
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_unfavorable_rate_parity)) %>%
    mutate(relative_unfavorable_rate_parity = absolute_unfavorable_rate_parity / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_unfavorable_rate_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_unfavorable_rate_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_unfavorable_rate_parity, 
                         relative_unfavorable_rate_parity, fairness))
}
for (featureName in protected) {
  pp = getUnfavorableRateParity(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_unfavorable_rate_parity, fill = fairness)) +
    geom_col() + 
    ggtitle('Unfavorable Rate Parity') +
    xlab(featureName) +
    ylab('Absolute Unfavorable Rate Parity') +
    scale_fill_manual(values = colours)
  print(plt)
}

# calculate and plot favorable predictive value parity
getFavorablePredictiveValueParity = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(target) == preferable_outcome, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveTarget) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveTarget, values_from = nRows) %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_favorable_predictive_value_parity = Pos / nTot)
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_favorable_predictive_value_parity)) %>%
    mutate(relative_favorable_predictive_value_parity = absolute_favorable_predictive_value_parity / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_favorable_predictive_value_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_favorable_predictive_value_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_favorable_predictive_value_parity, 
                         relative_favorable_predictive_value_parity, fairness))
}
for (featureName in protected) {
  pp = getFavorablePredictiveValueParity(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_favorable_predictive_value_parity, fill = fairness)) +
    geom_col() + 
    ggtitle('Favorable Predictive Value Parity') +
    xlab(featureName) +
    ylab('Absolute Favorable Predictive Value Parity') +
    scale_fill_manual(values = colours)
  print(plt)
}

# calculate and plot unfavorable predictive value parity
getUnfavorablePredictiveValueParity = function(featureName, thresh) {
  temp = mergedData %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult != 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(target) == preferable_outcome, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveTarget) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveTarget, values_from = nRows) %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_unfavorable_predictive_value_parity = Neg / nTot)
  temp = temp %>%
    mutate(maxRate = max(temp$absolute_unfavorable_predictive_value_parity)) %>%
    mutate(relative_unfavorable_predictive_value_parity = absolute_unfavorable_predictive_value_parity / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_unfavorable_predictive_value_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_unfavorable_predictive_value_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_unfavorable_predictive_value_parity, 
                         relative_unfavorable_predictive_value_parity, fairness))
}
for (featureName in protected) {
  pp = getUnfavorablePredictiveValueParity(featureName, optimalThresholdForProfit)
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_unfavorable_predictive_value_parity, fill = fairness)) +
    geom_col() + 
    ggtitle('Unfavorable Predictive Value Parity') +
    xlab(featureName) +
    ylab('Absolute Unfavorable Predictive Value Parity') +
    scale_fill_manual(values = colours)
  print(plt)
}

# get a list of the features used in our chosen model
null.is.na = function(x) { return(ifelse(is.null(x), NA, x))}
featureinfo.as.data.frame = function(x) {
  result = tibble(
    id = x$id,
    name = x$name,
    featureType = null.is.na(x$featureType),
    importance = null.is.na(x$importance),
    lowInformation = x$lowInformation,
    targetLeakage = x$targetLeakage,
    projectId = x$projectId,
    uniqueCount = x$uniqueCount,
    naCount = null.is.na(x$naCount),
    min = null.is.na(x$min),
    mean = null.is.na(x$mean),
    median = null.is.na(x$median),
    max = null.is.na(x$max),
    stdDev = null.is.na(x$stdDev),
    timeSeriesEligible = x$timeSeriesEligible,
    timeSeriesEligibility = x$timeSeriesEligibility,
    dateFormat = null.is.na(x$dateFormat),
    timeUnit = null.is.na(x$timeUnit),
    timeStep = null.is.na(x$timeStep)
  )
  if (! is.na(result$featureType[1])) {
    if (result$featureType[1] == 'Date')
      result = result %>%
        mutate(
          min = julian(ymd(min)),
          mean = julian(ymd(mean)),
          median = julian(ymd(median)),
          max = julian(ymd(max)),
          stdDev = as.numeric(gsub(' days', '', stdDev))
        )
    if (result$featureType[1] == 'Percentage')
      result = result %>%
        mutate(
          min = as.numeric(gsub('%', '', min)),
          mean = as.numeric(gsub('%', '', mean)),
          median = as.numeric(gsub('%', '', median)),
          max = as.numeric(gsub('%', '', max)),
          stdDev = as.numeric(gsub('%', '', stdDev))
        )
  }
  tibble::validate_tibble(result)
  return(result)
}
featureinfolist.as.data.frame = function(x) {
  return(bind_rows(lapply(x, featureinfo.as.data.frame)))
}
feature_list = GetFeaturelist(project, model$featurelistId)
input_features = feature_list$features[! feature_list$features == target]

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

###########################################################################################
# find indirect bias
###########################################################################################

# get cross-class data disparity for protected features
psi_scores = bind_rows(lapply(protected, function(protected_feature) {
  test_data = mergedData %>% 
    select(all_of(c(raw_features, target, protected_feature)))
  for (r in seq_len(nrow(engineered_date_features))) {
    if (engineered_date_features$period[r] == 'Year') test_data[, engineered_date_features$engineered[r]] = year(anydate(unname(unlist(test_data[, engineered_date_features$raw[r]]))))
    if (engineered_date_features$period[r] == 'Month') test_data[, engineered_date_features$engineered[r]] = month(anydate(unname(unlist(test_data[, engineered_date_features$raw[r]]))))
    if (engineered_date_features$period[r] == 'Day') test_data[, engineered_date_features$engineered[r]] = day(anydate(unname(unlist(test_data[, engineered_date_features$raw[r]]))))
  }
  group_levels = unique(unlist(test_data[, protected_feature]))
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
# get the feature impact
feature_impact = GetFeatureImpact(model)

# plot the results
for (protected_feature in protected) {
  group_levels = unique(unlist(mergedData[, protected_feature]))
  for (protected_group in group_levels) {
    plot_data = psi_scores %>% 
                  filter(protected_feature == protected_feature & group_level == protected_group) %>%
                  left_join(feature_impact %>% select(featureName, impactNormalized) %>% rename(Feature = featureName), by = 'Feature') %>%
                  mutate(impactNormalized = ifelse(Feature == target, 1, impactNormalized)) %>%
                  filter(! is.na(impactNormalized)) %>%
                  mutate(Impact = ifelse(PSI <= 0.1, 'Low', ifelse(PSI <= 0.25, 'Moderate', 'Major'))) %>%
                  mutate(Impact = factor(Impact, levels = c('Low', 'Moderate', 'Major')))
    plt = ggplot(data = plot_data, aes(x = impactNormalized, y = PSI, colour = Impact, label = Feature)) +
                  geom_point() + 
                  geom_text_repel(size = 2.5) +
                  scale_colour_manual(values = c('green2', 'yellow3', 'red')) +
                  ggtitle('Cross-Class Data Disparity',
                          subtitle = paste('Feature = ', protected_feature, '    Group = ', protected_group)) +
                  xlab('Importance') +
                  ylab('Data Disparity')
    print(plt)
  }
}

# get the feature association matrix
feature_association = GetFeatureAssociationMatrix(project, associationType = 'association', metric = 'mutualInfo')
#
# show the 5 strongest feature associations for each protected feature
for (protected_feature in protected) {
  strengths = feature_association$strengths %>%
    filter(feature1 == protected_feature | feature2 == protected_feature) %>%
    arrange(desc(statistic))
  strengths = strengths %>% 
    top_n(5, statistic)
  associated_features = sapply(seq_len(5), function(r) 
    return(ifelse(strengths$feature1[r] == strengths$feature2[r] | strengths$feature2[r] == protected_feature, 
                  strengths$feature1[r], strengths$feature2[r])))
  strengths = feature_association$strengths %>%
    filter(feature1 %in% associated_features & feature2 %in% associated_features) %>%
    mutate(feature1 = factor(feature1, levels = associated_features)) %>%
    mutate(feature2 = factor(feature2, levels = associated_features))
  strengths2 = strengths %>%
    rename(temp = feature1) %>%
    rename(feature1 = feature2) %>%
    rename(feature2 = temp) %>%
    filter(feature1 != feature2)
  strengths = bind_rows(list(strengths, strengths2)) %>%
    rename(Association = statistic)
  plt = ggplot(data = strengths, aes(x = feature1, y = feature2, fill = Association)) +
    geom_tile() +
    scale_fill_gradientn(colours = c('grey', 'green', 'yellow', 'red')) +
    ggtitle('Feature Associations', subtitle = paste0('Protected Feature = ', protected_feature)) +
    xlab('Feature Name') +
    ylab('Feature Name')
  print(plt)
}

# create separate DR projects that predict the protected features, then look at the feature impact, feature effects, and word cloud
indirect_projects = list()
indirect_protected = list()
for (protected_feature in protected) {
  group_list = unique(unlist(mergedData %>% select(all_of(protected_feature))))
  if (length(group_list) == 2) {
    temp_data = mergedData %>% 
      select(all_of(c(raw_features, protected_feature)))
    prj = StartProject(dataSource = temp_data,
                       projectName = paste0('Find indirect bias - ', protected_feature),
                       target = protected_feature,
                       mode = 'quick',
                       workerCount = 'max')
    i = 1 + length(indirect_projects)
    indirect_projects[[i]] = prj
  }
  if (length(group_list) > 2) {
    for (target_group in group_list) {
      temp_data = mergedData %>% 
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
    }
  }
}
for (prj in indirect_projects) WaitForAutopilot(prj)

# get insights from the projects we just built
for (i in length(protected)) {
  prj = indirect_projects[[i]]
  protected_feature = indirect_protected[[i]]
  #
  # get the top model
  indirect_leaderboard = as.data.frame(ListModels(prj))
  indirect_top_model = GetModel(prj, head(indirect_leaderboard$modelId[! grepl('Blender', indirect_leaderboard$modelType)], 1))
  #
  # get the feature impact, identifying possible proxies
  indirect_feature_impact = GetFeatureImpact(indirect_top_model)
  indirect_feature_impact = indirect_feature_impact %>% mutate(featureName = factor(featureName, levels = rev(indirect_feature_impact$featureName)))
  plt = ggplot(data = indirect_feature_impact, aes(x = featureName, y = impactNormalized)) +
    geom_col() + 
    coord_flip() + 
    ggtitle('Feature Impact', subtitle = paste0('Potential proxies for: ', protected_feature)) +
    xlab('Feature Name') +
    ylab('Feature Impact')
  print(plt)
  #
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
      top_n(125, weight)
    colours = c('blue', 'grey', 'red')[sort(unique(indirect_wordcloud$colour_num))]
    subtitle = paste0('Text feature = ', text_feature)
    if (! is.null(positive_class) & length(positive_class) > 0)
      subtitle = paste0(subtitle, '\nRed colour is ', positive_class)
    set.seed(123)
    plt = ggplot(data = indirect_wordcloud, aes(label = ngram, size = ngram_count, color = effect)) +
      geom_text_wordcloud_area(area_corr_power = 1, shape = 'square', rm_outside = TRUE) +
      #geom_text_wordcloud_area(area_corr_power = 1, rm_outside = TRUE) +
      scale_size_area(max_size = 24) +
      theme_minimal() +
      scale_colour_manual(values = colours) +
      ggtitle(paste0('Potential text proxies for: ', protected_feature), 
              subtitle = subtitle)
    print(plt)
  }
}

###########################################################################################
# remove bias 1: remove indirect bias features
###########################################################################################

# TO DO: show the effect of removing income upon unfair bias metrics
# TO DO: show the effect of removing income upon profit curve

###########################################################################################
# remove bias 2: vary global decision threshold
###########################################################################################

# TO DO: vary the threshold for all loan applicants, so see what that does to our unfair bias metrics
# TO DO: plot the relationship between the threshold vs. several unfair bias metrics
# TO DO: plot the relationship between the profit curve vs. unfair bias metrics
# TO DO: plot the relationship between group proportional outcomes vs. group accuracy

###########################################################################################
# remove bias 3: vary decision threshold separately for each protected class
###########################################################################################

# TO DO: vary the threshold for each group, so see what that does to our unfair bias metrics
# TO DO: plot the relationship between the thresholds vs. several unfair bias metrics
# TO DO: plot the relationship between the thresholds vs. profit curve
# TO DO: plot the relationship between the profit curve vs. unfair bias metrics
# TO DO: create what-if scenarios and show that this approach creates disparate treatment
# TO DO: plot the relationship between group proportional outcomes vs. group accuracy
# TO DO: repeat the process to show how optimising proportional outcomes has quite different results to optimizing group accuracy

###########################################################################################
# remove bias 4: alter the feature values to remove entrenched disadvantage
###########################################################################################

# TO DO: artificially increase the income feature value to remove the gender pay gap
# TO DO: plot the relationship between the percentage adjustment vs. unfair bias metrics
# TO DO: plot the relationship between the profit curve vs. unfair bias metrics
# TO DO: plot the relationship between the profit curve vs. unfair bias metrics
# TO DO: create what-if scenarios and show that this approach creates disparate treatment
# TO DO: plot the relationship between group proportional outcomes vs. group accuracy

###########################################################################################
# remove bias 5: reweighting
###########################################################################################
# TO DO: replicate this methodology
# https://towardsdatascience.com/reweighing-the-adult-dataset-to-make-it-discrimination-free-44668c9379e8

###########################################################################################
# remove bias 6: rejection option-based classification
###########################################################################################
# TO DO: replicate this methodology
# https://towardsdatascience.com/reducing-ai-bias-with-rejection-option-based-classification-54fefdb53c2e


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


# create a payoff matrix
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

plotProfitCurveComparison = function(pc1, pc1_label, pc2, pc2_label) {
  plot_data = bind_rows(list(pc1 %>% mutate(Model = pc1_label),
                             pc2 %>% mutate(Model = pc2_label))) %>%
    mutate(Model = factor(Model, levels = c(pc1_label, pc2_label)))
  plt = ggplot(data = plot_data, aes(x = threshold, y = profit, colour = Model)) +
    geom_line() +
    ggtitle('Profit Curve Comparison') +
    scale_y_continuous(labels=scales::dollar_format()) +
    scale_colour_manual(values = c('blue', 'red'))
  print(plt)
}

# binary accuracy given a threshold
getClassificationAccuracy = function(thresh) {
  temp = tibble(target = mergedData[, target], probability = mergedData$class_Yes) %>%
    mutate(confusion = ifelse(probability > thresh & target == 'Yes', 'TP', '')) %>%
    mutate(confusion = ifelse(probability <= thresh & target != 'Yes', 'TN', confusion)) %>%
    mutate(confusion = ifelse(probability > thresh & target != 'Yes', 'FP', confusion)) %>%
    mutate(confusion = ifelse(probability <= thresh & target == 'Yes', 'FN', confusion))
  return(unlist(temp$confusion))
}


# calculate proportional parity
getProportionalParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows, values_fill = 0)
  if (! 'Pos' %in% names(temp)) temp = temp %>% mutate(Pos = 0)
  if (! 'Neg' %in% names(temp)) temp = temp %>% mutate(Neg = 0)
  temp = temp %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_proportional_parity = Pos / (Pos + Neg))
  adj_max_rate = max(temp$absolute_proportional_parity[temp$nTot >= 1000 | temp$Pos > 10])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_proportional_parity = ifelse(maxRate <= 1e-50, 1, absolute_proportional_parity / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_proportional_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_proportional_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_proportional_parity,
                         relative_proportional_parity, fairness))
}

# calculate equal parity
getEqualParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows, values_fill = 0)
  if (! 'Pos' %in% names(temp)) temp = temp %>% mutate(Pos = 0)
  if (! 'Neg' %in% names(temp)) temp = temp %>% mutate(Neg = 0)
  temp = temp %>%
    mutate(absolute_equal_parity = Pos)
    mutate(nTot = Neg + Pos) %>%
  adj_max_rate = max(temp$absolute_equal_parity[temp$nTot >= 1000 | temp$Pos > 10])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_equal_parity = ifelse(maxRate <= 1e-50, 1, absolute_equal_parity / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_equal_parity < 0.1))
    mutate(fairness = ifelse(relative_equal_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_equal_parity,
                         relative_equal_parity, fairness))
}

# calculate favorable class balance
getFavorableClassBalance = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    group_by(!!as.name(featureName)) %>%
    summarise(nTot = n(), aveScore = mean(class_Yes), .groups = 'drop') %>%
    mutate(absolute_favorable_class_balance = aveScore)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 20) > 0)
      adj_max_rate = max(temp$absolute_favorable_class_balance[temp$nTot >= 20])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_favorable_class_balance = ifelse(maxRate < 1.e-50, NA, absolute_favorable_class_balance / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_favorable_class_balance < 0.1)) %>%
    mutate(fairness = ifelse(relative_favorable_class_balance >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_favorable_class_balance,
                         relative_favorable_class_balance, fairness))
}

# calculate favorable class balance
getUnfavorableClassBalance = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Neg') %>%
    group_by(!!as.name(featureName)) %>%
    summarise(nTot = n(), aveScore = mean(class_Yes), .groups = 'drop') %>%
    mutate(absolute_unfavorable_class_balance = aveScore)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 20) > 0)
      adj_max_rate = max(temp$absolute_unfavorable_class_balance[temp$nTot >= 20])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_unfavorable_class_balance = absolute_unfavorable_class_balance / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_unfavorable_class_balance < 0.1)) %>%
    mutate(fairness = ifelse(relative_unfavorable_class_balance >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_unfavorable_class_balance,
                         relative_unfavorable_class_balance, fairness))
}

# calculate true favorable rate parity
getFavorableRateParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(target) == preferable_outcome) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows, values_fill = 0)
  if (! 'Pos' %in% names(temp)) temp = temp %>% mutate(Pos = 0)
  if (! 'Neg' %in% names(temp)) temp = temp %>% mutate(Neg = 0)
  temp = temp %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_favorable_rate_parity = Pos / nTot)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 1000 | temp$Pos >= 10) > 0)
      adj_max_rate = max(temp$absolute_favorable_rate_parity[temp$nTot >= 1000 | temp$Pos >= 10])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_favorable_rate_parity = ifelse(maxRate <= 1e-50, NA, absolute_favorable_rate_parity / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_favorable_rate_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_favorable_rate_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_favorable_rate_parity,
                         relative_favorable_rate_parity, fairness))
}

# calculate true unfavorable rate parity
getUnfavorableRateParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(target) != preferable_outcome) %>%
    group_by(!!as.name(featureName), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveResult, values_from = nRows, values_fill = 0)
  if (! 'Pos' %in% names(temp)) temp = temp %>% mutate(Pos = 0)
  if (! 'Neg' %in% names(temp)) temp = temp %>% mutate(Neg = 0)
  temp = temp %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_unfavorable_rate_parity = Neg / nTot)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 1000 | temp$Neg >= 10) > 0)
      adj_max_rate = max(temp$absolute_unfavorable_rate_parity[temp$nTot >= 1000 | temp$Neg >= 10])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_unfavorable_rate_parity = ifelse(maxRate <= 1e-50, NA, absolute_unfavorable_rate_parity / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_unfavorable_rate_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_unfavorable_rate_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_unfavorable_rate_parity,
                         relative_unfavorable_rate_parity, fairness))
}

# calculate favorable predictive value parity
getFavorablePredictiveValueParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(target) == preferable_outcome, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveTarget) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveTarget, values_from = nRows, values_fill = 0)
  if (! 'Pos' %in% names(temp)) temp = temp %>% mutate(Pos = 0)
  if (! 'Neg' %in% names(temp)) temp = temp %>% mutate(Neg = 0)
  temp = temp %>%
    mutate(nTot = Pos + Neg) %>%
    mutate(absolute_favorable_predictive_value_parity = Pos / nTot)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 1000 | temp$Pos > 10) > 0)
      adj_max_rate = max(temp$absolute_favorable_predictive_value_parity[temp$nTot >= 1000 | temp$Pos > 10])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_favorable_predictive_value_parity = ifelse(maxRate <= 1e-50, NA, absolute_favorable_predictive_value_parity / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_favorable_predictive_value_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_favorable_predictive_value_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_favorable_predictive_value_parity,
                         relative_favorable_predictive_value_parity, fairness))
}

# calculate unfavorable predictive value parity
getUnfavorablePredictiveValueParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
    mutate(positiveResult = ifelse(class_Yes <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult != 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(target) == preferable_outcome, 'Pos', 'Neg')) %>%
    group_by(!!as.name(featureName), positiveTarget) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(featureName), names_from = positiveTarget, values_from = nRows, values_fill = 0)
  if (! 'Pos' %in% names(temp)) temp = temp %>% mutate(Pos = 0)
  if (! 'Neg' %in% names(temp)) temp = temp %>% mutate(Neg = 0)
  temp = temp %>%
      mutate(nTot = Pos + Neg) %>%
      mutate(absolute_unfavorable_predictive_value_parity = Neg / nTot)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 1000 | temp$Pos > 10) > 0)
      adj_max_rate = max(temp$absolute_unfavorable_predictive_value_parity[temp$nTot >= 1000 | temp$Pos > 10])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_unfavorable_predictive_value_parity = ifelse(maxRate <= 1e-50, NA, absolute_unfavorable_predictive_value_parity / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_unfavorable_predictive_value_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_unfavorable_predictive_value_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(featureName), absolute_unfavorable_predictive_value_parity,
                         relative_unfavorable_predictive_value_parity, fairness))
}

#####################################################################################################################
# generic purpose function for plotting fairness metrics
# is called by wrapper functions specific to a type of metric
plotFairnessMetric = function(dat, title, metric, metric_name) {
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  dat = dat %>% mutate(fairness = factor(as.character(fairness), levels = labels))
  colours = c('blue','red', 'grey')[labels %in% dat$fairness]
  plt = ggplot(data = dat, aes(x = get(featureName), y = get(metric), fill = fairness)) +
    geom_col() +
    ggtitle(title) +
    xlab(featureName) +
    ylab(metric_name) +
    scale_fill_manual(values = colours)
  print(plt)
}
# plot proportional parity
plotProportionalParity = function(dat, title = 'Proportional Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_proportional_parity'),
                     metric_name = metric_name)
}
# plot equal parity
plotEqualParity = function(dat, title = 'Equal Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_equal_parity'),
                     metric_name = metric_name)
}
# plot favorable class balance
plotFavorableClassBalance = function(dat, title = 'Favorable Class Balance', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_favorable_class_balance'),
                     metric_name = metric_name)
}
# plot unfavorable class balance
plotFavorableClassBalance = function(dat, title = 'Unfavorable Class Balance', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_unfavorable_class_balance'),
                     metric_name = metric_name)
}
# plot favorable rate parity
plotFavorableRateParity = function(dat, title = 'Favorable Rate Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_favorable_rate_parity'),
                     metric_name = metric_name)
}
# plot unfavorable rate parity
plotUnfavorableRateParity = function(dat, title = 'Unfavorable Rate Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_unfavorable_rate_parity'),
                     metric_name = metric_name)
}
# plot favorable predictive value parity
plotFavorablePredictiveValueParity = function(dat, title = 'Favorable Predictive Value Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_favorable_predictive_value_parity'),
                     metric_name = metric_name)
}
# plot favorable predictive value parity
plotUnfavorablePredictiveValueParity = function(dat, title = 'Unfavorable Predictive Value Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_unfavorable_predictive_value_parity'),
                     metric_name = metric_name)
}


# generic purpose function for plotting fairness metric comparisons for two competing models
# is called by wrapper functions specific to a type of metric
plotBiasMetricComparison = function(dat1, label1, dat2, label2, title, metric)
{
  feature_name = head(names(dat1), 1)
  plot_data = bind_rows(list(dat1 %>% mutate(Model = label1),
                             dat2 %>% mutate(Model = label2))) %>%
    mutate(Model = factor(Model, levels = c(label1, label2)))
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% plot_data$fairness]
  plt = ggplot(data = plot_data, aes(x = get(feature_name), y = get(metric), fill = fairness)) +
    geom_col() +
    ggtitle(paste0(title, ' Comparison')) +
    xlab(feature_name) +
    ylab(paste0('Relative ', title)) +
    scale_fill_manual(values = colours) +
    facet_wrap(~ Model) +
    theme(axis.text.x=element_text(angle=90,hjust=1))
  print(plt)
}
plotProportionalParityComparison = function(dat1, label1, dat2, label2) {
  title = 'Proportional Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotEqualParityComparison = function(dat1, label1, dat2, label2) {
  title = 'Equal Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotFavorableClassBalanceComparison = function(dat1, label1, dat2, label2) {
  title = 'Favorable Class Balance'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotUnfavorableClassBalanceComparison = function(dat1, label1, dat2, label2) {
  title = 'Unfavorable Class Balance'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotFavorableRateParityComparison = function(dat1, label1, dat2, label2) {
  title = 'Favorable Rate Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotUnfavorableRateParityComparison = function(dat1, label1, dat2, label2) {
  title = 'Unfavorable Rate Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotFavorablePredictiveValueParityComparison = function(dat1, label1, dat2, label2) {
  title = 'Favorable Predictive Value Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotUnfavorablePredictiveValueParityComparison = function(dat1, label1, dat2, label2) {
  title = 'Unfavorable Predictive Value Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}

# summarise the bias metrics across a range of thresholds
biasMetricFunctions = c('getProportionalParity',
                        'getEqualParity',
                        'getFavorableClassBalance',
                        'getUnfavorableClassBalance',
                        'getFavorableRateParity',
                        'getUnfavorableRateParity',
                        'getFavorablePredictiveValueParity',
                        'getUnfavorablePredictiveValueParity'
)
toMetricName = function(x) {
  i = which(x == biasMetricFunctions)
  names = c('Proportional Parity',
            'Equal Parity',
            'Favorable Class Balance',
            'Unfavorable Class Balance',
            'Favorable Rate Parity',
            'Unfavorable Rate Parity',
            'Favorable Predictive Value Parity',
            'Unfavorable Predictive Value Parity'
  )
  return(names[i])
}
calcBiasMetric = function(fn, merged_data, protected_feature, thresh) {
  if (length(thresh) > 1) return(calcBiasMetricMultiThresh(fn, merged_data, protected_feature, thresh))
  if (fn == 'getProportionalParity') return(getProportionalParity(merged_data, protected_feature, thresh))
  if (fn == 'getEqualParity') return(getEqualParity(merged_data, protected_feature, thresh))
  if (fn == 'getFavorableClassBalance') return(getFavorableClassBalance(merged_data, protected_feature, thresh))
  if (fn == 'getUnfavorableClassBalance') return(getUnfavorableClassBalance(merged_data, protected_feature, thresh))
  if (fn == 'getFavorableRateParity') return(getFavorableRateParity(merged_data, protected_feature, thresh))
  if (fn == 'getUnfavorableRateParity') return(getUnfavorableRateParity(merged_data, protected_feature, thresh))
  if (fn == 'getFavorablePredictiveValueParity') return(getFavorablePredictiveValueParity(merged_data, protected_feature, thresh))
  if (fn == 'getUnfavorablePredictiveValueParity') return(getUnfavorablePredictiveValueParity(merged_data, protected_feature, thresh))
  stop(paste0('Unknown function: ', fn))
}
calcBiasMetricMultiThresh = function(fn, merged_data, protected_feature, thresh) {
  base_result = calcBiasMetric(fn, merged_data, protected_feature, thresh[1])
  for (i in 2:(length(thresh))) {
    next_result = calcBiasMetric(fn, merged_data, protected_feature, thresh[i])
    j1 = seq_len(nrow(base_result))[base_result[, 1] == names(thresh)[i]]
    j2 = seq_len(nrow(next_result))[next_result[, 1] == names(thresh)[i]]
    base_result[j1, 2] = next_result[j2, 2]
  }
  base_result = base_result[, seq_len(2)]
  return(base_result)
}

getBiasMetricSummary = function(merged_data, profit_curve, protected_feature) {
  bias_metric_summary = bind_rows(lapply(biasMetricFunctions, function(fn) {
    bias_temp = bind_rows(lapply(profit_curve$threshold, function(thresh) {
      bias = calcBiasMetric(fn, merged_data, protected_feature, thresh) %>%
        filter(fairness != 'Not Enough Data')
      metric = names(bias)[3]
      gb = mean(bias[, 3] < 0.8)
      asb = mean(unlist(bias[bias[, 3] < 0.8, 3]))
      if (nrow(bias) == 0) {
        gb = NA
        asb = NA
      }
      result = tibble(protectedFeature = protected_feature,
                      biasMetric = metric,
                      threshold = thresh,
                      groupsBelow = gb,
                      aveRelativityBelow = asb
      ) %>%
        mutate(aveRelativityBelow = ifelse(is.nan(aveRelativityBelow), NA, aveRelativityBelow))
      return(result)
    }))
    return(bias_temp)
  }))
  return(bias_metric_summary)
}

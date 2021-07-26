################################################################################
# Helper functions for bias detection and plots
################################################################################
library(tidyverse)
library(magrittr)

# Functions all expect a data frame as input, with columns corresponding to:
# predict_name: the predicted value as a probability
# feature_name: the name of the protected feature
#
# Additionally, a prediction threshold is expected, and an optional fairness
# threshold.
# Subsets of functions also require knowledge of the actual outcome (expected
# as a column in the data frame), and the value corresponding to the preferable
# outcome.

# calculate proportional parity
getProportionalParity <- function(data, predict_name, feature_name, thresh, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    group_by(!!as.name(feature_name), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(feature_name), names_from = positiveResult, values_from = nRows, values_fill = 0)
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
    mutate(fairness = ifelse(relative_proportional_parity >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_proportional_parity,
                         relative_proportional_parity, fairness))
}

# calculate equal parity
getEqualParity <- function(data, predict_name, feature_name, thresh, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    group_by(!!as.name(feature_name), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(feature_name), names_from = positiveResult, values_from = nRows, values_fill = 0)
  if (! 'Pos' %in% names(temp)) temp = temp %>% mutate(Pos = 0)
  if (! 'Neg' %in% names(temp)) temp = temp %>% mutate(Neg = 0)
  temp = temp %>%
    mutate(nTot = Neg + Pos) %>%
    mutate(absolute_equal_parity = Pos)
  adj_max_rate = max(temp$absolute_equal_parity[temp$nTot >= 1000 | temp$Pos > 10])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_equal_parity = ifelse(maxRate <= 1e-50, 1, absolute_equal_parity / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_equal_parity < 0.1)) %>%
    mutate(fairness = ifelse(relative_equal_parity >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_equal_parity,
                         relative_equal_parity, fairness))
}

# calculate favorable class balance
getFavorableClassBalance <- function(data, predict_name, feature_name, thresh, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    group_by(!!as.name(feature_name)) %>%
    summarise(nTot = n(), aveScore = mean(!!as.name(predict_name)), .groups = 'drop') %>%
    mutate(absolute_favorable_class_balance = aveScore)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 20) > 0)
      adj_max_rate = max(temp$absolute_favorable_class_balance[temp$nTot >= 20])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_favorable_class_balance = ifelse(maxRate < 1.e-50, NA, absolute_favorable_class_balance / maxRate)) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_favorable_class_balance < 0.1)) %>%
    mutate(fairness = ifelse(relative_favorable_class_balance >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_favorable_class_balance,
                         relative_favorable_class_balance, fairness))
}

# calculate favorable class balance
getUnfavorableClassBalance <- function(data, predict_name, feature_name, thresh, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Neg') %>%
    group_by(!!as.name(feature_name)) %>%
    summarise(nTot = n(), aveScore = mean(!!as.name(predict_name)), .groups = 'drop') %>%
    mutate(absolute_unfavorable_class_balance = aveScore)
  adj_max_rate = 0
  if (nrow(temp) > 0)
    if (sum(temp$nTot >= 20) > 0)
      adj_max_rate = max(temp$absolute_unfavorable_class_balance[temp$nTot >= 20])
  temp = temp %>%
    mutate(maxRate = adj_max_rate) %>%
    mutate(relative_unfavorable_class_balance = absolute_unfavorable_class_balance / maxRate) %>%
    mutate(isSmall = (nTot < 100) | (nTot >= 100 & nTot <= 1000 & relative_unfavorable_class_balance < 0.1)) %>%
    mutate(fairness = ifelse(relative_unfavorable_class_balance >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_unfavorable_class_balance,
                         relative_unfavorable_class_balance, fairness))
}

# calculate true favorable rate parity
getFavorableRateParity <- function(data, predict_name, feature_name, thresh, actual_name, preferable_outcome, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(actual_name) == preferable_outcome) %>%
    group_by(!!as.name(feature_name), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(feature_name), names_from = positiveResult, values_from = nRows, values_fill = 0)
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
    mutate(fairness = ifelse(relative_favorable_rate_parity >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_favorable_rate_parity,
                         relative_favorable_rate_parity, fairness))
}

# calculate true unfavorable rate parity
getUnfavorableRateParity <- function(data, predict_name, feature_name, thresh, actual_name, preferable_outcome, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(actual_name) != preferable_outcome) %>%
    group_by(!!as.name(feature_name), positiveResult) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(feature_name), names_from = positiveResult, values_from = nRows, values_fill = 0)
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
    mutate(fairness = ifelse(relative_unfavorable_rate_parity >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_unfavorable_rate_parity,
                         relative_unfavorable_rate_parity, fairness))
}

# calculate favorable predictive value parity
getFavorablePredictiveValueParity <- function(data, predict_name, feature_name, thresh, actual_name, preferable_outcome, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(actual_name) == preferable_outcome, 'Pos', 'Neg')) %>%
    group_by(!!as.name(feature_name), positiveTarget) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(feature_name), names_from = positiveTarget, values_from = nRows, values_fill = 0)
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
    mutate(fairness = ifelse(relative_favorable_predictive_value_parity >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_favorable_predictive_value_parity,
                         relative_favorable_predictive_value_parity, fairness))
}

# calculate unfavorable predictive value parity
getUnfavorablePredictiveValueParity <- function(data, predict_name, feature_name, thresh, actual_name, preferable_outcome, fairness_threshold = 0.8) {
  temp <- getCleanedNames(data, project) %>%
    mutate(positiveResult = ifelse(!!as.name(predict_name) <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult != 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(actual_name) == preferable_outcome, 'Pos', 'Neg')) %>%
    group_by(!!as.name(feature_name), positiveTarget) %>%
    summarise(nRows = n(), .groups = 'drop') %>%
    pivot_wider(id_cols = !!as.name(feature_name), names_from = positiveTarget, values_from = nRows, values_fill = 0)
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
    mutate(fairness = ifelse(relative_unfavorable_predictive_value_parity >= fairness_threshold, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_unfavorable_predictive_value_parity,
                         relative_unfavorable_predictive_value_parity, fairness))
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
toMetricName <- function(x) {
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
calcBiasMetric <- function(fn, data, predict_name, protected_feature, thresh, actual_name, preferable_outcome) {
  if (length(thresh) > 1) return(calcBiasMetricMultiThresh(fn, data, predict_name, protected_feature, thresh))
  if (fn == 'getProportionalParity') return(getProportionalParity(data, predict_name, protected_feature, thresh))
  if (fn == 'getEqualParity') return(getEqualParity(data, predict_name, protected_feature, thresh))
  if (fn == 'getFavorableClassBalance') return(getFavorableClassBalance(data, predict_name, protected_feature, thresh))
  if (fn == 'getUnfavorableClassBalance') return(getUnfavorableClassBalance(data, predict_name, protected_feature, thresh))
  if (fn == 'getFavorableRateParity') return(getFavorableRateParity(data, predict_name, protected_feature, thresh, actual_name, preferable_outcome))
  if (fn == 'getUnfavorableRateParity') return(getUnfavorableRateParity(data, predict_name, protected_feature, thresh, actual_name, preferable_outcome))
  if (fn == 'getFavorablePredictiveValueParity') return(getFavorablePredictiveValueParity(data, predict_name, protected_feature, thresh, actual_name, preferable_outcome))
  if (fn == 'getUnfavorablePredictiveValueParity') return(getUnfavorablePredictiveValueParity(data, predict_name, protected_feature, thresh, actual_name, preferable_outcome))
  stop(paste0('Unknown function: ', fn))
}
calcBiasMetricMultiThresh <- function(fn, data, predict_name, protected_feature, thresh, actual_name, preferable_outcome) {
  base_result = calcBiasMetric(fn, data, predict_name, protected_feature, thresh[1], actual_name, preferable_outcome)
  for (i in 2:(length(thresh))) {
    next_result = calcBiasMetric(fn, data, predict_name, protected_feature, thresh[i], actual_name, preferable_outcome)
    j1 = seq_len(nrow(base_result))[base_result[, 1] == names(thresh)[i]]
    j2 = seq_len(nrow(next_result))[next_result[, 1] == names(thresh)[i]]
    base_result[j1, 2] = next_result[j2, 2]
  }
  base_result = base_result[, seq_len(2)]
  return(base_result)
}

getBiasMetricSummary <- function(data, profit_curve, predict_name, protected_feature, actual_name, preferable_outcome, fairness_threshold = 0.8) {
  bias_metric_summary = bind_rows(lapply(biasMetricFunctions, function(fn) {
    bias_temp = bind_rows(lapply(profit_curve$threshold, function(thresh) {
      bias = calcBiasMetric(fn, data, predict_name, protected_feature, thresh, actual_name, preferable_outcome) %>%
        filter(fairness != 'Not Enough Data')
      metric = names(bias)[3]
      if (nrow(bias) == 0) {
        gb = NA
        asb = NA
      } else {
        gb = mean(bias[, 3] < fairness_threshold)
        asb = mean(unlist(bias[bias[, 3] < fairness_threshold, 3]))
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


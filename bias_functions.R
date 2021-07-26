################################################################################
# Helper functions for bias detection and plots
################################################################################
library(tidyverse)
library(magrittr)
#source('datarobot_helpers.R')

# calculate proportional parity
getProportionalParity <- function(merged_data, feature_name, thresh) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
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
    mutate(fairness = ifelse(relative_proportional_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_proportional_parity,
                         relative_proportional_parity, fairness))
}

# calculate equal parity
getEqualParity <- function(merged_data, feature_name, thresh) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
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
    mutate(fairness = ifelse(relative_equal_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_equal_parity,
                         relative_equal_parity, fairness))
}

# calculate favorable class balance
getFavorableClassBalance <- function(merged_data, feature_name, thresh) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    group_by(!!as.name(feature_name)) %>%
    summarise(nTot = n(), aveScore = mean(probability), .groups = 'drop') %>%
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
  return(temp %>% select(!!as.name(feature_name), absolute_favorable_class_balance,
                         relative_favorable_class_balance, fairness))
}

# calculate favorable class balance
getUnfavorableClassBalance <- function(merged_data, feature_name, thresh) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Neg') %>%
    group_by(!!as.name(feature_name)) %>%
    summarise(nTot = n(), aveScore = mean(probability), .groups = 'drop') %>%
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
  return(temp %>% select(!!as.name(feature_name), absolute_unfavorable_class_balance,
                         relative_unfavorable_class_balance, fairness))
}

# calculate true favorable rate parity
getFavorableRateParity <- function(merged_data, feature_name, thresh, config) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(config$target) == config$preferable_outcome) %>%
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
    mutate(fairness = ifelse(relative_favorable_rate_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_favorable_rate_parity,
                         relative_favorable_rate_parity, fairness))
}

# calculate true unfavorable rate parity
getUnfavorableRateParity <- function(merged_data, feature_name, thresh, config) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
    filter(!!as.name(config$target) != config$preferable_outcome) %>%
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
    mutate(fairness = ifelse(relative_unfavorable_rate_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_unfavorable_rate_parity,
                         relative_unfavorable_rate_parity, fairness))
}

# calculate favorable predictive value parity
getFavorablePredictiveValueParity <- function(merged_data, feature_name, thresh, config) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult == 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(config$target) == config$preferable_outcome, 'Pos', 'Neg')) %>%
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
    mutate(fairness = ifelse(relative_favorable_predictive_value_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
    mutate(fairness = ifelse(isSmall, 'Not Enough Data', fairness))
  return(temp %>% select(!!as.name(feature_name), absolute_favorable_predictive_value_parity,
                         relative_favorable_predictive_value_parity, fairness))
}

# calculate unfavorable predictive value parity
getUnfavorablePredictiveValueParity <- function(merged_data, feature_name, thresh, config) {
  temp <- getCleanedNames(merged_data, project) %>%
    mutate(positiveResult = ifelse(probability <= thresh, 'Pos', 'Neg')) %>%
    filter(positiveResult != 'Pos') %>%
    mutate(positiveTarget = ifelse(!!as.name(config$target) == config$preferable_outcome, 'Pos', 'Neg')) %>%
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
    mutate(fairness = ifelse(relative_unfavorable_predictive_value_parity >= 0.8, 'Above fairness threshold', 'Below fairness threshold')) %>%
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
calcBiasMetric <- function(fn, merged_data, protected_feature, thresh, config) {
  if (length(thresh) > 1) return(calcBiasMetricMultiThresh(fn, merged_data, protected_feature, thresh))
  if (fn == 'getProportionalParity') return(getProportionalParity(merged_data, protected_feature, thresh))
  if (fn == 'getEqualParity') return(getEqualParity(merged_data, protected_feature, thresh))
  if (fn == 'getFavorableClassBalance') return(getFavorableClassBalance(merged_data, protected_feature, thresh))
  if (fn == 'getUnfavorableClassBalance') return(getUnfavorableClassBalance(merged_data, protected_feature, thresh))
  if (fn == 'getFavorableRateParity') return(getFavorableRateParity(merged_data, protected_feature, thresh, config))
  if (fn == 'getUnfavorableRateParity') return(getUnfavorableRateParity(merged_data, protected_feature, thresh, config))
  if (fn == 'getFavorablePredictiveValueParity') return(getFavorablePredictiveValueParity(merged_data, protected_feature, thresh, config))
  if (fn == 'getUnfavorablePredictiveValueParity') return(getUnfavorablePredictiveValueParity(merged_data, protected_feature, thresh, config))
  stop(paste0('Unknown function: ', fn))
}
calcBiasMetricMultiThresh <- function(fn, merged_data, protected_feature, thresh) {
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

getBiasMetricSummary <- function(merged_data, profit_curve, protected_feature, config) {
  bias_metric_summary = bind_rows(lapply(biasMetricFunctions, function(fn) {
    bias_temp = bind_rows(lapply(profit_curve$threshold, function(thresh) {
      bias = calcBiasMetric(fn, merged_data, protected_feature, thresh, config) %>%
        filter(fairness != 'Not Enough Data')
      metric = names(bias)[3]
      if (nrow(bias) == 0) {
        gb = NA
        asb = NA
      } else {
        gb = mean(bias[, 3] < 0.8)
        asb = mean(unlist(bias[bias[, 3] < 0.8, 3]))
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


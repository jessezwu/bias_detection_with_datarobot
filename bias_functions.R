################################################################################
# Helper functions for bias detection and plots
################################################################################
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
source('datarobot_helpers.R')

plotProfitCurveComparison <- function(pc1, pc1_label, pc2, pc2_label) {
  plot_data = bind_rows(list(pc1 %>% mutate(Model = pc1_label),
                             pc2 %>% mutate(Model = pc2_label))) %>%
    mutate(Model = factor(Model, levels = c(pc1_label, pc2_label)))
  plt = ggplot(data = plot_data, aes(x = threshold, y = profit, colour = Model)) +
    geom_line() +
    theme_minimal() +
    ggtitle('Profit Curve Comparison') +
    scale_y_continuous(labels=scales::dollar_format()) +
    scale_colour_manual(values = c('blue', 'red'))
  print(plt)
}

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

################################################################################
# Plot PSI along with Feature Impact
plotPSI <- function(plot_data, feature, group) {
  ggplot(plot_data, aes(x = impactNormalized, y = PSI, colour = Impact, label = Feature)) +
    geom_point() +
    theme_minimal() +
    geom_text_repel(size = 2.5) +
    scale_colour_manual(values = c('green2', 'yellow3', 'red')) +
    ggtitle('Cross-Class Data Disparity',
            subtitle = paste('Feature =', feature, '| Group =', group)) +
    xlab('Importance') +
    ylab('Data Disparity')
}

################################################################################
# generic purpose function for plotting fairness metrics
# is called by wrapper functions specific to a type of metric
plotFairnessMetric <- function(dat, title, metric, metric_name, feature_name) {
  labels <- c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  dat <- dat %>% mutate(fairness = factor(as.character(fairness), levels = labels))
  colours <- c('blue','red', 'grey')[labels %in% dat$fairness]
  plt <- ggplot(data = dat, aes(x = get(feature_name), y = get(metric), fill = fairness)) +
    geom_col() +
    theme_minimal() +
    ggtitle(title) +
    xlab(feature_name) +
    ylab(metric_name) +
    scale_fill_manual(values = colours)
  print(plt)
}
# plot proportional parity
plotProportionalParity <- function(dat, feature_name, title = 'Proportional Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_proportional_parity'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}
# plot equal parity
plotEqualParity <- function(dat, feature_name, title = 'Equal Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_equal_parity'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}
# plot favorable class balance
plotFavorableClassBalance <- function(dat, feature_name, title = 'Favorable Class Balance', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_favorable_class_balance'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}
# plot unfavorable class balance
plotUnfavorableClassBalance <- function(dat, feature_name, title = 'Unfavorable Class Balance', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_unfavorable_class_balance'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}
# plot favorable rate parity
plotFavorableRateParity <- function(dat, feature_name, title = 'Favorable Rate Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_favorable_rate_parity'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}
# plot unfavorable rate parity
plotUnfavorableRateParity <- function(dat, feature_name, title = 'Unfavorable Rate Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_unfavorable_rate_parity'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}
# plot favorable predictive value parity
plotFavorablePredictiveValueParity <- function(dat, feature_name, title = 'Favorable Predictive Value Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_favorable_predictive_value_parity'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}
# plot favorable predictive value parity
plotUnfavorablePredictiveValueParity <- function(dat, feature_name, title = 'Unfavorable Predictive Value Parity', absolute = TRUE) {
  prefix = ifelse(absolute, 'Absolute', 'Relative')
  metric_name = paste0(prefix, ' ', title)
  plotFairnessMetric(dat, title,
                     metric = paste0(tolower(prefix), '_unfavorable_predictive_value_parity'),
                     metric_name = metric_name,
                     feature_name = feature_name)
}


# generic purpose function for plotting fairness metric comparisons for two competing models
# is called by wrapper functions specific to a type of metric
plotBiasMetricComparison <- function(dat1, label1, dat2, label2, title, metric)
{
  feature_name = head(names(dat1), 1)
  plot_data = bind_rows(list(dat1 %>% mutate(Model = label1),
                             dat2 %>% mutate(Model = label2))) %>%
    mutate(Model = factor(Model, levels = c(label1, label2)))
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% plot_data$fairness]
  plt = ggplot(data = plot_data, aes(x = get(feature_name), y = get(metric), fill = fairness)) +
    geom_col() +
    theme_minimal() +
    ggtitle(paste0(title, ' Comparison')) +
    xlab(feature_name) +
    ylab(paste0('Relative ', title)) +
    scale_fill_manual(values = colours) +
    facet_wrap(~ Model) +
    theme(axis.text.x=element_text(angle=90,hjust=1))
  print(plt)
}
plotProportionalParityComparison <- function(dat1, label1, dat2, label2) {
  title = 'Proportional Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotEqualParityComparison <- function(dat1, label1, dat2, label2) {
  title = 'Equal Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotFavorableClassBalanceComparison <- function(dat1, label1, dat2, label2) {
  title = 'Favorable Class Balance'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotUnfavorableClassBalanceComparison <- function(dat1, label1, dat2, label2) {
  title = 'Unfavorable Class Balance'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotFavorableRateParityComparison <- function(dat1, label1, dat2, label2) {
  title = 'Favorable Rate Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotUnfavorableRateParityComparison <- function(dat1, label1, dat2, label2) {
  title = 'Unfavorable Rate Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotFavorablePredictiveValueParityComparison <- function(dat1, label1, dat2, label2) {
  title = 'Favorable Predictive Value Parity'
  metric = names(dat1)[3]
  plotBiasMetricComparison(dat1, label1, dat2, label2, title, metric)
}
plotUnfavorablePredictiveValueParityComparison <- function(dat1, label1, dat2, label2) {
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

# get set of thresholds for a protected feature that try to mitigate bias
getPerClassThresholdTable <- function(merged_data, feature, profit_threshold, config) {
  # what is the largest/larger group within a protected feature?
  # use that cutoff as the base against which everything is compared
  find_largest <- merged_data %>%
                  group_by(!!as.name(feature)) %>%
                  summarise(nRows = n()) %>%
                  arrange(desc(nRows))
  largest_group <- unname(unlist(find_largest[1, 1]))

  # debias proportional parity
  threshold_table <- tibble(Group = unlist(find_largest[, 1]))
  for (metric in biasMetricFunctions) {
    # for each group, other than the largest, change the threshold to bias
    # loop though one group at a time and get its absolute bias metric to as close to the largest group as possible
    n = nrow(find_largest) - 1
    best_group_thresholds = rep(optimal_threshold_for_profit, n + 1)
    names(best_group_thresholds) = unlist(find_largest[, 1])
    target_bias = calcBiasMetric(metric, merged_data, feature, optimal_threshold_for_profit, config) %>%
      filter(!!as.name(feature) == largest_group)
    target_bias = unlist(unname(target_bias[1, 2]))
    for (i in 2:nrow(find_largest)) {
      group_name = unname(unlist(find_largest[i, 1]))
      group_thresholds = best_group_thresholds
      best_score = 9e99
      best_thresh = optimal_threshold_for_profit
      for (j in seq_len(99)) {
        temp_thresh = 0.01 * j
        temp = calcBiasMetric(metric, merged_data, feature, temp_thresh, config) %>%
          filter(!!as.name(feature) == group_name)
        if (nrow(temp) > 0) {
          temp = unlist(unname(temp[1, 2]))
          score = abs(target_bias - temp)
          if (score < best_score) {
            best_score = score
            best_thresh = temp_thresh
          }
        }
      }
      x = best_thresh
      for (j in seq_len(20)) {
        temp_thresh = x + 0.001 * (j - 10)
        temp = calcBiasMetric(metric, merged_data, feature, temp_thresh, config) %>%
          filter(!!as.name(feature) == group_name)
        if (nrow(temp) > 0) {
          temp = unlist(unname(temp[1, 2]))
          score = abs(target_bias - temp)
          if (score < best_score) {
            best_score = score
            best_thresh = temp_thresh
          }
        }
      }
      best_group_thresholds[i] = best_thresh
    }
    #bias_measures = calcBiasMetric(metric, merged_data, feature, best_group_thresholds)
    threshold_table = threshold_table %>%
      mutate(!!gsub(' ', '', toMetricName(metric), fixed = TRUE) := unname(best_group_thresholds))
  }
  threshold_table
}

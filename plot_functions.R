library(ggplot2)
library(scales)
library(ggrepel)
library(ggwordcloud)
source('bias_functions.R')

################################################################################

# Plot two profit curves against each other
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
# Bias plots
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

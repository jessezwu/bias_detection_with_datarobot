library(tidyverse)
library(magrittr)

# library(creditmodel)
# library(lubridate)
# library(anytime)

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

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

# calculate proportional parity
getProportionParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
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

# plot proportional parity
plotProportionalParity = function(pp, title = 'Proportional Parity', absolute = TRUE) {
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  if (absolute) {
    plt = ggplot(data = pp, aes(x = get(featureName), y = absolute_proportional_parity, fill = fairness)) +
      geom_col() + 
      ggtitle(title) +
      xlab(featureName) +
      ylab('Absolute Proportional Parity') +
      scale_fill_manual(values = colours)
    print(plt)
  } else {
    plt = ggplot(data = pp, aes(x = get(featureName), y = relative_proportional_parity, fill = fairness)) +
      geom_col() + 
      ggtitle(title) +
      xlab(featureName) +
      ylab('Relative Proportional Parity') +
      scale_fill_manual(values = colours) +
      theme(axis.text.x=element_text(angle=90,hjust=1))
    print(plt)
  }
}
plotProportionalParityComparison = function(pp1, pp1_label, pp2, pp2_label) {
  title = 'Proportional Parity Comparison'
  feature_name = head(names(pp1), 1)
  plot_data = bind_rows(list(pp1 %>% mutate(Model = pp1_label),
                             pp2 %>% mutate(Model = pp2_label))) %>%
    mutate(Model = factor(Model, levels = c(pp1_label, pp2_label)))
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% plot_data$fairness]
  plt = ggplot(data = plot_data, aes(x = get(feature_name), y = relative_proportional_parity, fill = fairness)) +
      geom_col() + 
      ggtitle(title) +
      xlab(feature_name) +
      ylab('Relative Proportional Parity') +
      scale_fill_manual(values = colours) +
      facet_wrap(~ Model) +
      theme(axis.text.x=element_text(angle=90,hjust=1))
  print(plt)
}

# calculate equal parity
getEqualParity = function(merged_data, featureName, thresh) {
  temp = merged_data %>%
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

# plot proportional parity
plotEqualParity = function(eqp, title = 'Equal Parity', absolute = TRUE) {
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% pp$fairness]
  if (absolute) {
    plt = ggplot(data = eqp, aes(x = get(featureName), y = absolute_equal_parity, fill = fairness)) +
      geom_col() + 
      ggtitle(title) +
      xlab(featureName) +
      ylab('Absolute Equal Parity') +
      scale_fill_manual(values = colours)
    print(plt)
  } else {
    plt = ggplot(data = eqp, aes(x = get(featureName), y = relative_equal_parity, fill = fairness)) +
      geom_col() + 
      ggtitle(title) +
      xlab(featureName) +
      ylab('Relative Equal Parity') +
      scale_fill_manual(values = colours) +
      theme(axis.text.x=element_text(angle=90,hjust=1))
    print(plt)
  }
}
plotEqualParityComparison = function(eqp1, eqp1_label, eqp2, eqp2_label) {
  title = 'Equal Parity Comparison'
  feature_name = head(names(eqp1), 1)
  plot_data = bind_rows(list(eqp1 %>% mutate(Model = eqp1_label),
                             eqp2 %>% mutate(Model = eqp2_label))) %>%
    mutate(Model = factor(Model, levels = c(eqp1_label, eqp2_label)))
  labels = c('Above fairness threshold', 'Below fairness threshold', 'Not Enough Data')
  colours = c('blue','red', 'grey')[labels %in% plot_data$fairness]
  plt = ggplot(data = plot_data, aes(x = get(feature_name), y = relative_equal_parity, fill = fairness)) +
    geom_col() + 
    ggtitle(title) +
    xlab(feature_name) +
    ylab('Relative Equal Parity') +
    scale_fill_manual(values = colours) +
    facet_wrap(~ Model) +
    theme(axis.text.x=element_text(angle=90,hjust=1))
  print(plt)
}


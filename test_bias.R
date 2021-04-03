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

source('helper_functions.R')

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
# we will manually calculate the profit, accuracy, and bias metrics

model <- GetModel(best_model$projectId, best_model$modelId)
trainingPredictions <- getStackedPredictions(project, model)

# merge the training predictions with the training data
trainingData = read_csv(filename)
mergedData = bind_cols(trainingData, trainingPredictions)

# get the profit curve
profitCurve = getProfitCurve(mergedData, target, payoff_matrix)
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

# calculate the fairness metrics using the optimal threshold for profit
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
for (featureName in protected) {
  pp = getProportionParity(mergedData, featureName, optimalThresholdForProfit)
  plotProportionalParity(pp)
}

# calculate and plot equal parity
for (featureName in protected) {
  eqp = getEqualParity(mergedData, featureName, optimalThresholdForProfit)
  plotEqualParity(eqp)
}

# calculate and plot favorable class balance
for (featureName in protected) {
  fcb = getFavorableClassBalance(mergedData, featureName, optimalThresholdForProfit)
  plotFavorableClassBalance(fcb)
}

# calculate and plot favorable class balance
for (featureName in protected) {
  ucb = getUnfavorableClassBalance(mergedData, featureName, optimalThresholdForProfit)
  plotFavorableClassBalance(ucb)
}

# calculate and plot true favorable rate parity
for (featureName in protected) {
  frp = getFavorableRateParity(mergedData, featureName, optimalThresholdForProfit)
  plotFavorableRateParity(frp)
}

# calculate and plot true unfavorable rate parity
for (featureName in protected) {
  urp = getUnfavorableRateParity(mergedData, featureName, optimalThresholdForProfit)
  plotUnfavorableRateParity(urp)
}

# calculate and plot favorable predictive value parity
for (featureName in protected) {
  pvp = getFavorablePredictiveValueParity(mergedData, featureName, optimalThresholdForProfit)
  plotFavorablePredictiveValueParity(pvp)
}

# calculate and plot unfavorable predictive value parity
for (featureName in protected) {
  upvp = getUnfavorablePredictiveValueParity(mergedData, featureName, optimalThresholdForProfit)
  plotUnfavorablePredictiveValueParity(upvp)
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
indirect_group = list()
for (protected_feature in protected) {
  group_list = unique(unlist(mergedData %>% select(all_of(protected_feature))))
  if (length(group_list) == 2) {
    target_group = head(group_list, 1)
    temp_data = mergedData %>% 
      select(all_of(c(raw_features, protected_feature)))
    prj = StartProject(dataSource = temp_data,
                       projectName = paste0('Find indirect bias - ', protected_feature),
                       target = protected_feature,
                       positiveClass = target_group,
                       mode = 'quick',
                       workerCount = 'max')
    i = 1 + length(indirect_projects)
    indirect_projects[[i]] = prj
    indirect_protected[[i]] = protected_feature
    indirect_group[[i]] = target_group
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
      indirect_group[[i]] = target_group
    }
  }
}
for (prj in indirect_projects) WaitForAutopilot(prj)

# get insights from the projects we just built
for (i in seq_len(length(indirect_projects))) {
  prj = indirect_projects[[i]]
  protected_feature = indirect_protected[[i]]
  protected_group = indirect_group[[i]]
  cat(paste0('Analysing feature: ', protected_feature, ' and group: ', protected_group, '\n'))
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
    ggtitle('Feature Impact', subtitle = paste0('Potential proxies for: ', protected_feature, ' and group: ', protected_group)) +
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
      top_n(80, weight)
    colours = c('blue', 'grey', 'red')[sort(unique(indirect_wordcloud$colour_num))]
    subtitle = paste0('Text feature = ', text_feature)
    if (! is.null(positive_class) & length(positive_class) > 0)
      subtitle = paste0(subtitle, '\nRed colour is ', protected_group)
    set.seed(123)
    plt = ggplot(data = indirect_wordcloud, aes(label = ngram, size = ngram_count, color = effect)) +
      geom_text_wordcloud_area(area_corr_power = 1, shape = 'square', rm_outside = TRUE) +
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

raw_features_removal_V1 = raw_features[raw_features != 'zip_code']
feature_list_V1 = CreateFeaturelist(project, 'remove bias V1', raw_features_removal_V1)
UpdateProject(project, holdoutUnlocked = TRUE)
jobID = RequestNewModel(
  project,
  model,
  featurelist = feature_list_V1,
  samplePct = model$samplePct
)
WaitForJobToComplete(project, jobID)
model_V1 = GetModelFromJobId(project, jobID)

# show the effect upon accuracy of removing zip_code 
leaderboard = as.data.frame(ListModels(project)) %>%
  filter(modelId %in% c(model$modelId, model_V1$modelId))
leaderboard$crossValidationMetric = sapply(leaderboard$modelId, function(x) return(GetModel(project, x)$metrics$LogLoss$crossValidation))
leaderboard$holdoutMetric = sapply(leaderboard$modelId, function(x) return(GetModel(project, x)$metrics$LogLoss$holdout))
leaderboard = leaderboard %>% 
  select(modelType, featurelistName, samplePct, validationMetric, crossValidationMetric, holdoutMetric) %>%
  arrange(holdoutMetric)
ggplot(data = leaderboard) +
  geom_col(aes(x = featurelistName, y = crossValidationMetric)) +
  ggtitle('Change in Accuracy After Indirect Bias Feature Removal') +
  xlab('Feature List Name') +
  ylab('Cross-Validation Log-Loss')
ggplot(data = leaderboard) +
  geom_col(aes(x = featurelistName, y = holdoutMetric)) +
  ggtitle('Change in Accuracy After Indirect Bias Feature Removal') +
  xlab('Feature List Name') +
  ylab('Holdout Log-Loss')

# show the effect of removing zip_code upon profit curve
training_predictions_V1 <- getStackedPredictions(project, model_V1)
merged_data_V1 = bind_cols(trainingData, training_predictions_V1)
profit_curve_V1 = getProfitCurve(merged_data_V1, target, payoff_matrix)
optimal_threshold_profit_V1 = profit_curve_V1$threshold[which.max(profit_curve_V1$profit)]
merged_data_V1 = merged_data_V1 %>%
  mutate(positiveResult = ifelse(class_Yes <= optimal_threshold_profit_V1, 'Pos', 'Neg'))
ggplot(data = tibble(model = c('Original', 'Zip-Code Feature Removed'), 
                     profit = c(max(profitCurve$profit), max(profit_curve_V1$profit)))) +
  geom_col(aes(x = model, y = profit)) +
  ggtitle('Profit Effect of Indirect Discrimination Removal') +
  scale_y_continuous(labels=scales::dollar_format())
plotProfitCurveComparison(profitCurve, 'Original', profit_curve_V1, 'With Zip-Code Removed')

# TO DO: show the effect of removing zip_code upon unfair bias metrics
for (featureName in protected) {
  # calculate and plot proportional parity
  pp1 = getProportionParity(mergedData, featureName, optimalThresholdForProfit)
  pp2 = getProportionParity(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotProportionalParityComparison(pp1, 'Original', pp2, 'With Zip-Code Removed')
  
  # calculate and plot equal parity
  eqp1 = getEqualParity(mergedData, featureName, optimalThresholdForProfit)
  eqp2 = getEqualParity(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotEqualParityComparison(eqp1, 'Original', eqp2, 'With Zip-Code Removed')
  
  # calculate and plot favorable class balance
  fcb1 = getFavorableClassBalance(mergedData, featureName, optimalThresholdForProfit)
  fcb2 = getFavorableClassBalance(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotFavorableClassBalanceComparison(fcb1, 'Original', fcb2, 'With Zip-Code Removed')
  
  # calculate and plot unfavorable class balance
  ucb1 = getUnfavorableClassBalance(mergedData, featureName, optimalThresholdForProfit)
  ucb2 = getUnfavorableClassBalance(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotUnfavorableClassBalanceComparison(ucb1, 'Original', ucb2, 'With Zip-Code Removed')
  
  # calculate and plot favorable rate parity
  frp1 = getFavorableRateParity(mergedData, featureName, optimalThresholdForProfit)
  frp2 = getFavorableRateParity(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotFavorableRateParityComparison(frp1, 'Original', frp2, 'With Zip-Code Removed')
  
  # calculate and plot unfavorable rate parity
  urp1 = getUnfavorableRateParity(mergedData, featureName, optimalThresholdForProfit)
  urp2 = getUnfavorableRateParity(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotUnfavorableRateParityComparison(urp1, 'Original', urp2, 'With Zip-Code Removed')
  
  # calculate and plot favorable predictive value parity
  fpv1 = getFavorablePredictiveValueParity(mergedData, featureName, optimalThresholdForProfit)
  fpv2 = getFavorablePredictiveValueParity(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotFavorablePredictiveValueParityComparison(fpv1, 'Original', fpv2, 'With Zip-Code Removed')
  
  # calculate and plot unfavorable rate parity
  upv1 = getUnfavorablePredictiveValueParity(mergedData, featureName, optimalThresholdForProfit)
  upv2 = getUnfavorablePredictiveValueParity(merged_data_V1, featureName, optimal_threshold_profit_V1)
  plotUnfavorablePredictiveValueParityComparison(upv1, 'Original', upv2, 'With Zip-Code Removed')
}

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


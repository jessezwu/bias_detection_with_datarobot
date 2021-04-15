library(tidyverse)

null.is.na <- function(x) { return(ifelse(is.null(x), NA, x))}

# convert feature information into a data frame
featureinfo.as.data.frame <- function(x) {
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
featureinfolist.as.data.frame <- function(x) {
  return(bind_rows(lapply(x, featureinfo.as.data.frame)))
}

# stacked predictions for a model
getStackedPredictions <- function(project, model) {
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
CreatePayoffMatrix <- function(project, matrix_name = 'new payoff matrix',
                               TP_value = 1, TN_value = 1, FP_value = 1, FN_value = 1) {
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
# pull a previously created matrix
GetPayoffMatrix <- function(project, matrix_name) {
  projectId <- datarobot:::ValidateProject(project)
  routeString <- datarobot:::UrlJoin("projects", projectId, "payoffMatrices")
  rawReturn <- datarobot:::DataRobotGET(routeString, returnRawResponse = TRUE)
  matrices <- content(rawReturn)$data %>%
    keep(function(x) grepl(matrix_name, x$name))
  if(length(matrices) > 0) {
    matrices %>% extract2(1)
  } else {
    stop('The requested matrix does not exist')
  }
}


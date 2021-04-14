library(datarobot)
library(tidyverse)
library(magrittr)
library(httr)
library(yaml)

################################################################################
# Settings
################################################################################

config <- read_yaml('~/.config/datarobot/drconfig.yaml')

filename <- 'data/DR_Demo_LendingClub_Guardrails_Fairness.csv.zip'
target <- 'is_bad'
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

# project settings, note bias and fairness is currently only supported through the REST API
# for documentation, see:
# https://app.datarobot.com/apidocs/autodoc/api_reference.html#patch--api-v2-projects-(projectId)-aim-
settings <- list(
  target = target,
  mode = 'manual',
  protectedFeatures = as.list(protected),
  preferableTargetValue = preferable_outcome,
  fairnessMetricsSet = fairness_metric
)

# run project
response <- PATCH(
  sprintf('%s/projects/%s/aim', config$endpoint, project$projectId),
  add_headers(Authorization = sprintf('Bearer %s', config$token)),
  body = settings,
  encode = 'json'
)
UpdateProject(project, workerCount = 'max')
# wait for target leakage detection to complete
while(GetProjectStatus(project)$stage != 'modeling') {
  Sys.sleep(5)
}

# get informative features
featurelists <- ListFeaturelists(project)
informative <- keep(featurelists, function(x) grepl('Leakage Removed', x$name))
# if no leakage detected
if(length(informative) == 0) {
  informative <- keep(featurelists, function(x) grepl('Informative', x$name))
}
# remove protected features
no_protected <- informative %>%
  extract2(1) %>%
  extract2('features') %>%
  setdiff(c(target, protected))
features <- CreateFeaturelist(project, 'Protected Removed', no_protected)

# run autopilot on cleaned featurelist
StartNewAutoPilot(project, features$featurelistId)
WaitForAutopilot(project)

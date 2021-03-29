library(datarobot)
library(tidyverse)
library(magrittr)
library(httr)
library(yaml)

################################################################################
# Settings

config <- read_yaml('~/.config/datarobot/drconfig.yaml')

filename <- 'data/DR_Demo_LendingClub_Guardrails_Fairness.csv.zip'
target <- 'is_bad'
preferable_outcome <- 'No'
fairness_metric <- 'proportionalParity'
# One of proportionalParity, equalParity, predictionBalance, trueFavorableAndUnfavorableRateParity or FavorableAndUnfavorablePredictiveValueParity

# load in list of protected features
protected <- read_lines('protected_features.txt')

################################################################################

# upload dataset to create a project
# data <- read_csv(filename)
project <- SetupProject(filename, 'Bias Demo')

# remove protected features from informative features
featurelists <- ListFeaturelists(project)
informative <- keep(featurelists, function(x) x$name == 'Informative Features')
no_protected <- informative %>%
  extract2(1) %>%
  extract2('features') %>%
  setdiff(c(target, protected))
features <- CreateFeaturelist(project, 'Protected Removed', no_protected)

# TODO: get leakage removed featurelist
# run a project in manual mode first - then extract if leakage exists

# project settings, note bias and fairness is currently only supported through the REST API
# for documentation, see:
# https://app.datarobot.com/apidocs/autodoc/api_reference.html#patch--api-v2-projects-(projectId)-aim-
settings <- list(
  target = target,
  featurelistId = features$featurelistId,
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
WaitForAutopilot(project)

# take top model
best_model <- GetModelRecommendation(project, 'Recommended for Deployment')

# TODO: look at cross class data disparity for any proxies
# Does this API call exist yet?

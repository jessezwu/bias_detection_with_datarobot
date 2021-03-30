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

###########################################################################################
# find direct bias
###########################################################################################

# take top model
best_model <- GetModelRecommendation(project, 'Recommended for Deployment')

# TO DO: add a cost matrix so that we get a profit curve and the "optimal" threshold

# TODO: look at cross class data disparity for any proxies
# Does this API call exist yet?

###########################################################################################
# find indirect bias
###########################################################################################

# TO DO: look for indirect discrimination
# TO DO: get the feature association matrix
# TO DO: create separate DR projects that predict the protected features, then look at the feature impact, feature effects, and world cloud

###########################################################################################
# remove bias 1: remove indirect bias features
###########################################################################################

# TO DO: show the effect of removing income upon unfair bias metrics
# TO DO: show the effect of removing income upon profit curve

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

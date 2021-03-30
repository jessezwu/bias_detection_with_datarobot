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
# falseNegativeValue = -5000   # bad loan written, $10000 in loan writeoffs

payoff_matrix = CreatePayoffMatrix(project, 'payoff test 2', 0, 1500, 0, -10000)

# TO DO: get the profit curve
# TO DO: get the optimal threshold for profit


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

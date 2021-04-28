library(datarobot)
library(magrittr)
library(purrr)
library(yaml)
source('project_helpers.R')

config <- read_yaml('config/project_config.yaml')

################################################################################
# detect and remove leakage as well as protected features
################################################################################

# upload dataset to create a project
project <- SetupProject(config$filename, config$project_name)

# project settings, note bias and fairness is currently only supported through the REST API
# for documentation, see:
# https://app.datarobot.com/apidocs/autodoc/api_reference.html#patch--api-v2-projects-(projectId)-aim-
settings <- list(
  target = config$target,
  mode = 'manual',
  protectedFeatures = as.list(config$protected),
  preferableTargetValue = config$preferable_outcome,
  fairnessMetricsSet = config$fairness_metric
)

# run project
response <- datarobot:::DataRobotPATCH(
  datarobot:::UrlJoin("projects", project$projectId, "aim"),
  body = settings,
  encode = 'json',
  returnRawResponse = TRUE
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
  setdiff(c(config$target, config$protected))
features <- CreateFeaturelist(project, 'Protected Removed', no_protected)

# run autopilot on cleaned featurelist
StartNewAutoPilot(project, features$featurelistId)
WaitForAutopilot(project)

project <- GetProject(project$projectId)
write_project(project)

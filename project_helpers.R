library(readr)

get_project_file <- function(name, config_dir = 'config') {
  file.path(config_dir, paste0(name, '.RDS'))
}

load_project <- function(name, config_dir = 'config') {
  tryCatch({
      readRDS(get_project_file(name, config_dir))
    },
    error = function(e) {
      stop("Project doesn't exist, please run setup first!")
  })
}

write_project <- function(project, config_dir = 'config') {
  saveRDS(project, get_project_file(project$projectName, config_dir))
}

load_data <- function(name, project, data_dir = 'data') {
  tryCatch({
      read_csv(file.path(data_dir, project, paste0(name, '.csv')))
    },
    error = function(e) {
      print("Dataset doesn't exist!")
      stop()
  })
}

write_data <- function(df, name, project, data_dir = 'data') {
  dir.create(file.path(data_dir, project), showWarnings=FALSE)
  write_csv(df, file.path(data_dir, project, paste0(name, '.csv')))
}

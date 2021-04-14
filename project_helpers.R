get_project_path <- function(name, config_dir = 'config') {
  file.path(config_dir, paste0(name, '.RDS'))
}

load_project <- function(name, config_dir = 'config') {
  tryCatch({
      project <- readRDS(get_project_path(name, config_dir))
    },
    error = function(e) {
      print("Project doesn't exist, please run setup first!")
      stop()
  })
}

write_project <- function(project, config_dir = 'config') {
  saveRDS(project, get_project_path(project$projectName, config_dir))
}

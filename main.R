# RepoPulse entry point. Run source("main.R") from RStudio or an R console.

.find_project_root <- function() {
  frame_paths <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile) && length(frame$ofile)) as.character(frame$ofile[1L]) else ""
  }, character(1))
  frame_paths <- frame_paths[nzchar(frame_paths)]
  candidate <- if (length(frame_paths)) dirname(normalizePath(tail(frame_paths, 1L), winslash = "/", mustWork = TRUE)) else getwd()
  candidate <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(candidate, "R")) && file.exists(file.path(candidate, "main.R"))) return(candidate)
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }
  stop("Could not locate the RepoPulse project root. Set the working directory to the RepoPulse folder and run source('main.R').", call. = FALSE)
}

project_root <- .find_project_root()
setwd(project_root)

repopulse_modules <- c(
  "config.R", "data_cleaning.R", "github_api.R", "descriptive_stats.R",
  "arrival_models.R", "survival_models.R", "model_diagnostics.R",
  "forecast_engine.R", "visualizations.R", "report_generator.R", "interface.R"
)
for (module in repopulse_modules) {
  module_path <- file.path(project_root, "R", module)
  if (!file.exists(module_path)) stop(sprintf("Required module is missing: %s", module_path), call. = FALSE)
  sys.source(module_path, envir = .GlobalEnv)
}

run_repopulse_app()

# Shared loader for this script-based project (not an installed R package).
.find_repopulse_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(path, "R")) && file.exists(file.path(path, "main.R"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) stop("Run tests from the RepoPulse project directory.", call. = FALSE)
    path <- parent
  }
}
.repopulse_root <- .find_repopulse_root()
for (.module in c("config.R", "data_cleaning.R", "descriptive_stats.R", "arrival_models.R",
                  "survival_models.R", "model_diagnostics.R", "forecast_engine.R")) {
  source(file.path(.repopulse_root, "R", .module), local = .GlobalEnv)
}

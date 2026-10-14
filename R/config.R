# RepoPulse configuration and dependency helpers.

REPOPULSE_VERSION <- "1.0.0"

repopulse_required_packages <- function(feature = c("core", "github", "plots", "tests")) {
  feature <- match.arg(feature)
  switch(
    feature,
    core = character(0),
    github = c("httr2", "jsonlite"),
    plots = "ggplot2",
    tests = "testthat"
  )
}

check_repopulse_dependencies <- function(feature = c("core", "github", "plots", "tests"),
                                         stop_on_missing = TRUE) {
  feature <- match.arg(feature)
  packages <- repopulse_required_packages(feature)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) && stop_on_missing) {
    stop(
      sprintf(
        "Missing package(s): %s. Install them with install.packages(c(%s)).",
        paste(missing, collapse = ", "),
        paste(sprintf('"%s"', missing), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(list(feature = feature, required = packages, missing = missing))
}

install_repopulse_dependencies <- function(include_tests = TRUE) {
  packages <- unique(c("httr2", "jsonlite", "ggplot2", if (include_tests) "testthat"))
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
  invisible(packages)
}

repopulse_config <- function(cache_dir = file.path("cache"),
                             reports_dir = file.path("reports"),
                             cache_hours = 6,
                             max_github_pages = 100,
                             default_horizon_days = 30,
                             default_simulations = 1000,
                             seed = 2026L) {
  list(
    version = REPOPULSE_VERSION,
    cache_dir = cache_dir,
    reports_dir = reports_dir,
    cache_hours = as.numeric(cache_hours),
    max_github_pages = as.integer(max_github_pages),
    default_horizon_days = as.integer(default_horizon_days),
    default_simulations = as.integer(default_simulations),
    seed = as.integer(seed),
    github_api_root = "https://api.github.com"
  )
}

# Keep date handling consistently in UTC throughout the project.
repopulse_now_utc <- function() as.POSIXct(Sys.time(), tz = "UTC")

format_number <- function(x, digits = 2L) {
  if (length(x) == 0L || is.na(x[1L]) || !is.finite(x[1L])) return("N/A")
  formatC(x[1L], digits = digits, format = "f", big.mark = ",")
}

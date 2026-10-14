# Basic project integrity check; run from the repository root with source("scripts/check_project.R").
required_files <- c(
  "main.R", "R/config.R", "R/github_api.R", "R/data_cleaning.R",
  "R/descriptive_stats.R", "R/arrival_models.R", "R/survival_models.R",
  "R/model_diagnostics.R", "R/forecast_engine.R", "R/visualizations.R",
  "R/report_generator.R", "R/interface.R", "data/demo_issues.csv",
  "README.md", "docs/project_memo.qmd", "docs/references.bib"
)
missing <- required_files[!file.exists(required_files)]
if (length(missing)) stop(paste("Missing project files:", paste(missing, collapse = ", ")), call. = FALSE)

demo <- read.csv("data/demo_issues.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
required_columns <- c("issue_id", "created_at", "closed_at", "state", "labels", "is_pull_request", "source", "observed_at", "updated_at")
missing_columns <- setdiff(required_columns, names(demo))
if (length(missing_columns)) stop(paste("Demo CSV missing columns:", paste(missing_columns, collapse = ", ")), call. = FALSE)
if (anyDuplicated(demo$issue_id)) stop("Demo CSV has duplicated issue IDs.", call. = FALSE)
if (nrow(demo) < 50L) stop("Demo CSV unexpectedly has fewer than 50 rows.", call. = FALSE)
cat("RepoPulse static file/data check passed. This is not a substitute for running the R tests.\n")

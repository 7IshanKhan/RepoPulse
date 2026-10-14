# Reproducible report, CSV exports, and optional plot files.

report_scalar <- function(x, digits = 2L) {
  if (length(x) != 1L || is.na(x) || !is.finite(as.numeric(x))) return("N/A")
  formatC(as.numeric(x), digits = digits, format = "f", big.mark = ",")
}

create_analysis_report_lines <- function(issue_data, arrival_counts, arrival_fits,
                                         survival_fit, diagnostics, forecast = NULL) {
  issues <- issue_only_data(issue_data)
  stats_table <- summarise_issues(issues)
  repo_names <- unique(issues$source)
  report_lines <- c(
    "# RepoPulse repository health report",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("RepoPulse version: ", REPOPULSE_VERSION),
    paste0("Source label(s): ", paste(repo_names, collapse = ", ")), 
    "",
    "## Scope and data quality",
    "",
    paste0("- Rows analysed as issues (pull requests excluded): ", nrow(issues)),
    paste0("- Input rows containing pull requests: ", sum(issue_data$is_pull_request, na.rm = TRUE)),
    paste0("- Rows with missing closure timestamps but state marked closed: ", sum(issues$state == "closed" & is.na(issues$closed_at))),
    "- Unresolved issues are censored at their recorded observation time. No closure dates are imputed.",
    "- Closed-state rows missing closure timestamps are excluded from survival fitting because they cannot be treated as genuine right-censored open issues.",
    "- Historical backlog is reconstructed only from the issue records available to this run; deleted or unreturned issues can be missing.",
    "",
    "## Descriptive summary",
    "",
    "| Metric | Value |",
    "|---|---:|"
  )
  for (i in seq_len(nrow(stats_table))) {
    v <- stats_table$value[i]
    value_text <- if (is.na(v)) "N/A" else if (grepl("share", stats_table$metric[i], ignore.case = TRUE)) paste0(report_scalar(v), "%") else report_scalar(v)
    report_lines <- c(report_lines, paste0("| ", stats_table$metric[i], " | ", value_text, " |"))
  }

  arrival_table <- diagnostics$arrivals$table
  report_lines <- c(report_lines,
    "", "## Issue-arrival models", "",
    paste0("Arrival counts were binned daily for forecasting. The descriptive model table below fits distributions to ", nrow(arrival_counts), " daily bins."),
    "", "| Model | Mean per day | NB size | Log likelihood | AIC | Status |", "|---|---:|---:|---:|---:|---|"
  )
  for (i in seq_len(nrow(arrival_table))) {
    r <- arrival_table[i, ]
    report_lines <- c(report_lines, paste0("| ", r$model, " | ", report_scalar(r$mean_per_bin), " | ", report_scalar(r$size),
      " | ", report_scalar(r$log_likelihood), " | ", report_scalar(r$AIC), " | ", r$status, " |"))
  }
  report_lines <- c(report_lines,
    "", paste0("Poisson Pearson dispersion ratio: ", report_scalar(diagnostics$arrivals$poisson_dispersion_ratio)),
    "", "### Arrival-model interpretation", ""
  )
  if (length(diagnostics$arrivals$notes)) {
    report_lines <- c(report_lines, paste0("- ", diagnostics$arrivals$notes))
  } else {
    report_lines <- c(report_lines, "- No automatic diagnostic flags were raised; this does not establish model adequacy.")
  }

  survival_table <- diagnostics$survival$table
  report_lines <- c(report_lines,
    "", "## Resolution-time survival analysis", "",
    paste0("Observed durations: ", survival_fit$n_observations,
           "; observed closures (events): ", survival_fit$n_events,
           "; right-censored records: ", survival_fit$n_censored, "."),
    paste0("Selected parametric model by AIC (when estimable): ", diagnostics$survival$selected_model),
    "", "| Model | Scale (days) | Shape | Log likelihood | AIC | Status |", "|---|---:|---:|---:|---:|---|"
  )
  for (i in seq_len(nrow(survival_table))) {
    r <- survival_table[i, ]
    report_lines <- c(report_lines, paste0("| ", r$model, " | ", report_scalar(r$scale_days), " | ",
      report_scalar(r$shape), " | ", report_scalar(r$log_likelihood), " | ", report_scalar(r$AIC), " | ", r$status, " |"))
  }
  report_lines <- c(report_lines, "", "### Survival-model caveats", "")
  if (length(diagnostics$survival$notes)) {
    report_lines <- c(report_lines, paste0("- ", diagnostics$survival$notes))
  } else {
    report_lines <- c(report_lines, "- Both parametric fits were estimable; their distributional assumptions still require judgement.")
  }
  report_lines <- c(report_lines,
    "", "The Kaplan-Meier estimator uses each issue's observed follow-up time and closure indicator. The exponential and Weibull models are fitted by right-censored maximum likelihood.",
    ""
  )

  if (!is.null(forecast)) {
    s <- forecast$summary
    forecast_warning_lines <- if (length(forecast$warnings)) paste0("- ", forecast$warnings) else "- No forecast warning flags were raised."
    report_lines <- c(report_lines,
      "## Monte Carlo backlog forecast", "",
      paste0("Forecast horizon: ", s$horizon_days, " days; simulations: ", s$n_simulations, "; confidence interval: ", s$confidence_level * 100, "%."),
      paste0("Baseline backlog: ", s$baseline_backlog),
      paste0("Expected final backlog: ", report_scalar(s$expected_final_backlog)),
      paste0("Median final backlog: ", report_scalar(s$median_final_backlog)),
      paste0("Central interval: [", report_scalar(s$final_lower), ", ", report_scalar(s$final_upper), "]"),
      paste0("Probability final backlog exceeds starting backlog: ", report_scalar(100 * s$probability_backlog_grows), "%"),
      paste0("Monte Carlo standard error of expected final backlog: ", report_scalar(s$monte_carlo_se_final_mean)),
      if (!is.na(s$probability_backlog_exceeds_threshold)) paste0("Probability final backlog exceeds threshold ", s$backlog_threshold, ": ", report_scalar(100 * s$probability_backlog_exceeds_threshold), "%") else "No backlog threshold was requested.",
      "",
      "### Forecast limitations", "",
      paste0("- ", forecast$metadata$notes),
      forecast_warning_lines,
      "- Monte Carlo standard errors reflect finite simulation runs only. Parameter uncertainty and changes in maintainer behaviour are not included.",
      "- New issue arrivals are simulated from a stationary count distribution; real repositories may have releases, incidents, holidays, or bursts that violate stationarity.",
      ""
    )
  } else {
    report_lines <- c(report_lines, "## Monte Carlo backlog forecast", "", "No forecast was run for this report.", "")
  }

  c(report_lines,
    "## Reproducibility", "",
    "The application records the simulation seed and model choices in the exported output metadata. For reproducibility across machines, install the packages listed in README.md and record the exact local R/package versions.",
    "", "## References", "",
    "- Kaplan, E. L., & Meier, P. (1958). Nonparametric estimation from incomplete observations. *Journal of the American Statistical Association*, 53(282), 457–481.",
    "- McCullagh, P., & Nelder, J. A. (1989). *Generalized Linear Models* (2nd ed.). Chapman & Hall.",
    "- GitHub. *REST API documentation: Issues*. https://docs.github.com/en/rest/issues/issues", ""
  )
}

export_analysis_report <- function(issue_data,
                                   output_dir = file.path("reports"),
                                   arrival_counts = NULL,
                                   arrival_fits = NULL,
                                   survival_fit = NULL,
                                   diagnostics = NULL,
                                   forecast = NULL,
                                   save_plots = TRUE) {
  issues <- normalise_issue_data(issue_data, warn = FALSE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  prefix <- file.path(output_dir, paste0("repopulse_", stamp))

  if (is.null(arrival_counts)) arrival_counts <- bin_issue_arrivals(issues, interval = "day")
  if (is.null(arrival_fits)) arrival_fits <- fit_arrival_models(arrival_counts)
  if (is.null(survival_fit)) survival_fit <- fit_survival_models(issues)
  if (is.null(diagnostics)) diagnostics <- run_model_diagnostics(arrival_counts, arrival_fits, survival_fit)

  report_path <- paste0(prefix, "_report.md")
  lines <- create_analysis_report_lines(issues, arrival_counts, arrival_fits, survival_fit, diagnostics, forecast)
  writeLines(lines, report_path, useBytes = TRUE)

  data_path <- paste0(prefix, "_cleaned_issues.csv")
  write.csv(issues, data_path, row.names = FALSE, na = "")
  arrivals_path <- paste0(prefix, "_daily_arrivals.csv")
  write.csv(arrival_counts, arrivals_path, row.names = FALSE)
  arrival_models_path <- paste0(prefix, "_arrival_models.csv")
  write.csv(arrival_model_table(arrival_fits), arrival_models_path, row.names = FALSE)
  survival_models_path <- paste0(prefix, "_survival_models.csv")
  write.csv(survival_model_table(survival_fit), survival_models_path, row.names = FALSE)

  files <- c(report = report_path, cleaned_issues = data_path,
             daily_arrivals = arrivals_path, arrival_models = arrival_models_path,
             survival_models = survival_models_path)
  if (!is.null(forecast)) {
    forecast_path <- paste0(prefix, "_forecast_daily_summary.csv")
    write.csv(forecast$daily_summary, forecast_path, row.names = FALSE)
    paths_path <- paste0(prefix, "_forecast_paths.csv")
    path_df <- as.data.frame(forecast$paths)
    names(path_df) <- paste0("day_", 0:(ncol(path_df) - 1L))
    write.csv(path_df, paths_path, row.names = FALSE)
    metadata_path <- paste0(prefix, "_forecast_metadata.txt")
    writeLines(c(
      paste0("seed=", forecast$metadata$seed),
      paste0("arrival_model=", forecast$metadata$arrival_model),
      paste0("arrival_mean_per_day=", forecast$metadata$arrival_mean_per_day),
      paste0("survival_model=", forecast$metadata$survival_model),
      paste0("n_simulations=", forecast$summary$n_simulations),
      paste0("horizon_days=", forecast$summary$horizon_days),
      "", "Notes:", forecast$metadata$notes, "", "Warnings:", forecast$warnings
    ), metadata_path, useBytes = TRUE)
    files <- c(files, forecast_summary = forecast_path, forecast_paths = paths_path, forecast_metadata = metadata_path)
  }

  if (isTRUE(save_plots) && requireNamespace("ggplot2", quietly = TRUE)) {
    plots <- list(
      arrivals = tryCatch(plot_arrivals(arrival_counts, arrival_fits), error = function(e) NULL),
      survival = tryCatch(plot_survival_curves(survival_fit), error = function(e) NULL),
      backlog_history = tryCatch(plot_backlog_history(reconstruct_backlog_series(issues)), error = function(e) NULL)
    )
    if (!is.null(forecast)) plots$forecast <- tryCatch(plot_forecast(forecast), error = function(e) NULL)
    for (name in names(plots)) {
      if (!is.null(plots[[name]])) {
        plot_path <- paste0(prefix, "_", name, ".png")
        tryCatch(ggplot2::ggsave(plot_path, plot = plots[[name]], width = 9, height = 5.5, dpi = 150), error = function(e) NULL)
        if (file.exists(plot_path)) files <- c(files, setNames(plot_path, paste0("plot_", name)))
      }
    }
  }
  invisible(list(files = files, report = report_path, prefix = prefix))
}

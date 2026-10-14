# Local interactive console menu. No web server or cloud runtime is needed.

.print_issue_summary <- function(data) {
  summary <- summarise_issues(data)
  print(summary, row.names = FALSE)
  cat("\nTop labels:\n")
  print(summarise_labels(data), row.names = FALSE)
  invisible(summary)
}

.fit_current_models <- function(state) {
  if (is.null(state$data)) stop("Load issue data first.", call. = FALSE)
  state$arrival_counts <- bin_issue_arrivals(state$data, interval = "day")
  state$arrival_fits <- fit_arrival_models(state$arrival_counts)
  state$survival_fit <- fit_survival_models(state$data)
  state$diagnostics <- run_model_diagnostics(state$arrival_counts, state$arrival_fits, state$survival_fit)
  invisible(state)
}

.show_current_plots <- function(state) {
  check_repopulse_dependencies("plots")
  if (is.null(state$data)) stop("Load issue data first.", call. = FALSE)
  if (is.null(state$arrival_counts)) state$arrival_counts <- bin_issue_arrivals(state$data, interval = "day")
  print(plot_arrivals(state$arrival_counts, state$arrival_fits))
  print(plot_backlog_history(reconstruct_backlog_series(state$data)))
  if (!is.null(state$survival_fit)) print(plot_survival_curves(state$survival_fit))
  if (!is.null(state$forecast)) print(plot_forecast(state$forecast))
  invisible(NULL)
}

run_repopulse_app <- function() {
  state <- new.env(parent = emptyenv())
  state$data <- NULL
  state$metadata <- NULL
  state$arrival_counts <- NULL
  state$arrival_fits <- NULL
  state$survival_fit <- NULL
  state$diagnostics <- NULL
  state$forecast <- NULL

  cat("\n=============================================\n")
  cat(" RepoPulse | Repository Health & Backlog Lab\n")
  cat(" Version ", REPOPULSE_VERSION, " | Local R console application\n", sep = "")
  cat("=============================================\n")
  repeat {
    cat("\n1. Load included offline demo\n")
    cat("2. Fetch public repository issues from GitHub\n")
    cat("3. Import issue data from CSV\n")
    cat("4. Show descriptive summary\n")
    cat("5. Fit models and view diagnostics\n")
    cat("6. Run Monte Carlo backlog forecast\n")
    cat("7. Export report, CSV files, and plots\n")
    cat("8. Display charts\n")
    cat("9. Exit\n")
    choice <- trimws(readline("Choose an option (1-9): "))

    if (identical(choice, "1")) {
      path <- file.path("data", "demo_issues.csv")
      if (!file.exists(path)) {
        cat("Could not find ", path, ". Start RepoPulse from its project root directory.\n", sep = "")
        next
      }
      raw <- tryCatch(read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")), error = function(e) e)
      if (inherits(raw, "error")) { cat("Could not read demo CSV: ", conditionMessage(raw), "\n", sep = ""); next }
      state$data <- tryCatch(normalise_issue_data(raw), error = function(e) e)
      if (inherits(state$data, "error")) { cat("Could not clean demo data: ", conditionMessage(state$data), "\n", sep = ""); state$data <- NULL; next }
      state$metadata <- list(source = "offline demo")
      state$arrival_counts <- state$arrival_fits <- state$survival_fit <- state$diagnostics <- state$forecast <- NULL
      cat("Loaded ", nrow(state$data), " synthetic records. No internet connection was needed.\n", sep = "")


    } else if (identical(choice, "2")) {
      repo <- trimws(readline("GitHub repository (owner/name or URL): "))
      if (!nzchar(repo)) next

      result <- tryCatch(
        fetch_github_issues(repo),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        cat("GitHub fetch failed: ", conditionMessage(result), "\n", sep = "")
        next
      }

      # fetch_github_issues() returns a data frame directly.
      state$data <- tryCatch(
        normalise_issue_data(result),
        error = function(e) e
      )

      if (inherits(state$data, "error")) {
        cat("Could not clean GitHub data: ",
            conditionMessage(state$data), "\n", sep = "")
        state$data <- NULL
        next
      }

      state$metadata <- list(
        source = "GitHub",
        repository = attr(result, "github_repository"),
        cache_used = TRUE
      )

      state$arrival_counts <- state$arrival_fits <-
        state$survival_fit <- state$diagnostics <-
        state$forecast <- NULL

      cat("Fetched ", nrow(state$data),
          " rows from ", state$metadata$repository, ".\n", sep = "")
      cat("Pull requests are flagged and excluded from issue-only analysis.\n")


    } else if (identical(choice, "3")) {
      path <- trimws(readline("CSV path: "))
      if (!file.exists(path)) { cat("File not found.\n"); next }
      imported <- tryCatch(read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")), error = function(e) e)
      if (inherits(imported, "error")) { cat("Could not read CSV: ", conditionMessage(imported), "\n", sep = ""); next }
      state$data <- tryCatch(normalise_issue_data(imported), error = function(e) e)
      if (inherits(state$data, "error")) { cat("Import failed: ", conditionMessage(state$data), "\n", sep = ""); state$data <- NULL; next }
      state$metadata <- list(source = path)
      state$arrival_counts <- state$arrival_fits <- state$survival_fit <- state$diagnostics <- state$forecast <- NULL
      cat("Imported and cleaned ", nrow(state$data), " records.\n", sep = "")

    } else if (identical(choice, "4")) {
      if (is.null(state$data)) { cat("Load data first.\n"); next }
      .print_issue_summary(state$data)
      counts <- tryCatch(bin_issue_arrivals(state$data, "day"), error = function(e) NULL)
      if (!is.null(counts)) {
        cat("\nDaily arrival bins: ", nrow(counts), " | mean: ", format_number(mean(counts$count)),
            " | variance: ", format_number(stats::var(counts$count)), "\n", sep = "")
      }

    } else if (identical(choice, "5")) {
      if (is.null(state$data)) { cat("Load data first.\n"); next }
      result <- tryCatch({ .fit_current_models(state); TRUE }, error = function(e) e)
      if (inherits(result, "error")) { cat("Model fitting failed: ", conditionMessage(result), "\n", sep = ""); next }
      cat("\nArrival models:\n")
      print(state$diagnostics$arrivals$table, row.names = FALSE)
      cat("\nSurvival models:\n")
      print(state$diagnostics$survival$table, row.names = FALSE)
      cat("\nSelected arrival model: ", state$arrival_fits$selected_name, "\n", sep = "")
      cat("Selected survival model: ", state$diagnostics$survival$selected_model, "\n", sep = "")
      notes <- unique(c(state$diagnostics$arrivals$notes, state$diagnostics$survival$notes))
      if (length(notes)) { cat("\nDiagnostic notes:\n"); cat(paste0("- ", notes, collapse = "\n"), "\n") }

    } else if (identical(choice, "6")) {
      if (is.null(state$data)) { cat("Load data first.\n"); next }
      result <- tryCatch({ if (is.null(state$arrival_fits) || is.null(state$survival_fit)) .fit_current_models(state); TRUE }, error = function(e) e)
      if (inherits(result, "error")) { cat("Could not fit models: ", conditionMessage(result), "\n", sep = ""); next }
      horizon <- suppressWarnings(as.integer(readline("Forecast horizon in days [30]: ")))
      if (is.na(horizon)) horizon <- 30L
      if (horizon < 1L || horizon > 365L) { cat("Choose a horizon from 1 to 365 days.\n"); next }
      sims <- suppressWarnings(as.integer(readline("Simulation runs [1000]: ")))
      if (is.na(sims)) sims <- 1000L
      if (sims < 50L || sims > 50000L) { cat("Choose 50 to 50000 simulation runs.\n"); next }
      threshold_text <- trimws(readline("Optional backlog threshold (press Enter to skip): "))
      threshold <- if (nzchar(threshold_text)) suppressWarnings(as.numeric(threshold_text)) else NULL
      if (!is.null(threshold) && (!is.finite(threshold) || threshold < 0)) { cat("Threshold must be a non-negative number.\n"); next }
      seed <- suppressWarnings(as.integer(readline("Random seed [2026]: ")))
      if (is.na(seed)) seed <- 2026L
      forecast <- tryCatch(forecast_backlog(
        state$data, state$arrival_fits, state$survival_fit,
        horizon_days = horizon, n_simulations = sims,
        backlog_threshold = threshold, seed = seed
      ), error = function(e) e)
      if (inherits(forecast, "error")) { cat("Forecast failed: ", conditionMessage(forecast), "\n", sep = ""); next }
      state$forecast <- forecast
      cat("\nForecast summary\n")
      cat("Baseline backlog: ", forecast$summary$baseline_backlog, "\n", sep = "")
      cat("Expected final backlog: ", format_number(forecast$summary$expected_final_backlog), "\n", sep = "")
      cat("Median final backlog: ", forecast$summary$median_final_backlog, "\n", sep = "")
      cat("Central interval: [", forecast$summary$final_lower, ", ", forecast$summary$final_upper, "]\n", sep = "")
      cat("Probability backlog grows: ", format_number(100 * forecast$summary$probability_backlog_grows), "%\n", sep = "")
      if (!is.na(forecast$summary$probability_backlog_exceeds_threshold)) {
        cat("Probability of exceeding threshold: ", format_number(100 * forecast$summary$probability_backlog_exceeds_threshold), "%\n", sep = "")
      }
      if (length(forecast$warnings)) cat(paste0("Warning: ", forecast$warnings, collapse = "\n"), "\n")
      if (requireNamespace("ggplot2", quietly = TRUE)) print(plot_forecast(forecast))

    } else if (identical(choice, "7")) {
      if (is.null(state$data)) { cat("Load data first.\n"); next }
      result <- tryCatch(export_analysis_report(
        state$data, output_dir = file.path("reports"),
        arrival_counts = state$arrival_counts,
        arrival_fits = state$arrival_fits,
        survival_fit = state$survival_fit,
        diagnostics = state$diagnostics,
        forecast = state$forecast
      ), error = function(e) e)
      if (inherits(result, "error")) { cat("Export failed: ", conditionMessage(result), "\n", sep = ""); next }
      cat("Export complete. Files written:\n")
      cat(paste0("- ", unname(result$files), collapse = "\n"), "\n")

    } else if (identical(choice, "8")) {
      if (is.null(state$data)) { cat("Load data first.\n"); next }
      result <- tryCatch(.show_current_plots(state), error = function(e) e)
      if (inherits(result, "error")) cat("Chart display failed: ", conditionMessage(result), "\n", sep = "")

    } else if (identical(choice, "9")) {
      cat("RepoPulse session ended.\n")
      break
    } else {
      cat("Please choose an option from 1 to 9.\n")
    }
  }
  invisible(state)
}

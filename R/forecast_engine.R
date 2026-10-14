# Issue-level Monte Carlo backlog forecast.
# Existing open issues resolve according to a discrete conditional hazard derived
# from S(age + 1) / S(age). New issues are added as an explicit cohort at each day end.

.resolve_forecast_survival_model <- function(survival_fit) {
  if (is.list(survival_fit) && !is.null(survival_fit$selected) && !is.null(survival_fit$selected$type)) {
    return(list(model = survival_fit$selected, warning = NULL))
  }
  if (is.list(survival_fit) && !is.null(survival_fit$type) && identical(survival_fit$status, "ok")) {
    return(list(model = survival_fit, warning = NULL))
  }
  list(model = NULL, warning = "No estimable survival model was supplied; no issues are assumed to resolve during the forecast. This is a conservative fallback, not a fitted result.")
}

forecast_backlog <- function(issue_data,
                             arrival_model,
                             survival_model,
                             horizon_days = 30L,
                             n_simulations = 1000L,
                             backlog_threshold = NULL,
                             seed = 2026L,
                             confidence = 0.90) {
  horizon_days <- as.integer(horizon_days)
  n_simulations <- as.integer(n_simulations)
  if (length(horizon_days) != 1L || is.na(horizon_days) || horizon_days < 1L) stop("horizon_days must be a positive integer.", call. = FALSE)
  if (length(n_simulations) != 1L || is.na(n_simulations) || n_simulations < 1L) stop("n_simulations must be a positive integer.", call. = FALSE)
  if (!is.numeric(confidence) || length(confidence) != 1L || !is.finite(confidence) || confidence <= 0 || confidence >= 1) stop("confidence must be between 0 and 1.", call. = FALSE)
  if (length(seed) != 1L || is.na(seed) || !is.finite(as.numeric(seed))) stop("seed must be one finite integer value.", call. = FALSE)
  seed <- as.integer(seed)
  if (is.na(seed)) stop("seed must be representable as an R integer.", call. = FALSE)
  if (is.list(arrival_model) && !is.null(arrival_model$selected)) arrival_model <- arrival_model$selected
  if (is.null(arrival_model$type) || !is.finite(arrival_model$mu)) stop("A fitted arrival model is required.", call. = FALSE)
  if (!is.null(arrival_model$bin_width) && !arrival_model$bin_width %in% c("day", "unspecified")) {
    stop("Backlog forecasting requires an arrival model fitted to daily bins. Refit using bin_issue_arrivals(data, interval = 'day').", call. = FALSE)
  }

  issues <- issue_only_data(issue_data)
  if (!nrow(issues)) stop("No issue data available for backlog forecasting.", call. = FALSE)
  open_mask <- issues$state == "open" & is.na(issues$closed_at)
  initial_ages <- get_open_issue_ages(issues)
  baseline <- length(initial_ages)
  if (sum(open_mask) != baseline) {
    # Rows with dates outside their observation window should not be included as
    # active issue ages; keep the baseline and active cohort consistent.
    open_issues <- issues[open_mask, , drop = FALSE]
    age_vals <- as.numeric(difftime(open_issues$observed_at, open_issues$created_at, units = "days"))
    initial_ages <- pmax(age_vals[is.finite(age_vals) & age_vals >= 0], 0)
    baseline <- length(initial_ages)
  }

  survival_choice <- .resolve_forecast_survival_model(survival_model)
  fitted_survival <- survival_choice$model
  warnings <- character(0)
  if (!is.null(survival_choice$warning)) warnings <- c(warnings, survival_choice$warning)
  if (!is.null(survival_model$warnings)) warnings <- c(warnings, survival_model$warnings)

  if (n_simulations < 200L) warnings <- c(warnings, "Fewer than 200 simulation runs were requested; Monte Carlo summaries may be noisy.")
  if (baseline == 0L) warnings <- c(warnings, "The baseline active backlog is zero.")
  if (!is.null(backlog_threshold) && (!is.numeric(backlog_threshold) || length(backlog_threshold) != 1L || !is.finite(backlog_threshold) || backlog_threshold < 0)) {
    stop("backlog_threshold must be NULL or a non-negative finite number.", call. = FALSE)
  }

  set.seed(seed)
  arrivals <- predict_arrival_counts(arrival_model, n_bins = horizon_days, nsim = n_simulations)
  paths <- matrix(NA_integer_, nrow = n_simulations, ncol = horizon_days + 1L)
  paths[, 1L] <- baseline

  survival_fn <- if (!is.null(fitted_survival)) {
    function(t) predict_survival(fitted_survival, t)
  } else {
    function(t) rep(1, length(t))
  }

  for (sim in seq_len(n_simulations)) {
    active_ages <- initial_ages
    for (day in seq_len(horizon_days)) {
      if (length(active_ages)) {
        s_now <- survival_fn(active_ages)
        s_next <- survival_fn(active_ages + 1)
        conditional_survival <- ifelse(s_now > 1e-12, s_next / s_now, 0)
        conditional_survival <- pmin(1, pmax(0, conditional_survival))
        remain <- stats::runif(length(active_ages)) < conditional_survival
        active_ages <- active_ages[remain] + 1
      }
      new_count <- as.integer(arrivals[sim, day])
      if (new_count > 0L) active_ages <- c(active_ages, rep(0, new_count))
      paths[sim, day + 1L] <- length(active_ages)
    }
  }

  days <- 0:horizon_days
  alpha <- (1 - confidence) / 2
  daily_summary <- data.frame(
    day = days,
    mean_backlog = colMeans(paths),
    median_backlog = apply(paths, 2L, stats::median),
    lower = apply(paths, 2L, stats::quantile, probs = alpha, names = FALSE, type = 8),
    upper = apply(paths, 2L, stats::quantile, probs = 1 - alpha, names = FALSE, type = 8),
    probability_above_threshold = if (is.null(backlog_threshold)) NA_real_ else colMeans(paths > backlog_threshold),
    stringsAsFactors = FALSE
  )
  final_values <- paths[, ncol(paths)]
  final_mean <- mean(final_values)
  growth_probability <- mean(final_values > baseline)
  exceed_probability <- if (is.null(backlog_threshold)) NA_real_ else mean(final_values > backlog_threshold)
  summary <- list(
    baseline_backlog = baseline,
    horizon_days = horizon_days,
    expected_final_backlog = final_mean,
    median_final_backlog = stats::median(final_values),
    final_lower = as.numeric(stats::quantile(final_values, probs = alpha, names = FALSE, type = 8)),
    final_upper = as.numeric(stats::quantile(final_values, probs = 1 - alpha, names = FALSE, type = 8)),
    probability_backlog_grows = growth_probability,
    backlog_threshold = backlog_threshold,
    probability_backlog_exceeds_threshold = exceed_probability,
    monte_carlo_se_final_mean = stats::sd(final_values) / sqrt(n_simulations),
    monte_carlo_se_growth_probability = sqrt(growth_probability * (1 - growth_probability) / n_simulations),
    n_simulations = n_simulations,
    confidence_level = confidence
  )
  list(
    daily_summary = daily_summary,
    paths = paths,
    summary = summary,
    warnings = unique(warnings),
    metadata = list(
      seed = seed,
      arrival_model = arrival_model$type,
      arrival_mean_per_day = arrival_model$mu,
      arrival_nb_size = if (identical(arrival_model$type, "negative_binomial")) arrival_model$size else NA_real_,
      survival_model = if (is.null(fitted_survival)) "none (zero-resolution fallback)" else fitted_survival$type,
      notes = c(
        "Forecast is conditional on the fitted arrival and duration distributions; parameter uncertainty is not included.",
        "Monte Carlo standard errors quantify simulation noise only, not input-data or model uncertainty.",
        "New issues enter at the end of each simulated day and first become eligible for resolution on the following day."
      )
    )
  )
}

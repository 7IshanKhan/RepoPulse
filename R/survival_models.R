# Right-censored issue-resolution analysis: Kaplan-Meier plus exponential/Weibull fits.
# The duration for an unresolved issue ends at its observation snapshot, not at an
# invented closure date. Parametric models are fitted by censored maximum likelihood.

kaplan_meier_estimate <- function(durations, events, confidence = 0.95) {
  durations <- as.numeric(durations)
  events <- as.integer(events)
  keep <- is.finite(durations) & durations >= 0 & !is.na(events)
  durations <- pmax(durations[keep], 1e-6)
  events <- as.integer(events[keep] != 0L)
  if (!length(durations)) {
    return(data.frame(time = numeric(), at_risk = integer(), events = integer(),
                      survival = numeric(), std_error = numeric(), lower = numeric(), upper = numeric()))
  }
  event_times <- sort(unique(durations[events == 1L]))
  if (!length(event_times)) {
    return(data.frame(time = numeric(), at_risk = integer(), events = integer(),
                      survival = numeric(), std_error = numeric(), lower = numeric(), upper = numeric()))
  }
  alpha <- 1 - confidence
  z <- stats::qnorm(1 - alpha / 2)
  survival <- 1
  greenwood <- 0
  rows <- vector("list", length(event_times))
  for (i in seq_along(event_times)) {
    time <- event_times[i]
    risk <- sum(durations >= time)
    deaths <- sum(durations == time & events == 1L)
    if (risk > 0L && deaths > 0L) {
      survival <- survival * (1 - deaths / risk)
      if (risk > deaths) greenwood <- greenwood + deaths / (risk * (risk - deaths))
    }
    se <- if (survival > 0) survival * sqrt(greenwood) else 0
    rows[[i]] <- data.frame(
      time = time, at_risk = risk, events = deaths,
      survival = survival, std_error = se,
      lower = max(0, survival - z * se), upper = min(1, survival + z * se)
    )
  }
  do.call(rbind, rows)
}

.fit_exponential_survival <- function(durations, events) {
  event_count <- sum(events)
  exposure <- sum(durations)
  if (event_count < 1L || exposure <= 0) {
    return(list(type = "exponential", status = "not estimable: no observed closures",
                scale_days = NA_real_, shape = 1, logLik = NA_real_, AIC = Inf))
  }
  scale <- exposure / event_count
  ll <- sum(events * (-log(scale)) - durations / scale)
  list(type = "exponential", status = "ok", scale_days = scale, shape = 1,
       logLik = ll, AIC = -2 * ll + 2)
}

.fit_weibull_survival <- function(durations, events) {
  if (sum(events) < 2L || length(durations) < 3L) {
    return(list(type = "weibull", status = "not estimable: need at least two observed closures and three observations",
                scale_days = NA_real_, shape = NA_real_, logLik = NA_real_, AIC = Inf))
  }
  t <- pmax(durations, 1e-6)
  lower_scale <- log(max(min(t) * 1e-4, 1e-8))
  upper_scale <- log(max(max(t) * 1e4, 1))
  objective <- function(par) {
    scale <- exp(par[1L])
    shape <- exp(par[2L])
    ratio_power <- (t / scale)^shape
    log_density <- log(shape) - log(scale) + (shape - 1) * (log(t) - log(scale)) - ratio_power
    log_survival <- -ratio_power
    loglik <- sum(events * log_density + (1 - events) * log_survival)
    if (!is.finite(loglik)) 1e100 else -loglik
  }
  init_scale <- stats::median(t)
  fit <- tryCatch(
    stats::optim(
      par = c(log(init_scale), log(1.5)), fn = objective,
      method = "L-BFGS-B",
      lower = c(lower_scale, log(0.1)), upper = c(upper_scale, log(20)),
      control = list(maxit = 3000)
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || !is.finite(fit$value) || fit$convergence != 0L) {
    return(list(type = "weibull", status = "optimisation did not converge",
                scale_days = NA_real_, shape = NA_real_, logLik = NA_real_, AIC = Inf))
  }
  scale <- exp(fit$par[1L])
  shape <- exp(fit$par[2L])
  list(type = "weibull", status = "ok", scale_days = scale, shape = shape,
       logLik = -fit$value, AIC = 2 * fit$value + 4)
}

fit_survival_models <- function(data, observation_time = NULL) {
  observations <- build_survival_observations(data, observation_time = observation_time)
  if (!nrow(observations)) stop("No usable issue durations are available for survival analysis.", call. = FALSE)
  durations <- observations$duration_days
  events <- observations$event
  exponential <- .fit_exponential_survival(durations, events)
  weibull <- .fit_weibull_survival(durations, events)
  km <- kaplan_meier_estimate(durations, events)
  models <- list(exponential = exponential, weibull = weibull)
  valid_aic <- vapply(models, function(x) if (identical(x$status, "ok")) x$AIC else Inf, numeric(1))
  selected_name <- names(which.min(valid_aic))[1L]
  if (!length(selected_name) || !is.finite(min(valid_aic))) selected_name <- NA_character_
  selected <- if (!is.na(selected_name)) models[[selected_name]] else NULL
  warning_messages <- character(0)
  if (sum(events) == 0L) warning_messages <- c(warning_messages, "No closed issues have observed closure times; a resolution-time distribution cannot be estimated.")
  if (sum(events) < 5L) warning_messages <- c(warning_messages, "Fewer than five observed closures: parametric survival estimates may be unstable.")
  list(
    observations = observations,
    km = km,
    models = models,
    selected_name = selected_name,
    selected = selected,
    n_observations = nrow(observations),
    n_events = sum(events),
    n_censored = sum(events == 0L),
    warnings = warning_messages
  )
}

predict_survival <- function(model, time_days) {
  if (is.null(model) || is.null(model$type)) stop("A fitted survival model is required.", call. = FALSE)
  time_days <- pmax(as.numeric(time_days), 0)
  if (model$type == "exponential") {
    if (!is.finite(model$scale_days) || model$scale_days <= 0) stop("Invalid exponential scale parameter.", call. = FALSE)
    return(exp(-time_days / model$scale_days))
  }
  if (model$type == "weibull") {
    if (!is.finite(model$scale_days) || model$scale_days <= 0 || !is.finite(model$shape) || model$shape <= 0) {
      stop("Invalid Weibull parameters.", call. = FALSE)
    }
    return(exp(-((time_days / model$scale_days)^model$shape)))
  }
  stop(sprintf("Unknown survival model type: %s", model$type), call. = FALSE)
}

survival_model_table <- function(fit) {
  models <- fit$models
  do.call(rbind, lapply(c("exponential", "weibull"), function(name) {
    model <- models[[name]]
    data.frame(
      model = name,
      scale_days = if (is.finite(model$scale_days)) model$scale_days else NA_real_,
      shape = if (is.finite(model$shape)) model$shape else NA_real_,
      log_likelihood = if (is.finite(model$logLik)) model$logLik else NA_real_,
      AIC = if (is.finite(model$AIC)) model$AIC else NA_real_,
      status = model$status,
      stringsAsFactors = FALSE
    )
  }))
}

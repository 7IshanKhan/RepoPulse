# Diagnostics are descriptive checks, not formal proof that a model is correct.

diagnose_arrival_models <- function(counts, arrival_fits) {
  y <- as.numeric(counts$count)
  n <- length(y)
  poisson <- arrival_fits$poisson
  if (n > 1L && is.finite(poisson$mu) && poisson$mu > 0) {
    pearson <- sum((y - poisson$mu)^2 / poisson$mu)
    dispersion <- pearson / max(n - 1L, 1L)
  } else {
    pearson <- NA_real_
    dispersion <- NA_real_
  }
  notes <- character(0)
  if (is.finite(dispersion) && dispersion > 1.5) notes <- c(notes, "Poisson Pearson dispersion is above 1.5; negative-binomial variation may be more appropriate.")
  if (is.finite(dispersion) && dispersion < 0.5) notes <- c(notes, "Poisson dispersion is low; check for strong regularity or excess zeros.")
  if (n < 20L) notes <- c(notes, "Fewer than 20 time bins are available; model comparison is exploratory.")
  if (all(y == 0)) notes <- c(notes, "Every time bin has zero arrivals; an arrival distribution cannot be meaningfully compared.")
  list(
    table = arrival_model_table(arrival_fits),
    poisson_pearson_statistic = pearson,
    poisson_dispersion_ratio = dispersion,
    number_of_bins = n,
    notes = notes
  )
}

diagnose_survival_models <- function(survival_fit) {
  table <- survival_model_table(survival_fit)
  notes <- survival_fit$warnings
  if (survival_fit$n_observations > 0L && survival_fit$n_events / survival_fit$n_observations < 0.2) {
    notes <- c(notes, "Most records are right-censored; closure-time model uncertainty may be substantial.")
  }
  valid <- table[table$status == "ok" & is.finite(table$AIC), , drop = FALSE]
  if (nrow(valid) >= 2L) {
    delta <- abs(diff(sort(valid$AIC)))
    if (length(delta) && delta[1L] < 2) notes <- c(notes, "The leading parametric models have similar AIC; do not treat the selected distribution as definitive.")
  }
  list(
    table = table,
    n_observations = survival_fit$n_observations,
    n_events = survival_fit$n_events,
    n_censored = survival_fit$n_censored,
    selected_model = if (is.null(survival_fit$selected_name) || is.na(survival_fit$selected_name)) "none estimable" else survival_fit$selected_name,
    notes = unique(notes)
  )
}

run_model_diagnostics <- function(counts, arrival_fits, survival_fit) {
  list(
    arrivals = diagnose_arrival_models(counts, arrival_fits),
    survival = diagnose_survival_models(survival_fit)
  )
}

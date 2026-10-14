# Poisson and negative-binomial models for issue arrivals.
# Models are fitted to counts per equal-width bin. Forecasts use the same bin width.

fit_arrival_models <- function(counts) {
  if (!is.data.frame(counts) || !"count" %in% names(counts)) {
    stop("counts must be a data.frame containing a count column.", call. = FALSE)
  }
  y <- as.numeric(counts$count)
  if (!length(y) || any(!is.finite(y)) || any(y < 0) || any(abs(y - round(y)) > 1e-8)) {
    stop("Arrival counts must be a non-empty vector of non-negative integers.", call. = FALSE)
  }
  n <- length(y)
  mu <- mean(y)
  poisson_ll <- sum(stats::dpois(y, lambda = mu, log = TRUE))
  poisson <- list(
    type = "poisson", mu = mu, logLik = poisson_ll,
    AIC = -2 * poisson_ll + 2, n = n,
    converged = TRUE, status = "ok",
    bin_width = if ("interval" %in% names(counts)) unique(as.character(counts$interval))[1L] else "unspecified"
  )

  if (all(y == 0)) {
    negative_binomial <- list(
      type = "negative_binomial", mu = 0, size = NA_real_, logLik = 0,
      AIC = Inf, n = n, converged = FALSE,
      status = "not estimable: all bins contain zero arrivals",
      bin_width = poisson$bin_width
    )
  } else {
    variance <- stats::var(y)
    starting_size <- if (is.finite(variance) && variance > mu + 1e-8) mu^2 / (variance - mu) else 100
    starting_size <- min(max(starting_size, 0.1), 1e6)
    objective <- function(par) {
      candidate_mu <- exp(par[1L])
      candidate_size <- exp(par[2L])
      value <- -sum(stats::dnbinom(y, size = candidate_size, mu = candidate_mu, log = TRUE))
      if (!is.finite(value)) 1e100 else value
    }
    fit <- tryCatch(
      stats::optim(
        par = c(log(max(mu, 1e-8)), log(starting_size)), fn = objective,
        method = "L-BFGS-B",
        lower = c(-18, log(1e-3)), upper = c(18, log(1e7)),
        control = list(maxit = 2000)
      ),
      error = function(e) NULL
    )
    if (is.null(fit) || !is.finite(fit$value) || fit$convergence != 0L) {
      negative_binomial <- list(
        type = "negative_binomial", mu = NA_real_, size = NA_real_,
        logLik = NA_real_, AIC = Inf, n = n, converged = FALSE,
        status = "optimisation did not converge", bin_width = poisson$bin_width
      )
    } else {
      nb_mu <- exp(fit$par[1L])
      nb_size <- exp(fit$par[2L])
      nb_ll <- -fit$value
      negative_binomial <- list(
        type = "negative_binomial", mu = nb_mu, size = nb_size,
        logLik = nb_ll, AIC = -2 * nb_ll + 4, n = n,
        converged = TRUE, status = "ok", bin_width = poisson$bin_width
      )
    }
  }

  fits <- list(poisson = poisson, negative_binomial = negative_binomial)
  valid_aic <- vapply(fits, function(x) if (identical(x$status, "ok")) x$AIC else Inf, numeric(1))
  selected_name <- names(which.min(valid_aic))[1L]
  if (!length(selected_name) || !is.finite(min(valid_aic))) selected_name <- "poisson"
  fits$selected_name <- selected_name
  fits$selected <- fits[[selected_name]]
  fits$observed_counts <- y
  fits
}

predict_arrival_counts <- function(model, n_bins = 1L, nsim = 1L) {
  if (is.list(model) && !is.null(model$selected)) model <- model$selected
  n_bins <- as.integer(n_bins)
  nsim <- as.integer(nsim)
  if (length(n_bins) != 1L || is.na(n_bins) || n_bins < 1L ||
      length(nsim) != 1L || is.na(nsim) || nsim < 1L) {
    stop("n_bins and nsim must be positive integers.", call. = FALSE)
  }
  if (is.null(model$type) || is.null(model$mu) || !is.finite(model$mu)) {
    stop("The arrival model is not fitted or has invalid parameters.", call. = FALSE)
  }
  if (model$mu <= 0) return(matrix(0L, nrow = nsim, ncol = n_bins))
  if (model$type == "poisson") {
    return(matrix(stats::rpois(n_bins * nsim, lambda = model$mu), nrow = nsim, ncol = n_bins))
  }
  if (model$type == "negative_binomial") {
    if (!is.finite(model$size) || model$size <= 0) stop("Negative-binomial size parameter is invalid.", call. = FALSE)
    return(matrix(stats::rnbinom(n_bins * nsim, size = model$size, mu = model$mu), nrow = nsim, ncol = n_bins))
  }
  stop(sprintf("Unknown arrival model type: %s", model$type), call. = FALSE)
}

arrival_model_table <- function(fits) {
  names_to_use <- c("poisson", "negative_binomial")
  rows <- lapply(names_to_use, function(name) {
    model <- fits[[name]]
    data.frame(
      model = name,
      mean_per_bin = if (is.finite(model$mu)) model$mu else NA_real_,
      size = if (!is.null(model$size) && is.finite(model$size)) model$size else NA_real_,
      log_likelihood = if (is.finite(model$logLik)) model$logLik else NA_real_,
      AIC = if (is.finite(model$AIC)) model$AIC else NA_real_,
      status = model$status,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

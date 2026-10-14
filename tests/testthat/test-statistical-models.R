testthat::test_that("arrival models fit non-negative integer counts", {
  counts <- data.frame(bin_start = as.Date("2026-01-01") + 0:29,
                       count = c(0, 1, 2, 0, 3, 0, 1, 4, 0, 2, 1, 0, 0, 3, 2,
                                 1, 0, 2, 5, 1, 0, 1, 2, 0, 4, 3, 0, 1, 2, 0),
                       interval = "day")
  fit <- fit_arrival_models(counts)
  testthat::expect_true(fit$poisson$mu >= 0)
  testthat::expect_true(fit$selected$type %in% c("poisson", "negative_binomial"))
  generated <- predict_arrival_counts(fit, n_bins = 5, nsim = 4)
  testthat::expect_equal(dim(generated), c(4L, 5L))
  testthat::expect_true(all(generated >= 0))
})

testthat::test_that("Kaplan-Meier survival estimates remain in [0, 1]", {
  km <- kaplan_meier_estimate(c(2, 3, 3, 5, 8), c(1, 1, 0, 1, 0))
  testthat::expect_true(nrow(km) > 0L)
  testthat::expect_true(all(km$survival >= 0 & km$survival <= 1))
  testthat::expect_true(all(diff(km$survival) <= 1e-12))
  testthat::expect_true(all(km$lower >= 0 & km$upper <= 1))
})

testthat::test_that("survival duration keeps unresolved issues right-censored", {
  data <- data.frame(
    issue_id = c("closed", "open"),
    created_at = c("2026-01-01T00:00:00Z", "2026-01-01T00:00:00Z"),
    closed_at = c("2026-01-06T00:00:00Z", ""),
    state = c("closed", "open"),
    observed_at = c("2026-01-20T00:00:00Z", "2026-01-20T00:00:00Z")
  )
  clean <- normalise_issue_data(data, warn = FALSE)
  obs <- build_survival_observations(clean)
  testthat::expect_equal(sort(obs$event), c(0L, 1L))
  testthat::expect_equal(sort(round(obs$duration_days)), c(5, 19))
})

testthat::test_that("Monte Carlo backlog paths are coherent and reproducible", {
  demo <- utils::read.csv(file.path(.repopulse_root, "data", "demo_issues.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  clean <- normalise_issue_data(demo, warn = FALSE)
  daily <- bin_issue_arrivals(clean, "day")
  a <- fit_arrival_models(daily)
  s <- fit_survival_models(clean)
  f1 <- forecast_backlog(clean, a, s, horizon_days = 7, n_simulations = 100, seed = 77)
  f2 <- forecast_backlog(clean, a, s, horizon_days = 7, n_simulations = 100, seed = 77)
  testthat::expect_equal(dim(f1$paths), c(100L, 8L))
  testthat::expect_true(all(f1$paths >= 0))
  testthat::expect_equal(f1$paths, f2$paths)
  testthat::expect_equal(f1$paths[, 1L], rep(f1$summary$baseline_backlog, 100L))
})

testthat::test_that("backlog reconstruction agrees with the snapshot open count", {
  demo <- utils::read.csv(file.path(.repopulse_root, "data", "demo_issues.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  clean <- normalise_issue_data(demo, warn = FALSE)
  history <- reconstruct_backlog_series(clean)
  expected_open <- as.integer(sum(!clean$is_pull_request & clean$state == "open" & is.na(clean$closed_at)))
  testthat::expect_equal(tail(history$estimated_backlog, 1L), expected_open)
})

testthat::test_that("forecast rejects weekly arrival counts", {
  demo <- utils::read.csv(file.path(.repopulse_root, "data", "demo_issues.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  clean <- normalise_issue_data(demo, warn = FALSE)
  weekly <- fit_arrival_models(bin_issue_arrivals(clean, interval = "week"))
  survival <- fit_survival_models(clean)
  testthat::expect_error(forecast_backlog(clean, weekly, survival, horizon_days = 2, n_simulations = 10), "daily bins")
})

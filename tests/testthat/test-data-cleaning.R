testthat::test_that("demo data normalises to the canonical schema", {
  demo <- utils::read.csv(file.path(.repopulse_root, "data", "demo_issues.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  clean <- normalise_issue_data(demo, warn = FALSE)
  testthat::expect_true(all(repopulse_canonical_columns() %in% names(clean)))
  testthat::expect_s3_class(clean$created_at, "POSIXct")
  testthat::expect_equal(anyDuplicated(clean$issue_id), 0L)
  testthat::expect_false(anyNA(clean$created_at))
  testthat::expect_true(all(clean$state %in% c("open", "closed")))
})

testthat::test_that("invalid close dates are censored rather than converted into negative durations", {
  x <- data.frame(issue_id = "x", created_at = "2026-01-02T00:00:00Z",
                  closed_at = "2026-01-01T00:00:00Z", state = "closed",
                  observed_at = "2026-02-01T00:00:00Z")
  clean <- suppressWarnings(normalise_issue_data(x, warn = TRUE))
  testthat::expect_true(is.na(clean$closed_at))
  testthat::expect_equal(clean$state, "open")
})

testthat::test_that("pull requests are excluded from issue analyses", {
  x <- data.frame(issue_id = c("i", "pr"),
                  created_at = c("2026-01-01", "2026-01-02"),
                  state = c("open", "open"), is_pull_request = c(FALSE, TRUE))
  clean <- normalise_issue_data(x, warn = FALSE)
  testthat::expect_equal(nrow(issue_only_data(clean)), 1L)
  testthat::expect_equal(issue_only_data(clean)$issue_id, "i")
})


testthat::test_that("duplicate IDs prefer the latest known update", {
  x <- data.frame(
    issue_id = c("dup", "dup"),
    created_at = c("2026-01-01T00:00:00Z", "2026-01-01T00:00:00Z"),
    state = c("open", "closed"),
    closed_at = c("", "2026-01-05T00:00:00Z"),
    updated_at = c("2026-01-02T00:00:00Z", "2026-01-06T00:00:00Z"),
    labels = c("older", "newer")
  )
  clean <- suppressWarnings(normalise_issue_data(x, warn = TRUE))
  testthat::expect_equal(nrow(clean), 1L)
  testthat::expect_equal(clean$labels, "newer")
  testthat::expect_equal(clean$state, "closed")
})

testthat::test_that("validation reports defects from the raw input", {
  x <- data.frame(issue_id = c("a", "a", "b"),
                  created_at = c("2026-01-01", "2026-01-02", "not-a-date"),
                  state = c("open", "open", "open"))
  checked <- validate_issue_data(x)
  testthat::expect_equal(checked$duplicate_ids, 1L)
  testthat::expect_equal(checked$missing_created_at, 1L)
  testthat::expect_false(checked$valid)
})

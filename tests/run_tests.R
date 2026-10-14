# Run from the RepoPulse project root: source("tests/run_tests.R")
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("The test suite requires testthat. Run install.packages('testthat') first.", call. = FALSE)
}
testthat::test_dir(file.path("tests", "testthat"), reporter = "summary")

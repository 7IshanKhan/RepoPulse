# Testing and manual smoke test

## Automated tests

From the project root, install `testthat` and run:

```r
source("tests/run_tests.R")
```

The tests cover canonical schema creation, timestamps, invalid close times, pull-request filtering, arrival-model fitting/sampling, Kaplan–Meier bounds/monotonicity, right-censoring semantics, and fixed-seed forecast reproducibility.

## Suggested manual smoke test

1. Run `source("main.R")` from RStudio.
2. Select option 1; ensure the synthetic dataset loads without internet.
3. Select option 4; review the summary and daily count diagnostics.
4. Select option 5; confirm model tables show either `ok` or an honest non-estimable status.
5. Select option 6 with a seven-day horizon and 500 simulations. Verify the baseline equals the number of currently open non-PR issues; verify all forecast counts are non-negative.
6. Select option 7; inspect the Markdown report and CSV files in `reports/`.
7. If GitHub access is configured, fetch a small public repository and compare returned row count to the metadata. Check the cache on a second fetch.

## Interpretation checks

- Do not treat a simulation quantile as a guarantee or as parameter uncertainty.
- If few closures are observed, a fitted survival distribution may not be reliable.
- Compare arrival model AIC and dispersion; do not choose negative binomial just because it is more flexible.
- Confirm issue data coverage before making any claim about a real repository's backlog trend.

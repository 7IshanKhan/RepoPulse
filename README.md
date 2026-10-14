# RepoPulse

**RepoPulse** is a local R application for analysing open-source software issue activity and estimating how a repository's open backlog may evolve. It combines issue-data engineering, count modelling, right-censored survival analysis, Monte Carlo simulation, charts, and exportable reports in one reproducible project.

> **Coursework status:** This repository is designed as a substantial Probability & Statistics project and includes a synthetic demo data set. It is an educational analysis tool, not a production-grade SRE or project-management forecasting service. Model assumptions and limitations are deliberately reported rather than hidden.

## What it does

- Loads the included offline sample, a CSV export, or issues from a public GitHub repository.
- Normalises timestamps to UTC, removes unusable rows, handles duplicate IDs, and flags pull requests so they are excluded from issue-only analysis.
- Summarises open/closed counts, resolution durations, open-issue ages, labels, and daily/weekly arrival patterns.
- Fits Poisson and negative-binomial distributions to issue-arrival counts and compares their likelihood/AIC, with a simple dispersion diagnostic.
- Estimates the Kaplan–Meier survival curve and fits exponential and Weibull resolution-time models using right-censored maximum likelihood.
- Simulates issue-by-issue backlog paths. An existing issue can resolve at most once; new arrivals enter the backlog explicitly; unresolved issues are not given fabricated completion dates.
- Reports expected/median final backlog, a central simulation interval, the probability the backlog grows, optional threshold-exceedance probabilities, and Monte Carlo standard errors.
- Exports a Markdown report, cleaned input data, model tables, forecast summaries/paths, metadata, and (when `ggplot2` is installed) PNG charts.

## Requirements

- R 4.1 or newer is recommended.
- Optional feature packages: `httr2` and `jsonlite` for GitHub access; `ggplot2` for charts; `testthat` for automated tests.
- No Python, Shiny server, database, cloud account, or paid API is required. The core synthetic-demo analysis uses base R.

Open `RepoPulse.Rproj` in RStudio if you use RStudio. Alternatively, in an R console, set the working directory to this folder and run:

```r
source("main.R")
```

The console menu should appear. Choose **1** to load the included offline demo. Then try options 4–8.

### Install optional packages

Run this once if you want GitHub import, plots, and the automated test suite:

```r
source("R/config.R")
install_repopulse_dependencies(include_tests = TRUE)
```

Or install them individually:

```r
install.packages(c("httr2", "jsonlite", "ggplot2", "testthat"))
```

## Fetching GitHub issues

Choose menu option 2 and enter `owner/repository`, for example `r-lib/cli`. Public repositories can usually be fetched without authentication. To increase API rate limits, create a GitHub personal access token and place it in the `GITHUB_TOKEN` environment variable; do not hard-code secrets in R files or commit a token to Git.

Linux/macOS shell example for the current terminal session:

```bash
export GITHUB_TOKEN="your-token"
```

Windows PowerShell example for the current session:

```powershell
$env:GITHUB_TOKEN = "your-token"
```

RepoPulse requests the Issues endpoint with pagination and uses a local RDS cache (default six hours). The API endpoint returns pull requests as well as issues, so the application flags them and excludes them from issue-only analysis. The default limit of 100 pages protects against an unexpectedly large fetch; if this limit is reached, a warning indicates that returned data may be incomplete. Repository deletion, pagination limits, API access, and the repository's history can all affect coverage. The code does not claim that an API snapshot is a complete event log.

## Importing a CSV

Choose menu option 3. RepoPulse expects these required columns:

| Column | Meaning |
|---|---|
| `issue_id` | Unique issue identifier; converted to character. |
| `created_at` | Creation timestamp. |
| `state` | `open` or `closed`. |

The remaining canonical columns are optional and default as follows:

| Column | Meaning / default |
|---|---|
| `closed_at` | Closure timestamp; blank for unresolved issues. |
| `labels` | Labels separated by semicolons. |
| `is_pull_request` | Logical flag; defaults to `FALSE`. |
| `source` | Source label; defaults to `import`. |
| `observed_at` | Snapshot time used to censor unresolved issues; defaults to the import time. |
| `updated_at` | Last update timestamp; optional. |

ISO 8601 timestamps such as `2026-03-02T14:30:00Z` are recommended. A duplicate ID is reduced to one row, preferring the latest `updated_at` value. A close time earlier than creation is treated as invalid and censored rather than retained as a negative duration. A row marked closed but lacking a usable closure timestamp is excluded from survival fitting rather than misrepresented as an open, right-censored issue. Unresolved issues are never assigned a made-up `closed_at` value.

## Statistical approach

### Issue arrivals

For equal-width bins, the Poisson model uses \(Y_t \sim \operatorname{Poisson}(\lambda)\), estimated by the observed mean count. The negative-binomial model estimates its mean \(\mu\) and size/dispersion parameter \(r\) by maximum likelihood. The models are compared using AIC and the Poisson Pearson dispersion ratio. The built-in comparison is a baseline for an approximately stationary process; it does not account for release cycles or changepoints.

### Resolution times and censoring

For each issue, the observed duration is time from creation to closure if a valid closure is recorded; otherwise it is time from creation to the issue's observation snapshot and the record is right-censored. The Kaplan–Meier curve estimates \(S(t)=P(T>t)\) nonparametrically. Exponential and Weibull models are fitted with the censored log-likelihood

\[
\ell = \sum_i \left[\delta_i\log f(t_i) + (1-\delta_i)\log S(t_i)\right],
\]

where \(\delta_i=1\) for a recorded closure and \(0\) for a censored issue. The parametric model with the lower AIC is selected only when it converges. Few recorded closures or a short observation window can make these models unstable; RepoPulse reports such caveats.

### Backlog simulation

At each forecast day, every active issue gets a conditional chance of resolution based on its current age:

\[
P(\text{resolve in next day}\mid T>a)=1-\frac{S(a+1)}{S(a)}.
\]

New arrivals are sampled from the fitted daily arrival model and added after that day's current issues have had their opportunity to resolve. The application records one backlog path per simulation. Summary quantiles describe simulation variation conditional on estimated parameters. The Monte Carlo standard error describes finite simulation noise; **it is not** a confidence interval for all model or data uncertainty.

## Run the tests

From the project root, after installing `testthat`:

```r
source("tests/run_tests.R")
```

The suite checks canonical cleaning, pull-request exclusion, right-censoring, arrival-model output, Kaplan–Meier monotonicity, and reproducibility/basic invariants of the backlog simulator.

## Output files

Menu option 7 writes timestamped files into `reports/`, including:

- `*_report.md` — human-readable analysis report and caveats.
- `*_cleaned_issues.csv` and `*_daily_arrivals.csv` — cleaned input and time-bin counts.
- `*_arrival_models.csv` and `*_survival_models.csv` — fitted model parameters, likelihood/AIC and status.
- `*_forecast_daily_summary.csv`, `*_forecast_paths.csv` and `*_forecast_metadata.txt` — forecast outputs, random seed and model metadata (when a forecast is available).
- `*_arrivals.png`, `*_survival.png`, `*_backlog_history.png`, `*_forecast.png` — charts if `ggplot2` is installed and the relevant plot can be produced.

## Suggested division of labour for a four-person team

1. **Data engineering:** validate imported CSV/GitHub data, API caching/pagination, data quality notes.
2. **Count models:** arrival bins, Poisson/negative-binomial estimation and diagnostics.
3. **Survival and forecasting:** censoring, Kaplan–Meier/parametric fits, Monte Carlo engine, edge cases.
4. **Integration and communication:** CLI workflow, plots, report review, tests, memo, Git history, demo.

Everyone should be able to explain the assumptions and run the demo independently; the module division is not a substitute for cross-review.

## Important limitations

- Issue snapshots are not a perfect historical record. Closed/deleted/older records may be unavailable or truncated.
- A stationary count model may miss release spikes, outages, staffing changes, holidays, and policy shifts.
- Exponential/Weibull survival models impose parametric shapes. A lower AIC does not prove that the distribution is true.
- The forecasting implementation does not integrate over parameter uncertainty; forecasts are conditional on fitted parameters.
- The model assumes observed closure time is a useful proxy for resolution and does not model reopenings as multiple events.
- Demo records are synthetic, and results from them are not evidence about a real repository.

## Reproducible environment

This is a script-based RStudio project rather than an installable package. Exact package snapshots depend on the R version and operating system. To create a version-specific lockfile after installing dependencies, run:

```r
install.packages("renv")
renv::init(bare = TRUE)
renv::snapshot()
```

Then commit the generated `renv.lock` and any renv project files together. A lockfile is not pre-generated because no R runtime is available in the build environment to resolve and test a truthful platform-specific dependency snapshot.

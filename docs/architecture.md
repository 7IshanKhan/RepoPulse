# RepoPulse architecture

```text
User input (offline demo / CSV / GitHub REST API)
                  |
                  v
       R/data_cleaning.R  <---- R/config.R
                  |
                  v
     R/descriptive_stats.R
          |             |
          v             v
 R/arrival_models.R  R/survival_models.R
          |             |
          v             v
    R/model_diagnostics.R
                  |
                  v
       R/forecast_engine.R
                  |
          +-------+--------+
          |                |
          v                v
 R/visualizations.R  R/report_generator.R
          \                /
           \              /
            v            v
               R/interface.R
                     |
                     v
             Console user menu
```

## Design decisions

- **Canonical schema:** all sources are converted to one issue-row representation, with timestamps in UTC.
- **Pull-request separation:** GitHub's Issues endpoint can contain pull-request records; a boolean flag prevents their inclusion in issue arrival, resolution, or backlog calculations.
- **Explicit censoring:** missing closure time means unresolved/censored, not a zero duration and not a completed event.
- **Simple count models:** the Poisson and negative-binomial fits estimate stationary per-bin counts. No hidden time-trend model is imposed.
- **Issue-level queue simulation:** each open issue has an age, each day has a conditional resolution opportunity, and each new issue is added once. This avoids the inconsistent bookkeeping that can result from subtracting an independent total number of resolutions from the backlog.
- **Observable outputs:** fitted tables, simulation paths, seed, metadata, and caveats are exportable so a reviewer can audit intermediate results.
- **Optional user-facing dependencies:** the core statistical computations use base R; external packages are checked only when the associated feature is used.

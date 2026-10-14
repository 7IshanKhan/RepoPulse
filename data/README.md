# Demo data

`demo_issues.csv` is a synthetic, GitHub-shaped data set created specifically for RepoPulse testing and classroom demonstration. It is **not** downloaded from or intended to represent any real repository. It contains issue identifiers, creation/closure timestamps, states, labels, pull-request flags, source labels, observation timestamps, and update timestamps.

The observation snapshot is fixed at 2026-10-01 UTC so the demo is deterministic and its right-censoring behaviour is reproducible. Some records are unresolved at that snapshot. Their `closed_at` values are blank by design; do not fill them with the observation date, because RepoPulse uses that date as a censoring time internally.

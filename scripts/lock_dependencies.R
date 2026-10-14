# Optional: create a platform- and R-version-specific lockfile.
# Run only after dependencies are installed and the application has been smoke-tested.
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::init(bare = TRUE)
renv::snapshot(prompt = FALSE)
cat("Created/updated renv.lock for this local R environment. Review and commit it with the project.\n")

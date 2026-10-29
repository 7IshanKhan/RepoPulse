# Run with:  source("run.R")   then open http://127.0.0.1:8000
# One-time setup: install.packages(c("plumber", "httr2"))
here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
www <- file.path(here, "www")
if (!file.exists(file.path(www, "index.html"))) {
  stop("Can't find www/index.html in ", here,
       ". Set the working directory to the 'app' folder and run source(\"run.R\") again.")
}
options(repopulse.www = www)
plumber::pr_run(plumber::pr(file.path(here, "api.R")), host = "127.0.0.1", port = 8000)

# api.R - serves the RepoPulse frontend (www/index.html) and fetches GitHub issues.
library(plumber)

#* Proxy one page of issues for owner/name. Uses GITHUB_TOKEN if it is set.
#* @get /issues
function(repo, page = 1, res) {
  if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repo)) {
    res$status <- 400
    return(list(error = "repo must look like owner/name"))
  }
  req <- httr2::request(paste0("https://api.github.com/repos/", repo, "/issues"))
  req <- httr2::req_url_query(req, state = "all", per_page = 100, page = as.integer(page))
  req <- httr2::req_headers(req, Accept = "application/vnd.github+json")
  req <- httr2::req_error(req, is_error = function(r) FALSE)
  token <- Sys.getenv("GITHUB_TOKEN")
  if (nzchar(token)) req <- httr2::req_auth_bearer_token(req, token)
  resp <- httr2::req_perform(req)
  res$status <- httr2::resp_status(resp)
  res$setHeader("Content-Type", "application/json")
  res$body <- httr2::resp_body_string(resp)
  res
}

#* Serve the frontend page.
#* @get /
#* @serializer html
function() {
  f <- file.path(getOption("repopulse.www", "www"), "index.html")
  x <- readChar(f, file.size(f), useBytes = TRUE)
  Encoding(x) <- "UTF-8"
  x
}

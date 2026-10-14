
# RepoPulse - GitHub API integration
# Fetches repository issues using GitHub's Link-header pagination.

parse_github_repo <- function(repo_input) {
  repo_input <- trimws(as.character(repo_input)[1])

  if (is.na(repo_input) || !nzchar(repo_input)) {
    stop("Enter a GitHub repository as owner/repository.")
  }

  repo_input <- sub("^https?://github\\.com/", "", repo_input)
  repo_input <- sub("/+$", "", repo_input)
  repo_input <- sub("[?#].*$", "", repo_input)
  repo_input <- sub("\\.git$", "", repo_input)

  parts <- strsplit(repo_input, "/", fixed = TRUE)[[1]]

  if (length(parts) != 2L ||
      any(!nzchar(parts)) ||
      grepl("\\s", repo_input)) {
    stop("Invalid repository. Use the format owner/repository.")
  }

  paste(parts, collapse = "/")
}


.github_timestamp <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) {
    return(NA_character_)
  }

  as.character(x[1])
}


.github_issue_rows_to_data <- function(issues) {
  if (length(issues) == 0L) {
    return(data.frame())
  }

  rows <- lapply(issues, function(issue) {
    labels <- issue$labels

    label_names <- character(0)

    if (length(labels) > 0L) {
      label_names <- vapply(
        labels,
        function(label) {
          if (is.null(label$name)) {
            ""
          } else {
            as.character(label$name)
          }
        },
        character(1)
      )
    }

    label_names <- label_names[nzchar(label_names)]

    assignee_names <- character(0)

    if (length(issue$assignees) > 0L) {
      assignee_names <- vapply(
        issue$assignees,
        function(person) {
          if (is.null(person$login)) {
            ""
          } else {
            as.character(person$login)
          }
        },
        character(1)
      )
    }

    assignee_names <- assignee_names[nzchar(assignee_names)]

    data.frame(
      issue_id = as.character(issue$number %||% NA),
      title = as.character(issue$title %||% ""),
      state = as.character(issue$state %||% ""),
      created_at = .github_timestamp(issue$created_at),
      updated_at = .github_timestamp(issue$updated_at),
      closed_at = .github_timestamp(issue$closed_at),
      labels = paste(label_names, collapse = "; "),
      assignees = paste(assignee_names, collapse = "; "),
      is_pull_request = !is.null(issue$pull_request),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


# Safe fallback for NULL values.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}


.github_next_url <- function(response) {
  link_header <- httr2::resp_header(response, "link")

  if (is.null(link_header) ||
      length(link_header) == 0L ||
      is.na(link_header) ||
      !nzchar(link_header)) {
    return(NULL)
  }

  links <- strsplit(link_header, ",")[[1]]

  for (link in links) {
    link <- trimws(link)

    if (grepl('rel="?next"?', link)) {
      match <- regmatches(
        link,
        regexec("<([^>]+)>", link)
      )[[1]]

      if (length(match) >= 2L) {
        next_url <- sub("&page=[0-9]+", "", match[2])
        return(next_url)
      }
    }
  }

  NULL
}


.github_get_issue_page <- function(
    url,
    token = "",
    max_tries = 3L) {

  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(
      {
        request <- httr2::request(url)

        request <- httr2::req_headers(
          request,
          Accept = "application/vnd.github+json",
          `X-GitHub-Api-Version` = "2022-11-28",
          `User-Agent` = "RepoPulse"
        )

        if (nzchar(token)) {
          request <- httr2::req_auth_bearer_token(
            request,
            token
          )
        }

        response <- httr2::req_perform(request)

        status <- httr2::resp_status(response)

        if (status >= 400L) {
          body <- httr2::resp_body_string(response)

          stop(
            sprintf(
              "GitHub API returned HTTP %s for %s.\n%s",
              status,
              url,
              body
            )
          )
        }

        body <- httr2::resp_body_json(
          response,
          simplifyVector = FALSE
        )

        list(
          issues = body,
          next_url = .github_next_url(response)
        )
      },
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )

    if (is.null(result$error)) {
      return(result)
    }

    if (attempt == max_tries) {
      stop(result$error, call. = FALSE)
    }

    Sys.sleep(attempt)
  }

  stop("GitHub request failed.", call. = FALSE)
}


fetch_github_issues <- function(
    repo_input,
    token = "",
    max_pages = 10L,
    per_page = 100L,
    refresh = FALSE) {

  if (exists("check_repopulse_dependencies", mode = "function")) {
    check_repopulse_dependencies("github")
  }

  repo <- parse_github_repo(repo_input)

  max_pages <- as.integer(max_pages)
  per_page <- as.integer(per_page)

  if (is.na(max_pages) || max_pages < 1L) {
    stop("max_pages must be at least 1.")
  }

  if (is.na(per_page) || per_page < 1L || per_page > 100L) {
    stop("per_page must be between 1 and 100.")
  }

  cache_dir <- file.path("data", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  safe_repo <- gsub("[^A-Za-z0-9_-]", "_", repo)
  cache_file <- file.path(
    cache_dir,
    paste0("github_", safe_repo, ".rds")
  )

  if (!refresh && file.exists(cache_file)) {
    cached <- tryCatch(
      readRDS(cache_file),
      error = function(e) NULL
    )

    if (is.data.frame(cached)) {
      message("Loaded cached GitHub data for ", repo, ".")
      return(cached)
    }
  }

  current_url <- sprintf(
    "https://api.github.com/repos/%s/issues?state=all&per_page=%d",
    repo,
    per_page
  )

  all_issues <- list()
  pages_fetched <- 0L

  while (!is.null(current_url) && pages_fetched < max_pages) {
    page_result <- .github_get_issue_page(
      url = current_url,
      token = token
    )

    issues <- page_result$issues

    if (!is.list(issues)) {
      stop("Unexpected response from GitHub. Expected a list of issues.")
    }

    # GitHub's issues endpoint also returns pull requests.
    # They are identified here and excluded during analysis.
    if (length(issues) > 0L) {
      all_issues <- c(all_issues, issues)
    }

    pages_fetched <- pages_fetched + 1L
    current_url <- page_result$next_url

    message(
      "Fetched page ",
      pages_fetched,
      " for ",
      repo,
      " (",
      length(all_issues),
      " records so far)."
    )
  }

  if (length(all_issues) == 0L) {
    stop(
      "No issues were returned. Check the repository name, ",
      "permissions, and whether the repository has issues enabled."
    )
  }

  raw_data <- .github_issue_rows_to_data(all_issues)

  if (exists("normalise_issue_data", mode = "function")) {
    cleaned_data <- normalise_issue_data(raw_data)
  } else {
    cleaned_data <- raw_data
  }

  attr(cleaned_data, "github_repository") <- repo
  attr(cleaned_data, "github_pages_fetched") <- pages_fetched

  tryCatch(
    saveRDS(cleaned_data, cache_file),
    error = function(e) {
      warning(
        "Could not save GitHub cache: ",
        conditionMessage(e)
      )
    }
  )

  message(
    "Successfully fetched ",
    nrow(cleaned_data),
    " records from ",
    repo,
    "."
  )

  cleaned_data
}

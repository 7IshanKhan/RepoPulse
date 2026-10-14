# Canonical issue schema, validation, and time-at-risk helpers.

repopulse_canonical_columns <- function() {
  c("issue_id", "created_at", "closed_at", "state", "labels",
    "is_pull_request", "source", "observed_at", "updated_at")
}

.parse_one_utc <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  value <- trimws(as.character(x))
  formats <- c(
    "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M",
    "%Y-%m-%d"
  )
  for (fmt in formats) {
    parsed <- suppressWarnings(as.POSIXct(value, format = fmt, tz = "UTC"))
    if (length(parsed) == 1L && !is.na(parsed)) return(parsed)
  }
  as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
}

parse_datetime_utc <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  as.POSIXct(vapply(as.character(x), function(z) {
    value <- .parse_one_utc(z)
    if (is.na(value)) NA_character_ else format(value, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  }, character(1)), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}

.as_logical_flag <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

.normalise_labels <- function(x) {
  if (is.list(x)) {
    return(vapply(x, function(item) {
      if (is.null(item) || !length(item)) return("")
      if (is.list(item)) {
        names_found <- vapply(item, function(one) {
          if (is.list(one) && !is.null(one$name)) as.character(one$name) else as.character(one)
        }, character(1))
        paste(names_found[nzchar(names_found)], collapse = ";")
      } else paste(as.character(item), collapse = ";")
    }, character(1)))
  }
  if (is.null(x)) return(character(0))
  out <- as.character(x)
  out[is.na(out)] <- ""
  out
}

normalise_issue_data <- function(data, observation_time = repopulse_now_utc(), warn = TRUE) {
  if (!is.data.frame(data)) stop("Issue data must be a data.frame.", call. = FALSE)
  if (!nrow(data)) stop("No issue records were supplied.", call. = FALSE)

  required <- c("issue_id", "created_at", "state")
  missing_required <- setdiff(required, names(data))
  if (length(missing_required)) {
    stop(sprintf("Missing required column(s): %s", paste(missing_required, collapse = ", ")), call. = FALSE)
  }

  n <- nrow(data)
  result <- data.frame(
    issue_id = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  result$issue_id <- as.character(data$issue_id)
  result$created_at <- parse_datetime_utc(data$created_at)
  result$closed_at <- if ("closed_at" %in% names(data)) parse_datetime_utc(data$closed_at) else as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  result$state <- tolower(trimws(as.character(data$state)))
  result$state[is.na(result$state) | !nzchar(result$state)] <- "open"
  result$labels <- if ("labels" %in% names(data)) .normalise_labels(data$labels) else rep("", n)
  result$is_pull_request <- if ("is_pull_request" %in% names(data)) .as_logical_flag(data$is_pull_request) else rep(FALSE, n)
  result$source <- if ("source" %in% names(data)) as.character(data$source) else rep("import", n)
  result$source[is.na(result$source) | !nzchar(result$source)] <- "import"
  result$observed_at <- if ("observed_at" %in% names(data)) parse_datetime_utc(data$observed_at) else as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  result$updated_at <- if ("updated_at" %in% names(data)) parse_datetime_utc(data$updated_at) else as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")

  obs_default <- .parse_one_utc(observation_time[1L])
  if (is.na(obs_default)) obs_default <- repopulse_now_utc()
  missing_observation <- is.na(result$observed_at)
  result$observed_at[missing_observation] <- obs_default

  bad_id <- is.na(result$issue_id) | !nzchar(trimws(result$issue_id))
  if (any(bad_id) && warn) warning(sprintf("Removed %d row(s) with missing issue_id.", sum(bad_id)), call. = FALSE)
  if (any(bad_id)) result <- result[!bad_id, , drop = FALSE]

  bad_created <- is.na(result$created_at)
  if (any(bad_created) && warn) warning(sprintf("Removed %d row(s) with invalid or missing created_at.", sum(bad_created)), call. = FALSE)
  if (any(bad_created)) result <- result[!bad_created, , drop = FALSE]

  invalid_close <- !is.na(result$closed_at) & result$closed_at < result$created_at
  if (any(invalid_close)) {
    if (warn) warning(sprintf("Found %d close timestamp(s) earlier than creation; treated these as censored instead of inventing a duration.", sum(invalid_close)), call. = FALSE)
    result$closed_at[invalid_close] <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    result$state[invalid_close] <- "open"
  }

  # A real closure timestamp is stronger evidence than the textual state.
  result$state[!is.na(result$closed_at)] <- "closed"
  allowed_state <- c("open", "closed")
  unknown_state <- !result$state %in% allowed_state
  if (any(unknown_state) && warn) warning("Normalised nonstandard state values; state is treated as open unless a closure timestamp exists.", call. = FALSE)
  result$state[unknown_state] <- "open"

  # Rows for one issue can appear more than once after concatenating snapshots.
  # Keep the row with the latest known update timestamp; ties keep the last row.
  if (anyDuplicated(result$issue_id)) {
    if (warn) warning("Duplicate issue_id values found; retaining the latest updated_at row for each ID.", call. = FALSE)
    result$.row_order <- seq_len(nrow(result))
    result$.updated_rank <- as.numeric(result$updated_at)
    result$.updated_rank[is.na(result$.updated_rank)] <- -Inf
    result <- result[order(result$issue_id, result$.updated_rank, result$.row_order), , drop = FALSE]
    result <- result[!duplicated(result$issue_id, fromLast = TRUE), , drop = FALSE]
    result$.row_order <- NULL
    result$.updated_rank <- NULL
  }

  result <- result[, repopulse_canonical_columns(), drop = FALSE]
  rownames(result) <- NULL
  result
}

validate_issue_data <- function(data) {
  if (!is.data.frame(data)) stop("Issue data must be a data.frame.", call. = FALSE)
  source_rows <- nrow(data)
  duplicate_ids <- if ("issue_id" %in% names(data)) {
    ids <- as.character(data$issue_id)
    sum(duplicated(ids[!is.na(ids) & nzchar(trimws(ids))]))
  } else source_rows
  missing_created_at <- if ("created_at" %in% names(data)) {
    sum(is.na(parse_datetime_utc(data$created_at)))
  } else source_rows
  cleaned <- normalise_issue_data(data, warn = FALSE)
  list(
    valid = nrow(cleaned) > 0L && duplicate_ids == 0L && missing_created_at == 0L,
    source_rows = source_rows,
    row_count = nrow(cleaned),
    rows_removed_or_collapsed = max(source_rows - nrow(cleaned), 0L),
    duplicate_ids = duplicate_ids,
    missing_created_at = missing_created_at,
    open_without_close_time = sum(cleaned$state == "open" & is.na(cleaned$closed_at)),
    closed_without_close_time = sum(cleaned$state == "closed" & is.na(cleaned$closed_at)),
    pull_requests = sum(cleaned$is_pull_request),
    issues = cleaned
  )
}

issue_only_data <- function(data) {
  # Normalise even when the expected columns exist: direct callers may have
  # imported TRUE/FALSE flags as character strings rather than logical values.
  data <- normalise_issue_data(data, warn = FALSE)
  data[!data$is_pull_request, , drop = FALSE]
}

build_survival_observations <- function(data, observation_time = NULL) {
  issues <- issue_only_data(data)
  if (!nrow(issues)) return(data.frame(issue_id = character(), duration_days = numeric(), event = integer()))
  observed <- issues$observed_at
  if (!is.null(observation_time)) {
    global_obs <- .parse_one_utc(observation_time[1L])
    if (!is.na(global_obs)) observed[is.na(observed) | observed > global_obs] <- global_obs
  }
  has_closure <- !is.na(issues$closed_at) & issues$closed_at <= observed
  end_time <- observed
  end_time[has_closure] <- issues$closed_at[has_closure]
  duration <- as.numeric(difftime(end_time, issues$created_at, units = "days"))
  # A record known to be closed but lacking its closure timestamp is neither a
  # completed duration nor a genuinely right-censored open issue; exclude it.
  usable_state <- !(issues$state == "closed" & is.na(issues$closed_at))
  keep <- is.finite(duration) & duration >= 0 & usable_state
  data.frame(
    issue_id = issues$issue_id[keep],
    duration_days = pmax(duration[keep], 1e-6),
    event = as.integer(has_closure[keep]),
    stringsAsFactors = FALSE
  )
}

get_open_issue_ages <- function(data, observation_time = NULL) {
  issues <- issue_only_data(data)
  keep <- issues$state == "open" & is.na(issues$closed_at)
  if (!any(keep)) return(numeric(0))
  observed <- issues$observed_at[keep]
  if (!is.null(observation_time)) {
    global_obs <- .parse_one_utc(observation_time[1L])
    if (!is.na(global_obs)) observed[is.na(observed) | observed > global_obs] <- global_obs
  }
  ages <- as.numeric(difftime(observed, issues$created_at[keep], units = "days"))
  ages[is.finite(ages) & ages >= 0]
}

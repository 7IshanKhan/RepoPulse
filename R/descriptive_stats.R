# Descriptive summaries and issue arrival/backlog time series.

summarise_issues <- function(data) {
  issues <- issue_only_data(data)
  closed_durations <- build_survival_observations(issues)
  closed_durations <- closed_durations[closed_durations$event == 1L, , drop = FALSE]
  open_n <- sum(issues$state == "open" & is.na(issues$closed_at))
  closed_n <- sum(!is.na(issues$closed_at))
  total <- nrow(issues)
  age_days <- get_open_issue_ages(issues)

  data.frame(
    metric = c(
      "Issues analysed (pull requests excluded)", "Currently open issues",
      "Issues with closure timestamp", "Open share (%)", "Median completed resolution time (days)",
      "Mean completed resolution time (days)", "Median age of currently open issues (days)",
      "Oldest currently open issue (days)"
    ),
    value = c(
      total, open_n, closed_n,
      if (total > 0) 100 * open_n / total else NA_real_,
      if (nrow(closed_durations)) stats::median(closed_durations$duration_days) else NA_real_,
      if (nrow(closed_durations)) mean(closed_durations$duration_days) else NA_real_,
      if (length(age_days)) stats::median(age_days) else NA_real_,
      if (length(age_days)) max(age_days) else NA_real_
    ),
    stringsAsFactors = FALSE
  )
}

bin_issue_arrivals <- function(data, interval = c("day", "week"), start_date = NULL, end_date = NULL) {
  interval <- match.arg(interval)
  issues <- issue_only_data(data)
  dates <- as.Date(issues$created_at, tz = "UTC")
  dates <- dates[!is.na(dates)]
  if (is.null(start_date)) {
    if (!length(dates)) stop("No valid creation dates are available.", call. = FALSE)
    start_date <- min(dates)
  } else start_date <- as.Date(start_date)
  if (is.null(end_date)) {
    if (!length(dates)) stop("No valid creation dates are available.", call. = FALSE)
    observed_dates <- as.Date(issues$observed_at, tz = "UTC")
    observed_end <- suppressWarnings(max(observed_dates, na.rm = TRUE))
    if (!is.finite(as.numeric(observed_end))) observed_end <- max(dates)
    # Include zero-arrival days through the observation snapshot. Stopping at the
    # latest creation date would understate the observation window and inflate rate.
    end_date <- max(max(dates), observed_end)
  } else end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) stop("Invalid date range.", call. = FALSE)

  to_bin <- function(x) {
    if (interval == "day") return(as.Date(x))
    as.Date(x) - (as.integer(format(as.Date(x), "%u")) - 1L)
  }
  first <- to_bin(start_date)
  last <- to_bin(end_date)
  bins <- seq(first, last, by = if (interval == "day") "day" else "week")
  issue_bins <- to_bin(dates)
  matched_bins <- match(issue_bins, bins)
  matched_bins <- matched_bins[!is.na(matched_bins)]
  counts <- tabulate(matched_bins, nbins = length(bins))
  data.frame(
    bin_start = bins,
    count = as.integer(counts),
    interval = interval,
    stringsAsFactors = FALSE
  )
}

reconstruct_backlog_series <- function(data, start_date = NULL, end_date = NULL) {
  issues <- issue_only_data(data)
  if (!nrow(issues)) return(data.frame(date = as.Date(character()), estimated_backlog = integer()))
  created <- as.Date(issues$created_at, tz = "UTC")
  closed <- as.Date(issues$closed_at, tz = "UTC")
  if (is.null(start_date)) start_date <- min(created, na.rm = TRUE) else start_date <- as.Date(start_date)
  if (is.null(end_date)) end_date <- max(as.Date(issues$observed_at, tz = "UTC"), na.rm = TRUE) else end_date <- as.Date(end_date)
  if (!is.finite(as.numeric(start_date)) || !is.finite(as.numeric(end_date)) || end_date < start_date) {
    stop("Invalid backlog reconstruction date range.", call. = FALSE)
  }
  dates <- seq(start_date, end_date, by = "day")
  known_for_reconstruction <- !(issues$state == "closed" & is.na(issues$closed_at))
  backlog <- vapply(dates, function(day) {
    as.integer(sum(known_for_reconstruction & !is.na(created) & created <= day & (is.na(closed) | closed > day)))
  }, integer(1))
  data.frame(date = dates, estimated_backlog = backlog, stringsAsFactors = FALSE)
}

summarise_labels <- function(data, top_n = 10L) {
  issues <- issue_only_data(data)
  if (!nrow(issues)) return(data.frame(label = character(), count = integer()))
  all_labels <- unlist(strsplit(issues$labels[nzchar(issues$labels)], ";", fixed = TRUE), use.names = FALSE)
  all_labels <- trimws(all_labels[nzchar(trimws(all_labels))])
  if (!length(all_labels)) return(data.frame(label = character(), count = integer()))
  counts <- sort(table(all_labels), decreasing = TRUE)
  top_n <- max(1L, as.integer(top_n))
  data.frame(label = names(counts)[seq_len(min(top_n, length(counts)))],
             count = as.integer(counts[seq_len(min(top_n, length(counts)))]),
             row.names = NULL, stringsAsFactors = FALSE)
}

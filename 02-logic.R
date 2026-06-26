# 02-logic.R --------------------------------------------------------------
# LOGIC LAYER
# Pure functions for adding up time and shaping data for the live chart.
# No Shiny, no Google here on purpose - easy to test in isolation.

library(dplyr)

# Return b when a is NULL / empty / NA, otherwise a.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# Total minutes of one type ("study" or "break") in a log.
sum_minutes <- function(log_df, type) {
  if (nrow(log_df) == 0) return(0)
  log_df |>
    filter(.data$type == !!type) |>
    summarise(m = sum(duration_min, na.rm = TRUE)) |>
    pull(m) %||% 0
}

# Per-DATE breakdown: study, break, and combined total.
# This is your "date encoded" + "total_time / total_study_time" view.
daily_summary <- function(log_df) {
  if (nrow(log_df) == 0) {
    return(tibble::tibble(date = as.Date(character()),
                          study_min = numeric(),
                          break_min = numeric(),
                          total_min = numeric()))
  }
  log_df |>
    group_by(date) |>
    summarise(
      study_min = sum(duration_min[type == "study"], na.rm = TRUE),
      break_min = sum(duration_min[type == "break"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(total_min = study_min + break_min) |>
    arrange(date)
}

# Build a cumulative-study-minutes time series for the CURRENT session,
# including the in-progress segment so the line moves while you study.
# (When nothing is running, the live point is simply omitted -> no false counting.)
cumulative_study <- function(session_log, seg_start, mode, running) {
  pts <- tibble::tibble(time = as.POSIXct(character()), cum_min = numeric())
  
  study <- session_log |> filter(type == "study") |> arrange(end_time)
  if (nrow(study) > 0) {
    pts <- tibble::tibble(time = study$end_time, cum_min = cumsum(study$duration_min))
    pts <- bind_rows(tibble::tibble(time = min(study$start_time), cum_min = 0), pts)
  }
  
  if (isTRUE(running) && identical(mode, "study") && !is.null(seg_start)) {
    base <- if (nrow(pts)) max(pts$cum_min) else 0
    live <- as.numeric(difftime(Sys.time(), seg_start, units = "mins"))
    if (!nrow(pts)) pts <- bind_rows(pts, tibble::tibble(time = seg_start, cum_min = 0))
    pts <- bind_rows(pts, tibble::tibble(time = Sys.time(), cum_min = base + live))
  }
  pts
}

# Pretty time formatting for the dashboard.
fmt_hms <- function(secs) {            # 01:23:45
  secs <- as.integer(round(secs))
  sprintf("%02d:%02d:%02d", secs %/% 3600, (secs %% 3600) %/% 60, secs %% 60)
}
fmt_hm <- function(mins) {             # 2h 05m
  secs <- as.integer(round(mins * 60))
  sprintf("%dh %02dm", secs %/% 3600, (secs %% 3600) %/% 60)
}
# 01-main.R ---------------------------------------------------------------
# DATA LAYER
# Authenticate to Google and read/write the study log Google Sheet.
# Conceptually this is your "study_time / task_name / break_time" plumbing:
# every completed segment becomes one row in the sheet.

library(googlesheets4)
library(dplyr)

# Name of the tab (worksheet) inside your spreadsheet that holds the log.
SHEET_TAB <- "log"

# The columns we store for every segment.
LOG_COLS <- c("date", "task_name", "type", "start_time", "end_time", "duration_min")

# An empty, correctly-typed log (used as a fallback and to seed headers).
empty_log <- function() {
  tibble::tibble(
    date         = as.Date(character()),
    task_name    = character(),
    type         = character(),                 # "study" or "break"
    start_time   = as.POSIXct(character()),
    end_time     = as.POSIXct(character()),
    duration_min = numeric()
  )
}

# Authenticate WITHOUT a browser, so it works on a deployed server.
#   Local dev : place the service-account JSON at  service-account.json  (gitignored)
#   Deployed  : paste the JSON *contents* into env var GOOGLE_SERVICE_ACCOUNT_JSON
init_sheets_auth <- function() {
  json_env <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
  if (nzchar(json_env)) {
    tmp <- tempfile(fileext = ".json")
    writeLines(json_env, tmp)
    gs4_auth(path = tmp)
  } else if (file.exists("service-account.json")) {
    gs4_auth(path = "service-account.json")
  } else {
    stop("No Google credentials found. Set GOOGLE_SERVICE_ACCOUNT_JSON ",
         "or add a service-account.json file. See README.md.")
  }
}

# Make sure the "log" tab exists with the right header row. Safe to call on startup.
ensure_log_sheet <- function(sheet_id) {
  tryCatch({
    tabs <- sheet_names(sheet_id)
    if (!(SHEET_TAB %in% tabs)) {
      sheet_add(sheet_id, sheet = SHEET_TAB)
      sheet_write(empty_log(), ss = sheet_id, sheet = SHEET_TAB)  # writes header row only
    }
  }, error = function(e) warning("ensure_log_sheet: ", conditionMessage(e)))
}

# Build one log row from a finished segment.
new_segment_row <- function(task_name, type, start, end, duration_min) {
  tibble::tibble(
    date         = as.Date(start),
    task_name    = if (is.null(task_name) || !nzchar(task_name)) "(no label)" else task_name,
    type         = type,
    start_time   = start,
    end_time     = end,
    duration_min = round(duration_min, 3)
  )
}

# Read everything currently in the sheet. Returns empty_log() if blank/unavailable.
read_history <- function(sheet_id) {
  out <- tryCatch(read_sheet(sheet_id, sheet = SHEET_TAB),
                  error = function(e) NULL)
  if (is.null(out) || nrow(out) == 0) return(empty_log())
  out |>
    mutate(
      date         = as.Date(date),
      start_time   = as.POSIXct(start_time),
      end_time     = as.POSIXct(end_time),
      duration_min = as.numeric(duration_min)
    )
}

# Append a single finished segment as a new row at the bottom of the sheet.
append_segment <- function(sheet_id, row) {
  tryCatch(
    sheet_append(sheet_id, data = row, sheet = SHEET_TAB),
    error = function(e) warning("Could not write to sheet: ", conditionMessage(e))
  )
}
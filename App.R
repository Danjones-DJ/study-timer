# app.R -------------------------------------------------------------------
# ENTRY POINT (this is the file you tell Posit Connect Cloud to run).
# Wires the buttons to the logic and the Google Sheet.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(googlesheets4)

source("01-main.R")   # data layer  (Google Sheets)
source("02-logic.R")  # logic layer (adding up time, chart)

# --- config + auth (runs once at startup) --------------------------------
SHEET_ID <- Sys.getenv("STUDY_SHEET_ID")          # set this as an env var
init_sheets_auth()
ensure_log_sheet(SHEET_ID)

# --- UI -------------------------------------------------------------------
ui <- page_sidebar(
  title = "Study Timer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    textInput("task_name", "What are you working on?",
              placeholder = "e.g. Stats revision"),
    actionButton("start_study", "Start studying", class = "btn-success w-100 mb-2"),
    actionButton("start_break", "Start break",    class = "btn-warning w-100 mb-2"),
    actionButton("stop",        "Stop",           class = "btn-danger  w-100"),
    hr(),
    strong(textOutput("status", inline = TRUE))
  ),
  
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    value_box("Current segment", textOutput("current_elapsed"), theme = "primary"),
    value_box("Study today",     textOutput("study_today"),     theme = "success"),
    value_box("Break today",     textOutput("break_today"),     theme = "warning"),
    value_box("Total today",     textOutput("total_today"),     theme = "secondary")
  ),
  
  card(
    card_header("Study time this session (live)"),
    plotOutput("live_plot", height = "320px")
  )
)

# --- server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  tick <- reactiveTimer(1000)        # heartbeat: refresh readouts every second
  # (change to 60000 for once-a-minute)
  
  rv <- reactiveValues(
    running     = FALSE,
    mode        = NA_character_,     # "study" or "break"
    seg_start   = NULL,              # POSIXct when current segment started
    session_log = empty_log()        # segments completed this session
  )
  
  history <- reactiveVal(read_history(SHEET_ID))  # everything from past days
  
  # Close + record the active segment.
  close_segment <- function() {
    if (!rv$running || is.null(rv$seg_start)) return(invisible())
    end <- Sys.time()
    dur <- as.numeric(difftime(end, rv$seg_start, units = "mins"))
    row <- new_segment_row(isolate(input$task_name), rv$mode, rv$seg_start, end, dur)
    rv$session_log <- bind_rows(rv$session_log, row)
    append_segment(SHEET_ID, row)            # persist to Google Sheet
    rv$running <- FALSE; rv$seg_start <- NULL; rv$mode <- NA_character_
  }
  
  # Start a new segment (auto-closes any running one first).
  start_segment <- function(mode) {
    close_segment()
    rv$mode <- mode; rv$seg_start <- Sys.time(); rv$running <- TRUE
  }
  
  observeEvent(input$start_study, start_segment("study"))
  observeEvent(input$start_break, start_segment("break"))
  observeEvent(input$stop,        close_segment())
  
  # Live elapsed of the running segment; 0 when stopped (never false-counts).
  current_secs <- reactive({
    tick()
    if (rv$running && !is.null(rv$seg_start))
      as.numeric(difftime(Sys.time(), rv$seg_start, units = "secs")) else 0
  })
  
  # Today's totals = history + this session's closed segments + the live one.
  today_minutes <- reactive({
    tick()
    today <- bind_rows(history(), rv$session_log) |> filter(date == Sys.Date())
    s <- sum_minutes(today, "study")
    b <- sum_minutes(today, "break")
    live <- current_secs() / 60
    if (isTRUE(rv$running) && identical(rv$mode, "study")) s <- s + live
    if (isTRUE(rv$running) && identical(rv$mode, "break")) b <- b + live
    list(study = s, brk = b, total = s + b)
  })
  
  output$status          <- renderText(if (rv$running) paste("Running:", rv$mode) else "Stopped")
  output$current_elapsed <- renderText(fmt_hms(current_secs()))
  output$study_today     <- renderText(fmt_hm(today_minutes()$study))
  output$break_today     <- renderText(fmt_hm(today_minutes()$brk))
  output$total_today     <- renderText(fmt_hm(today_minutes()$total))
  
  output$live_plot <- renderPlot({
    tick()
    df <- cumulative_study(rv$session_log, rv$seg_start, rv$mode, rv$running)
    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "Press \"Start studying\" to begin") +
        theme_void()
    } else {
      ggplot(df, aes(time, cum_min)) +
        geom_line(linewidth = 1, colour = "#2C7BB6") +
        geom_point(size = 1.6, colour = "#2C7BB6") +
        labs(x = NULL, y = "Cumulative study minutes") +
        theme_minimal(base_size = 14)
    }
  })
}

shinyApp(ui, server)
# app.R -------------------------------------------------------------------
# ENTRY POINT (the file Posit Connect Cloud runs).
# Dark minimalist UI. Chart tracks cumulative study time in minutes.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(googlesheets4)

source("01-main.R")   # data layer  (Google Sheets)
source("02-logic.R")  # logic layer (adding up time, chart)

# --- config + auth (runs once at startup) --------------------------------
SHEET_ID <- Sys.getenv("STUDY_SHEET_ID")
init_sheets_auth()
ensure_log_sheet(SHEET_ID)

# --- theme ----------------------------------------------------------------
my_theme <- bs_theme(
  version   = 5,
  bg        = "#0E0F12",
  fg        = "#ECEDEF",
  primary   = "#7DE2C3",
  base_font = font_collection(font_google("Inter", local = FALSE),
                              "system-ui", "sans-serif")
)

css <- "
:root{
  --bg:#0E0F12; --surface:#16181D; --surface2:#1C1F26;
  --border:rgba(255,255,255,.07);
  --text:#ECEDEF; --muted:#8B919C;
  --accent:#7DE2C3; --accent-soft:rgba(125,226,195,.12);
  --break:#E2C37D; --break-soft:rgba(226,195,125,.12);
  --stop:#E27D8B;  --stop-soft:rgba(226,125,139,.10);
}
*{box-sizing:border-box;}
body,.bslib-page-sidebar{background:var(--bg)!important;color:var(--text);}
.navbar{display:none;}                       /* drop the default header bar */

/* sidebar */
.bslib-sidebar-layout>.sidebar{background:var(--surface2)!important;
  border-right:1px solid var(--border)!important;}
.bslib-sidebar-layout .form-control{background:var(--surface);
  border:1px solid var(--border);color:var(--text);border-radius:10px;
  padding:10px 12px;font-size:.92rem;}
.bslib-sidebar-layout .form-control:focus{border-color:var(--accent);
  box-shadow:0 0 0 3px var(--accent-soft);}
.form-control::placeholder{color:var(--muted);}
.fld-label{font-size:.7rem;letter-spacing:.12em;text-transform:uppercase;
  color:var(--muted);margin-bottom:6px;display:block;font-weight:600;}

/* buttons */
.bslib-sidebar-layout .btn{width:100%;border-radius:11px;padding:12px 14px;
  font-weight:600;font-size:.92rem;border:1px solid var(--border);margin-top:10px;
  transition:all .15s ease;background:transparent;color:var(--text);}
.btn-study{background:var(--accent)!important;color:#0E0F12!important;border:none!important;}
.btn-study:hover{filter:brightness(1.08);transform:translateY(-1px);}
.btn-break{color:var(--break)!important;background:var(--break-soft)!important;
  border-color:transparent!important;}
.btn-break:hover{filter:brightness(1.15);}
.btn-stop{color:var(--stop)!important;background:var(--stop-soft)!important;
  border-color:transparent!important;}
.btn-stop:hover{filter:brightness(1.15);}

/* status */
.status-row{margin-top:20px;display:flex;align-items:center;gap:9px;
  font-size:.85rem;color:var(--muted);}
.status-dot{width:8px;height:8px;border-radius:50%;background:var(--muted);}
.status-dot.live{background:var(--accent);box-shadow:0 0 0 4px var(--accent-soft);}

/* page */
.page{padding:22px 24px;}
.topbar{display:flex;align-items:center;gap:10px;margin-bottom:24px;}
.topbar .dot{width:9px;height:9px;border-radius:50%;background:var(--accent);}
.topbar .brand{font-size:1.15rem;font-weight:700;letter-spacing:-.01em;}
.topbar .sub{font-size:.8rem;color:var(--muted);margin-left:2px;}

/* stat cards */
.stat-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:18px;}
@media(max-width:820px){.stat-grid{grid-template-columns:repeat(2,1fr);}}
.stat{background:var(--surface);border:1px solid var(--border);
  border-radius:14px;padding:16px 18px;}
.stat-label{font-size:.7rem;letter-spacing:.1em;text-transform:uppercase;
  color:var(--muted);font-weight:600;margin-bottom:8px;}
.stat-value{font-size:1.7rem;font-weight:600;font-variant-numeric:tabular-nums;
  letter-spacing:-.02em;line-height:1;}
.stat-value .shiny-text-output{display:inline;}
.stat-accent .stat-value{color:var(--accent);}

/* chart */
.chart-card{background:var(--surface);border:1px solid var(--border);
  border-radius:16px;padding:18px 18px 8px;}
.chart-head{font-size:.72rem;letter-spacing:.12em;text-transform:uppercase;
  color:var(--muted);font-weight:600;margin-bottom:12px;}
"

# small helper for a stat card
stat_card <- function(label, id, accent = FALSE) {
  div(class = paste("stat", if (accent) "stat-accent"),
      div(class = "stat-label", label),
      div(class = "stat-value", textOutput(id)))
}

# --- UI -------------------------------------------------------------------
ui <- page_sidebar(
  theme = my_theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 300,
    tags$span(class = "fld-label", "Task"),
    textInput("task_name", NULL, placeholder = "What are you working on?"),
    actionButton("start_study", "Start studying", class = "btn-study"),
    actionButton("start_break", "Start break",    class = "btn-break"),
    actionButton("stop",        "Stop",           class = "btn-stop"),
    uiOutput("status_ui")
  ),
  tags$head(tags$style(HTML(css))),
  div(class = "page",
      div(class = "topbar",
          span(class = "dot"), span(class = "brand", "Focus"),
          span(class = "sub", "study timer")),
      div(class = "stat-grid",
          stat_card("Current",     "current_elapsed", accent = TRUE),
          stat_card("Study today", "study_today"),
          stat_card("Break today", "break_today"),
          stat_card("Total today", "total_today")),
      div(class = "chart-card",
          div(class = "chart-head", "Study minutes \u00B7 this session"),
          plotOutput("live_plot", height = "300px"))
  )
)

# --- server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  tick_sec <- reactiveTimer(1000)    # live clock + today's totals
  tick_min <- reactiveTimer(60000)   # chart advances once a minute
  
  rv <- reactiveValues(running = FALSE, mode = NA_character_,
                       seg_start = NULL, session_log = empty_log())
  history <- reactiveVal(read_history(SHEET_ID))
  
  close_segment <- function() {
    if (!rv$running || is.null(rv$seg_start)) return(invisible())
    end <- Sys.time()
    dur <- as.numeric(difftime(end, rv$seg_start, units = "mins"))
    row <- new_segment_row(isolate(input$task_name), rv$mode, rv$seg_start, end, dur)
    rv$session_log <- bind_rows(rv$session_log, row)
    append_segment(SHEET_ID, row)
    rv$running <- FALSE; rv$seg_start <- NULL; rv$mode <- NA_character_
  }
  start_segment <- function(mode) {
    close_segment()
    rv$mode <- mode; rv$seg_start <- Sys.time(); rv$running <- TRUE
  }
  
  observeEvent(input$start_study, start_segment("study"))
  observeEvent(input$start_break, start_segment("break"))
  observeEvent(input$stop,        close_segment())
  
  current_secs <- reactive({
    tick_sec()
    if (rv$running && !is.null(rv$seg_start))
      as.numeric(difftime(Sys.time(), rv$seg_start, units = "secs")) else 0
  })
  
  today_minutes <- reactive({
    tick_sec()
    today <- bind_rows(history(), rv$session_log) |> filter(date == Sys.Date())
    s <- sum_minutes(today, "study"); b <- sum_minutes(today, "break")
    live <- current_secs() / 60
    if (isTRUE(rv$running) && identical(rv$mode, "study")) s <- s + live
    if (isTRUE(rv$running) && identical(rv$mode, "break")) b <- b + live
    list(study = s, brk = b, total = s + b)
  })
  
  output$status_ui <- renderUI({
    tick_sec()
    live  <- isTRUE(rv$running)
    label <- if (!live) "idle"
    else if (identical(rv$mode, "study")) "studying\u2026" else "on a break\u2026"
    div(class = "status-row",
        span(class = paste("status-dot", if (live) "live")),
        span(label))
  })
  
  output$current_elapsed <- renderText(fmt_hms(current_secs()))
  output$study_today     <- renderText(fmt_hm(today_minutes()$study))
  output$break_today     <- renderText(fmt_hm(today_minutes()$brk))
  output$total_today     <- renderText(fmt_hm(today_minutes()$total))
  
  output$live_plot <- renderPlot({
    tick_min()                                   # redraw each minute
    df <- cumulative_study(rv$session_log, rv$seg_start, rv$mode, rv$running)
    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0, y = 0,
                 label = "Press \u201CStart studying\u201D to begin",
                 colour = "#8B919C", size = 4.4) +
        theme_void()
    } else {
      ggplot(df, aes(time, cum_min)) +
        geom_area(fill = "#7DE2C3", alpha = 0.10) +
        geom_line(colour = "#7DE2C3", linewidth = 1.1) +
        geom_point(data = utils::tail(df, 1), colour = "#7DE2C3", size = 2.4) +
        scale_y_continuous(labels = function(x) paste0(round(x), "m")) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 13) +
        theme(
          plot.background   = element_rect(fill = "transparent", colour = NA),
          panel.background  = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor  = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#FFFFFF12"),
          axis.text  = element_text(colour = "#8B919C"),
          axis.ticks = element_blank(),
          text       = element_text(colour = "#8B919C")
        )
    }
  }, bg = "transparent")
}

shinyApp(ui, server)
# app.R -------------------------------------------------------------------
# ENTRY POINT (the file Posit Connect Cloud runs).
# Premium dark UI: ambient glow, gradient buttons, hover lifts, tooltips.
# Current timer ticks in min:sec; chart accrues in whole minutes.

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
  bg        = "#0A0B0D",
  fg        = "#F4F5F6",
  primary   = "#5EEAD4",
  base_font = font_collection(font_google("Inter", local = FALSE),
                              "system-ui", "sans-serif")
)

css <- "
:root{
  --bg:#0A0B0D; --surface:#131519; --surface2:#15171C;
  --border:rgba(255,255,255,.07); --border-hi:rgba(255,255,255,.14);
  --text:#F4F5F6; --muted:#888F9B;
  --accent:#5EEAD4; --accent2:#34D399; --accent-soft:rgba(94,234,212,.12);
  --break:#FBBF77; --break-soft:rgba(251,191,119,.10);
  --stop:#F4889A;  --stop-soft:rgba(244,136,154,.10);
  --grad:linear-gradient(135deg,#5EEAD4 0%,#34D399 100%);
  --mono:ui-monospace,'SF Mono',SFMono-Regular,Menlo,monospace;
}
*{box-sizing:border-box;}
body,.bslib-page-sidebar{background:var(--bg)!important;color:var(--text);}
.navbar{display:none;}

/* ambient glow behind everything */
body::before{content:'';position:fixed;inset:0;pointer-events:none;z-index:0;
  background:radial-gradient(620px 380px at 78% -8%,rgba(94,234,212,.10),transparent 60%),
             radial-gradient(520px 360px at 8% 110%,rgba(52,211,153,.06),transparent 60%);}
.bslib-sidebar-layout,.page{position:relative;z-index:1;}

/* sidebar */
.bslib-sidebar-layout>.sidebar{background:var(--surface2)!important;
  border-right:1px solid var(--border)!important;}
.bslib-sidebar-layout .form-control{background:#0F1115;
  border:1px solid var(--border);color:var(--text);border-radius:10px;
  padding:11px 13px;font-size:.92rem;transition:border-color .15s,box-shadow .15s;}
.bslib-sidebar-layout .form-control:focus{border-color:var(--accent);
  box-shadow:0 0 0 3px var(--accent-soft);}
.form-control::placeholder{color:var(--muted);}
.fld-label{font-size:.68rem;letter-spacing:.14em;text-transform:uppercase;
  color:var(--muted);margin-bottom:7px;display:block;font-weight:600;}

/* buttons */
.bslib-sidebar-layout .btn{width:100%;border-radius:12px;padding:12px 14px;
  font-weight:600;font-size:.92rem;border:1px solid var(--border);margin-top:11px;
  display:flex;align-items:center;justify-content:center;gap:9px;
  background:transparent;color:var(--text);
  transition:transform .16s cubic-bezier(.2,.8,.2,1),box-shadow .16s,filter .16s,background .16s;}
.bslib-sidebar-layout .btn .fa,.bslib-sidebar-layout .btn svg{opacity:.9;}
.btn-study{background:var(--grad)!important;color:#06231D!important;border:none!important;
  box-shadow:0 6px 20px -8px rgba(52,211,153,.55);}
.btn-study:hover{transform:translateY(-2px);filter:brightness(1.05);
  box-shadow:0 12px 28px -10px rgba(52,211,153,.7);}
.btn-study:active{transform:translateY(0) scale(.99);}
.btn-break{color:var(--break)!important;background:var(--break-soft)!important;border-color:transparent!important;}
.btn-break:hover{transform:translateY(-2px);background:rgba(251,191,119,.16)!important;}
.btn-stop{color:var(--stop)!important;background:var(--stop-soft)!important;border-color:transparent!important;}
.btn-stop:hover{transform:translateY(-2px);background:rgba(244,136,154,.16)!important;}

/* status */
.status-row{margin-top:22px;display:flex;align-items:center;gap:9px;
  font-size:.85rem;color:var(--muted);}
.status-dot{width:8px;height:8px;border-radius:50%;background:var(--muted);}
.status-dot.live{background:var(--accent);box-shadow:0 0 0 0 var(--accent-soft);
  animation:pulse 1.8s infinite;}
@keyframes pulse{0%{box-shadow:0 0 0 0 rgba(94,234,212,.45);}
  70%{box-shadow:0 0 0 8px rgba(94,234,212,0);}100%{box-shadow:0 0 0 0 rgba(94,234,212,0);}}

/* page + header */
.page{padding:24px 26px;}
.topbar{display:flex;align-items:center;gap:11px;margin-bottom:26px;}
.topbar .dot{width:10px;height:10px;border-radius:50%;background:var(--grad);
  box-shadow:0 0 14px rgba(94,234,212,.6);}
.topbar .brand{font-size:1.18rem;font-weight:700;letter-spacing:-.015em;}
.topbar .sub{font-size:.8rem;color:var(--muted);}

/* stat cards */
.stat-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:18px;}
@media(max-width:860px){.stat-grid{grid-template-columns:repeat(2,1fr);}}
.stat{background:linear-gradient(180deg,rgba(255,255,255,.025),transparent),var(--surface);
  border:1px solid var(--border);border-radius:16px;padding:17px 19px;cursor:default;
  transition:transform .18s cubic-bezier(.2,.8,.2,1),border-color .18s,box-shadow .18s;}
.stat:hover{transform:translateY(-3px);border-color:var(--border-hi);
  box-shadow:0 16px 34px -20px rgba(0,0,0,.8);}
.stat-label{font-size:.68rem;letter-spacing:.12em;text-transform:uppercase;
  color:var(--muted);font-weight:600;margin-bottom:9px;}
.stat-value{font-size:1.75rem;font-weight:650;font-variant-numeric:tabular-nums;
  letter-spacing:-.02em;line-height:1;}
.stat-value .shiny-text-output{display:inline;}
.stat-accent .stat-value{color:var(--accent);font-family:var(--mono);font-weight:600;
  text-shadow:0 0 22px rgba(94,234,212,.35);}

/* chart */
.chart-card{background:linear-gradient(180deg,rgba(255,255,255,.02),transparent),var(--surface);
  border:1px solid var(--border);border-radius:18px;padding:18px 18px 8px;
  transition:border-color .18s;}
.chart-card:hover{border-color:var(--border-hi);}
.chart-head{font-size:.7rem;letter-spacing:.13em;text-transform:uppercase;
  color:var(--muted);font-weight:600;margin-bottom:14px;}

/* tooltip polish */
.tooltip .tooltip-inner{background:#1C1F26;color:var(--text);border:1px solid var(--border-hi);
  border-radius:9px;font-size:.8rem;padding:7px 10px;box-shadow:0 10px 30px -12px rgba(0,0,0,.9);}
.tooltip .tooltip-arrow::before{border-top-color:#1C1F26!important;
  border-bottom-color:#1C1F26!important;}
"

# helper: a stat card with a hover tooltip
stat_card <- function(label, id, tip, accent = FALSE) {
  card <- div(class = paste("stat", if (accent) "stat-accent"),
              div(class = "stat-label", label),
              div(class = "stat-value", textOutput(id)))
  tooltip(card, tip, placement = "bottom")
}

# --- UI -------------------------------------------------------------------
ui <- page_sidebar(
  theme = my_theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 300,
    tags$span(class = "fld-label", "Task"),
    textInput("task_name", NULL, placeholder = "What are you working on?"),
    tooltip(actionButton("start_study",
                         tagList(icon("play"), "Start studying"), class = "btn-study"),
            "Begin a focus block", placement = "right"),
    tooltip(actionButton("start_break",
                         tagList(icon("mug-hot"), "Start break"), class = "btn-break"),
            "Pause and log break time", placement = "right"),
    tooltip(actionButton("stop",
                         tagList(icon("stop"), "Stop"), class = "btn-stop"),
            "End and save the current segment", placement = "right"),
    uiOutput("status_ui")
  ),
  tags$head(tags$style(HTML(css))),
  div(class = "page",
      div(class = "topbar",
          span(class = "dot"), span(class = "brand", "Focus"),
          span(class = "sub", "study timer")),
      div(class = "stat-grid",
          stat_card("Current",     "current_elapsed",
                    "Live time on the current segment (min : sec)", accent = TRUE),
          stat_card("Study today", "study_today",
                    "All study time logged today, including the live block"),
          stat_card("Break today", "break_today", "All break time logged today"),
          stat_card("Total today", "total_today", "Study + break for today")),
      div(class = "chart-card",
          div(class = "chart-head", "Cumulative study \u00B7 this session"),
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
  
  # Current segment as MIN:SEC (minute-forward, ticks live)
  output$current_elapsed <- renderText({
    s <- as.integer(current_secs())
    sprintf("%02d:%02d", s %/% 60, s %% 60)
  })
  output$study_today <- renderText(fmt_hm(today_minutes()$study))
  output$break_today <- renderText(fmt_hm(today_minutes()$brk))
  output$total_today <- renderText(fmt_hm(today_minutes()$total))
  
  output$live_plot <- renderPlot({
    tick_min()
    df <- cumulative_study(rv$session_log, rv$seg_start, rv$mode, rv$running)
    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 0, y = 0,
                 label = "Press \u201CStart studying\u201D to begin",
                 colour = "#888F9B", size = 4.4) +
        theme_void()
    } else {
      last <- utils::tail(df, 1)
      ggplot(df, aes(time, cum_min)) +
        geom_area(fill = "#5EEAD4", alpha = 0.08) +
        geom_line(colour = "#5EEAD4", linewidth = 1.2, lineend = "round") +
        geom_point(data = last, colour = "#5EEAD4", size = 6, alpha = 0.18) +
        geom_point(data = last, colour = "#5EEAD4", size = 2.6) +
        scale_y_continuous(labels = function(x) paste0(round(x), "m")) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 13) +
        theme(
          plot.background    = element_rect(fill = "transparent", colour = NA),
          panel.background   = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#FFFFFF10"),
          axis.text  = element_text(colour = "#888F9B"),
          axis.ticks = element_blank(),
          text       = element_text(colour = "#888F9B")
        )
    }
  }, bg = "transparent")
}
 
shinyApp(ui, server)
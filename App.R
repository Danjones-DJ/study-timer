# app.R -------------------------------------------------------------------
# ENTRY POINT (the file Posit Connect Cloud runs).
# Dark, precise, focus-first UI. The live session timer is the hero; an
# ambient ring breathes mint while studying, amber on a break, quiet when idle.
# Chart tracks cumulative study time in minutes for the current session.

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
  bg        = "#0B0C0E",
  fg        = "#F2F3F5",
  primary   = "#74E3C4",
  base_font = font_collection(font_google("Inter", local = FALSE),
                              "system-ui", "sans-serif")
)

css <- "
@import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;600&display=swap');

:root{
  --bg:#0B0C0E; --surface:#131519; --surface-2:#15171C; --elevated:#181B21;
  --border:rgba(255,255,255,.06); --border-strong:rgba(255,255,255,.11);
  --text:#F2F3F5; --text-dim:#9AA0AA; --text-faint:#5C626C;
  --study:#74E3C4; --study-soft:rgba(116,227,196,.14);
  --break:#E8C57E; --break-soft:rgba(232,197,126,.13);
  --stop:#F0809A;  --stop-soft:rgba(240,128,154,.12);
  --mono:'JetBrains Mono',ui-monospace,SFMono-Regular,Menlo,monospace;
}
*{box-sizing:border-box;}
body,.bslib-page-sidebar{background:var(--bg)!important;color:var(--text);}
.navbar{display:none;}                       /* drop the default header bar */

/* ambient depth — one faint glow up top, nothing loud */
body::before{content:'';position:fixed;inset:0;z-index:0;pointer-events:none;
  background:radial-gradient(900px 480px at 72% -12%, rgba(116,227,196,.05), transparent 60%);}

/* ---- sidebar ------------------------------------------------------------ */
.bslib-sidebar-layout>.sidebar{background:var(--surface-2)!important;
  border-right:1px solid var(--border)!important;}
.side{padding-top:4px;}
.fld-label{font-size:.66rem;letter-spacing:.14em;text-transform:uppercase;
  color:var(--text-faint);margin-bottom:8px;display:block;font-weight:600;}
.bslib-sidebar-layout .form-control{background:var(--surface);
  border:1px solid var(--border);color:var(--text);border-radius:10px;
  padding:11px 13px;font-size:.9rem;
  transition:border-color .2s ease, box-shadow .2s ease;}
.bslib-sidebar-layout .form-control:focus{border-color:var(--study);
  box-shadow:0 0 0 3px var(--study-soft);outline:none;}
.form-control::placeholder{color:var(--text-faint);}

/* ---- buttons ------------------------------------------------------------ */
.btn-stack{display:flex;flex-direction:column;gap:9px;margin-top:16px;}
.side .btn{width:100%;border-radius:11px;padding:11px 14px;font-weight:600;
  font-size:.9rem;border:1px solid var(--border);background:transparent;
  color:var(--text);
  transition:transform .12s ease, filter .2s ease, background .2s ease,
             box-shadow .2s ease, border-color .2s ease;}
.side .btn:active{transform:translateY(0)!important;}
.side .btn:focus-visible{outline:none;box-shadow:0 0 0 3px var(--study-soft);}
.btn-study{background:var(--study)!important;color:#06120E!important;
  border:none!important;box-shadow:0 6px 20px rgba(116,227,196,.16);}
.btn-study:hover{filter:brightness(1.05);transform:translateY(-1px);
  box-shadow:0 10px 26px rgba(116,227,196,.22);}
.btn-break{color:var(--break)!important;background:var(--break-soft)!important;
  border-color:transparent!important;}
.btn-break:hover{background:rgba(232,197,126,.18)!important;transform:translateY(-1px);}
.btn-stop{color:var(--stop)!important;background:var(--stop-soft)!important;
  border-color:transparent!important;}
.btn-stop:hover{background:rgba(240,128,154,.16)!important;transform:translateY(-1px);}

/* ---- sidebar status ----------------------------------------------------- */
.status-row{margin-top:22px;display:flex;align-items:center;gap:9px;
  font-size:.82rem;color:var(--text-dim);}
.status-dot{width:8px;height:8px;border-radius:50%;background:var(--text-faint);
  transition:all .3s ease;}
.status-dot.live{background:var(--study);box-shadow:0 0 0 4px var(--study-soft);}
.status-dot.brk{background:var(--break);box-shadow:0 0 0 4px var(--break-soft);}

/* ---- page --------------------------------------------------------------- */
.page{position:relative;z-index:1;padding:26px 30px 30px;max-width:760px;margin:0 auto;}
.topbar{display:flex;align-items:center;gap:10px;margin-bottom:4px;}
.topbar .mark{width:9px;height:9px;border-radius:50%;background:var(--study);
  box-shadow:0 0 0 4px var(--study-soft);}
.topbar .brand{font-size:1.05rem;font-weight:700;letter-spacing:-.02em;color:var(--text);}
.topbar .sub{font-size:.78rem;color:var(--text-faint);letter-spacing:.02em;}

/* ---- hero (the signature): live session time + breathing ring ----------- */
.hero{position:relative;display:flex;align-items:center;justify-content:center;
  padding:48px 20px 52px;margin:8px 0 26px;}
.hero-ring{position:absolute;width:338px;height:338px;max-width:86vw;border-radius:50%;
  border:1px solid var(--border);opacity:.45;pointer-events:none;
  transition:border-color .6s ease, box-shadow .6s ease, opacity .6s ease;}
.hero-inner{position:relative;z-index:1;text-align:center;}
.hero-time, .hero-time .shiny-text-output{font-family:var(--mono);
  font-size:clamp(3rem,8vw,4.6rem);font-weight:500;letter-spacing:-.02em;
  line-height:1;color:var(--text);font-variant-numeric:tabular-nums;}
.hero-label{margin-top:15px;font-size:.78rem;letter-spacing:.18em;
  text-transform:uppercase;color:var(--text-faint);font-weight:600;
  display:flex;align-items:center;justify-content:center;gap:9px;
  transition:color .4s ease;}
.hero-label .ld{width:6px;height:6px;border-radius:50%;background:var(--text-faint);
  transition:all .4s ease;}

#hero[data-state='study'] .hero-ring{border-color:rgba(116,227,196,.32);opacity:1;
  box-shadow:0 0 60px rgba(116,227,196,.10), inset 0 0 90px rgba(116,227,196,.06);
  animation:breathe 4s ease-in-out infinite;}
#hero[data-state='study'] .hero-time,
#hero[data-state='study'] .hero-time .shiny-text-output{color:#EAFBF5;}
#hero[data-state='study'] .hero-label{color:var(--study);}
#hero[data-state='study'] .hero-label .ld{background:var(--study);
  box-shadow:0 0 0 4px var(--study-soft);}

#hero[data-state='break'] .hero-ring{border-color:rgba(232,197,126,.32);opacity:1;
  box-shadow:0 0 60px rgba(232,197,126,.10), inset 0 0 90px rgba(232,197,126,.06);
  animation:breathe 5s ease-in-out infinite;}
#hero[data-state='break'] .hero-label{color:var(--break);}
#hero[data-state='break'] .hero-label .ld{background:var(--break);
  box-shadow:0 0 0 4px var(--break-soft);}

@keyframes breathe{0%,100%{transform:scale(1);}50%{transform:scale(1.045);}}

/* ---- supporting stats (quiet, three up) --------------------------------- */
.stat-row{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-bottom:22px;}
@media(max-width:640px){.stat-row{grid-template-columns:1fr;}}
.stat{background:var(--surface);border:1px solid var(--border);border-radius:14px;
  padding:15px 17px;transition:border-color .2s ease, background .2s ease;}
.stat:hover{border-color:var(--border-strong);background:var(--elevated);}
.stat-label{font-size:.66rem;letter-spacing:.13em;text-transform:uppercase;
  color:var(--text-faint);font-weight:600;margin-bottom:9px;}
.stat-value, .stat-value .shiny-text-output{font-family:var(--mono);font-size:1.35rem;
  font-weight:500;color:var(--text);font-variant-numeric:tabular-nums;
  letter-spacing:-.01em;line-height:1;}

/* ---- chart -------------------------------------------------------------- */
.chart-card{background:var(--surface);border:1px solid var(--border);
  border-radius:16px;padding:18px 18px 10px;}
.chart-head{font-size:.66rem;letter-spacing:.14em;text-transform:uppercase;
  color:var(--text-faint);font-weight:600;margin-bottom:10px;}

/* ---- one orchestrated page-load reveal ---------------------------------- */
.page>*{animation:rise .5s cubic-bezier(.22,1,.36,1) both;}
.topbar{animation-delay:.02s;} .hero{animation-delay:.07s;}
.stat-row{animation-delay:.13s;} .chart-card{animation-delay:.19s;}
@keyframes rise{from{opacity:0;transform:translateY(9px);}to{opacity:1;transform:none;}}

@media (prefers-reduced-motion: reduce){
  *,.page>*,#hero .hero-ring{animation:none!important;transition:none!important;}
}
"

js <- "
$(function(){
  if (window.Shiny && Shiny.addCustomMessageHandler){
    Shiny.addCustomMessageHandler('focusState', function(state){
      var h = document.getElementById('hero');
      if (h) h.setAttribute('data-state', state);
    });
  }
});
"

# small helper for a supporting stat card
stat_card <- function(label, id) {
  div(class = "stat",
      div(class = "stat-label", label),
      div(class = "stat-value", textOutput(id, inline = TRUE)))
}

# --- UI -------------------------------------------------------------------
ui <- page_sidebar(
  theme = my_theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 296,
    class = "side",
    tags$span(class = "fld-label", "Task"),
    textInput("task_name", NULL, placeholder = "What are you working on?"),
    div(class = "btn-stack",
        actionButton("start_study", "Start studying", class = "btn-study"),
        actionButton("start_break", "Take a break",   class = "btn-break"),
        actionButton("stop",        "Stop",           class = "btn-stop")),
    uiOutput("status_ui")
  ),
  tags$head(tags$style(HTML(css)), tags$script(HTML(js))),
  div(class = "page",
      div(class = "topbar",
          span(class = "mark"), span(class = "brand", "Focus"),
          span(class = "sub", "study timer")),
      
      # HERO — the live session time, the thing the app is about
      div(id = "hero", class = "hero", `data-state` = "idle",
          div(class = "hero-ring"),
          div(class = "hero-inner",
              div(class = "hero-time", textOutput("current_elapsed")),
              uiOutput("hero_label"))),
      
      # supporting metrics, kept quiet
      div(class = "stat-row",
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
  
  # drive the hero's ambient state (mint while studying, amber on break, quiet idle)
  observe({
    state <- if (!isTRUE(rv$running)) "idle"
    else if (identical(rv$mode, "study")) "study" else "break"
    session$sendCustomMessage("focusState", state)
  })
  
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
    label <- if (!live) "Idle"
    else if (identical(rv$mode, "study")) "Studying\u2026" else "On a break\u2026"
    cls <- paste("status-dot",
                 if (live) "live",
                 if (live && identical(rv$mode, "break")) "brk")
    div(class = "status-row",
        span(class = cls),
        span(label))
  })
  
  output$hero_label <- renderUI({
    live <- isTRUE(rv$running)
    txt  <- if (!live) "Ready when you are"
    else if (identical(rv$mode, "study")) "Studying" else "On a break"
    div(class = "hero-label", span(class = "ld"), span(txt))
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
                 label = "Start a session \u2014 your minutes appear here",
                 colour = "#5C626C", size = 4.3) +
        theme_void()
    } else {
      last <- utils::tail(df, 1)
      ggplot(df, aes(time, cum_min)) +
        geom_area(fill = "#74E3C4", alpha = 0.08) +
        geom_line(colour = "#74E3C4", linewidth = 3.0, alpha = 0.16,
                  lineend = "round") +                       # soft glow
        geom_line(colour = "#74E3C4", linewidth = 1.2,
                  lineend = "round") +                       # crisp line
        geom_point(data = last, colour = "#74E3C4", size = 5.5, alpha = 0.22) +
        geom_point(data = last, colour = "#74E3C4", size = 2.6) +
        scale_y_continuous(labels = function(x) paste0(round(x), "m")) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 13) +
        theme(
          plot.background    = element_rect(fill = "transparent", colour = NA),
          panel.background   = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#FFFFFF0F", linewidth = 0.4),
          axis.text          = element_text(colour = "#6B7280"),
          axis.ticks         = element_blank(),
          text               = element_text(colour = "#6B7280")
        )
    }
  }, bg = "transparent")
}

shinyApp(ui, server)
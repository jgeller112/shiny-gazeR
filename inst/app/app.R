# shiny gazeR: pupil pre-processing app built on gazeR
#
# Launch with shinygazer::run_app(). The processing itself lives in R/pipeline.R
# (auto-sourced by Shiny); this file only defines the interface.
#
# Layout
#   Sidebar  - the pipeline settings, one accordion panel per step
#   Data     - overview of the loaded file(s)
#   Trial    - raw vs. processed trace for any subject/trial
#   Quality  - missing data and exclusions
#   Results  - time course / raincloud / pupil-vs-behaviour, with within- or
#              between-subject error
#   Export   - downloads + processing log
#   Help     - what each step does and which gazeR function runs it

library(shiny)
library(bslib)

options(shiny.maxRequestSize = 2 * 1024^3)  # allow uploads up to 2 GB

example_path <- system.file("extdata", "pupil_sample_files_edf.xls", package = "gazer")
gazer_version <- as.character(utils::packageVersion("gazer"))
app_version <- as.character(utils::packageVersion("shinygazer"))

palette <- c("#3b5bdb", "#e8590c", "#2f9e44", "#ae3ec9", "#f08c00", "#1098ad", "#c92a2a")

# ---- Helpers -------------------------------------------------------------

# Guess which column holds a canonical variable (case-insensitive).
guess_col <- function(cols, candidates) {
  hit <- cols[match(tolower(candidates), tolower(cols))]
  hit <- hit[!is.na(hit)]
  if (length(hit)) hit[1] else ""
}

# Common names for each canonical column, gazeR first, then SR Data Viewer.
col_candidates <- list(
  subject = c("subject", "RECORDING_SESSION_LABEL", "participant", "ID"),
  trial   = c("trial", "TRIAL_INDEX", "trial_index"),
  time    = c("time", "sr_trial_time", "timestamp", "TIMESTAMP"),
  pupil   = c("pupil", "AVERAGE_PUPIL_SIZE", "RIGHT_PUPIL_SIZE", "LEFT_PUPIL_SIZE"),
  blink   = c("blink", "AVERAGE_IN_BLINK", "RIGHT_IN_BLINK", "LEFT_IN_BLINK"),
  message = c("message", "SAMPLE_MESSAGE", "msg", "event")
)

# Input label with a hover tooltip explaining the setting.
info <- function(label, tip) {
  tags$span(label, tooltip(icon("circle-info", class = "info-icon"), tip, placement = "right"))
}

# Plotly defaults shared by every figure.
tidy_plot <- function(fig) {
  fig |>
    plotly::layout(font = list(family = "system-ui, -apple-system, Segoe UI, sans-serif", size = 12),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   margin = list(t = 30), hovermode = "x unified") |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

empty_state <- function(icon_name, title, text) {
  div(class = "empty-state",
      icon(icon_name, class = "fa-2x mb-3"),
      h5(title),
      p(class = "text-muted", text))
}

# ---- UI ------------------------------------------------------------------

feature <- function(icon_name, title, ...) {
  div(class = "feature",
      div(class = "feature-icon", icon(icon_name)),
      h6(title),
      p(...))
}

home_ui <- div(
  class = "home",
  div(class = "hero",
      tags$img(src = "logo.svg", class = "hero-logo", alt = "shiny gazeR hex logo"),
      div(class = "hero-text",
          h1("shiny gazeR"),
          p(class = "lead", "Pupil pre-processing for eye-tracking data, without writing code."),
          p(class = "text-muted",
            "Load recordings from EyeLink, Tobii or Pupil Labs, clean them step by step, ",
            "check every trial, and export analysis-ready data. Every setting is saved with the results, so ",
            "the same pipeline can be re-run in R with gazeR."),
          div(class = "hero-actions",
              actionButton("home_load", "Load your data", icon = icon("file-import"), class = "btn-primary btn-lg"),
              actionButton("home_example", "Try the example data", icon = icon("flask"), class = "btn-outline-primary btn-lg"),
              actionButton("home_help", "How it works", icon = icon("circle-question"), class = "btn-link btn-lg")))),
  div(class = "features",
      feature("eye", "Any tracker",
              "EyeLink .asc, SR Data Viewer reports, Tobii (Titta), Pupil Labs Neon, or your own CSV. Monocular or binocular."),
      feature("eye-slash", "Blinks & artifacts",
              "Tracker or noise-based (Hershman) blink detection, padding, dilation-speed outliers and plausible-range limits."),
      feature("wave-square", "Interpolate & smooth",
              "Linear or cubic interpolation with moving-average or Hanning smoothing, in either order."),
      feature("ruler-horizontal", "Baseline & time-lock",
              "Subtractive or divisive baselines from an event or a fixed window, with trials aligned to any message."),
      feature("clipboard-check", "Quality & exclusions",
              "Missing data per trial and subject, behavioural exclusions (accuracy, RT), and a reason for every dropped trial."),
      feature("chart-line", "Results",
              "Time courses with within- or between-subject error, rainclouds, and pupil vs behaviour, all downloadable.")),
  div(class = "home-footer text-muted small",
      paste0("shiny gazeR v", app_version, ", built on gazeR v", gazer_version, " · "),
      "Geller, Winn, Mahr & Mirman (2020). GazeR: A package for processing gaze position and pupil size data. ",
      tags$em("Behavior Research Methods, 52"), ", 2232–2255.")
)

sidebar_ui <- sidebar(
  width = 360,
  accordion(
    id = "steps", open = "load", multiple = FALSE,

    accordion_panel(
      "1 · Load data", value = "load", icon = icon("file-import"),
      selectInput("tracker", info("Eye tracker", "Chooses how files are read. Auto-detect reads .asc files as EyeLink and anything else as a delimited table."),
                  c("Auto-detect" = "auto",
                    "EyeLink (.asc)" = "eyelink",
                    "SR Data Viewer sample report" = "sr",
                    "Tobii (Titta)" = "tobii",
                    "Tobii Pro Lab (.tsv)" = "prolab",
                    "Pupil Labs Neon" = "pupillabs",
                    "BIDS (eye-tracking)" = "bids",
                    "gazeR / other CSV" = "generic")),
      uiOutput("tracker_hint"),
      fileInput("files", NULL, multiple = TRUE, buttonLabel = "Browse…",
                placeholder = "Select file(s)",
                accept = c(".asc", ".csv", ".tsv", ".txt", ".xls", ".gz", ".zip", ".json")),
      conditionalPanel("input.tracker == 'tobii'",
                       fileInput("msg_files", "Tobii message CSVs", multiple = TRUE,
                                 buttonLabel = "Browse…", accept = ".csv"),
                       layout_columns(
                         col_widths = c(6, 6), gap = "0.5rem",
                         textInput("tobii_onset", info("Onset prefix", "Messages starting with this begin a trial."), "onset_"),
                         textInput("tobii_offset", info("Offset prefix", "Messages starting with this end a trial. Leave blank for onset-only recordings."), "offset_")
                       )),
      conditionalPanel("input.tracker == 'pupillabs'",
                       radioButtons("pl_start_mode", info("Trials start at", "Any message: every event (except recording.begin/end) starts a trial. Specific messages: only the ones listed below do, so in-between events such as 'blank' stay inside the trial."),
                                    c("Any message" = "any", "Specific messages" = "exact"), inline = TRUE),
                       conditionalPanel("input.pl_start_mode == 'exact'",
                                        textInput("pl_start_messages", "Start messages (comma-separated)",
                                                  placeholder = "trial-started-light, start-dark")),
                       layout_columns(
                         col_widths = c(6, 6), gap = "0.5rem",
                         numericInput("pl_lag", info("Max event lag (ms)", "Events are attached to the nearest gaze sample within this distance."), 20, min = 1),
                         textInput("pl_subject", info("Subject ID", "Used for loose CSVs. For a .zip, each recording folder name becomes the subject."), "sub01")
                       ),
                       checkboxInput("pl_aoi", info("Use surface (AOI) data", "Reads gaze_surface.csv from the Marker Mapper enrichment and adds gaze_detected_on_surface plus normalised surface x/y."), FALSE)),
      conditionalPanel("input.tracker == 'prolab'",
                       textInput("prolab_trial_start", info("Trial start messages", "Optional, comma-separated. Leave blank and each stimulus presentation (Presented Stimulus name) is a trial; calibration is dropped. Or list events (e.g. TrialStart) and a new trial starts at each event beginning with one of these."),
                                 placeholder = "blank = one trial per stimulus")),
      conditionalPanel("input.tracker == 'bids'",
                       textInput("bids_trial_start", info("Trial start messages", "Comma-separated. A new trial starts at each event message beginning with one of these (e.g. TRIALID for EyeLink data), and time is ms from it. Leave blank to treat each run as one trial (e.g. resting state)."),
                                 placeholder = "e.g. TRIALID")),
      conditionalPanel("['auto', 'eyelink', 'tobii', 'prolab', 'bids'].indexOf(input.tracker) >= 0",
                       radioButtons("asc_eye", info("Eye (binocular)", "Average: pupil, x and y are the monocular mean, as EyeLink reports the average eye: the mean of both eyes, or the one valid eye; blink is 1 if either eye is blinking. Both: also keeps pupil_left / pupil_right, x/y and blink_left / blink_right for each eye, so you can map one eye below (use its matching blink column). Changing this re-reads the files."),
                                    c(Average = "average", Left = "left", Right = "right", Both = "both"), inline = TRUE),
                       conditionalPanel("input.asc_eye == 'both'",
                                        p(class = "small text-muted mb-1", icon("eye"),
                                          " Both: the left and right eye are processed separately, with the same settings and the same blink mask (blink in either eye). Only trials that pass for both eyes are kept, and results have an eye column."),
                                        checkboxInput("shared_missing", info("Same missing samples in both eyes",
                                                                             "A sample missing in one eye (tracker loss) is set missing in the other too, so both eyes are padded and interpolated over exactly the same gaps."), TRUE))),
      actionLink("load_example", tagList(icon("flask"), "Try the example data")),
      tags$div(class = "section-label", "Column mapping"),
      layout_columns(
        col_widths = c(6, 6), gap = "0.5rem",
        selectInput("col_subject", "Subject", NULL),
        selectInput("col_trial", "Trial", NULL),
        selectInput("col_time", info("Time", "Time stamp in milliseconds. For SR sample reports, choose sr_trial_time (TIMESTAMP − IP_START_TIME)."), NULL),
        selectInput("col_pupil", "Pupil", NULL),
        selectInput("col_blink", info("Blink", "Optional. 1 = sample is in a blink."), NULL),
        selectInput("col_message", info("Message", "Optional. Event markers, needed for event-based baselines and time-locking."), NULL)
      ),
      uiOutput("rate_ui"),
      radioButtons("rate_target", info("Resample",
                                       "Keep: each participant is processed at their recorded rate (see above if rates differ). Resample all to: every participant is brought to one rate. Higher rates are downsampled by averaging consecutive samples (a bin with any missing sample stays missing; blinks and messages are kept); lower rates are upsampled with gazer::upsample_gaze(), new samples interpolated from their neighbours."),
                   c("Keep recorded rates" = "keep", "Resample all to" = "set"), inline = TRUE),
      conditionalPanel("input.rate_target == 'set'",
                       numericInput("target_rate", "Target rate (Hz)", 500, min = 1, step = 50)),
      checkboxInput("resample", info("Fill dropped samples (uniform grid)",
                                     "Puts each trial on an evenly spaced grid at its own sampling rate and inserts any dropped samples as missing, so they are padded and interpolated like other gaps (like eyeris::resample()). EyeLink files are usually already evenly spaced; useful for trackers that drop samples. Does not change the sampling rate."),
                    FALSE)
    ),

    accordion_panel(
      "2 · Blinks & artifacts", value = "blinks", icon = icon("eye-slash"),
      layout_columns(
        col_widths = c(6, 6), gap = "0.5rem",
        numericInput("pupil_min", info("Min pupil", "Samples below this become missing. Leave blank for no limit. 2 mm is a common floor for diameter in mm."), NA),
        numericInput("pupil_max", info("Max pupil", "Samples above this become missing. Leave blank for no limit (9 mm for diameter in mm)."), NA)
      ),
      radioButtons("blink_method", info("Blink detection",
                                        "Tracker: blinks flagged by the eye tracker (EyeLink blink events or your blink column), padded by the amounts below. Noise-based: Hershman, Henik & Cohen (2018). Each run of missing samples is widened along the smoothed pupil signal to cover the eyelid closing and reopening, so it needs little or no padding. gazeR::detect_blinks_noise()"),
                   c("Tracker" = "tracker", "Noise-based (Hershman)" = "hershman"), inline = TRUE),
      conditionalPanel("input.blink_method == 'hershman'",
                       layout_columns(
                         col_widths = c(6, 6), gap = "0.5rem",
                         numericInput("hershman_smooth", info("Smoothing (ms)", "Moving-average window used to find blink onset and offset."), 10, min = 1),
                         numericInput("hershman_gap", info("Merge gap (ms)", "Blinks closer together than this are merged."), 100, min = 0, step = 10)
                       )),
      checkboxInput("use_blink_col", "Also treat tracker blink samples as missing", TRUE),
      layout_columns(
        col_widths = c(6, 6), gap = "0.5rem",
        numericInput("fillback", info("Extend back (ms)", "Pad every gap in the signal backwards by this much. gazeR::extend_blinks()"), 100, min = 0, step = 10),
        numericInput("fillforward", info("Extend forward (ms)", "Pad every gap forwards by this much."), 100, min = 0, step = 10)
      ),
      checkboxInput("use_mad", info("Remove dilation-speed outliers",
                                    "Drops samples whose change in pupil size is implausibly fast (median + k × MAD). gazeR::speed_pupil(), calc_mad()"), TRUE),
      conditionalPanel("input.use_mad",
                       numericInput("mad_n", "MAD multiplier (k)", 16, min = 1)),
      sliderInput("missingthresh", info("Exclusion threshold",
                                        "Excludes any trial with MORE than this percentage of samples missing, and any participant with more than this percentage missing across all their trials (pooled). Measured after blink removal and padding, inside the analysis window when trials are time-locked."),
                  min = 0, max = 100, value = 20, step = 5, post = "%")
    ),

    accordion_panel(
      "3 · Interpolate & smooth", value = "interp", icon = icon("wave-square"),
      radioButtons("interp_type", "Interpolation", inline = TRUE,
                   c(Linear = "linear", "Cubic spline" = "cubic")),
      conditionalPanel("input.interp_type == 'cubic'",
                       numericInput("maxgap", info("Max gap (ms)", "Leave blank to interpolate gaps of any length."), NA)),
      tags$div(class = "section-label", "Filter"),
      radioButtons("filter", info("Smoothing filter",
                                  "Choose one. Moving average: mean of a centred window of samples. Hanning: 11-point Hann window. Low-pass: zero-phase Butterworth filter that removes fluctuations faster than the cutoff (gazer::lowpass_filter); it needs gap-free data, so it runs after interpolation. None: interpolation only."),
                   c("Moving average" = "moving", "Hanning" = "Hann",
                     "Low-pass (Butterworth)" = "lowpass", "None" = "none")),
      conditionalPanel("input.filter == 'moving'",
                       numericInput("ma_n", "Window (samples)", 5, min = 1)),
      conditionalPanel("input.filter == 'moving' || input.filter == 'Hann'",
                       radioButtons("step_first", "Order", inline = TRUE,
                                    c("Interpolate → smooth" = "interp", "Smooth → interpolate" = "smooth"))),
      conditionalPanel("input.filter == 'lowpass'",
                       layout_columns(
                         col_widths = c(6, 6), gap = "0.5rem",
                         numericInput("lp_cutoff", info("Cutoff (Hz)", "Must be below half the sampling rate."), 4, min = 0.1, step = 0.5),
                         numericInput("lp_order", "Filter order", 3, min = 1, max = 8, step = 1)
                       ))
    ),

    accordion_panel(
      "4 · Baseline & time-lock", value = "baseline", icon = icon("ruler-horizontal"),
      radioButtons("baseline", info("Baseline correction", "Median pupil size in the baseline period, per trial."),
                   c("Before an event" = "event", "Fixed window" = "window", None = "none")),
      conditionalPanel("input.baseline == 'event'",
                       layout_columns(
                         col_widths = c(7, 5), gap = "0.5rem",
                         selectizeInput("baseline_event", info("Event(s)", "Pick one or more. Each trial uses the first of these events it contains, e.g. LONG_TRIAL_START or SHORT_TRIAL_START."),
                                        NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
                         numericInput("baseline_dur", "ms before", 100, min = 1)
                       )),
      conditionalPanel("input.baseline == 'window'",
                       layout_columns(
                         col_widths = c(6, 6), gap = "0.5rem",
                         numericInput("bw_start", "From (ms)", 0),
                         numericInput("bw_end", "To (ms)", 200)
                       )),
      conditionalPanel("input.baseline != 'none'",
                       radioButtons("baseline_method", NULL, inline = TRUE,
                                    c(Subtractive = "sub", Divisive = "div")),
                       radioButtons("baseline_stat", info("Baseline value",
                                                          "Median (gazeR's default) or mean of the pupil in the baseline period, per trial. Mathôt & Vilotijević (2022) recommend the mean of a short baseline period, with subtractive correction."),
                                    c(Median = "median", Mean = "mean"), inline = TRUE)),
      tags$div(class = "section-label", "Re-scaling"),
      radioButtons("zscore", info("Z-score pupil",
                                  "After baseline correction: subtract each participant's mean and divide by their SD, over all their kept trials. Puts participants with different pupil sizes or units on one scale while keeping differences between their trials and conditions; results are then in SD units. gazeR::zscore_pupil()"),
                   c(None = "none", "Per participant" = "subject"), inline = TRUE),
      tags$div(class = "section-label", "Time-locking"),
      checkboxInput("timelock", info("Align trials to an event", "Sets time 0 to the event onset. gazeR::onset_pupil()"), TRUE),
      conditionalPanel("input.timelock",
                       selectizeInput("timelock_event", info("Event(s)", "Pick one or more. Each trial is aligned to the first of these events it contains, so trials that start with different messages (e.g. LONG_TRIAL_START or SHORT_TRIAL_START) are all kept. Trials with none of them are excluded."),
                                      NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
                       layout_columns(
                         col_widths = c(6, 6), gap = "0.5rem",
                         numericInput("win_start", "Keep from (ms)", -100),
                         numericInput("win_end", "to (ms)", 2000)
                       ))
    ),

    accordion_panel(
      "5 · Bin & group", value = "bin", icon = icon("chart-column"),
      numericInput("bin_length", "Time-bin length (ms)", 100, min = 1),
      selectizeInput("group_cols", info("Group by", "Bins are averaged within every combination of these columns. Pick subject and/or trial plus any condition columns (for .asc files, Experiment Builder trial variables appear here). Leave empty to average everything."),
                     c("subject", "trial"), selected = "subject", multiple = TRUE,
                     options = list(placeholder = "Everything pooled", plugins = list("remove_button"))),
      p(class = "text-muted small",
        "This sets the binned data you download. Plots on the Results tab choose their own conditions and error bars.")
    ),

    accordion_panel(
      "6 · Behaviour & exclusions", value = "behav", icon = icon("person-running"),
      p(class = "text-muted small",
        "Optional. A table with one row per trial (e.g. RT, accuracy, ratings), joined onto the eye data. ",
        "Its columns can then be used for grouping, conditions and the Results plots."),
      fileInput("behav_file", NULL, buttonLabel = "Browse…", placeholder = "csv / tsv / txt",
                accept = c(".csv", ".tsv", ".txt")),
      selectizeInput("behav_keys", info("Join on", "Columns that identify a trial in both files, usually subject and trial. Values must match the eye data (check the Data tab)."),
                     c("subject", "trial"), selected = c("subject", "trial"), multiple = TRUE),
      uiOutput("behav_status"),
      tags$div(class = "section-label", "Exclude trials by behaviour"),
      p(class = "text-muted small",
        "Uses behavioural columns from the eye-tracking file (e.g. Experiment Builder trial variables) or the table above."),
      layout_columns(
        col_widths = c(6, 6), gap = "0.5rem",
        selectInput("acc_col", "Accuracy column", c("(none)" = "")),
        selectizeInput("acc_correct", info("Correct =", "Trials whose accuracy value is one of these are kept; all others are excluded."),
                       NULL, multiple = TRUE)
      ),
      selectInput("rt_col", "RT column (ms)", c("(none)" = "")),
      conditionalPanel("input.rt_col != ''",
        layout_columns(
          col_widths = c(4, 4, 4), gap = "0.5rem",
          numericInput("rt_min", info("Min", "Exclude RTs below this (ms). Blank = no limit. 200 ms is common for anticipations."), NA),
          numericInput("rt_max", info("Max", "Exclude RTs above this (ms). Blank = no limit."), NA),
          numericInput("rt_sd", info("± SD", "Exclude RTs more than this many SDs from the subject's mean (e.g. 2.5). Computed on trials that pass the accuracy and min/max checks. Blank = off."), NA, step = 0.5)
        )),
      uiOutput("behav_excl_preview")
    )
  ),
  div(class = "run-bar",
      input_task_button("run", "Run pipeline", icon = icon("play"),
                        label_busy = "Processing…", class = "w-100"))
)

ui <- page_sidebar(
  title = tags$span(class = "app-title", tags$img(src = "logo.svg", class = "app-logo", alt = ""),
                    "shiny gazeR",
                    tags$small(class = "app-subtitle", "Pupil pre-processing"),
                    tags$span(class = "badge text-bg-light ms-2", paste0("v", app_version))),
  window_title = "shiny gazeR · pupil pre-processing",
  theme = bs_theme(
    version = 5,
    primary = "#3b5bdb",
    "navbar-bg" = "#1f2a44",
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    code_font = font_collection("ui-monospace", "SFMono-Regular", "Menlo", "monospace"),
    "border-radius" = "0.5rem"
  ),
  fillable = FALSE,
  tags$head(tags$link(rel = "stylesheet", href = "styles.css"),
            tags$link(rel = "icon", type = "image/svg+xml", href = "logo.svg")),
  sidebar = sidebar_ui,

  navset_card_underline(
    id = "tabs", selected = "home",

    nav_panel(
      tagList(icon("house"), "Home"), value = "home",
      home_ui
    ),

    nav_panel(
      tagList(icon("table"), "Data"), value = "data",
      uiOutput("data_summary"),
      uiOutput("raw_download_ui"),
      uiOutput("eye_ui"),
      uiOutput("events_ui"),
      div(class = "d-flex align-items-center gap-3 mt-2",
          h6(class = "mb-0", "Samples"),
          input_switch("only_messages", "Only rows with a message", FALSE)),
      DT::DTOutput("raw_table")
    ),

    nav_panel(
      tagList(icon("magnifying-glass-chart"), "Trial viewer"), value = "trial",
      div(class = "d-flex flex-wrap align-items-end gap-2 mb-2",
          selectInput("view_subject", "Subject", NULL, width = "280px"),
          selectInput("view_trial", "Trial", NULL, width = "140px"),
          uiOutput("view_eye_ui"),
          div(class = "btn-group mb-3",
              actionButton("prev_trial", NULL, icon = icon("chevron-left"),
                           class = "btn-outline-secondary btn-sm", title = "Previous trial"),
              actionButton("next_trial", NULL, icon = icon("chevron-right"),
                           class = "btn-outline-secondary btn-sm", title = "Next trial")),
          div(class = "mb-3 ms-2",
              input_switch("raw_only", info("Raw data only", "Show the signal exactly as loaded: no blink removal, interpolation, smoothing or baseline. Both eyes and gaze position are shown when the file has them."), FALSE)),
          div(class = "ms-auto mb-3", uiOutput("trial_note"))),
      uiOutput("trial_plot_ui")
    ),

    nav_panel(
      tagList(icon("clipboard-check"), "Quality"), value = "quality",
      uiOutput("quality_ui")
    ),

    nav_panel(
      tagList(icon("chart-line"), "Results"), value = "results",
      uiOutput("results_ui")
    ),

    nav_panel(
      tagList(icon("download"), "Export & log"), value = "export",
      card(
        card_header(tagList(icon("users"), "Data by participant")),
        p(class = "text-muted small",
          tags$b("Raw combined"), " is the data as loaded, one row per sample, with messages, trial variables and any ",
          "behavioural columns joined, before any cleaning. It's available as soon as files are loaded. ",
          tags$b("Processed"), " and ", tags$b("binned"), " data need a pipeline run."),
        layout_columns(
          col_widths = c(3, 3, 3, 3), gap = "0.75rem",
          selectInput("dl_dataset", "Dataset",
                      c("Raw combined" = "raw", "Processed samples" = "processed", "Binned" = "binned")),
          selectInput("dl_subject", "Participants", c("All participants" = "")),
          radioButtons("dl_format", "Format",
                       c("One CSV" = "csv", "One CSV per participant (.zip)" = "zip")),
          div(class = "pt-4", downloadButton("dl_by_subject", "Download", class = "btn-primary"),
              uiOutput("dl_by_subject_note"))
        )
      ),
      card(
        card_header(tagList(icon("file-pen"), "Methods paragraph for your paper")),
        uiOutput("methods_ui")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header(tagList(icon("file-medical"), "QC report")),
          p(class = "small text-muted",
            "A self-contained HTML report per participant: summary, exclusions, the signal at each step, every trial, and all parameters. Needs a pipeline run."),
          selectInput("qc_subject", "Participant", c("All participants (.zip)" = "")),
          downloadButton("dl_qc", "Download report", class = "btn-outline-primary")
        ),
        card(
          card_header(tagList(icon("folder-tree"), "BIDS-like export")),
          p(class = "small text-muted",
            "A zip with derivatives/shinygazer/sub-*/eye/: processed time series, events, blinks, binned data and trial quality per participant, plus JSON metadata (sampling rate, column descriptions, every setting)."),
          textInput("bids_task", "Task label", "task"),
          downloadButton("dl_bids", "Download (.zip)", class = "btn-outline-primary")
        ),
        card(
          card_header(tagList(icon("floppy-disk"), "Study file")),
          p(class = "small text-muted",
            "Save every setting to a file, and load it later (or on another computer) to restore them. Data files aren't included: load them again in step 1."),
          downloadButton("dl_study", "Save settings (.json)", class = "btn-outline-primary mb-2"),
          fileInput("study_file", NULL, buttonLabel = "Load settings…", accept = ".json", placeholder = "study .json")
        )
      ),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Downloads"),
          p(class = "text-muted small",
            "Save the parameters file with your outputs. It records every setting and the gazeR version, so the run can be reproduced."),
          downloadButton("dl_samples", "Processed samples (.csv)", class = "btn-outline-primary mb-2"),
          downloadButton("dl_binned", "Binned data (.csv)", class = "btn-outline-primary mb-2"),
          downloadButton("dl_quality", "Trial quality (.csv)", class = "btn-outline-primary mb-2"),
          downloadButton("dl_params", "Parameters (.json)", class = "btn-outline-primary mb-2"),
          downloadButton("dl_dictionary", "Data dictionary (.csv)", class = "btn-outline-primary")
        ),
        card(card_header("Processing log"), verbatimTextOutput("log"))
      )
    ),

    nav_panel(
      tagList(icon("circle-question"), "Help"), value = "help",
      div(class = "help-content", includeMarkdown("help.md"))
    )
  )
)

# ---- Server --------------------------------------------------------------

server <- function(input, output, session) {
  raw <- reactiveVal(NULL)       # data as read from disk
  result <- reactiveVal(NULL)    # output of run_pupil_pipeline()
  log_lines <- reactiveVal(character())
  pending_groups <- reactiveVal(NULL)  # preselected grouping for the example data

  add_log <- function(...) {
    log_lines(c(log_lines(), paste0(format(Sys.time(), "%H:%M:%S"), "  ", ...)))
  }

  # Store newly loaded data and guess the column mapping.
  set_raw <- function(d, label) {
    raw(d)
    result(NULL)
    cols <- names(d)
    for (nm in names(col_candidates)) {
      optional <- nm %in% c("blink", "message")
      choices <- if (optional) c("(none)" = "", cols) else cols
      updateSelectInput(session, paste0("col_", nm), choices = choices,
                        selected = guess_col(cols, col_candidates[[nm]]))
    }
    add_log("Loaded ", label, ": ", format(nrow(d), big.mark = ","), " samples, ",
            length(cols), " columns")
    nav_select("tabs", "data")
  }

  # What to upload for each tracker
  output$tracker_hint <- renderUI({
    hint <- switch(input$tracker,
      auto = "EyeLink .asc files, SR sample reports, or gazeR-format csv/tsv.",
      eyelink = "One .asc file per subject (convert .edf with SR Research's edf2asc). Blinks, events and trial variables are read automatically.",
      sr = "Data Viewer sample reports (.xls/.txt, tab-delimited). Time is taken as TIMESTAMP − IP_START_TIME.",
      tobii = "Gaze CSVs here, message CSVs below. Each subject's two files must have the same name. Tobii has no blink events, so noise-based blink detection is turned on.",
      pupillabs = "Upload a .zip of the Pupil Cloud Time Series export (one folder per recording, each becomes a subject), or gaze.csv, 3d_eye_states.csv and events.csv from one recording. The plausible pupil range is set to 2–9 mm.",
      prolab = "Tab-separated data exports from Tobii Pro Lab (Analyze > Data Export, .tsv), one or more; a file can hold several participants. Include pupil diameter, validity, eye movement type and (if your tracker has it) eye openness. Pupil is in mm (range set to 2–9 mm). Blinks are Pro Lab's own Blink samples plus gaps where the pupil was lost.",
      bids = "Upload a .zip of a BIDS dataset (BEP020 eye-tracking), or the *_physio.tsv.gz files with their .json sidecars and *_physioevents files. Each sub/ses/task/run becomes a subject; left- and right-eye recordings are merged.",
      generic = "One row per sample, with subject, trial, time (ms) and pupil columns, e.g. output from parse_asc().")
    p(class = "text-muted small mb-2", hint)
  })

  # Tobii has no tracker blinks, so default to noise-based detection
  # Tracker-specific defaults
  observeEvent(input$tracker, {
    if (input$tracker == "tobii") updateRadioButtons(session, "blink_method", selected = "hershman")
    if (input$tracker == "prolab") updateRadioButtons(session, "blink_method", selected = "tracker")
    if (input$tracker %in% c("pupillabs", "prolab")) {
      updateNumericInput(session, "pupil_min", value = 2)
      updateNumericInput(session, "pupil_max", value = 9)
    }
  }, ignoreInit = TRUE)

  read_uploads <- function() {
    f <- input$files
    switch(input$tracker,
      eyelink = {
        if (!all(grepl("\\.asc(\\.gz)?$", f$name, ignore.case = TRUE))) {
          stop("EyeLink mode needs .asc files. Convert .edf files with edf2asc first.", call. = FALSE)
        }
        read_gazer_files(f$datapath, f$name, eye = input$asc_eye)
      },
      tobii = {
        req(input$msg_files)
        read_tobii_upload(f$datapath, f$name, input$msg_files$datapath, input$msg_files$name,
                          eye = input$asc_eye, onset_prefix = input$tobii_onset,
                          offset_prefix = input$tobii_offset)
      },
      pupillabs = {
        starts <- trimws(unlist(strsplit(if (is.null(input$pl_start_messages)) "" else input$pl_start_messages, ",")))
        starts <- starts[nzchar(starts)]
        if (input$pl_start_mode == "exact" && !length(starts)) {
          stop("List the start messages, or choose 'Any message'.", call. = FALSE)
        }
        read_pupillabs_upload(f$datapath, f$name, subject = input$pl_subject,
                              start_mode = input$pl_start_mode,
                              start_messages = if (length(starts)) starts else NULL,
                              max_event_lag_ms = input$pl_lag, aoi = isTRUE(input$pl_aoi))
      },
      prolab = {
        starts <- trimws(unlist(strsplit(if (is.null(input$prolab_trial_start)) "" else input$prolab_trial_start, ",")))
        starts <- starts[nzchar(starts)]
        read_prolab_upload(f$datapath, f$name, eye = input$asc_eye,
                           trial_start = if (length(starts)) starts else NULL)
      },
      bids = {
        starts <- trimws(unlist(strsplit(if (is.null(input$bids_trial_start)) "" else input$bids_trial_start, ",")))
        starts <- starts[nzchar(starts)]
        read_bids_upload(f$datapath, f$name, eye = input$asc_eye,
                         trial_start = if (length(starts)) starts else NULL)
      },
      read_gazer_files(f$datapath, f$name, eye = input$asc_eye)
    )
  }

  observeEvent(list(input$files, input$msg_files, input$asc_eye, input$tracker,
                    input$tobii_onset, input$tobii_offset, input$pl_subject,
                    input$pl_start_mode, input$pl_start_messages, input$pl_lag, input$pl_aoi,
                    input$bids_trial_start, input$prolab_trial_start), {
    req(input$files)
    if (input$tracker == "tobii" && is.null(input$msg_files)) return()
    d <- tryCatch(withProgress(message = "Reading files", value = 0.3, read_uploads()),
                  error = function(e) {
                    showNotification(conditionMessage(e), type = "error", duration = 10)
                    NULL
                  })
    req(d)
    # BIDS: defaults from the sidecars (pupil in mm, e.g. Pupil Labs; no
    # blink events, e.g. Tobii)
    info <- attr(d, "bids_info")
    bids_info(info)
    if (!is.null(info)) {
      if (any(tolower(info$pupil_units) %in% c("mm", "millimeter", "millimeters"))) {
        updateNumericInput(session, "pupil_min", value = 2)
        updateNumericInput(session, "pupil_max", value = 9)
      }
      if (!any(info$blink_events)) {
        updateRadioButtons(session, "blink_method", selected = "hershman")
        showNotification("No blink events in the BIDS data, so noise-based blink detection is turned on.", type = "message")
      }
    }
    set_raw(d, paste(nrow(input$files), "file(s)"))
  })
  bids_info <- reactiveVal(NULL)

  load_example <- function() {
    pending_groups(c("subject", "script"))
    set_raw(read_gazer_files(example_path), "example data")
  }
  observeEvent(input$load_example, load_example())
  observeEvent(input$home_example, load_example())

  # Landing-page buttons
  observeEvent(input$home_load, {
    accordion_panel_open("steps", "load")
    showNotification("Choose your eye tracker and files in step 1 on the left.", type = "message")
  })
  observeEvent(input$home_help, nav_select("tabs", "help"))

  # Standardised data, recomputed whenever the column mapping changes.
  # Behavioural data (optional), joined onto the eye data by subject/trial
  behav <- reactive({
    req(input$behav_file)
    tryCatch(tibble::as_tibble(data.table::fread(input$behav_file$datapath, na.strings = c("NA", ""))),
             error = function(e) {
               showNotification(paste("Could not read behavioural file:", conditionMessage(e)), type = "error")
               NULL
             })
  })

  observeEvent(behav(), {
    b <- behav()
    req(b)
    keys <- intersect(c("subject", "trial"), names(b))
    updateSelectizeInput(session, "behav_keys", choices = names(b), selected = keys)
    add_log("Loaded behavioural data: ", nrow(b), " rows, ", ncol(b), " columns")
  })

  # Eye = Both with per-eye pupil columns: each eye is processed separately,
  # with the either-eye blink mask (pupil and blink mappings are not used)
  eyes_mode <- reactive({
    identical(input$asc_eye, "both") && !is.null(raw()) &&
      input$tracker %in% c("auto", "eyelink", "tobii", "prolab", "bids") &&
      all(c("pupil_left", "pupil_right") %in% names(raw()))
  })

  std <- reactive({
    req(raw(), input$col_subject, input$col_trial, input$col_time, input$col_pupil)
    eyes <- eyes_mode()
    d <- tryCatch(
      standardise_columns(raw(), list(subject = input$col_subject, trial = input$col_trial,
                                      time = input$col_time,
                                      pupil = if (eyes && "pupil" %in% names(raw())) "pupil" else input$col_pupil,
                                      blink = if (eyes && "blink" %in% names(raw())) "blink" else input$col_blink,
                                      message = input$col_message)),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    req(d)
    if (!is.null(input$behav_file) && !is.null(behav()) && length(input$behav_keys)) {
      d <- tryCatch(join_behaviour(d, behav(), input$behav_keys),
                    error = function(e) {
                      showNotification(conditionMessage(e), type = "error")
                      d
                    })
    }
    d
  })

  observeEvent(list(input$acc_col, std()), {
    d <- std()
    req(d)
    if (is.null(input$acc_col) || !nzchar(input$acc_col) || !input$acc_col %in% names(d)) {
      updateSelectizeInput(session, "acc_correct", choices = character(), selected = character())
      return()
    }
    vals <- sort(unique(as.character(d[[input$acc_col]])))
    vals <- vals[!is.na(vals)]
    correct <- vals[tolower(vals) %in% c("1", "true", "correct", "yes", "hit", "c")]
    current <- intersect(isolate(input$acc_correct), vals)
    updateSelectizeInput(session, "acc_correct", choices = vals,
                         selected = if (length(current)) current else correct)
  })

  # Live preview of how many trials the behavioural rules remove
  output$behav_excl_preview <- renderUI({
    d <- std()
    req(d)
    p <- list(acc_col = input$acc_col, acc_correct = input$acc_correct, rt_col = input$rt_col,
              rt_min = input$rt_min, rt_max = input$rt_max, rt_sd = input$rt_sd)
    ex <- tryCatch(behaviour_exclusions(d, p), error = function(e) NULL)
    if (is.null(ex) || !nrow(ex)) return(NULL)
    all_tr <- dplyr::distinct(d, subject, trial)
    n_tr <- nrow(all_tr)
    tab <- table(ex$reason)
    # Share of each participant's trials excluded
    per_sub <- table(factor(ex$subject, levels = unique(all_tr$subject))) / table(all_tr$subject)
    worst <- names(per_sub)[which.max(per_sub)]
    div(class = "small text-warning mt-1",
        icon("filter"), sprintf(" %d of %d trials (%.1f%%) will be excluded: ", nrow(ex), n_tr, 100 * nrow(ex) / n_tr),
        paste(sprintf("%s (%d, %.1f%%)", names(tab), as.integer(tab), 100 * as.integer(tab) / n_tr), collapse = ", "),
        if (length(per_sub) > 1)
          tags$div(sprintf("Per participant: %.1f%%–%.1f%% of trials (most: %s).",
                           100 * min(per_sub), 100 * max(per_sub), worst)))
  })

  output$behav_status <- renderUI({
    req(input$behav_file, behav(), std())
    b <- behav()
    new_cols <- setdiff(names(b), input$behav_keys)
    new_cols <- intersect(c(new_cols, paste0(new_cols, "_behav")), names(std()))
    if (!length(new_cols)) {
      return(div(class = "alert alert-warning small py-2", "Not joined: check the join columns."))
    }
    trials <- dplyr::distinct(std(), dplyr::across(dplyr::all_of(c("subject", "trial", new_cols[1]))))
    matched <- mean(!is.na(trials[[new_cols[1]]]))
    div(class = paste("small", if (matched < 0.9) "text-warning" else "text-success"),
        icon(if (matched < 0.9) "triangle-exclamation" else "check"),
        sprintf(" %.0f%% of eye-tracking trials matched. Added: %s", 100 * matched,
                paste(new_cols, collapse = ", ")))
  })

  # Fill in event and condition choices, and detect the sampling rate.
  # (For .asc files, trial variables such as condition become columns.)
  observeEvent(std(), {
    d <- std()
    req(d)
    if (!"message" %in% names(d) || all(is.na(d$message))) {
      # e.g. resting-state recordings: nothing to time-lock to
      updateCheckboxInput(session, "timelock", value = FALSE)
      if (identical(isolate(input$baseline), "event")) updateRadioButtons(session, "baseline", selected = "none")
      showNotification("No event messages in these files, so time-locking and event-based baseline are turned off (step 4).",
                       type = "warning", duration = 10)
    }
    if ("message" %in% names(d)) {
      msgs <- names(sort(table(d$message), decreasing = TRUE))
      pick <- function(cur) {
        cur <- intersect(cur, msgs)
        if (length(cur)) return(cur)
        if ("target" %in% msgs) return("target")
        # Every start/onset-type event (e.g. LONG_TRIAL_START and
        # SHORT_TRIAL_START), not *_END
        starts <- msgs[grepl("start|onset|stim", msgs, ignore.case = TRUE) &
                         !grepl("end|offset", msgs, ignore.case = TRUE)]
        if (length(starts)) starts else msgs[1]
      }
      updateSelectizeInput(session, "baseline_event", choices = msgs,
                           selected = pick(isolate(input$baseline_event)))
      updateSelectizeInput(session, "timelock_event", choices = msgs,
                           selected = pick(isolate(input$timelock_event)))
    }
    canon <- c("subject", "trial", "time", "pupil", "blink", "message", "source_file")
    cand <- setdiff(names(d), canon)
    # Conditions should be low-cardinality columns
    cand <- cand[vapply(cand, function(cl) length(unique(d[[cl]])) <= 50, logical(1))]
    choices <- c("subject", "trial", cand)
    current <- isolate(input$group_cols)
    if (is.null(current)) current <- "subject"
    updateSelectizeInput(session, "group_cols", choices = choices,
                         selected = intersect(unique(c(current, pending_groups())), choices))
    pending_groups(NULL)

    # Behavioural exclusion choices
    beh_cols <- setdiff(names(d), c("subject", "trial", "time", "pupil", "x", "y", "blink",
                                    "message", "tracker_time", "source_file"))
    guess <- function(pattern) { hit <- grep(pattern, beh_cols, ignore.case = TRUE, value = TRUE); if (length(hit)) hit[1] else "" }
    acc_now <- isolate(input$acc_col)
    rt_now <- isolate(input$rt_col)
    acc_cand <- beh_cols[vapply(beh_cols, function(cl) dplyr::n_distinct(d[[cl]]) <= 10, logical(1))]
    rt_cand <- beh_cols[vapply(beh_cols, function(cl) is.numeric(d[[cl]]), logical(1))]
    updateSelectInput(session, "acc_col", choices = c("(none)" = "", acc_cand),
                      selected = if (isTRUE(acc_now %in% acc_cand)) acc_now else "")
    updateSelectInput(session, "rt_col", choices = c("(none)" = "", rt_cand),
                      selected = if (isTRUE(rt_now %in% rt_cand)) rt_now else "")

    # Trial viewer works on loaded data, before any processing
    subs <- sort(unique(as.character(d$subject)))
    updateSelectInput(session, "view_subject", choices = subs,
                      selected = if (isTRUE(isolate(input$view_subject) %in% subs)) isolate(input$view_subject) else subs[1])

  })

  # ---- Sampling rates ----------------------------------------------------
  # Each participant's rate: from the .asc header, or the median time step
  detected_rates <- reactive({
    req(std())
    subject_rates(std())
  })

  output$rate_ui <- renderUI({
    r <- detected_rates()
    rates <- sort(unique(stats::na.omit(r$hz)))
    if (length(rates) <= 1) {
      return(tagList(
        numericInput("hz", info("Recorded rate (Hz)",
                                "The rate the data were recorded at: read from the file header for EyeLink .asc files, otherwise estimated from the time column. Change it only if it's wrong. To change the rate of the data, use Resample below."),
                     if (length(rates)) rates else NA, min = 1),
        uiOutput("hz_warning")))
    }
    groups <- vapply(rates, function(hz) {
      who <- r$subject[r$hz == hz]
      sprintf("%g Hz: %d participant%s%s", hz, length(who), if (length(who) != 1) "s" else "",
              if (length(who) <= 6) paste0(" (", paste(who, collapse = ", "), ")") else "")
    }, character(1))
    current <- isolate(input$rate_mode)
    tagList(
      div(class = "alert alert-warning small py-2 mb-2",
          icon("triangle-exclamation"), tags$b(" Sampling rates differ"),
          tags$ul(class = "mb-0 ps-3", lapply(groups, tags$li))),
      radioButtons("rate_mode", info("Mixed sampling rates",
                                     "Upsample: lower-rate participants get new samples in between, filled from the real samples either side (gaps such as blinks stay empty), so everyone is processed at the same rate (gazer::upsample_gaze). Own rate: each participant is processed at their own rate; time bins still line up because they're in ms."),
                   c(stats::setNames("upsample", sprintf("Upsample to %g Hz (recommended)", max(rates))),
                     "Use each participant's own rate" = "own"),
                   selected = if (is.null(current)) "upsample" else current)
    )
  })

  # Changing the recorded rate relabels the data; it does not resample them
  output$hz_warning <- renderUI({
    r <- sort(unique(stats::na.omit(detected_rates()$hz)))
    req(length(r) == 1, isTRUE(is.finite(input$hz)), input$hz != r)
    div(class = "alert alert-warning small py-2 mb-2",
        icon("triangle-exclamation"),
        sprintf(" The file says %g Hz. This box only corrects a wrong rate: it does not resample the data. To resample to %g Hz, choose Resample all to below.", r, input$hz))
  })
  # Start the target rate at the highest recorded rate
  observeEvent(detected_rates(), {
    r <- stats::na.omit(detected_rates()$hz)
    if (length(r)) updateNumericInput(session, "target_rate", value = max(r))
  })

  # ---- Data tab -----------------------------------------------------------
  output$data_summary <- renderUI({
    d <- std()
    if (is.null(d)) {
      return(div(
        class = "getting-started",
        h4("Getting started"),
        tags$ol(
          tags$li(tags$b("Load"), " one or more sample-level files: EyeLink ",
                  tags$code(".asc"), " files, gazeR-format csv/tsv, ",
                  "or SR Data Viewer sample reports. Or ",
                  actionLink("load_example_2", "try the example data"), "."),
          tags$li(tags$b("Check the column mapping"), " and sampling rate in step 1."),
          tags$li(tags$b("Adjust the settings"), " in steps 2–5. Hover the ",
                  icon("circle-info"), " icons for details."),
          tags$li(tags$b("Run the pipeline,"), " then inspect trials, check data quality, ",
                  "and export.")
        )
      ))
    }
    n_tr <- nrow(dplyr::distinct(d, subject, trial))
    miss <- mean(is.na(d$pupil) | d$pupil <= 0, na.rm = TRUE)
    rr <- result()$rates
    tagList(
    p(class = "small text-muted mb-2", icon("circle-info"),
      " Data as loaded, before any processing or resampling.",
      if (!is.null(rr) && !identical(rr$hz, result()$rates_original$hz))
        sprintf(" The last run resampled to %s Hz: see Processed samples on the Export tab.",
                paste(unique(rr$hz), collapse = "/"))),
    layout_columns(
      fill = FALSE,
      value_box("Samples", format(nrow(d), big.mark = ","), showcase_layout = "left center", showcase = icon("list-ol"), theme = "primary"),
      value_box("Subjects", length(unique(d$subject)), showcase_layout = "left center", showcase = icon("users")),
      value_box("Trials", format(n_tr, big.mark = ","), showcase_layout = "left center", showcase = icon("layer-group")),
      value_box("Missing pupil", sprintf("%.1f%%", 100 * miss), showcase_layout = "left center", showcase = icon("eye-slash"),
                theme = if (miss > 0.25) "warning" else NULL)
    ))
  })
  observeEvent(input$load_example_2, load_example())

  # Missing data and blinks per participant and eye, from the data as loaded
  eye_stats <- reactive({
    req(std())
    label <- if (input$tracker %in% c("auto", "eyelink", "tobii", "prolab", "bids") && !is.null(input$asc_eye) &&
                 input$asc_eye %in% c("left", "right", "average")) {
      c(left = "left", right = "right", average = "average (both eyes)")[[input$asc_eye]]
    } else "pupil"
    eye_summary(std(), single_label = label)
  })

  eye_datatable <- function() {
    e <- eye_stats()
    has_r <- "pupil_r" %in% names(e)
    tbl <- DT::datatable(e, rownames = FALSE, filter = "top",
                  colnames = c("Subject", "Eye", "Samples", "Missing", "Blink",
                               "Missing outside blinks", "Trials", "Trials >50% missing",
                               if (has_r) "L–R pupil r"),
                  options = list(pageLength = 10, dom = "tip", scrollX = TRUE)) |>
      DT::formatPercentage(c("missing", "blink", "tracker_loss"), 1) |>
      DT::formatStyle("missing", target = "row",
                      backgroundColor = DT::styleInterval(0.3, c("transparent", "rgba(240,140,0,.28)")))
    if (has_r) {
      tbl <- tbl |>
        DT::formatRound("pupil_r", 3) |>
        DT::formatStyle("pupil_r", color = DT::styleInterval(0.9, c("#c92a2a", "inherit")),
                        fontWeight = DT::styleInterval(0.9, c("bold", "normal")))
    }
    tbl
  }
  output$eye_table <- DT::renderDT(eye_datatable())
  output$eye_table_quality <- DT::renderDT(eye_datatable())

  output$eye_ui <- renderUI({
    req(std())
    card(class = "mt-3",
         card_header(tagList(icon("eye"), "Missing data and blinks by eye"),
                     tags$small(class = "text-muted ms-2",
                                "rows over 30% missing are highlighted")),
         p(class = "small text-muted mb-2",
           "Average (both eyes) is missing whenever either eye is, and its blink is 1 if either eye blinks. ",
           "If one eye is much worse, choose the other eye in step 1."),
         DT::DTOutput("eye_table"))
  })

  # Event messages are sparse (one sample per event), so summarise them
  # separately rather than relying on the sample preview.
  output$events_ui <- renderUI({
    d <- std()
    req(d)
    if (!"message" %in% names(d) || all(is.na(d$message))) {
      return(div(class = "alert alert-light border small mt-3", icon("circle-info"),
                 " No event messages mapped. Event-based baselines and time-locking need a message column."))
    }
    card(class = "mt-3",
         card_header(tagList(icon("flag"), "Events found"),
                     tags$small(class = "text-muted ms-2", "each event is stored on one sample")),
         DT::DTOutput("events_table"))
  })

  output$events_table <- DT::renderDT({
    d <- std()
    req(d, "message" %in% names(d))
    ev <- d[!is.na(d$message), c("subject", "trial", "time", "message")]
    tab <- ev |>
      dplyr::group_by(message) |>
      dplyr::summarise(count = dplyr::n(),
                       trials = dplyr::n_distinct(paste(subject, trial)),
                       `median time (ms)` = stats::median(time), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(count))
    DT::datatable(tab, rownames = FALSE, options = list(dom = "t", pageLength = 50))
  })

  output$raw_table <- DT::renderDT({
    req(raw())
    d <- raw()
    if (isTRUE(input$only_messages) && !is.null(input$col_message) &&
        nzchar(input$col_message) && input$col_message %in% names(d)) {
      d <- d[!is.na(d[[input$col_message]]) & nzchar(as.character(d[[input$col_message]])), ]
      caption <- "Rows with a message (first 500)"
    } else {
      caption <- "First 500 rows as loaded"
    }
    d <- utils::head(d, 500)
    num <- vapply(d, is.double, logical(1))
    d[num] <- lapply(d[num], round, 3)
    DT::datatable(d, rownames = FALSE, caption = caption,
                  options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })

  # ---- Run ----------------------------------------------------------------
  params <- reactive({
    list(
      hz_override = if (dplyr::n_distinct(detected_rates()$hz) <= 1) input$hz else NA,
      rate_mode = if (is.null(input$rate_mode)) "upsample" else input$rate_mode,
      use_blink_col = input$use_blink_col,
      acc_col = input$acc_col, acc_correct = input$acc_correct, rt_col = input$rt_col,
      rt_min = input$rt_min, rt_max = input$rt_max, rt_sd = input$rt_sd,
      pupil_min = input$pupil_min, pupil_max = input$pupil_max,
      blink_method = input$blink_method, hershman_smooth = input$hershman_smooth,
      hershman_gap = input$hershman_gap,
      fillback = input$fillback, fillforward = input$fillforward,
      use_mad = input$use_mad, mad_n = input$mad_n,
      missingthresh = input$missingthresh / 100,  # slider is in %, pipeline uses a proportion
      interp_type = input$interp_type, filter = input$filter, step_first = input$step_first,
      maxgap = if (is.na(input$maxgap)) Inf else input$maxgap, ma_n = input$ma_n,
      use_lowpass = identical(input$filter, "lowpass"), lp_cutoff = input$lp_cutoff, lp_order = input$lp_order,
      baseline = input$baseline, baseline_event = input$baseline_event,
      baseline_dur = input$baseline_dur, baseline_window = c(input$bw_start, input$bw_end),
      baseline_method = input$baseline_method,
      baseline_stat = if (is.null(input$baseline_stat)) "median" else input$baseline_stat,
      timelock = input$timelock, timelock_event = input$timelock_event,
      window = c(input$win_start, input$win_end),
      bin_length = input$bin_length, group_cols = input$group_cols,
      zscore = if (is.null(input$zscore)) "none" else input$zscore,
      shared_missing = isTRUE(input$shared_missing),
      resample = isTRUE(input$resample),
      target_rate = if (identical(input$rate_target, "set")) input$target_rate else NA
    )
  })

  observeEvent(input$run, {
    d <- std()
    if (is.null(d)) {
      showNotification("Load data first.", type = "warning")
      return()
    }
    add_log("---- Run started ----")
    res <- tryCatch(if (eyes_mode()) run_pipeline_eyes(d, params(), log = add_log)
                    else run_pupil_pipeline(d, params(), log = add_log),
                    error = function(e) {
                      add_log("ERROR: ", conditionMessage(e))
                      showNotification(conditionMessage(e), type = "error", duration = 10)
                      NULL
                    })
    req(res)
    result(res)
    updateSelectInput(session, "dl_dataset", selected = "processed")
    q <- res$quality
    resampled <- !identical(res$rates$hz, res$rates_original$hz)
    showNotification(sprintf("Done: %d of %d trials retained.%s", sum(!q$excluded), nrow(q),
                             if (resampled) sprintf(" Processed at %s Hz (%s samples).",
                                                    paste(unique(res$rates$hz), collapse = "/"),
                                                    format(nrow(res$data), big.mark = ",")) else ""),
                     type = "message")
    subs <- sort(unique(as.character(d$subject)))
    updateSelectInput(session, "view_subject", choices = subs,
                      selected = if (isTRUE(input$view_subject %in% subs)) input$view_subject else subs[1])
    nav_select("tabs", "trial")
  })

  # ---- Trial viewer -------------------------------------------------------
  observeEvent(list(input$view_subject, std()), {
    req(std(), input$view_subject)
    trials <- sort(unique(std()$trial[std()$subject == input$view_subject]))
    updateSelectInput(session, "view_trial", choices = trials,
                      selected = if (isTRUE(input$view_trial %in% trials)) input$view_trial else trials[1])
  })

  step_trial <- function(delta) {
    req(std(), input$view_subject, input$view_trial)
    trials <- as.character(sort(unique(std()$trial[std()$subject == input$view_subject])))
    i <- match(input$view_trial, trials) + delta
    if (i >= 1 && i <= length(trials)) updateSelectInput(session, "view_trial", selected = trials[i])
  }
  observeEvent(input$prev_trial, step_trial(-1))
  observeEvent(input$next_trial, step_trial(1))

  trial_status <- reactive({
    req(result(), input$view_subject, input$view_trial)
    q <- result()$quality
    q[as.character(q$subject) == input$view_subject & as.character(q$trial) == input$view_trial, ]
  })

  output$trial_note <- renderUI({
    q <- trial_status()
    if (!nrow(q)) return(NULL)
    pct <- if (is.na(q$prop_missing)) q$reason else sprintf("%.1f%% missing", 100 * q$prop_missing)
    if (isTRUE(q$excluded)) {
      tags$span(class = "badge rounded-pill text-bg-warning fs-6", icon("triangle-exclamation"),
                "Excluded:", if (q$reason %in% c("trial missing data", "subject missing data")) pct else q$reason)
    } else {
      tags$span(class = "badge rounded-pill text-bg-success fs-6", icon("check"), "Kept:", pct)
    }
  })

  output$trial_plot_ui <- renderUI({
    if (is.null(std())) {
      return(empty_state("magnifying-glass-chart", "No data loaded",
                         "Load data in step 1 to view individual trials."))
    }
    tagList(
      if (is.null(result()) && !isTRUE(input$raw_only))
        div(class = "alert alert-light border small",
            icon("circle-info"), " This is the raw trace. Run the pipeline to overlay the processed signal."),
      uiOutput("missing_key"),
      plotly::plotlyOutput("trial_plot", height = "560px")
    )
  })

  blink_col <- "rgba(201,42,42,.14)"
  miss_col <- "rgba(110,110,110,.16)"

  # plotly's subplot() drops shapes/annotations added with layout(), so attach
  # them to the built figure instead. yref = "paper" spans every panel.
  with_overlays <- function(fig, shapes, annots) {
    built <- plotly::plotly_build(fig)
    built$x$layout$shapes <- c(built$x$layout$shapes, shapes)
    built$x$layout$annotations <- c(built$x$layout$annotations, annots)
    built
  }

  # One shaded rectangle per run of TRUE in `mask`
  missing_shapes <- function(mask, time, fill) {
    mask[is.na(mask)] <- FALSE
    if (!any(mask)) return(list())
    r <- rle(mask)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    # Half a sample either side so single missing samples stay visible
    half <- if (length(time) > 1) stats::median(diff(time), na.rm = TRUE) / 2 else 0.5
    # On long trials (e.g. minutes of resting state) short blinks would be
    # narrower than a pixel; give each band a minimum visible width
    half <- max(half, diff(range(time, na.rm = TRUE)) * 0.0015)
    lapply(which(r$values), function(k) list(
      type = "rect", xref = "x", yref = "paper", layer = "below",
      x0 = time[starts[k]] - half, x1 = time[ends[k]] + half, y0 = 0, y1 = 1,
      fillcolor = fill, line = list(width = 0)))
  }

  output$view_eye_ui <- renderUI({
    req(isTRUE(result()$eyes))
    selectInput("view_eye", "Eye", c(Left = "left", Right = "right"),
                selected = if (is.null(isolate(input$view_eye))) "left" else isolate(input$view_eye), width = "120px")
  })
  view_eye <- reactive(if (isTRUE(result()$eyes)) { if (is.null(input$view_eye)) "left" else input$view_eye })
  # The current trial of the data as loaded, with the viewed eye's pupil
  raw_trial <- function() {
    rawt <- std()[as.character(std()$subject) == input$view_subject &
                    as.character(std()$trial) == input$view_trial, ]
    e <- view_eye()
    if (!is.null(e) && paste0("pupil_", e) %in% names(rawt)) rawt$pupil <- rawt[[paste0("pupil_", e)]]
    rawt
  }
  this_eye <- function(df) {
    e <- view_eye()
    if (is.null(e) || !"eye" %in% names(df)) rep(TRUE, nrow(df)) else df$eye == e
  }

  # Missing-data summary for the current trial, shown above the plot
  trial_missing <- reactive({
    req(std(), input$view_subject, input$view_trial)
    rawt <- raw_trial()
    raw_miss <- mean(is.na(rawt$pupil) | rawt$pupil <= 0, na.rm = TRUE)
    res <- result()
    clean <- if (!is.null(res) && !isTRUE(input$raw_only)) {
      res$cleaned[as.character(res$cleaned$subject) == input$view_subject &
                    as.character(res$cleaned$trial) == input$view_trial & this_eye(res$cleaned), ]
    }
    if (!is.null(clean) && nrow(clean)) {
      list(raw = raw_miss, blink = mean(clean$blink_detected > 0, na.rm = TRUE),
           removed = mean(is.na(clean$extendpupil)))
    } else {
      list(raw = raw_miss, blink = if ("blink" %in% names(rawt)) mean(rawt$blink > 0, na.rm = TRUE) else NA,
           removed = NA)
    }
  })

  output$missing_key <- renderUI({
    m <- trial_missing()
    swatch <- function(col) tags$span(class = "missing-swatch", style = paste0("background:", col))
    pct <- function(x) if (is.na(x)) "–" else sprintf("%.1f%%", 100 * x)
    div(class = "missing-key small text-muted",
        "Shaded: ",
        swatch(blink_col), "blink ", tags$b(pct(m$blink)),
        swatch(miss_col), if (is.na(m$removed)) "missing in recording " else "other removed / missing ",
        tags$b(if (is.na(m$removed)) pct(m$raw) else pct(max(m$removed - m$blink, 0))),
        if (!is.na(m$removed)) tagList(tags$span(class = "ms-3"), "Total missing after cleaning ", tags$b(pct(m$removed))))
  })

  # Dotted lines + labels for each event message in a trial
  event_markers <- function(rawt) {
    out <- list(shapes = list(), annots = list())
    if (!"message" %in% names(rawt)) return(out)
    ev <- rawt[!is.na(rawt$message), c("time", "message")]
    for (i in seq_len(nrow(ev))) {
      out$shapes[[i]] <- list(
        type = "line", xref = "x", yref = "paper", x0 = ev$time[i], x1 = ev$time[i],
        y0 = 0, y1 = 1, line = list(color = "rgba(110,110,110,.4)", dash = "dot", width = 1))
      out$annots[[i]] <- list(
        x = ev$time[i], y = 1, xref = "x", yref = "paper", text = ev$message[i],
        showarrow = FALSE, textangle = -90, xanchor = "left", yanchor = "top",
        font = list(size = 10, color = "#868e96"))
    }
    out
  }

  # Unprocessed view: every pupil and gaze column as loaded
  raw_trial_plot <- function(rawt) {
    eye_cols <- intersect(c("pupil_left", "pupil_right"), names(rawt))
    pupil_cols <- if (length(eye_cols)) c(eye_cols, "pupil") else "pupil"
    labels <- c(pupil_left = "Left eye", pupil_right = "Right eye",
                pupil = if (length(eye_cols)) paste0("Mapped (", input$col_pupil, ")") else "Pupil")
    cols <- c(pupil_left = palette[1], pupil_right = palette[2], pupil = "#868e96")
    top <- plotly::plot_ly()
    for (pc in pupil_cols) {
      top <- plotly::add_lines(top, x = rawt$time, y = rawt[[pc]], name = labels[[pc]],
                               line = list(color = cols[[pc]], width = if (pc == "pupil" && length(eye_cols)) 1 else 1.5,
                                           dash = if (pc == "pupil" && length(eye_cols)) "dot" else "solid"))
    }
    top <- plotly::layout(top, yaxis = list(title = "Pupil size (raw)"))

    gaze_cols <- intersect(c("x", "y", "x_left", "y_left", "x_right", "y_right"), names(rawt))
    gaze_cols <- gaze_cols[vapply(gaze_cols, function(g) any(!is.na(rawt[[g]])), logical(1))]
    # With per-eye gaze, the averaged x/y would just overlap them
    if (any(c("x_left", "x_right") %in% gaze_cols)) gaze_cols <- setdiff(gaze_cols, c("x", "y"))
    if (!length(gaze_cols)) return(top)
    gaze_pal <- c(x = palette[3], y = palette[4], x_left = palette[3], y_left = palette[4],
                  x_right = palette[6], y_right = palette[5])
    bottom <- plotly::plot_ly()
    for (g in gaze_cols) {
      bottom <- plotly::add_lines(bottom, x = rawt$time, y = rawt[[g]], name = paste("Gaze", g),
                                  line = list(color = gaze_pal[[g]], width = 1))
    }
    plotly::subplot(top, plotly::layout(bottom, yaxis = list(title = "Gaze (px)")),
                    nrows = 2, shareX = TRUE, heights = c(0.62, 0.38), titleY = TRUE, margin = 0.04)
  }

  output$trial_plot <- plotly::renderPlotly({
    req(std(), input$view_subject, input$view_trial)
    rawt <- raw_trial()

    if (isTRUE(input$raw_only)) {
      m <- event_markers(rawt)
      blink_mask <- if ("blink" %in% names(rawt)) !is.na(rawt$blink) & rawt$blink > 0
                    else rep(FALSE, nrow(rawt))
      miss <- missing_shapes((is.na(rawt$pupil) | rawt$pupil <= 0) & !blink_mask, rawt$time, miss_col)
      blinks <- missing_shapes(blink_mask, rawt$time, blink_col)
      return(raw_trial_plot(rawt) |>
               plotly::layout(xaxis = list(title = "Time (ms)"),
                              legend = list(orientation = "h", y = 1.08, x = 0)) |>
               tidy_plot() |>
               with_overlays(c(miss, blinks, m$shapes), m$annots))
    }

    rawt$pupil[!is.na(rawt$pupil) & rawt$pupil <= 0] <- NA
    is_this <- function(df) {
      as.character(df$subject) == input$view_subject & as.character(df$trial) == input$view_trial & this_eye(df)
    }

    top <- plotly::plot_ly(rawt, x = ~time, y = ~pupil, type = "scatter", mode = "lines",
                           name = "Raw signal", line = list(color = "rgba(134,142,150,.45)", width = 1))
    fig <- plotly::layout(top, yaxis = list(title = "Pupil size"))

    res <- result()
    # Blink/artifact removal is kept for every trial, including excluded ones
    clean <- if (!is.null(res)) res$cleaned[is_this(res$cleaned), ]
    proc <- if (!is.null(res)) res$data[is_this(res$data), ]
    if (!is.null(clean) && nrow(clean)) {
      top <- plotly::add_trace(top, data = clean, x = ~time, y = ~extendpupil, mode = "lines",
                               name = "After blink/artifact removal",
                               line = list(color = palette[1], width = 1.5))
      fig <- plotly::layout(top, yaxis = list(title = "Pupil size"))
    }
    if (!is.null(proc) && nrow(proc)) {
      top <- top |>
        plotly::add_trace(data = proc, x = ~time, y = ~pup_interp, mode = "lines",
                          name = "Interpolated + smoothed signal",
                          line = list(color = palette[2], width = 2.5))
      zs <- identical(result()$value_col, "pup_z") && "pup_z" %in% names(proc)
      bottom <- plotly::plot_ly(proc, x = ~time, y = proc[[if (zs) "pup_z" else "baselinecorrectedp"]], type = "scatter",
                                mode = "lines", name = if (zs) "Baseline-corrected, z-scored" else "Baseline-corrected",
                                line = list(color = palette[3], width = 2))
      fig <- plotly::subplot(
        plotly::layout(top, yaxis = list(title = "Pupil size")),
        plotly::layout(bottom, yaxis = list(title = if (zs) "Corrected (z)" else "Corrected")),
        nrows = 2, shareX = TRUE, heights = c(0.62, 0.38), titleY = TRUE, margin = 0.04
      )
    }

    shapes <- list()
    annots <- list()

    # Shade missing data: red = blink, grey = any other missing sample
    # (padding, speed outliers, out-of-range, tracker dropout). Before a run,
    # use the data as loaded.
    if (!is.null(clean) && nrow(clean)) {
      blink_mask <- !is.na(clean$blink_detected) & clean$blink_detected > 0
      shapes <- c(shapes,
                  missing_shapes(is.na(clean$extendpupil) & !blink_mask, clean$time, miss_col),
                  missing_shapes(blink_mask, clean$time, blink_col))
    } else {
      blink_mask <- if ("blink" %in% names(rawt)) !is.na(rawt$blink) & rawt$blink > 0
                    else rep(FALSE, nrow(rawt))
      shapes <- c(shapes,
                  missing_shapes(is.na(rawt$pupil) & !blink_mask, rawt$time, miss_col),
                  missing_shapes(blink_mask, rawt$time, blink_col))
    }

    # Event markers as dotted vertical lines with labels
    if ("message" %in% names(rawt)) {
      ev <- rawt[!is.na(rawt$message), c("time", "message")]
      for (i in seq_len(nrow(ev))) {
        shapes[[length(shapes) + 1]] <- list(
          type = "line", xref = "x", yref = "paper", x0 = ev$time[i], x1 = ev$time[i],
          y0 = 0, y1 = 1, line = list(color = "rgba(110,110,110,.4)", dash = "dot", width = 1))
        annots[[length(annots) + 1]] <- list(
          x = ev$time[i], y = 1, xref = "x", yref = "paper", text = ev$message[i],
          showarrow = FALSE, textangle = -90, xanchor = "left", yanchor = "top",
          font = list(size = 10, color = "#868e96"))
      }
    }
    fig |>
      plotly::layout(xaxis = list(title = "Time (ms)"),
                     legend = list(orientation = "h", y = 1.08, x = 0)) |>
      tidy_plot() |>
      with_overlays(shapes, annots)
  })

  # ---- Quality ------------------------------------------------------------
  output$quality_ui <- renderUI({
    if (is.null(result())) {
      return(empty_state("clipboard-check", "Nothing to report yet",
                         "Run the pipeline to see missing data and exclusions."))
    }
    q <- result()$quality
    kept <- q[!q$excluded, ]
    tagList(
      layout_columns(
        fill = FALSE,
        value_box("Trials retained", sprintf("%d / %d", nrow(kept), nrow(q)),
                  sprintf("%.0f%%", 100 * nrow(kept) / nrow(q)), showcase_layout = "left center", showcase = icon("layer-group"),
                  theme = "primary"),
        value_box("Subjects retained",
                  sprintf("%d / %d", length(unique(kept$subject)), length(unique(q$subject))),
                  showcase_layout = "left center", showcase = icon("users")),
        value_box("Median missing per trial", sprintf("%.1f%%", 100 * stats::median(q$prop_missing, na.rm = TRUE)),
                  showcase_layout = "left center", showcase = icon("eye-slash"))
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Missing data per trial"),
             plotly::plotlyOutput("missing_hist", height = "340px")),
        card(card_header("Trial table"), DT::DTOutput("quality_table"))
      ),
      card(card_header(tagList(icon("users"), "By subject")),
           DT::DTOutput("subject_quality_table")),
      card(card_header(tagList(icon("eye"), "Missing data and blinks by eye (as loaded)")),
           DT::DTOutput("eye_table_quality"))
    )
  })

  output$missing_hist <- plotly::renderPlotly({
    req(result())
    q <- result()$quality
    thr <- result()$params$missingthresh
    plotly::plot_ly(q, x = ~prop_missing, color = ~subject, colors = palette,
                    type = "histogram", nbinsx = 40) |>
      plotly::layout(barmode = "stack",
                     xaxis = list(title = "Proportion of samples missing", range = c(0, 1)),
                     yaxis = list(title = "Trials"),
                     shapes = list(list(type = "line", x0 = thr, x1 = thr, yref = "paper",
                                        y0 = 0, y1 = 1,
                                        line = list(color = "#c92a2a", dash = "dash"))),
                     annotations = list(list(x = thr, y = 1, yref = "paper", text = "threshold",
                                             showarrow = FALSE, xanchor = "left",
                                             font = list(color = "#c92a2a")))) |>
      tidy_plot()
  })

  # Per-subject summary of missing data and exclusions
  output$subject_quality_table <- DT::renderDT({
    req(result())
    q <- result()$quality
    by_subj <- q |>
      dplyr::group_by(subject) |>
      dplyr::summarise(trials = dplyr::n(),
                       kept = sum(!excluded),
                       excluded = sum(excluded),
                       mean_missing = mean(prop_missing, na.rm = TRUE),
                       median_missing = stats::median(prop_missing, na.rm = TRUE),
                       max_missing = max(prop_missing, na.rm = TRUE),
                       subject_excluded = any(reason == "subject missing data"),
                       .groups = "drop")
    DT::datatable(by_subj, rownames = FALSE,
                  colnames = c("Subject", "Trials", "Kept", "Excluded", "Mean missing",
                               "Median missing", "Max missing", "Subject excluded"),
                  options = list(pageLength = 10, dom = "tip", scrollX = TRUE)) |>
      DT::formatPercentage(c("mean_missing", "median_missing", "max_missing"), 1) |>
      DT::formatStyle("subject_excluded", target = "row",
                      backgroundColor = DT::styleEqual(TRUE, "rgba(240,140,0,.12)"))
  })

  output$quality_table <- DT::renderDT({
    req(result())
    q <- result()$quality
    DT::datatable(q, rownames = FALSE, filter = "top",
                  colnames = c("Subject", "Trial", "Samples", "Missing", "Subject missing", "Reason", "Excluded"),
                  options = list(pageLength = 8, scrollX = TRUE, dom = "tip")) |>
      DT::formatPercentage(c("prop_missing", "subject_prop_missing"), 1) |>
      DT::formatStyle("excluded", target = "row",
                      backgroundColor = DT::styleEqual(TRUE, "rgba(240,140,0,.12)"))
  })

  # ---- Results ------------------------------------------------------------
  # Plots are computed from the processed samples, not the Group-by table:
  # trials are averaged within subject first, so subjects are the unit (or
  # trials, when there is only one subject).

  canonical_cols <- c("subject", "trial", "time", "time_zero", "pupil", "x", "y", "blink",
                      "message", "tracker_time", "source_file", "extendpupil", "pup_interp",
                      "interp", "pup", "baseline", "baselinecorrectedp", "blink_detected", "eyes_recorded",
                      "timebins", "movingavgpup", "index", ".hz", "processing_hz", "upsampled", "sample_rate")

  # Columns usable as conditions: low-cardinality, not pipeline internals
  condition_choices <- reactive({
    req(result())
    d <- result()$data
    cand <- setdiff(names(d), canonical_cols)
    cand[vapply(cand, function(cl) dplyr::n_distinct(d[[cl]]) <= 20, logical(1))]
  })

  # Numeric trial-level columns (RT, accuracy, ...) for behaviour plots
  numeric_choices <- reactive({
    req(result())
    d <- result()$data
    cand <- setdiff(names(d), canonical_cols)
    cand[vapply(cand, function(cl) is.numeric(d[[cl]]) && dplyr::n_distinct(d[[cl]]) > 1, logical(1))]
  })

  output$results_ui <- renderUI({
    if (is.null(result())) {
      return(empty_state("chart-line", "No results yet",
                         "Run the pipeline to see the pupil response."))
    }
    p <- result()$params
    keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }
    default_conds <- intersect(setdiff(p$group_cols, c("subject", "trial")), condition_choices())
    win_default <- if (isTRUE(p$timelock)) c(0, if (is.na(p$window[2])) 2000 else p$window[2]) else c(NA, NA)
    tagList(
      card(
        class = "results-controls",
        layout_columns(
          col_widths = c(3, 3, 3, 3), gap = "0.75rem",
          radioButtons("plot_type", "Plot", c("Time course" = "timecourse", "Raincloud" = "raincloud",
                                              "Pupil vs behaviour" = "scatter"),
                       selected = keep("plot_type", "timecourse")),
          selectizeInput("plot_conds", info("Conditions", "Columns that define the lines (time course) or groups (raincloud). Behavioural columns appear here too."),
                         condition_choices(), selected = keep("plot_conds", default_conds), multiple = TRUE,
                         options = list(placeholder = "All trials pooled", plugins = list("remove_button"))),
          selectInput("plot_error", info("Error", "Within-subject SE removes between-subject differences (Cousineau, 2005; Morey, 2008), which suits within-subject conditions. Between-subject SE suits conditions that differ between people. Trials are averaged within subject first; with one subject, trials are the unit."),
                      c("Within-subject SE" = "within_se", "Within-subject 95% CI" = "within_ci",
                        "Between-subject SE" = "between_se", "Between-subject 95% CI" = "between_ci",
                        "None" = "none_none"),
                      selected = keep("plot_error", "within_se")),
          numericInput("plot_bin", "Plot bin (ms)", keep("plot_bin", p$bin_length), min = 1)
        ),
        conditionalPanel("input.plot_type != 'timecourse'",
          layout_columns(
            col_widths = c(3, 3, 3, 3), gap = "0.75rem",
            numericInput("win_from", info("Window from (ms)", "Mean pupil in this window is used for the raincloud and the behaviour plot."), keep("win_from", win_default[1])),
            numericInput("win_to", "Window to (ms)", keep("win_to", win_default[2])),
            conditionalPanel("input.plot_type == 'raincloud'",
                             checkboxInput("connect_units", "Connect each subject's points", keep("connect_units", TRUE))),
            conditionalPanel("input.plot_type == 'scatter'",
                             selectInput("behav_var", "Behavioural variable", numeric_choices(),
                                         selected = keep("behav_var", intersect(c("rt", "RT", "acc"), numeric_choices())[1])),
                             radioButtons("scatter_level", NULL, c("Trials" = "trial", "Subject means" = "subject"),
                                          selected = keep("scatter_level", "trial"), inline = TRUE))
          )),
        conditionalPanel("input.plot_type == 'timecourse'",
          layout_columns(
            col_widths = c(3, 9), gap = "0.75rem",
            selectInput("rt_marker", info("Mark behavioural times", "Dashed line at the mean of this column for each condition, e.g. response time. It should be in ms on the same clock as the time axis."),
                        c("(none)" = "", numeric_choices()), selected = keep("rt_marker", ""))
          ))
      ),
      card(card_header(textOutput("results_title", inline = TRUE)),
           plotly::plotlyOutput("results_plot", height = "480px"),
           card_footer(class = "small text-muted", textOutput("results_note", inline = TRUE))),
      cpt_card(),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Plotted values"),
             DT::DTOutput("results_table"),
             downloadButton("dl_plot_values", "Download (.csv)", class = "btn-sm btn-outline-primary mt-2")),
        card(card_header("Binned data (Group by)"), DT::DTOutput("binned_table"))
      )
    )
  })

  # ---- Cluster-based permutation test -----------------------------------
  cpt_card <- function() {
    keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }
    conds <- condition_choices()
    if (isTRUE(result()$eyes)) conds <- unique(c("eye", conds))
    p <- result()$params
    win <- if (isTRUE(p$timelock)) c(0, if (is.na(p$window[2])) NA else p$window[2]) else c(NA, NA)
    card(
      card_header(tagList(icon("layer-group"), " Cluster-based permutation test")),
      p(class = "small text-muted",
        "Where in time do the conditions differ? A mixed model is fit in every time bin (the binned data from step 5 for subject means; the plot bin for trials); ",
        "adjacent bins past the threshold form clusters, and each cluster's summed statistic is compared with ",
        "the largest cluster found when the condition labels are shuffled within subjects (Maris & Oostenveld, 2007)."),
      layout_columns(
        col_widths = c(3, 3, 2, 2, 2), gap = "0.75rem",
        selectInput("cpt_cond", "Condition", conds, selected = keep("cpt_cond", intersect(input$plot_conds, conds)[1])),
        radioButtons("cpt_model", info("Model", "Subject means: the binned data from step 5 (aggbaseline per subject, condition and time bin; Group by must include subject and the condition), with a random intercept per subject. Trials: every trial binned at the plot bin, with random intercepts per subject (and item, if chosen); slower."),
                     c("Subject means" = "subject", "Trials" = "trial"), selected = keep("cpt_model", "subject")),
        numericInput("cpt_from", "From (ms)", keep("cpt_from", win[1])),
        numericInput("cpt_to", "To (ms)", keep("cpt_to", win[2])),
        numericInput("cpt_nperm", info("Permutations", "1000 or more for publication. Each one fits a model in every time bin, so this can take minutes (longer in the browser)."),
                     keep("cpt_nperm", 1000), min = 10, step = 100)
      ),
      layout_columns(
        col_widths = c(3, 3, 2, 4), gap = "0.75rem",
        conditionalPanel("input.cpt_model == 'trial'",
                         selectInput("cpt_item", "Item random intercept", c("(none)" = "", condition_choices(), numeric_choices()),
                                     selected = keep("cpt_item", ""))),
        if (isTRUE(result()$eyes)) conditionalPanel("input.cpt_cond != 'eye'",
                         radioButtons("cpt_eye", "Eye", c(Left = "left", Right = "right"),
                                      selected = keep("cpt_eye", "left"), inline = TRUE)) else div(),
        numericInput("cpt_alpha", info("Alpha", "Cluster-forming threshold: |t| above the two-sided critical value (1.96 for .05); also the significance level for clusters."),
                     keep("cpt_alpha", 0.05), min = 0.001, max = 0.5, step = 0.01),
        div(class = "mt-4", actionButton("cpt_run", "Run test", icon = icon("play"), class = "btn-primary"))
      ),
      uiOutput("cpt_output")
    )
  }

  cpt_res <- reactiveVal(NULL)
  observeEvent(result(), cpt_res(NULL))

  observeEvent(input$cpt_run, {
    res <- result()
    req(res, input$cpt_cond, input$plot_bin)
    cond <- input$cpt_cond
    if (!requireNamespace("lme4", quietly = TRUE)) {
      showNotification("The cluster test needs the lme4 package: install.packages(\"lme4\").", type = "error", duration = 10)
      return()
    }
    d <- res$data
    if (isTRUE(res$eyes) && cond != "eye") d <- d[d$eye == input$cpt_eye, ]
    vc <- if (is.null(res$value_col)) "baselinecorrectedp" else res$value_col
    tv <- res$timevar
    bin <- input$plot_bin
    d$timebins <- round(d[[tv]] / bin) * bin
    if (isTRUE(is.finite(input$cpt_from))) d <- d[d$timebins >= input$cpt_from, ]
    if (isTRUE(is.finite(input$cpt_to))) d <- d[d$timebins <= input$cpt_to, ]
    item <- if (identical(input$cpt_model, "trial") && isTRUE(nzchar(input$cpt_item))) input$cpt_item
    keys <- unique(c("subject", "trial", if (cond == "eye") "eye", item))
    x <- data.frame(subject = as.character(d$subject), trial = d$trial, condition = d[[cond]],
                    pupil = d[[vc]], timebins = d$timebins)
    if (cond == "eye") x$eye <- d$eye
    if (!is.null(item)) x$item <- as.character(d[[item]])
    x <- x[!is.na(x$condition) & !is.na(x$pupil), ]
    if (length(unique(x$condition)) < 2) {
      showNotification("The condition needs at least two levels in the data.", type = "error")
      return()
    }
    unit <- if (identical(input$cpt_model, "trial")) c("subject", "trial", if (cond == "eye") "eye") else c("subject", "condition")
    if (identical(input$cpt_model, "subject")) {
      # Subject means: the binned data from step 5 (Group by), as exported
      b <- res$binned
      need <- c("subject", cond)
      if (!all(need %in% names(b))) {
        showNotification(paste0("The Subject means model uses the binned data, which must be grouped by ",
                                paste(setdiff(need, names(b)), collapse = " and "),
                                ". Add it to Group by (step 5) and run again, or use the Trials model."),
                         type = "error", duration = 12)
        return()
      }
      if (isTRUE(res$eyes) && cond != "eye") b <- b[b$eye == input$cpt_eye, ]
      if (isTRUE(is.finite(input$cpt_from))) b <- b[b$timebins >= input$cpt_from, ]
      if (isTRUE(is.finite(input$cpt_to))) b <- b[b$timebins <= input$cpt_to, ]
      x <- data.frame(subject = as.character(b$subject), condition = b[[cond]],
                      pupil = b$aggbaseline, timebins = b$timebins)
      x <- x[!is.na(x$condition) & !is.na(x$pupil), ]
      bin <- if (length(unique(x$timebins)) > 1) min(diff(sort(unique(x$timebins)))) else res$params$bin_length
    }
    n_sub <- length(unique(x$subject))
    re <- c(if (n_sub > 1) "(1 | subject)", if (!is.null(item)) "(1 | item)")
    f <- stats::as.formula(paste("pupil ~ condition", if (length(re)) paste("+", paste(re, collapse = " + "))))
    if (n_sub == 1 && identical(input$cpt_model, "subject")) {
      showNotification(paste0("Only one participant (", x$subject[1], ") has data for this test, so subject means can't be compared. ",
                              "If others were excluded, see the Quality tab; or use the Trials model."),
                       type = "error", duration = 12)
      return()
    }
    n_perm <- max(10, round(input$cpt_nperm))
    out <- withProgress(message = "Cluster permutation test", value = 0, {
      tryCatch(
        cluster_permutation(x, f, perm_var = "condition", unit = unit, n_perm = n_perm,
                            alpha = input$cpt_alpha, seed = 1,
                            progress = function(k, n) if (k %% max(1, n %/% 50) == 0) setProgress(k / n, detail = paste(k, "of", n)),
                            labels = list(response = y_label(), condition = cond)),
        error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
    })
    req(out)
    out$bin <- bin
    out$eye <- if (isTRUE(res$eyes) && cond != "eye") input$cpt_eye
    out$model_label <- paste0(if (identical(input$cpt_model, "trial")) "trial-level" else "subject-mean", " data, ",
                              deparse(f))
    cpt_res(out)
    add_log("Cluster permutation test: ", cond, ", ", out$model_label, ", ", n_perm, " permutations; ",
            sum(out$clusters$p < input$cpt_alpha), " significant cluster(s)")
  })

  output$cpt_output <- renderUI({
    r <- cpt_res()
    if (is.null(r)) return(p(class = "small text-muted mt-2", "Choose the settings and click Run test."))
    tagList(
      plotOutput("cpt_plot", height = "420px"),
      DT::DTOutput("cpt_table"),
      p(class = "small mt-2", textOutput("cpt_methods", inline = TRUE)),
      div(class = "d-flex gap-2",
          downloadButton("dl_cpt_png", "Figure (.png)", class = "btn-sm btn-outline-primary"),
          downloadButton("dl_cpt_csv", "Clusters (.csv)", class = "btn-sm btn-outline-primary"))
    )
  })
  cpt_plot_obj <- reactive({
    r <- cpt_res(); req(r)
    plot_cluster_perm(r, alpha = input$cpt_alpha) +
      ggplot2::labs(title = if (!is.null(r$eye)) paste(tools::toTitleCase(r$eye), "eye"))
  })
  output$cpt_plot <- renderPlot(cpt_plot_obj(), res = 96)
  output$cpt_table <- DT::renderDT({
    r <- cpt_res(); req(r)
    cl <- as.data.frame(r$clusters)
    cl$significant <- ifelse(cl$p < input$cpt_alpha, "yes", "no")
    DT::datatable(cl, rownames = FALSE, options = list(dom = "t", pageLength = 50),
                  colnames = c("Cluster", "Start (ms)", "End (ms)", "Bins", "Mass", "Sign", "p", "Significant")) |>
      DT::formatRound(c("mass", "p"), 3)
  })
  output$cpt_methods <- renderText({
    r <- cpt_res(); req(r)
    sig <- r$clusters[r$clusters$p < input$cpt_alpha, ]
    found <- if (nrow(sig)) paste0("The conditions differed from ",
                                   paste(sprintf("%g to %g ms (cluster mass = %.2f, p = %s)", sig$start, sig$end, sig$mass,
                                                 sub("^0", "", sprintf("%.3f", sig$p))), collapse = " and from "), ".")
             else "No cluster was significant."
    paste0("Differences between levels of ", r$cond_label, " were tested with a cluster-based permutation test ",
           "(Maris & Oostenveld, 2007). In each ", r$bin, " ms bin, a linear mixed model (",
           gsub("\\bcondition\\b", r$cond_label, deparse(r$formula)),
           ", lme4) was fit to ", sub(", .*", "", r$model_label), "; adjacent bins with |", r$statistic, "| > ",
           sprintf("%.2f", r$threshold), " formed clusters, and each cluster's summed ", r$statistic,
           " was compared with the largest cluster mass in each of ", r$n_perm,
           " permutations of the condition labels ", if (r$within) "within participants" else "across participants", ". ", found)
  })
  output$dl_cpt_png <- downloadHandler(
    filename = function() paste0(stamp("cluster_test"), ".png"),
    content = function(file) ggplot2::ggsave(file, cpt_plot_obj(), width = 8, height = 5, dpi = 300, bg = "white")
  )
  output$dl_cpt_csv <- downloadHandler(
    filename = function() paste0(stamp("cluster_test"), ".csv"),
    content = function(file) data.table::fwrite(cpt_res()$clusters, file)
  )

  plot_bins <- reactive({
    req(result(), input$plot_bin)
    vc <- if (is.null(result()$value_col)) "baselinecorrectedp" else result()$value_col
    conds <- if (isTRUE(result()$eyes)) unique(c("eye", input$plot_conds)) else input$plot_conds
    bin_for_plot(result()$data, result()$timevar, input$plot_bin, conds, value_col = vc)
  })

  error_spec <- reactive({
    parts <- strsplit(if (is.null(input$plot_error)) "within_se" else input$plot_error, "_")[[1]]
    list(se_type = if (parts[1] == "none") "within" else parts[1], interval = parts[2])
  })

  timecourse <- reactive({
    e <- error_spec()
    summarise_timecourse(plot_bins()$unit, e$se_type, e$interval)
  })

  windowed <- reactive({
    req(input$plot_type != "timecourse")
    window_means(plot_bins(), c(input$win_from, input$win_to))
  })

  y_label <- reactive({
    p <- result()$params
    if (p$baseline == "none") "Pupil size"
    else if (p$baseline_method == "div") "Pupil change (proportion of baseline)"
    else "Pupil change (baseline-subtracted)"
  })

  x_label <- reactive({
    p <- result()$params
    if (result()$timevar == "time_zero") paste0("Time from ", event_label(p$timelock_event), " (ms)") else "Time (ms)"
  })

  cond_colours <- reactive({
    conds <- sort(unique(plot_bins()$unit$condition))
    stats::setNames(palette[(seq_along(conds) - 1) %% length(palette) + 1], conds)
  })

  output$results_title <- renderText({
    req(input$plot_type)
    unit <- plot_bins()$unit_name
    n <- dplyr::n_distinct(plot_bins()$unit$unit)
    switch(input$plot_type,
      timecourse = paste0("Pupil time course (", n, " ", unit, if (n != 1) "s", ")"),
      raincloud = paste0("Mean pupil, ", input$win_from, "–", input$win_to, " ms (one point per ", unit, ")"),
      scatter = paste0("Mean pupil, ", input$win_from, "–", input$win_to, " ms, vs ", input$behav_var))
  })

  output$results_note <- renderText({
    req(input$plot_type)
    unit <- plot_bins()$unit_name
    e <- error_spec()
    J <- dplyr::n_distinct(plot_bins()$unit$condition)
    err <- if (e$interval == "none") "No error bands."
      else paste0(if (e$interval == "ci") "95% CI" else "±1 SE", ", ",
                  if (e$se_type == "within" && J > 1) "within-subject (Cousineau–Morey)" else "between-units",
                  ", across ", unit, "s",
                  if (unit == "subject") " (trials averaged within subject first)" else " (single subject)", ".",
                  if (e$se_type == "within" && J < 2) " Within-subject error needs 2+ conditions, so between-units error is shown." else "")
    switch(input$plot_type,
      timecourse = err,
      raincloud = "Half-violin = distribution, box = median and IQR, points = each unit's mean in the window.",
      scatter = if (identical(input$scatter_level, "subject")) "Each point is one subject's mean in each condition; lines are least-squares fits."
                else "Each point is one trial; lines are least-squares fits per condition.")
  })

  output$results_plot <- plotly::renderPlotly({
    req(input$plot_type)
    switch(input$plot_type,
           timecourse = plot_timecourse(),
           raincloud = plot_raincloud(),
           scatter = plot_scatter()) |>
      tidy_plot()
  })

  plot_timecourse <- function() {
    g <- timecourse()
    cols <- cond_colours()
    fig <- plotly::plot_ly()
    for (cond in names(cols)) {
      gi <- g[g$condition == cond, ]
      if (error_spec()$interval != "none") {
        fig <- plotly::add_ribbons(fig, data = gi, x = ~timebins, ymin = ~lower, ymax = ~upper,
                                   fillcolor = grDevices::adjustcolor(cols[[cond]], 0.18),
                                   line = list(width = 0), showlegend = FALSE, hoverinfo = "skip",
                                   legendgroup = cond)
      }
      fig <- plotly::add_lines(fig, data = gi, x = ~timebins, y = ~mean, name = cond,
                               line = list(color = cols[[cond]], width = 2.5), legendgroup = cond,
                               text = ~paste0("n = ", n), hovertemplate = "%{y:.3f} (%{text})")
    }
    shapes <- list()
    # Mean behavioural time per condition (subject means first)
    if (!is.null(input$rt_marker) && nzchar(input$rt_marker)) {
      conds <- intersect(input$plot_conds, names(result()$data))
      tr <- dplyr::distinct(result()$data, dplyr::across(dplyr::all_of(unique(c("subject", "trial", conds, input$rt_marker)))))
      tr$condition <- if (length(conds)) do.call(paste, c(tr[conds], sep = " · ")) else "All"
      rts <- tr |>
        dplyr::group_by(subject, condition) |>
        dplyr::summarise(v = mean(.data[[input$rt_marker]], na.rm = TRUE), .groups = "drop") |>
        dplyr::group_by(condition) |>
        dplyr::summarise(v = mean(v, na.rm = TRUE), .groups = "drop")
      for (i in seq_len(nrow(rts))) {
        if (!is.finite(rts$v[i]) || !rts$condition[i] %in% names(cols)) next
        shapes[[length(shapes) + 1]] <- list(
          type = "line", xref = "x", yref = "paper", x0 = rts$v[i], x1 = rts$v[i], y0 = 0, y1 = 1,
          line = list(color = cols[[rts$condition[i]]], dash = "dash", width = 1.5))
        fig <- plotly::add_markers(fig, x = rts$v[i], y = 0, inherit = FALSE, showlegend = FALSE,
                                   marker = list(size = 1, opacity = 0),
                                   hovertemplate = paste0("Mean ", input$rt_marker, " (", rts$condition[i], "): %{x:.0f} ms<extra></extra>"))
      }
    }
    plotly::layout(fig, xaxis = list(title = x_label(), zeroline = TRUE),
                   yaxis = list(title = y_label(), zeroline = TRUE),
                   legend = list(orientation = "h", y = 1.1, x = 0), shapes = shapes)
  }

  plot_raincloud <- function() {
    w <- windowed()$unit
    cols <- cond_colours()
    conds <- names(cols)
    set.seed(1)
    fig <- plotly::plot_ly()
    pos <- stats::setNames(seq_along(conds), conds)
    w$x_pt <- pos[w$condition] - 0.28 + stats::runif(nrow(w), -0.06, 0.06)
    if (isTRUE(input$connect_units) && length(conds) > 1) {
      for (u in unique(w$unit)) {
        wu <- w[w$unit == u, ]
        wu <- wu[order(pos[wu$condition]), ]
        fig <- plotly::add_lines(fig, x = wu$x_pt, y = wu$value, inherit = FALSE, showlegend = FALSE,
                                 line = list(color = "rgba(134,142,150,.35)", width = 1), hoverinfo = "skip")
      }
    }
    for (cond in conds) {
      wi <- w[w$condition == cond, ]
      col <- cols[[cond]]
      fig <- fig |>
        plotly::add_trace(type = "violin", x = rep(pos[[cond]], nrow(wi)), y = wi$value, name = cond,
                          side = "positive", width = 0.7, points = FALSE, legendgroup = cond,
                          line = list(color = col), fillcolor = grDevices::adjustcolor(col, 0.35),
                          meanline = list(visible = FALSE), hoverinfo = "skip", showlegend = FALSE) |>
        plotly::add_trace(type = "box", x = rep(pos[[cond]] - 0.1, nrow(wi)), y = wi$value, name = cond,
                          width = 0.08, boxpoints = FALSE, legendgroup = cond, showlegend = FALSE,
                          line = list(color = col), fillcolor = "rgba(255,255,255,.6)") |>
        plotly::add_markers(x = wi$x_pt, y = wi$value, name = cond, legendgroup = cond,
                            marker = list(color = col, size = 8, opacity = 0.8,
                                          line = list(color = "white", width = 1)),
                            text = wi$unit, hovertemplate = "%{text}: %{y:.3f}<extra></extra>")
    }
    plotly::layout(fig,
                   xaxis = list(title = "", tickvals = unname(pos), ticktext = names(pos),
                                range = c(0.4, length(pos) + 0.9), zeroline = FALSE),
                   yaxis = list(title = y_label(), zeroline = TRUE),
                   showlegend = FALSE, hovermode = "closest")
  }

  plot_scatter <- function() {
    req(input$behav_var)
    w <- windowed()$trial
    beh <- dplyr::distinct(result()$data[, c("subject", "trial", input$behav_var)])
    names(beh)[3] <- "behav"
    beh$subject <- as.character(beh$subject)
    w$subject <- as.character(w$subject)
    d <- dplyr::inner_join(w, beh, by = c("subject", "trial"))
    if (identical(input$scatter_level, "subject")) {
      d <- d |>
        dplyr::group_by(subject, condition) |>
        dplyr::summarise(value = mean(value, na.rm = TRUE), behav = mean(behav, na.rm = TRUE), .groups = "drop")
      d$label <- d$subject
    } else {
      d$label <- paste(d$subject, "trial", d$trial)
    }
    d <- d[is.finite(d$value) & is.finite(d$behav), ]
    cols <- cond_colours()
    fig <- plotly::plot_ly()
    for (cond in intersect(names(cols), unique(d$condition))) {
      di <- d[d$condition == cond, ]
      fig <- plotly::add_markers(fig, x = di$behav, y = di$value, name = cond, legendgroup = cond,
                                 marker = list(color = cols[[cond]], size = 7, opacity = 0.7),
                                 text = di$label, hovertemplate = "%{text}<br>%{x:.3f}, %{y:.3f}<extra></extra>")
      if (nrow(di) >= 3 && stats::var(di$behav) > 0) {
        fit <- stats::lm(value ~ behav, data = di)
        xs <- range(di$behav)
        r <- stats::cor(di$behav, di$value)
        fig <- plotly::add_lines(fig, x = xs, y = stats::predict(fit, data.frame(behav = xs)),
                                 name = sprintf("%s (r = %.2f)", cond, r), legendgroup = cond,
                                 line = list(color = cols[[cond]], width = 2), hoverinfo = "skip")
      }
    }
    plotly::layout(fig, xaxis = list(title = input$behav_var),
                   yaxis = list(title = paste(y_label(), "in window")),
                   legend = list(orientation = "h", y = 1.1, x = 0), hovermode = "closest")
  }

  plot_values <- reactive({
    req(input$plot_type)
    if (input$plot_type == "timecourse") timecourse() else windowed()$unit
  })

  output$results_table <- DT::renderDT({
    v <- plot_values()
    num <- vapply(v, is.double, logical(1))
    v[num] <- lapply(v[num], round, 4)
    DT::datatable(v, rownames = FALSE, filter = "top", options = list(pageLength = 8, dom = "tip", scrollX = TRUE))
  })

  output$dl_plot_values <- downloadHandler(
    filename = function() paste0("gazer_", input$plot_type, "_", format(Sys.time(), "%Y%m%d-%H%M"), ".csv"),
    content = function(file) data.table::fwrite(plot_values(), file)
  )

  output$binned_table <- DT::renderDT({
    req(result())
    DT::datatable(result()$binned, rownames = FALSE, filter = "top",
                  options = list(pageLength = 8, dom = "tip", scrollX = TRUE)) |>
      DT::formatRound("aggbaseline", 4)
  })

  # ---- Export -------------------------------------------------------------
  stamp <- function(x) paste0("gazer_", x, "_", format(Sys.time(), "%Y%m%d-%H%M"))
  csv_download <- function(what) {
    downloadHandler(
      filename = function() paste0(stamp(what), ".csv"),
      content = function(file) {
        req(result())
        data.table::fwrite(switch(what, samples = result()$data, binned = result()$binned,
                                  quality = result()$quality), file)
      }
    )
  }
  # ---- Data by participant: raw combined / processed / binned ------------
  observeEvent(std(), {
    subs <- sort(unique(as.character(std()$subject)))
    cur <- isolate(input$dl_subject)
    updateSelectInput(session, "dl_subject", choices = c("All participants" = "", subs),
                      selected = if (isTRUE(cur %in% subs)) cur else "")
  })
  observeEvent(input$dl_subject, {
    # A single participant is always one CSV
    if (nzchar(input$dl_subject)) updateRadioButtons(session, "dl_format", selected = "csv")
  })

  dl_data <- reactive({
    d <- switch(input$dl_dataset,
                raw = std(),
                processed = if (!is.null(result())) result()$data,
                binned = if (!is.null(result())) result()$binned)
    if (!is.null(d) && nzchar(input$dl_subject) && "subject" %in% names(d)) {
      d <- d[as.character(d$subject) == input$dl_subject, ]
    }
    d
  })

  output$dl_by_subject_note <- renderUI({
    d <- dl_data()
    if (is.null(d)) {
      msg <- if (input$dl_dataset == "raw") "Load data first." else "Run the pipeline first."
      return(div(class = "small text-warning mt-1", msg))
    }
    n_sub <- if ("subject" %in% names(d)) dplyr::n_distinct(d$subject) else 1
    div(class = "small text-muted mt-1",
        sprintf("%s rows, %d participant%s", format(nrow(d), big.mark = ","), n_sub, if (n_sub != 1) "s" else ""))
  })

  output$dl_by_subject <- downloadHandler(
    filename = function() {
      who <- if (nzchar(input$dl_subject)) gsub("[^A-Za-z0-9_.-]", "_", input$dl_subject) else "all"
      ext <- if (input$dl_format == "zip" && !nzchar(input$dl_subject)) ".zip" else ".csv"
      paste0(stamp(paste(input$dl_dataset, who, sep = "_")), ext)
    },
    content = function(file) {
      d <- dl_data()
      validate(need(!is.null(d), "Nothing to download yet."))
      write_dataset(d, file, label = input$dl_dataset,
                    per_subject = input$dl_format == "zip" && !nzchar(input$dl_subject))
    }
  )

  # Write one CSV, or a zip with one CSV per participant
  write_dataset <- function(d, file, label, per_subject = FALSE) {
    if (per_subject && "subject" %in% names(d)) {
      dir <- tempfile("gazer_dl_")
      dir.create(dir)
      parts <- split(d, as.character(d$subject))
      paths <- vapply(names(parts), function(sid) {
        f <- file.path(dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", sid), "_", label, ".csv"))
        data.table::fwrite(parts[[sid]], f)
        f
      }, character(1))
      dict <- file.path(dir, "data_dictionary.csv")
      data.table::fwrite(data_dictionary(stats::setNames(list(d), label)), dict)
      zip::zip(file, files = c(basename(paths), "data_dictionary.csv"), root = dir)
    } else {
      data.table::fwrite(d, file)
    }
  }

  # Shortcut on the Data tab: combined data as loaded, no pre-processing
  output$raw_download_ui <- renderUI({
    req(std())
    div(class = "d-flex flex-wrap align-items-center gap-2 mt-3",
        tags$span(class = "small text-muted me-1", icon("download"),
                  "Combined data as loaded (no pre-processing):"),
        downloadButton("dl_raw_csv", "All participants (.csv)", class = "btn-sm btn-outline-primary"),
        downloadButton("dl_raw_zip", "One file per participant (.zip)", class = "btn-sm btn-outline-primary"),
        tags$span(class = "small text-muted", "One participant at a time: Export tab."))
  })
  output$dl_raw_csv <- downloadHandler(
    filename = function() paste0(stamp("raw_all"), ".csv"),
    content = function(file) write_dataset(std(), file, "raw")
  )
  output$dl_raw_zip <- downloadHandler(
    filename = function() paste0(stamp("raw_all"), ".zip"),
    content = function(file) write_dataset(std(), file, "raw", per_subject = TRUE)
  )

  # Every column of every dataset available now, with a description
  # ---- QC reports and BIDS-like export ----------------------------------
  observeEvent(result(), {
    subs <- sort(unique(as.character(result()$quality$subject)))
    updateSelectInput(session, "qc_subject", choices = c("All participants (.zip)" = "", subs))
  })

  output$dl_qc <- downloadHandler(
    filename = function() {
      if (nzchar(input$qc_subject)) paste0("qc_report_", bids_label(input$qc_subject), ".html")
      else paste0(stamp("qc_reports"), ".zip")
    },
    content = function(file) {
      res <- result()
      validate(need(!is.null(res), "Run the pipeline first."))
      meta <- list(app_version = app_version, gazer_version = gazer_version)
      if (nzchar(input$qc_subject)) {
        writeLines(qc_report_html(res, std(), input$qc_subject, meta), file)
      } else {
        dir <- tempfile("gazer_qc_"); dir.create(dir)
        subs <- unique(as.character(res$quality$subject))
        withProgress(message = "Writing QC reports", value = 0, {
          for (i in seq_along(subs)) {
            writeLines(qc_report_html(res, std(), subs[i], meta),
                       file.path(dir, paste0("qc_report_", bids_label(subs[i]), ".html")))
            incProgress(1 / length(subs))
          }
        })
        zip::zip(file, files = list.files(dir), root = dir)
      }
    }
  )

  output$dl_bids <- downloadHandler(
    filename = function() paste0(stamp("bids"), ".zip"),
    content = function(file) {
      res <- result()
      validate(need(!is.null(res), "Run the pipeline first."))
      root <- tempfile("gazer_bids_"); dir.create(root)
      write_bids(root, res, std(), list(app_version = app_version, gazer_version = gazer_version),
                 task = input$bids_task)
      zip::zip(file, files = "derivatives", root = root)
    }
  )

  # ---- Study file: save / restore settings ------------------------------
  study_inputs <- list(
    numeric = c("hz", "fillback", "fillforward", "mad_n", "hershman_smooth", "hershman_gap", "maxgap", "ma_n",
                "lp_cutoff", "lp_order", "baseline_dur", "bw_start", "bw_end", "win_start", "win_end",
                "bin_length", "pupil_min", "pupil_max", "rt_min", "rt_max", "rt_sd", "pl_lag", "target_rate"),
    slider = "missingthresh",
    radio = c("interp_type", "filter", "step_first", "baseline", "baseline_method", "baseline_stat",
              "blink_method", "rate_mode", "asc_eye", "pl_start_mode", "rate_target", "zscore"),
    checkbox = c("use_blink_col", "use_mad", "timelock", "resample", "pl_aoi", "shared_missing"),
    text = c("tobii_onset", "tobii_offset", "pl_subject", "pl_start_messages", "bids_task"),
    select = c("tracker"),
    # These depend on the loaded data, so they are applied once data are loaded
    data_select = c("col_subject", "col_trial", "col_time", "col_pupil", "col_blink", "col_message",
                    "acc_col", "rt_col"),
    data_multi = c("baseline_event", "timelock_event", "group_cols", "acc_correct")
  )

  output$dl_study <- downloadHandler(
    filename = function() paste0(stamp("study"), ".json"),
    content = function(file) {
      ids <- unlist(study_inputs, use.names = FALSE)
      vals <- lapply(stats::setNames(ids, ids), function(id) input[[id]])
      vals <- vals[!vapply(vals, is.null, logical(1))]
      jsonlite::write_json(list(app = "shinygazer", app_version = app_version, gazer_version = gazer_version,
                                saved = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                                files = if (!is.null(input$files)) input$files$name else list(),
                                settings = vals),
                           file, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
    }
  )

  pending_study <- reactiveVal(NULL)

  apply_settings <- function(st, data_dependent) {
    for (id in intersect(study_inputs$numeric, names(st))) updateNumericInput(session, id, value = st[[id]])
    for (id in intersect(study_inputs$slider, names(st))) updateSliderInput(session, id, value = st[[id]])
    for (id in intersect(study_inputs$radio, names(st))) updateRadioButtons(session, id, selected = st[[id]])
    for (id in intersect(study_inputs$checkbox, names(st))) updateCheckboxInput(session, id, value = isTRUE(st[[id]]))
    for (id in intersect(study_inputs$text, names(st))) updateTextInput(session, id, value = st[[id]])
    for (id in intersect(study_inputs$select, names(st))) updateSelectInput(session, id, selected = st[[id]])
    if (data_dependent) {
      for (id in intersect(study_inputs$data_select, names(st))) updateSelectInput(session, id, selected = st[[id]])
      for (id in intersect(study_inputs$data_multi, names(st))) updateSelectizeInput(session, id, selected = st[[id]])
    }
  }

  observeEvent(input$study_file, {
    st <- tryCatch(jsonlite::read_json(input$study_file$datapath, simplifyVector = TRUE),
                   error = function(e) NULL)
    if (is.null(st) || !identical(st$app, "shinygazer") || is.null(st$settings)) {
      showNotification("That doesn't look like a shiny gazeR study file.", type = "error")
      return()
    }
    have_data <- !is.null(raw())
    apply_settings(st$settings, data_dependent = have_data)
    if (!have_data) pending_study(st$settings)
    files <- if (length(st$files)) paste0(" Data files used: ", paste(st$files, collapse = ", "), ".") else ""
    showNotification(paste0("Settings restored from ", input$study_file$name, ".",
                            if (!have_data) " Column mapping, events and conditions will be applied when you load your data." else "",
                            files), type = "message", duration = 12)
    add_log("Loaded study settings from ", input$study_file$name, " (saved ", st$saved, ")")
  })

  # Apply data-dependent settings after the data's own choices are set up
  observeEvent(std(), {
    st <- pending_study()
    req(st)
    apply_settings(st, data_dependent = TRUE)
    pending_study(NULL)
  }, priority = -10)

  output$dl_dictionary <- downloadHandler(
    filename = function() paste0(stamp("data_dictionary"), ".csv"),
    content = function(file) {
      res <- result()
      sets <- list(raw = std())
      if (!is.null(res)) {
        sets <- c(sets, list(processed = res$data, binned = res$binned, quality = res$quality))
      }
      data.table::fwrite(data_dictionary(sets), file)
    }
  )

  # ---- Methods paragraph ---------------------------------------------------
  methods_meta <- reactive({
    files <- if (!is.null(input$files)) input$files$name else character()
    makers <- if (identical(input$tracker, "bids") && !is.null(bids_info()))
      unique(stats::na.omit(bids_info()$manufacturer)) else character()
    tracker <- switch(input$tracker,
      eyelink = "an SR Research EyeLink eye tracker",
      sr = "an SR Research EyeLink eye tracker (samples exported with Data Viewer)",
      tobii = "a Tobii eye tracker (recorded with Titta)",
      prolab = "a Tobii eye tracker (data exported from Tobii Pro Lab)",
      pupillabs = "Pupil Labs Neon eye-tracking glasses",
      bids = if (length(makers)) paste("an eye tracker from", paste(makers, collapse = " / ")) else "",
      auto = if (length(files) && all(grepl("\\.asc(\\.gz)?$", files, ignore.case = TRUE)))
               "an SR Research EyeLink eye tracker" else "",
      "")
    rec <- if ("eyes_recorded" %in% names(raw())) unique(stats::na.omit(raw()$eyes_recorded)) else character()
    list(tracker = tracker,
         binocular = identical(input$tracker, "tobii") || "both" %in% rec,
         mono_eye = if (length(rec) == 1 && rec != "both") rec else NULL,
         eye = if (input$tracker %in% c("auto", "eyelink", "tobii", "prolab", "bids")) input$asc_eye else NA,
         pupil_col = input$col_pupil,
         gazer_version = gazer_version, app_version = app_version)
  })

  methods <- reactive({
    req(result())
    methods_paragraph(result(), methods_meta())
  })

  output$methods_ui <- renderUI({
    if (is.null(result())) {
      return(p(class = "text-muted small mb-0",
               "Run the pipeline to generate a paragraph describing exactly what was done, with the gazeR citation, every pre-processing setting, exclusions and the final sample."))
    }
    m <- methods()
    tagList(
      p(class = "small text-muted",
        "Generated from this run's settings and results. Check it, and edit event names, conditions and wording to suit your paper."),
      div(id = "methods_text", class = "methods-text", m$text),
      h6(class = "mt-3", "References"),
      tags$ul(class = "small", lapply(m$references, function(r) tags$li(markdown::mark_html(text = r, template = FALSE) |> HTML()))),
      div(class = "d-flex gap-2",
          tags$button(class = "btn btn-sm btn-primary", type = "button",
                      onclick = "navigator.clipboard.writeText(document.getElementById('methods_text').innerText); this.innerText = 'Copied';",
                      icon("copy"), " Copy paragraph"),
          downloadButton("dl_methods", "Download with references (.md)", class = "btn-sm btn-outline-primary"))
    )
  })

  output$dl_methods <- downloadHandler(
    filename = function() paste0(stamp("methods"), ".md"),
    content = function(file) {
      m <- methods()
      writeLines(c("## Pupil pre-processing", "", m$text, "", "### References", "",
                   paste("-", m$references)), file)
    }
  )

  output$dl_samples <- csv_download("samples")
  output$dl_binned <- csv_download("binned")
  output$dl_quality <- csv_download("quality")

  output$dl_params <- downloadHandler(
    filename = function() paste0(stamp("params"), ".json"),
    content = function(file) {
      req(result())
      p <- result()$params
      p$maxgap <- if (is.infinite(p$maxgap)) NULL else p$maxgap
      p$sampling_rates <- stats::setNames(as.list(detected_rates()$hz), detected_rates()$subject)
      out <- list(
        shinygazer_version = app_version,
        gazer_version = gazer_version,
        created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        columns = list(subject = input$col_subject, trial = input$col_trial,
                       time = input$col_time, pupil = input$col_pupil,
                       blink = input$col_blink, message = input$col_message),
        parameters = p
      )
      jsonlite::write_json(out, file, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
  )

  output$log <- renderText({
    if (!length(log_lines())) "Nothing yet." else paste(log_lines(), collapse = "\n")
  })
}

shinyApp(ui, server)

# Pupil pre-processing pipeline used by the gazeR Shiny app.
# Kept free of Shiny code so it can be called (and tested) from plain R.

# Read one or more files and stack them. EyeLink .asc files go through
# gazer::read_asc(); everything else is read as a delimited gazeR-style file.
read_gazer_files <- function(paths, names = basename(paths), eye = "average") {
  dfs <- lapply(seq_along(paths), function(i) {
    if (grepl("\\.asc(\\.gz)?$", names[i], ignore.case = TRUE)) {
      d <- gazer::read_asc(paths[i], eye = eye,
                           subject = sub("\\.asc(\\.gz)?$", "", names[i], ignore.case = TRUE))
      d$sample_rate <- attr(d, "asc_info")$hz
      d$eyes_recorded <- attr(d, "asc_info")$recorded_eyes
      attr(d, "asc_info") <- NULL
    } else {
      d <- data.table::fread(paths[i], na.strings = c("NA", "."), fill = TRUE)
      # SR Data Viewer sample reports: trial-relative time in ms
      if (all(c("TIMESTAMP", "IP_START_TIME") %in% names(d))) {
        d$sr_trial_time <- d$TIMESTAMP - d$IP_START_TIME
      }
    }
    if (!"source_file" %in% names(d)) d$source_file <- names[i]
    d
  })
  tibble::as_tibble(data.table::rbindlist(dfs, fill = TRUE))
}

# Copy uploaded files into a fresh folder under their original names, since
# the readers below find files (and subjects) by name.
stage_uploads <- function(paths, names, dir = tempfile("gazer_upload_")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(paths, file.path(dir, names), overwrite = TRUE)
  file.path(dir, names)
}

# Pick or average the two eyes. `left`/`right` are named lists of vectors
# (pupil, x, y); average is taken only where both eyes are valid.
combine_eyes <- function(d, left, right, eye = "average") {
  avg <- function(a, b) ifelse(!is.na(left$pupil) & !is.na(right$pupil), (a + b) / 2, NA_real_)
  d$pupil_left <- left$pupil
  d$pupil_right <- right$pupil
  d$x_left <- left$x
  d$y_left <- left$y
  d$x_right <- right$x
  d$y_right <- right$y
  if (eye %in% c("average", "both")) {
    d$pupil <- avg(left$pupil, right$pupil)
    d$x <- avg(left$x, right$x)
    d$y <- avg(left$y, right$y)
  } else {
    src <- if (eye == "left") left else right
    d$pupil <- src$pupil
    d$x <- src$x
    d$y <- src$y
  }
  if (eye != "both") {
    d[c("pupil_left", "pupil_right", "x_left", "y_left", "x_right", "y_right")] <- NULL
  }
  d
}

# Tobii (Titta): paired gaze + message CSVs, matched by file name.
read_tobii_upload <- function(gaze_paths, gaze_names, msg_paths, msg_names,
                              eye = "average", onset_prefix = "onset_",
                              offset_prefix = "offset_") {
  missing <- setdiff(tools::file_path_sans_ext(gaze_names), tools::file_path_sans_ext(msg_names))
  if (length(missing)) {
    stop("No message file for: ", paste(missing, collapse = ", "),
         ". Gaze and message files must have the same name.", call. = FALSE)
  }
  gaze <- stage_uploads(gaze_paths, gaze_names)
  msg <- stage_uploads(msg_paths, msg_names)
  d <- suppressMessages(gazer::merge_tobii_titta(
    gaze, msg, onset_prefix = onset_prefix,
    offset_prefix = if (nzchar(offset_prefix)) offset_prefix else NULL))
  # Invalid samples -> NA, then choose/average eyes
  valid <- function(v, ok) ifelse(!is.na(ok) & as.logical(ok), v, NA_real_)
  side <- function(s) list(
    pupil = valid(d[[paste0(s, "_pupil_diameter")]], d[[paste0(s, "_pupil_valid")]]),
    x = valid(d[[paste0(s, "_gaze_point_on_display_area_x")]], d[[paste0(s, "_gaze_point_valid")]]),
    y = valid(d[[paste0(s, "_gaze_point_on_display_area_y")]], d[[paste0(s, "_gaze_point_valid")]]))
  d <- combine_eyes(d, side("left"), side("right"), eye)
  front <- intersect(c("subject", "trial", "time", "pupil", "x", "y", "message", "event_label"), names(d))
  tibble::as_tibble(d[, c(front, setdiff(names(d), front))])
}

# Pupil Labs (Neon) Time Series export, read with gazer::parse_pl().
# Either a .zip of the export (one subfolder per recording, any depth) or
# the loose CSVs from one recording folder. `...` goes to parse_pl()
# (start_mode, start_messages, max_event_lag_ms, aoi).
read_pupillabs_upload <- function(paths, names, subject = "sub01", ...) {
  need <- c("gaze.csv", "3d_eye_states.csv", "events.csv")
  is_zip <- grepl("\\.zip$", names, ignore.case = TRUE)
  if (any(is_zip)) {
    exdir <- tempfile("gazer_pl_")
    for (z in paths[is_zip]) utils::unzip(z, exdir = exdir)
    gaze_files <- list.files(exdir, pattern = "^gaze\\.csv$", recursive = TRUE, full.names = TRUE)
    dirs <- dirname(gaze_files)
    # Skip folders that aren't full recordings (e.g. surface-only exports)
    dirs <- dirs[vapply(dirs, function(d) all(file.exists(file.path(d, need))), logical(1))]
    dirs <- dirs[!grepl("__MACOSX", dirs)]
    if (!length(dirs)) {
      stop("No recording folders with gaze.csv, 3d_eye_states.csv and events.csv found in the zip.",
           call. = FALSE)
    }
    subjects <- basename(dirs)
  } else {
    missing <- setdiff(need, names)
    if (length(missing)) {
      stop("Pupil Labs export is missing: ", paste(missing, collapse = ", "),
           ". Select gaze.csv, 3d_eye_states.csv and events.csv from one recording folder, ",
           "or upload a .zip of the whole export.", call. = FALSE)
    }
    dirs <- dirname(stage_uploads(paths, names)[1])
    subjects <- subject
  }
  out <- lapply(seq_along(dirs), function(i) {
    d <- gazer::parse_pl(dirs[i], subject_id = subjects[i], ...)
    d[!is.na(d$trial), ]  # samples before the first trial-start message
  })
  tibble::as_tibble(dplyr::bind_rows(out))
}

# BIDS eye-tracking (BEP020), read with gazer::read_bids_eyetrack(). Either a
# .zip of the dataset (any depth) or the loose *_physio.tsv.gz files with
# their .json sidecars (and *_physioevents files). `...` goes to
# read_bids_eyetrack() (eye, trial_start).
read_bids_upload <- function(paths, names, ...) {
  is_zip <- grepl("\\.zip$", names, ignore.case = TRUE)
  root <- if (any(is_zip)) {
    exdir <- tempfile("gazer_bids_")
    for (z in paths[is_zip]) utils::unzip(z, exdir = exdir)
    exdir
  } else {
    dirname(stage_uploads(paths, names)[1])
  }
  files <- list.files(root, pattern = "_physio\\.tsv(\\.gz)?$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("__MACOSX", files)]
  if (!length(files)) {
    stop("No *_physio.tsv.gz files found. Upload a .zip of the BIDS dataset, or the ",
         "*_physio.tsv.gz files with their .json sidecars and *_physioevents files.", call. = FALSE)
  }
  no_json <- files[!file.exists(sub("\\.tsv(\\.gz)?$", ".json", files))]
  if (length(no_json)) {
    stop("Missing JSON sidecar for: ", paste(basename(no_json), collapse = ", "), call. = FALSE)
  }
  gazer::read_bids_eyetrack(files, ...)
}

# Rename user-selected columns to gazeR's canonical names.
# `cols` is a named list: subject, trial, time, pupil (required),
# blink, message (optional; "" or NULL means absent).
standardise_columns <- function(data, cols) {
  required <- c("subject", "trial", "time", "pupil")
  for (nm in required) {
    if (is.null(cols[[nm]]) || !nzchar(cols[[nm]])) {
      stop("Please choose a column for `", nm, "`.", call. = FALSE)
    }
  }
  mapping <- unlist(cols[vapply(cols, function(x) !is.null(x) && nzchar(x), logical(1))])
  missing <- setdiff(mapping, names(data))
  if (length(missing)) {
    stop("Column(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  # Drop any existing columns that would collide with the canonical names.
  clash <- setdiff(intersect(names(mapping), names(data)), mapping)
  data <- data[, setdiff(names(data), clash), drop = FALSE]

  out <- dplyr::rename(data, dplyr::all_of(mapping))
  out$time <- as.numeric(out$time)
  out$pupil <- suppressWarnings(as.numeric(out$pupil))
  if ("message" %in% names(out)) {
    out$message <- as.character(out$message)
    out$message[!is.na(out$message) & !nzchar(out$message)] <- NA
  }
  dplyr::arrange(out, subject, trial, time)
}

# Remove dilation-speed outliers within a trial (Kret & Sjak-Shie, 2019),
# using gazeR's speed_pupil() and calc_mad(). Only valid samples are used.
remove_speed_outliers <- function(pupil, time, n = 16) {
  idx <- which(!is.na(pupil))
  if (length(idx) < 3) return(pupil)
  speed <- gazer::speed_pupil(pupil[idx], time[idx])
  thresh <- gazer::calc_mad(speed, n = n)
  pupil[idx[speed > thresh]] <- NA
  pupil
}

# Sampling rate per participant: the file header for EyeLink files
# (`sample_rate`), otherwise estimated from the median time step.
subject_rates <- function(d) {
  has_col <- "sample_rate" %in% names(d)
  out <- lapply(split(d, as.character(d$subject)), function(s) {
    hz <- NA_real_
    if (has_col && any(!is.na(s$sample_rate))) {
      hz <- stats::median(s$sample_rate, na.rm = TRUE)
    } else {
      dt <- unlist(lapply(split(s$time, s$trial), function(t) diff(sort(t))), use.names = FALSE)
      dt <- stats::median(dt[dt > 0], na.rm = TRUE)
      if (is.finite(dt)) hz <- round(1000 / dt)
    }
    tibble::tibble(subject = as.character(s$subject[1]), hz = hz)
  })
  dplyr::bind_rows(out)
}

# Upsample participants recorded below `target_hz` with gazer::resample_gaze():
# new samples are interpolated from the recorded samples either side.
upsample_subjects <- function(d, rates, target_hz) {
  `%>%` <- dplyr::`%>%`
  low <- rates$subject[!is.na(rates$hz) & rates$hz < target_hz]
  if (!length(low)) return(d)
  up <- gazer::resample_gaze(d[as.character(d$subject) %in% low, ], target_hz,
                             hz = stats::setNames(rates$hz, rates$subject))
  up$upsampled <- up$resampled
  up$resampled <- NULL
  if ("tracker_time" %in% names(up)) up$tracker_time[up$upsampled] <- NA
  if ("sample_rate" %in% names(up)) up$sample_rate <- target_hz
  hi <- d[!(as.character(d$subject) %in% low), ]
  hi$upsampled <- rep(FALSE, nrow(hi))
  dplyr::bind_rows(hi, tibble::as_tibble(up)) %>% dplyr::arrange(subject, trial, time)
}

# TRUE where a sample's message is one of `events`. A sample can carry several
# messages joined with "; ".
has_event <- function(message, events) {
  out <- rep(FALSE, length(message))
  idx <- which(!is.na(message))
  if (length(idx)) {
    out[idx] <- vapply(strsplit(message[idx], "; ", fixed = TRUE),
                       function(m) any(m %in% events), logical(1))
  }
  out
}

# Time relative to the first of `events` in the trial (NA if none occurs).
# Lets one setting cover trials that start with different events, e.g.
# LONG_TRIAL_START or SHORT_TRIAL_START.
event_onset <- function(time, message, events) {
  hit <- which(has_event(message, events))
  if (!length(hit)) return(rep(NA_real_, length(time)))
  time - time[hit[1]]
}

event_label <- function(events) paste0("'", paste(events, collapse = "' or '"), "'")

# Error listing why every trial was excluded.
all_excluded_error <- function(quality, p) {
  tab <- table(quality$reason)
  why <- paste(sprintf("%s (%d)", names(tab), as.integer(tab)), collapse = ", ")
  hint <- if (any(grepl("^no ", names(tab)))) "Check the time-lock event(s), or turn time-locking off if the recording has no events."
          else if (any(grepl("missing", names(tab)))) sprintf("Trials or participants with more than %.0f%% missing are excluded; raise the threshold, reduce blink padding, or pick the better eye.", 100 * p$missingthresh)
          else "See the reasons above."
  stop("All trials were excluded: ", why, ". ", hint, call. = FALSE)
}

# Run the pipeline one participant at a time, to keep memory low (every
# step, including the exclusion rules, is per participant), then combine
# and bin everyone together. Returns the same structure as
# run_pipeline_core().
run_pupil_pipeline <- function(data, p, log = function(...) NULL) {
  subs <- unique(as.character(data$subject))
  if (length(subs) <= 1) return(run_pipeline_core(data, p, log))
  n_tr <- nrow(dplyr::distinct(data, subject, trial))
  log("Starting with ", n_tr, " trials from ", length(subs), " participants (processed one at a time)")
  rates <- subject_rates(data)
  if (!is.null(p$target_rate) && isTRUE(is.finite(p$target_rate))) {
    log("Resampling all participants to ", p$target_rate, " Hz")
  } else if (identical(p$rate_mode, "upsample") && !anyNA(rates$hz)) {
    p$target_hz <- max(rates$hz)
    low <- rates[rates$hz < p$target_hz, ]
    if (nrow(low)) log("Upsampling ", paste(sprintf("%s (%g Hz)", low$subject, low$hz), collapse = ", "),
                       " to ", p$target_hz, " Hz")
  } else if (dplyr::n_distinct(rates$hz) > 1) {
    log("Mixed sampling rates, each participant processed at their own rate")
  }
  res <- vector("list", length(subs))
  first_log <- character()
  for (i in seq_along(subs)) {
    msgs <- character()
    lg <- function(...) msgs <<- c(msgs, paste0(...))
    di <- data[as.character(data$subject) == subs[i], , drop = FALSE]
    res[[i]] <- tryCatch(run_pipeline_core(di, p, lg, stop_if_empty = FALSE),
                         error = function(e) stop(subs[i], ": ", conditionMessage(e), call. = FALSE))
    if (i == 1) first_log <- msgs
    rm(di)
    invisible(gc(verbose = FALSE))
  }
  # Settings from the first participant; counts are summarised below
  skip <- "^(Starting|Excluding|Done|Blinks detected|Pupil outside|Dropped|Low-pass: |Binning|Sampling rate|Upsampling|Downsampling|Mixed|Resampled)"
  for (m in first_log[!grepl(skip, first_log)]) log(m)
  quality <- dplyr::bind_rows(lapply(res, `[[`, "quality"))
  cleaned <- tibble::as_tibble(data.table::rbindlist(lapply(res, `[[`, "cleaned"), fill = TRUE))
  log("Blinks detected: ", sprintf("%.1f%%", 100 * mean(cleaned$blink_detected > 0, na.rm = TRUE)), " of samples")
  for (r in setdiff(unique(quality$reason), "kept")) {
    log("Excluding ", sum(quality$reason == r), " of ", nrow(quality), " trials (",
        sprintf("%.1f%%", 100 * sum(quality$reason == r) / nrow(quality)), "): ", r,
        if (grepl("missing", r)) paste0(" (> ", p$missingthresh * 100, "%)") else "")
  }
  d <- tibble::as_tibble(data.table::rbindlist(lapply(res, `[[`, "data"), fill = TRUE))
  if (!nrow(d)) all_excluded_error(quality, p)
  timevar <- Filter(Negate(is.null), lapply(res, `[[`, "timevar"))[[1]]
  aggvars <- unique(c(p$group_cols, "timebins"))
  log("Binning into ", p$bin_length, " ms bins by ", paste(aggvars, collapse = ", "))
  binned <- gazer::downsample_gaze(d, bin.length = p$bin_length, timevar = timevar,
                                   aggvars = aggvars, type = "pupil", pupil = "baselinecorrectedp")
  log("Done: ", nrow(dplyr::distinct(d, subject, trial)), " trials retained")
  list(data = d, binned = binned, quality = quality, cleaned = cleaned, timevar = timevar,
       params = p,
       rates = dplyr::bind_rows(lapply(res, `[[`, "rates")),
       rates_original = dplyr::bind_rows(lapply(res, `[[`, "rates_original")))
}

# Run the full pipeline on one participant (or a whole data set). Returns a list with the processed sample-level data,
# the binned data, a per-trial quality table and the parameters used.
run_pipeline_core <- function(data, p, log = function(...) NULL, stop_if_empty = TRUE) {
  `%>%` <- dplyr::`%>%`
  n_trials <- function(d) nrow(dplyr::distinct(d, subject, trial))

  if (isTRUE(p$timelock) && !length(p$timelock_event)) {
    stop("Time-locking is on but no event is selected. Pick the event(s) in step 4, or turn time-locking off ",
         "(e.g. for resting-state recordings with no events).", call. = FALSE)
  }
  if (identical(p$baseline, "event") && !length(p$baseline_event)) {
    stop("Event-based baseline is selected but no event is chosen. Pick the event(s) in step 4, ",
         "or use a fixed window or no baseline.", call. = FALSE)
  }

  log("Starting with ", n_trials(data), " trials from ",
      length(unique(data$subject)), " subject(s)")

  # 0. Behavioural exclusions (accuracy, RT) before any processing
  behav_excl <- behaviour_exclusions(data, p)
  for (r in unique(behav_excl$reason)) {
    n_r <- sum(behav_excl$reason == r)
    n_all <- nrow(dplyr::distinct(data, subject, trial))
    log("Excluding ", n_r, " of ", n_all, " trials (", sprintf("%.1f%%", 100 * n_r / n_all), "): ", r)
  }
  d <- dplyr::anti_join(data, behav_excl, by = c("subject", "trial"))
  if (nrow(d) == 0) stop("All trials were removed by the behavioural exclusions.", call. = FALSE)

  # 0b. Sampling rates: each participant's own, or upsample to the highest
  rates <- subject_rates(d)
  if (dplyr::n_distinct(rates$hz) == 1 && !is.null(p$hz_override) && isTRUE(is.finite(p$hz_override))) {
    rates$hz <- p$hz_override
  }
  if (anyNA(rates$hz)) stop("Could not determine the sampling rate for: ",
                            paste(rates$subject[is.na(rates$hz)], collapse = ", "), call. = FALSE)
  rates_original <- rates
  rate_summary <- function(r) paste(sprintf("%g Hz (%d)", sort(unique(r$hz)),
                                            as.integer(table(r$hz))), collapse = ", ")
  set_rate <- !is.null(p$target_rate) && isTRUE(is.finite(p$target_rate)) && p$target_rate > 0
  target <- if (set_rate) p$target_rate else if (!is.null(p$target_hz)) p$target_hz else max(rates$hz)
  if (set_rate) {
    hi <- rates[rates$hz > target, ]
    lo <- rates[rates$hz < target, ]
    if (nrow(hi)) {
      log("Downsampling ", paste(sprintf("%s (%g Hz)", hi$subject, hi$hz), collapse = ", "),
          " to ", target, " Hz (averaging consecutive samples)")
      d <- downsample_subjects(d, rates, target)
    }
    if (nrow(lo)) {
      if ((1000 / target) %% 1 != 0) stop("Upsampling needs a target rate with a whole-millisecond step (e.g. 1000, 500, 250 Hz).", call. = FALSE)
      log("Upsampling ", paste(sprintf("%s (%g Hz)", lo$subject, lo$hz), collapse = ", "), " to ", target, " Hz")
      d <- upsample_subjects(d, rates, target)
    }
    if (!nrow(hi) && !nrow(lo)) log("Sampling rate: ", target, " Hz (already at the target rate)")
    rates$hz <- target
  } else if (identical(p$rate_mode, "upsample") && any(rates$hz < target)) {
    {
      low <- rates[rates$hz < target, ]
      log("Upsampling ", paste(sprintf("%s (%g Hz)", low$subject, low$hz), collapse = ", "),
          " to ", target, " Hz")
      d <- upsample_subjects(d, rates, target)
      rates$hz <- target
    }
  } else if (dplyr::n_distinct(rates$hz) > 1) {
    log("Mixed sampling rates, each participant processed at their own rate: ", rate_summary(rates))
  } else {
    log("Sampling rate: ", rates$hz[1], " Hz")
  }
  d$.hz <- rates$hz[match(as.character(d$subject), rates$subject)]

  # 0c. Optional: resample onto a uniform time grid at each participant's own
  # rate (like eyeris::resample()). Dropped samples are inserted as missing,
  # so they are padded and interpolated like any other gap.
  if (isTRUE(p$resample)) {
    n_before <- nrow(d)
    d <- resample_uniform(d)
    log("Resampled onto a uniform grid: ", nrow(d) - n_before, " dropped sample(s) inserted as missing")
  }

  # 1. Mark blinks / invalid samples as missing
  d$pupil[!is.na(d$pupil) & d$pupil <= 0] <- NA
  # Physiologically plausible range (e.g. 2-9 mm for diameter in mm)
  lo <- if (is.null(p$pupil_min) || is.na(p$pupil_min)) -Inf else p$pupil_min
  hi <- if (is.null(p$pupil_max) || is.na(p$pupil_max)) Inf else p$pupil_max
  if (is.finite(lo) || is.finite(hi)) {
    out_range <- !is.na(d$pupil) & (d$pupil < lo | d$pupil > hi)
    log("Pupil outside ", lo, "-", hi, ": ", sum(out_range), " samples set to missing")
    d$pupil[out_range] <- NA
  }
  d$blink_detected <- 0L
  if (isTRUE(p$use_blink_col) && "blink" %in% names(d)) {
    d$blink_detected <- as.integer(!is.na(d$blink) & d$blink == 1)
  }
  if (identical(p$blink_method, "hershman")) {
    # Hershman et al. (2018): widen each missing-data run to the eyelid
    # descent/recovery. Combined with tracker blinks if those are used.
    log("Noise-based blink detection (Hershman et al., 2018; smoothing ",
        p$hershman_smooth, " ms, merge gap ", p$hershman_gap, " ms)")
    d <- d %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(blink_detected = pmax(
        blink_detected,
        as.integer(gazer::detect_blinks_noise(pupil, hz = .hz[1], smooth_ms = p$hershman_smooth,
                                              gap_ms = p$hershman_gap) > 0))) %>%
      dplyr::ungroup()
  }
  d$pupil[d$blink_detected == 1] <- NA
  r <- rle(paste(d$subject, d$trial, d$blink_detected))
  log("Blinks detected: ", sum(grepl(" 1$", r$values)), " (",
      sprintf("%.1f%%", 100 * mean(d$blink_detected)), " of samples)")

  # 2. Extend blinks
  log("Extending blinks: ", p$fillback, " ms back, ", p$fillforward, " ms forward")
  d <- d %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(extendpupil = gazer::extend_blinks(pupil, fillback = p$fillback,
                                                     fillforward = p$fillforward,
                                                     hz = .hz[1])) %>%
    dplyr::ungroup()

  # 3. Dilation-speed artifact rejection
  if (isTRUE(p$use_mad)) {
    log("Removing dilation-speed outliers (MAD constant = ", p$mad_n, ")")
    d <- d %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(extendpupil = remove_speed_outliers(extendpupil, time, n = p$mad_n)) %>%
      dplyr::ungroup()
  }

  # Keep the cleaned trace for every trial so excluded trials can still be
  # inspected in the trial viewer.
  cleaned <- d[, c("subject", "trial", "time", "extendpupil", "blink_detected")]

  # 4. Missing-data exclusion (before interpolation). When trials are
  # time-locked, missingness is measured inside the analysis window only, so
  # long pre-/post-trial periods (rest, ITI) don't count against a trial.
  in_window <- rep(TRUE, nrow(d))
  use_window <- isTRUE(p$timelock) && "message" %in% names(d)
  if (use_window) {
    tz <- d %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(.tz = event_onset(time, message, p$timelock_event)) %>%
      dplyr::pull(.tz)
    lo <- if (is.na(p$window[1])) -Inf else p$window[1]
    hi <- if (is.na(p$window[2])) Inf else p$window[2]
    # Trials without the event keep all samples here; they're dropped at time-locking
    in_window <- is.na(tz) | (tz >= lo & tz <= hi)
  }
  has_event <- if (use_window) !is.na(tz) else rep(TRUE, nrow(d))
  quality <- d %>%
    dplyr::mutate(.in = in_window, .ev = has_event) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(samples = sum(.in),
                     missing_n = sum(is.na(extendpupil[.in])),
                     prop_missing = missing_n / samples,
                     valid_total = sum(!is.na(extendpupil)),
                     has_event = any(.ev),
                     .groups = "drop") %>%
    dplyr::group_by(subject) %>%
    # Participant level: missing samples pooled across all their analysable
    # trials (as in gazer::count_missing_pupil()), not the mean of trial %s
    dplyr::mutate(subject_prop_missing = if (any(has_event))
                    sum(missing_n[has_event]) / sum(samples[has_event]) else NA_real_) %>%
    dplyr::ungroup() %>%
    # Excluded when MORE than the threshold is missing. A participant over the
    # threshold loses every trial, so that reason comes first.
    dplyr::mutate(reason = dplyr::case_when(
      !has_event ~ paste0("no ", event_label(p$timelock_event), " event"),
      subject_prop_missing > p$missingthresh ~ "subject missing data",
      valid_total < 2 ~ "no valid samples",
      prop_missing > p$missingthresh ~ "trial missing data",
      TRUE ~ "kept"),
      excluded = reason != "kept") %>%
    dplyr::select(-valid_total, -has_event, -missing_n)
  keep <- dplyr::filter(quality, !excluded) %>% dplyr::select(subject, trial)
  # Report behaviourally excluded trials in the same table
  if (nrow(behav_excl)) {
    quality <- dplyr::bind_rows(
      quality,
      dplyr::mutate(behav_excl, samples = NA_integer_, prop_missing = NA_real_,
                    subject_prop_missing = NA_real_, excluded = TRUE)) %>%
      dplyr::arrange(subject, trial)
  }
  for (r in setdiff(unique(quality$reason), c("kept", unique(behav_excl$reason)))) {
    log("Excluding ", sum(quality$reason == r), " of ", nrow(quality), " trials (",
        sprintf("%.1f%%", 100 * sum(quality$reason == r) / nrow(quality)), "): ", r,
        if (grepl("missing", r)) paste0(" (> ", p$missingthresh * 100, "%",
                                        if (use_window) " in the analysis window" else "", ")") else "")
  }
  d <- dplyr::semi_join(d, keep, by = c("subject", "trial"))
  if (nrow(d) == 0) {
    names(cleaned)[names(cleaned) == ".hz"] <- "processing_hz"
    if (!stop_if_empty) {
      return(list(data = d[0, ], binned = NULL, quality = quality, cleaned = cleaned, timevar = NULL,
                  params = p, rates = rates, rates_original = rates_original))
    }
    all_excluded_error(quality, p)
  }

  # 5. Interpolate + smooth. One smoothing filter: moving average or Hanning
  # (via smooth_interpolate_pupil), or low-pass / none. For the latter,
  # interpolate only (a 1-sample moving average leaves values unchanged);
  # the low-pass filter runs in step 5b.
  interp_only <- p$filter %in% c("lowpass", "none")
  if (interp_only) {
    log("Interpolating (", p$interp_type, "), no moving-average/Hanning smoothing")
  } else {
    log("Interpolating (", p$interp_type, ") and smoothing (", p$filter, ", ",
        p$step_first, " first)")
  }
  # hz only sets the cubic max-gap length, so run once per sampling rate
  d <- dplyr::bind_rows(lapply(split(d, d$.hz), function(di) {
    suppressWarnings(suppressMessages(
      gazer::smooth_interpolate_pupil(di, pupil = "pupil", extendpupil = "extendpupil",
                                      extendblinks = TRUE,
                                      step.first = if (interp_only) "interp" else p$step_first,
                                      filter = if (interp_only) "moving" else p$filter,
                                      maxgap = p$maxgap, type = p$interp_type, hz = di$.hz[1],
                                      n = if (interp_only) 1 else p$ma_n)
    ))
  }))
  d <- dplyr::arrange(dplyr::as_tibble(d), subject, trial, time)

  # 5b. Optional low-pass filter: zero-phase Butterworth (gazer::lowpass_filter),
  # per trial. It can't handle NAs, so it runs after interpolation.
  if (isTRUE(p$use_lowpass)) {
    nyquist <- min(d$.hz) / 2
    if (is.null(p$lp_cutoff) || !is.finite(p$lp_cutoff) || p$lp_cutoff <= 0 || p$lp_cutoff >= nyquist) {
      stop("Low-pass cutoff must be above 0 and below half the sampling rate (", nyquist, " Hz).",
           call. = FALSE)
    }
    log("Low-pass filtering at ", p$lp_cutoff, " Hz (Butterworth, order ", p$lp_order, ", zero-phase)")
    skipped <- 0L
    lp <- function(x, hz) {
      if (anyNA(x) || length(x) <= 3 * (p$lp_order + 1)) {
        skipped <<- skipped + 1L
        return(x)
      }
      suppressMessages(gazer::lowpass_filter(x, order = p$lp_order, filter_to_use = p$lp_cutoff,
                                             samplerate = hz))
    }
    d <- d %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(pup_interp = lp(pup_interp, .hz[1])) %>%
      dplyr::ungroup()
    if (skipped) log("Low-pass: ", skipped, " trial(s) left unfiltered (missing values or too short)")
  }

  # 6. Baseline correction
  value_col <- "pup_interp"
  if (p$baseline == "event") {
    if (!"message" %in% names(d)) stop("Event-based baseline needs a message column.", call. = FALSE)
    log("Baseline: ", if (is.null(p$baseline_stat)) "median" else p$baseline_stat, " of ", p$baseline_dur, " ms before ", event_label(p$baseline_event),
        " (", p$baseline_method, ")")
    # Mark the first selected event in each trial, so gazeR's function sees
    # exactly one baseline event per trial, then restore the messages
    d <- d %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(.orig_message = message,
                    message = {
                      h <- has_event(message, p$baseline_event)
                      ifelse(h & cumsum(h) == 1, ".BASELINE_EVENT", NA_character_)
                    }) %>%
      dplyr::ungroup()
    d <- suppressMessages(
      gazer::baseline_correction_pupil_msg(d, pupil_colname = "pup_interp",
                                           baseline_dur = p$baseline_dur,
                                           event = ".BASELINE_EVENT",
                                           baseline_method = p$baseline_method,
                                           baseline_stat = if (is.null(p$baseline_stat)) "median" else p$baseline_stat)
    )
    d$message <- d$.orig_message
    d$.orig_message <- NULL
    d$event_offset_time <- NULL
    value_col <- "baselinecorrectedp"
  } else if (p$baseline == "window") {
    log("Baseline: ", if (is.null(p$baseline_stat)) "median" else p$baseline_stat, " of ", p$baseline_window[1], "-", p$baseline_window[2],
        " ms (", p$baseline_method, ")")
    d <- suppressMessages(
      gazer::baseline_correction_pupil(d, pupil_colname = "pup_interp",
                                       baseline_window = p$baseline_window,
                                       baseline_method = p$baseline_method,
                                           baseline_stat = if (is.null(p$baseline_stat)) "median" else p$baseline_stat)
    )
    value_col <- "baselinecorrectedp"
  } else {
    d$baselinecorrectedp <- d$pup_interp
  }

  # 7. Time-lock to an event
  timevar <- "time"
  if (isTRUE(p$timelock) && "message" %in% names(d)) {
    log("Time-locking to ", event_label(p$timelock_event))
    d <- d %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(time_zero = event_onset(time, message, p$timelock_event)) %>%
      dplyr::ungroup()
    n_before <- n_trials(d)
    d <- dplyr::filter(d, !is.na(time_zero))
    if (n_trials(d) < n_before) {
      log("Dropped ", n_before - n_trials(d), " trial(s) without ", event_label(p$timelock_event))
    }
    if (!is.na(p$window[1])) d <- dplyr::filter(d, time_zero >= p$window[1])
    if (!is.na(p$window[2])) d <- dplyr::filter(d, time_zero <= p$window[2])
    timevar <- "time_zero"
  }

  # 8. Downsample into time bins
  aggvars <- unique(c(p$group_cols, "timebins"))
  log("Binning into ", p$bin_length, " ms bins by ", paste(aggvars, collapse = ", "))
  binned <- gazer::downsample_gaze(d, bin.length = p$bin_length, timevar = timevar,
                                   aggvars = aggvars, type = "pupil",
                                   pupil = "baselinecorrectedp")

  log("Done: ", n_trials(d), " trials retained")
  # Name the per-participant rate clearly; drop intermediate columns
  names(d)[names(d) == ".hz"] <- "processing_hz"
  d[intersect(c("interp", "pup", "index", "movingavgpup"), names(d))] <- NULL
  names(cleaned)[names(cleaned) == ".hz"] <- "processing_hz"
  list(data = d, binned = binned, quality = quality, cleaned = cleaned,
       timevar = timevar, params = p, rates = rates, rates_original = rates_original)
}

# ---- Summaries for plotting ---------------------------------------------

# Bin the processed samples and average them to one value per trial, then
# per subject. Returns trial-level and unit-level tables, where the unit is
# the subject (or the trial, when there is only one subject).
bin_for_plot <- function(d, timevar, bin_length, conds = character()) {
  `%>%` <- dplyr::`%>%`
  conds <- intersect(conds, names(d))
  d$timebins <- round(d[[timevar]] / bin_length) * bin_length
  trial_level <- d %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(unique(c("subject", "trial", conds, "timebins"))))) %>%
    dplyr::summarise(value = mean(baselinecorrectedp, na.rm = TRUE), .groups = "drop")
  multi_subject <- dplyr::n_distinct(trial_level$subject) > 1
  unit_level <- if (multi_subject) {
    trial_level %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("subject", conds, "timebins")))) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE), n_trials = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(unit = as.character(subject))
  } else {
    dplyr::mutate(trial_level, unit = paste(subject, trial, sep = "_"), n_trials = 1L)
  }
  label <- if (length(conds)) do.call(paste, c(unit_level[conds], sep = " · ")) else "All"
  unit_level$condition <- label
  tl_label <- if (length(conds)) do.call(paste, c(trial_level[conds], sep = " · ")) else "All"
  trial_level$condition <- tl_label
  list(trial = trial_level, unit = unit_level,
       unit_name = if (multi_subject) "subject" else "trial")
}

# Mean and error per condition and time bin.
#   se_type  "within"  Cousineau (2005) normalisation with Morey (2008)
#                      correction: remove each unit's mean across conditions,
#                      add back the grand mean, scale by sqrt(J / (J - 1)).
#            "between" SD across units / sqrt(n).
#   interval "se" (+/- 1 SE), "ci" (95% CI, t-based) or "none".
summarise_timecourse <- function(unit_level, se_type = c("within", "between"),
                                 interval = c("se", "ci", "none")) {
  `%>%` <- dplyr::`%>%`
  se_type <- match.arg(se_type)
  interval <- match.arg(interval)
  J <- dplyr::n_distinct(unit_level$condition)
  use_within <- se_type == "within" && J > 1
  u <- unit_level
  if (use_within) {
    u <- u %>%
      dplyr::group_by(timebins) %>%
      dplyr::mutate(grand = mean(value, na.rm = TRUE)) %>%
      dplyr::group_by(unit, timebins) %>%
      dplyr::mutate(norm = value - mean(value, na.rm = TRUE) + grand) %>%
      dplyr::ungroup()
  } else {
    u$norm <- u$value
  }
  morey <- if (use_within) sqrt(J / (J - 1)) else 1
  u %>%
    dplyr::group_by(condition, timebins) %>%
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      n = sum(!is.na(value)),
      se = stats::sd(norm, na.rm = TRUE) / sqrt(n) * morey,
      .groups = "drop") %>%
    dplyr::mutate(
      se = ifelse(is.finite(se), se, 0),
      half = switch(interval,
                    se = se,
                    ci = se * stats::qt(0.975, pmax(n - 1, 1)),
                    none = 0),
      lower = mean - half, upper = mean + half,
      error_type = if (interval == "none") "none"
                   else paste(if (use_within) "within" else "between", interval))
}

# Mean pupil within a time window: one value per unit x condition (for
# rainclouds) and one per trial (for relating pupil to behaviour).
window_means <- function(binned, window) {
  `%>%` <- dplyr::`%>%`
  lo <- if (is.na(window[1])) -Inf else window[1]
  hi <- if (is.na(window[2])) Inf else window[2]
  per_unit <- binned$unit %>%
    dplyr::filter(timebins >= lo, timebins <= hi) %>%
    dplyr::group_by(unit, condition) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  per_trial <- binned$trial %>%
    dplyr::filter(timebins >= lo, timebins <= hi) %>%
    dplyr::group_by(subject, trial, condition) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  list(unit = per_unit, trial = per_trial)
}

# Join behavioural data (one row per subject/trial) onto the samples.
join_behaviour <- function(d, behav, keys) {
  keys <- intersect(keys, intersect(names(d), names(behav)))
  if (!length(keys)) stop("The behavioural file has no columns in common with the eye data to join on.", call. = FALSE)
  for (k in keys) {
    d[[k]] <- as.character(d[[k]])
    behav[[k]] <- as.character(behav[[k]])
  }
  behav <- dplyr::distinct(behav, dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
  # Don't overwrite eye-data columns; suffix clashing behavioural ones
  clash <- setdiff(intersect(names(behav), names(d)), keys)
  if (length(clash)) names(behav)[match(clash, names(behav))] <- paste0(clash, "_behav")
  out <- dplyr::left_join(d, behav, by = keys)
  if ("trial" %in% keys) out$trial <- utils::type.convert(out$trial, as.is = TRUE)
  out
}

# Trials to drop on behavioural grounds. Returns (subject, trial, reason).
#   acc_col / acc_correct  keep only trials where acc_col equals acc_correct
#   rt_col                 RT column (ms)
#   rt_min / rt_max        absolute cutoffs (NA = none)
#   rt_sd                  drop RTs beyond +/- rt_sd SDs of the subject's mean,
#                          computed on trials that pass the checks above
behaviour_exclusions <- function(data, p) {
  `%>%` <- dplyr::`%>%`
  none <- tibble::tibble(subject = data$subject[0], trial = data$trial[0], reason = character())
  has <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  use_acc <- has(p$acc_col) && p$acc_col %in% names(data) && !is.null(p$acc_correct)
  use_rt <- has(p$rt_col) && p$rt_col %in% names(data)
  if (!use_acc && !use_rt) return(none)

  cols <- unique(c("subject", "trial", if (use_acc) p$acc_col, if (use_rt) p$rt_col))
  tr <- dplyr::distinct(data[, cols]) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  tr$reason <- NA_character_

  if (use_acc) {
    ok <- as.character(tr[[p$acc_col]]) %in% as.character(p$acc_correct)
    tr$reason[!ok] <- "incorrect"
  }
  if (use_rt) {
    rt <- suppressWarnings(as.numeric(tr[[p$rt_col]]))
    pending <- is.na(tr$reason)
    tr$reason[pending & is.na(rt)] <- "RT missing"
    if (!is.null(p$rt_min) && !is.na(p$rt_min)) {
      tr$reason[is.na(tr$reason) & rt < p$rt_min] <- paste0("RT < ", p$rt_min, " ms")
    }
    if (!is.null(p$rt_max) && !is.na(p$rt_max)) {
      tr$reason[is.na(tr$reason) & rt > p$rt_max] <- paste0("RT > ", p$rt_max, " ms")
    }
    if (!is.null(p$rt_sd) && !is.na(p$rt_sd)) {
      tr$.rt <- rt
      tr <- tr %>%
        dplyr::group_by(subject) %>%
        dplyr::mutate(.m = mean(.rt[is.na(reason)], na.rm = TRUE),
                      .s = stats::sd(.rt[is.na(reason)], na.rm = TRUE)) %>%
        dplyr::ungroup()
      far <- is.na(tr$reason) & is.finite(tr$.s) & abs(tr$.rt - tr$.m) > p$rt_sd * tr$.s
      tr$reason[far] <- paste0("RT beyond ", p$rt_sd, " SD (within subject)")
      tr <- dplyr::select(tr, -".rt", -".m", -".s")
    }
  }
  tr[!is.na(tr$reason), c("subject", "trial", "reason")]
}

# ---- Data dictionary ----------------------------------------------------

# Descriptions of the columns the app writes. Columns not listed here came
# from the user's own files (trial variables, behavioural or tracker columns).
column_descriptions <- c(
  subject = "Participant label (file name without extension for EyeLink files, or your subject column).",
  trial = "Trial number. For EyeLink files, one per TRIALID message.",
  time = "Time in ms from the start of recording in the trial (EyeLink: the !MODE RECORD message).",
  pupil = "Pupil size in tracker units (EyeLink: area or diameter). NA when missing. Binocular 'Average'/'Both': the mean of both eyes, only where both are valid.",
  x = "Horizontal gaze position (screen pixels; Tobii: 0-1 display area). Binocular 'Average'/'Both': mean of both eyes.",
  y = "Vertical gaze position (screen pixels; Tobii: 0-1 display area). Binocular 'Average'/'Both': mean of both eyes.",
  blink = "1 during a blink flagged by the tracker, 0 otherwise. Binocular 'Average'/'Both': 1 if either eye is in a blink.",
  message = "Event message(s) on this sample (first sample at or after the message; Experiment Builder offsets applied). NA otherwise.",
  tracker_time = "The eye tracker's own clock (ms). NA on samples added by upsampling.",
  pupil_left = "Left-eye pupil size (binocular 'Both' only).",
  pupil_right = "Right-eye pupil size (binocular 'Both' only).",
  x_left = "Left-eye horizontal gaze position (binocular 'Both' only).",
  y_left = "Left-eye vertical gaze position (binocular 'Both' only).",
  x_right = "Right-eye horizontal gaze position (binocular 'Both' only).",
  y_right = "Right-eye vertical gaze position (binocular 'Both' only).",
  blink_left = "1 while the left eye is in a blink (binocular 'Both' only).",
  blink_right = "1 while the right eye is in a blink (binocular 'Both' only).",
  sample_rate = "Sampling rate in Hz as recorded (EyeLink file header). After upsampling, the new rate.",
  eyes_recorded = "Eyes recorded in the file: left, right or both (EyeLink file header).",
  source_file = "Name of the file the row came from.",
  sr_trial_time = "SR Data Viewer sample reports: TIMESTAMP - IP_START_TIME (ms).",
  event_label = "Tobii (Titta): text after the onset prefix of the trial's onset message.",
  upsampled = "TRUE for samples added by upsampling (values interpolated from the real samples either side), FALSE for recorded samples.",
  processing_hz = "Sampling rate (Hz) used to process this participant.",
  blink_detected = "1 if the sample was removed as a blink (tracker blinks and/or noise-based detection), before padding.",
  extendpupil = "Pupil after removing blinks (with padding), out-of-range values and dilation-speed outliers. NA = removed or missing.",
  pup = "Pupil with blink samples set to NA (intermediate step of smooth_interpolate_pupil()).",
  interp = "Pupil after interpolation, before smoothing (when interpolating first).",
  index = "Sample index used for cubic-spline interpolation.",
  pup_interp = "Pupil after interpolation and smoothing (and low-pass filtering, if used).",
  baseline = "Baseline pupil size for the trial (median or mean of the baseline period, as chosen). For divisive correction with a fixed window: per participant.",
  resampled = "TRUE for samples inserted by resampling onto a uniform grid (dropped samples; treated as missing).",
  baselinecorrectedp = "Baseline-corrected pupil: pup_interp - baseline (subtractive) or (pup_interp - baseline) / baseline (divisive).",
  time_zero = "Time in ms relative to the time-lock event (0 = event onset).",
  timebins = "Start of the time bin (ms); samples are averaged within each bin.",
  aggbaseline = "Mean baseline-corrected pupil in the time bin.",
  samples = "Number of samples in the trial (inside the analysis window when time-locked).",
  prop_missing = "Proportion of samples missing after blink and artifact removal (inside the analysis window when time-locked).",
  subject_prop_missing = "Proportion of the participant's samples missing, pooled across all their analysable trials (inside the analysis window when time-locked).",
  reason = "Why the trial was excluded, or 'kept'.",
  excluded = "TRUE if the trial was excluded from processing.",
  condition = "Plot condition: the selected condition columns joined with ' · '.",
  mean = "Mean across units (subjects, or trials for a single subject).",
  n = "Number of units contributing to the mean.",
  se = "Standard error (within-subject Cousineau-Morey or between-subject, as chosen).",
  half = "Half-width of the plotted band (SE or 95% CI).",
  lower = "Lower edge of the plotted band.",
  upper = "Upper edge of the plotted band.",
  error_type = "Which error the band shows.",
  unit = "Unit of analysis (subject, or subject_trial for a single subject).",
  value = "Mean baseline-corrected pupil in the chosen time window."
)

# One row per column of each dataset: dataset, column, type, description.
data_dictionary <- function(datasets) {
  rows <- lapply(names(datasets), function(nm) {
    d <- datasets[[nm]]
    if (is.null(d)) return(NULL)
    cols <- names(d)
    desc <- unname(column_descriptions[cols])
    desc[is.na(desc)] <- "From your data file (e.g. an Experiment Builder trial variable, a behavioural column, or a tracker export column)."
    tibble::tibble(dataset = nm, column = cols,
                   type = vapply(d, function(v) class(v)[1], character(1)),
                   description = desc)
  })
  dplyr::bind_rows(rows)
}

# ---- Missing data and blinks by eye -------------------------------------

# Per participant and eye: % of samples missing, % in a blink, % missing
# outside blinks (tracker loss), and trials that are mostly missing. With
# both eyes loaded, left, right and their average are reported separately;
# otherwise the single pupil column is labelled `single_label`.
eye_summary <- function(d, single_label = "pupil") {
  `%>%` <- dplyr::`%>%`
  eyes <- if (all(c("pupil_left", "pupil_right") %in% names(d))) {
    list(left = c("pupil_left", "blink_left"), right = c("pupil_right", "blink_right"),
         `average (both eyes)` = c("pupil", "blink"))
  } else {
    stats::setNames(list(c("pupil", "blink")), single_label)
  }
  rows <- lapply(names(eyes), function(eye) {
    pcol <- eyes[[eye]][1]
    bcol <- eyes[[eye]][2]
    p <- suppressWarnings(as.numeric(d[[pcol]]))
    tibble::tibble(
      subject = d$subject, trial = d$trial,
      miss = is.na(p) | p <= 0,
      blink = if (bcol %in% names(d)) !is.na(d[[bcol]]) & d[[bcol]] > 0 else NA
    ) %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::summarise(n = dplyr::n(), miss_n = sum(miss), blink_n = sum(blink),
                       loss_n = sum(miss & !blink), .groups = "drop") %>%
      dplyr::group_by(subject) %>%
      dplyr::summarise(eye = eye,
                       samples = sum(n),
                       missing = sum(miss_n) / sum(n),
                       blink = sum(blink_n) / sum(n),
                       tracker_loss = sum(loss_n) / sum(n),
                       trials = dplyr::n(),
                       trials_mostly_missing = sum(miss_n / n > 0.5),
                       .groups = "drop")
  })
  dplyr::bind_rows(rows) %>% dplyr::arrange(subject, eye)
}

# ---- Methods paragraph --------------------------------------------------

and_join <- function(x) if (length(x) <= 1) x else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])

# A methods paragraph and reference list describing one pipeline run, for
# users to adapt in a paper. `meta` holds what the pipeline doesn't know:
# tracker (label), eye ("left", "right", "average", "both", or NA),
# pupil_col (mapped pupil column), and package versions.
methods_paragraph <- function(res, meta) {
  p <- res$params
  q <- res$quality
  num <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
  pct <- function(x, d = 1) paste0(num(100 * x, d), "%")
  ev <- function(e) paste(e, collapse = " or ")
  refs <- c(gazer = "Geller, J., Winn, M. B., Mahr, T., & Mirman, D. (2020). GazeR: A package for processing gaze position and pupil size data. *Behavior Research Methods, 52*, 2232–2255.")
  s <- character()

  # Software and recording
  s <- c(s, sprintf("Pupil data were pre-processed in R using gazeR (version %s; Geller et al., 2020) through the shiny gazeR app (version %s).",
                    meta$gazer_version, meta$app_version))
  r0 <- res$rates_original
  hz <- sort(unique(r0$hz))
  rec <- if (length(hz) == 1) sprintf("Data were recorded at %s Hz", num(hz)) else
    sprintf("Data were recorded at %s", paste(vapply(hz, function(h) sprintf("%s Hz (n = %d)", num(h), sum(r0$hz == h)), ""), collapse = " and "))
  if (!is.null(meta$tracker) && nzchar(meta$tracker)) rec <- sub("^Data were recorded", paste("Data were recorded with", meta$tracker), rec)
  if (!is.null(p$target_rate) && isTRUE(is.finite(p$target_rate))) {
    tr <- p$target_rate
    parts <- character()
    if (any(hz > tr)) parts <- c(parts, sprintf("recordings above %s Hz were downsampled by averaging consecutive samples (a bin was treated as missing if any of its samples was missing)", num(tr)))
    if (any(hz < tr)) parts <- c(parts, sprintf("recordings below %s Hz were upsampled, with new samples linearly interpolated between adjacent recorded samples", num(tr)))
    if (length(parts)) rec <- paste0(rec, sprintf("; all data were resampled to %s Hz: ", num(tr)), paste(parts, collapse = ", and "))
  } else if (length(hz) > 1 && identical(p$rate_mode, "upsample")) {
    rec <- paste0(rec, sprintf("; participants recorded below %s Hz were upsampled to %s Hz, with new samples linearly interpolated between adjacent recorded samples", num(max(hz)), num(max(hz))))
  } else if (length(hz) > 1) {
    rec <- paste0(rec, "; each participant was processed at their own sampling rate")
  }
  s <- c(s, paste0(rec, "."))
  if (isTRUE(p$resample)) {
    s <- c(s, "Each trial was resampled onto a uniform time grid at its sampling rate, with any dropped samples treated as missing.")
  }
  eye <- meta$eye
  pcol <- if (is.null(meta$pupil_col)) "pupil" else meta$pupil_col
  if (!isTRUE(meta$binocular)) {
    if (!is.null(meta$mono_eye) && meta$mono_eye %in% c("left", "right")) {
      s <- c(s, sprintf("The %s eye was recorded.", meta$mono_eye))
    }
  } else if (identical(eye, "left") || identical(pcol, "pupil_left")) {
    s <- c(s, "Pupil size from the left eye was analysed.")
  } else if (identical(eye, "right") || identical(pcol, "pupil_right")) {
    s <- c(s, "Pupil size from the right eye was analysed.")
  } else if (eye %in% c("average", "both")) {
    s <- c(s, "Pupil size was averaged across both eyes for samples where both eyes were valid; a sample was treated as part of a blink if either eye was blinking.")
  }

  # Cleaning
  rng <- c(p$pupil_min, p$pupil_max)
  if (any(is.finite(rng))) {
    s <- c(s, if (all(is.finite(rng))) sprintf("Pupil values outside %s–%s were removed.", rng[1], rng[2])
              else if (is.finite(rng[1])) sprintf("Pupil values below %s were removed.", rng[1])
              else sprintf("Pupil values above %s were removed.", rng[2]))
  }
  if (identical(p$blink_method, "hershman")) {
    refs <- c(refs, hershman = "Hershman, R., Henik, A., & Cohen, N. (2018). A novel blink detection method based on pupillometry noise. *Behavior Research Methods, 50*, 107–114.")
    b <- sprintf("Blinks were identified with a noise-based algorithm (Hershman et al., 2018; %s-ms smoothing window, blinks less than %s ms apart merged)%s",
                 p$hershman_smooth, p$hershman_gap, if (isTRUE(p$use_blink_col)) " in addition to the eye tracker's blink events" else "")
  } else {
    b <- "Blinks were identified from the eye tracker's blink events"
  }
  pad <- if (p$fillback > 0 || p$fillforward > 0)
    sprintf(", and each period of missing data was extended by %s ms before and %s ms after", p$fillback, p$fillforward) else ""
  s <- c(s, paste0(b, pad, "; these samples were set to missing."))
  if (isTRUE(p$use_mad)) {
    refs <- c(refs, kret = "Kret, M. E., & Sjak-Shie, E. E. (2019). Preprocessing pupil size data: Guidelines and code. *Behavior Research Methods, 51*, 1336–1342.")
    s <- c(s, sprintf("Samples with implausibly fast changes in pupil size (dilation speed more than %s median absolute deviations above the median, per trial) were also removed (Kret & Sjak-Shie, 2019).", p$mad_n))
  }

  # Behavioural exclusions
  beh <- q[!q$reason %in% c("kept", "trial missing data", "subject missing data", "no valid samples") &
             !grepl("^no '", q$reason), ]
  if (nrow(beh)) {
    parts <- vapply(split(beh, beh$reason), function(b) sprintf("%s (%d trials, %s)", b$reason[1], nrow(b), pct(nrow(b) / nrow(q))), "")
    s <- c(s, sprintf("Before pre-processing, %s of %s trials (%s) were excluded on behavioural grounds: %s.",
                      num(nrow(beh)), num(nrow(q)), pct(nrow(beh) / nrow(q)), paste(parts, collapse = "; ")))
  }

  # Missing data exclusion
  thr <- pct(p$missingthresh, 0)
  win <- isTRUE(p$timelock)
  missing_tr <- q[q$reason %in% c("trial missing data", "no valid samples"), ]
  subj_out <- unique(q$subject[q$reason == "subject missing data"])
  noevent <- q[grepl("^no '", q$reason), ]
  s <- c(s, sprintf("Participants with more than %s of samples missing across all their trials, and trials with more than %s missing, were excluded%s.",
                    thr, thr, if (win) ", counting only samples within the analysis window" else ""))

  # Interpolation and smoothing
  interp <- if (p$interp_type == "cubic") "cubic-spline" else "linear"
  sm <- switch(p$filter,
    moving = sprintf("smoothed with a %s-sample moving average", p$ma_n),
    Hann = "smoothed with an 11-point Hanning window",
    lowpass = sprintf("low-pass filtered at %s Hz (order-%s Butterworth filter applied forwards and backwards, i.e. zero phase)", p$lp_cutoff, p$lp_order),
    none = NULL)
  s <- c(s, if (is.null(sm)) sprintf("Missing samples were reconstructed by %s interpolation.", interp)
            else if (p$filter %in% c("moving", "Hann") && identical(p$step_first, "smooth"))
              sprintf("The pupil trace was %s, and missing samples were then reconstructed by %s interpolation.", sm, interp)
            else sprintf("Missing samples were reconstructed by %s interpolation, and the pupil trace was then %s.", interp, sm))

  # Baseline and time-locking
  if (p$baseline != "none") {
    refs <- c(refs, mathot = "Mathôt, S., Fabius, J., Van Heusden, E., & Van der Stigchel, S. (2018). Safe and sensible preprocessing and baseline correction of pupil-size data. *Behavior Research Methods, 50*, 94–106.")
    how <- if (p$baseline_method == "div") "dividing by" else "subtracting"
    period <- if (p$baseline == "event") sprintf("the %s ms before %s", p$baseline_dur, ev(p$baseline_event))
              else sprintf("%s–%s ms", p$baseline_window[1], p$baseline_window[2])
    stat <- if (is.null(p$baseline_stat)) "median" else p$baseline_stat
    cites <- "Mathôt et al., 2018"
    if (stat == "mean") {
      refs <- c(refs, mathot22 = "Mathôt, S., & Vilotijević, A. (2022). Methods in cognitive pupillometry: Design, preprocessing, and statistical analysis. *bioRxiv*. https://doi.org/10.1101/2022.02.23.481628")
      cites <- "Mathôt et al., 2018; Mathôt & Vilotijević, 2022"
    }
    s <- c(s, sprintf("Pupil size was baseline-corrected per trial by %s the %s pupil size in %s (%s)%s.",
                      how, stat, period, cites, if (p$baseline_method == "div") ", giving proportional change from baseline" else ""))
  }
  if (win) {
    w <- p$window
    s <- c(s, sprintf("Trials were time-locked to %s%s%s.", ev(p$timelock_event),
                      if (all(is.finite(w))) sprintf(" and analysed from %s to %s ms", w[1], w[2]) else "",
                      if (nrow(noevent)) sprintf(" (%d trials without this event were excluded)", nrow(noevent)) else ""))
  }
  grp <- setdiff(p$group_cols, "timebins")
  s <- c(s, sprintf("Data were then averaged into %s-ms time bins%s.", p$bin_length,
                    if (length(grp)) paste0(" per ", and_join(grp)) else ""))

  # Outcome
  n_sub <- length(unique(q$subject))
  kept <- q[q$reason == "kept", ]
  n_tr <- nrow(q)
  per_sub <- table(kept$subject)
  s <- c(s, sprintf("In total, %d of %d participants and %s of %s trials (%s) were retained.",
                    length(unique(kept$subject)), n_sub, num(nrow(kept)), num(n_tr), pct(nrow(kept) / n_tr)))
  excl <- character()
  if (length(subj_out)) excl <- c(excl, sprintf("%d participant%s (all of their trials)", length(subj_out),
                                                if (length(subj_out) != 1) "s" else ""))
  if (nrow(missing_tr)) excl <- c(excl, sprintf("%s%s trial%s", if (length(subj_out)) "a further " else "",
                                                num(nrow(missing_tr)), if (nrow(missing_tr) != 1) "s" else ""))
  if (length(excl)) s <- c(s, sprintf("Missing data led to the exclusion of %s.", paste(excl, collapse = " and ")))
  if (length(per_sub) > 1) {
    s <- c(s, sprintf("Participants contributed on average %s trials (SD = %s, range %d–%d), and on average %s of samples per retained trial were interpolated.",
                      num(mean(per_sub), 1), num(stats::sd(per_sub), 1), min(per_sub), max(per_sub),
                      pct(mean(kept$prop_missing, na.rm = TRUE))))
  } else if (length(per_sub) == 1) {
    s <- c(s, sprintf("The retained participant contributed %d trials, and on average %s of samples per retained trial were interpolated.",
                      as.integer(per_sub), pct(mean(kept$prop_missing, na.rm = TRUE))))
  }

  refs <- c(refs, app = sprintf("Geller, J. (%s). *shiny gazeR* (Version %s) [Computer software]. https://github.com/jgeller112/shiny-gazeR",
                                format(Sys.Date(), "%Y"), meta$app_version))
  list(text = paste(s, collapse = " "), references = unname(refs[order(names(refs) != "gazer", unname(refs))]))
}


# ---- Downsampling to a target rate -----------------------------------------

# Participants recorded above `target_hz` are downsampled with
# gazer::resample_gaze() (averaging consecutive samples).
downsample_subjects <- function(d, rates, target_hz) {
  hi <- rates$subject[!is.na(rates$hz) & rates$hz > target_hz]
  if (!length(hi)) return(d)
  down <- gazer::resample_gaze(d[as.character(d$subject) %in% hi, ], target_hz,
                               hz = stats::setNames(rates$hz, rates$subject))
  down$resampled <- NULL
  if ("sample_rate" %in% names(down)) down$sample_rate <- target_hz
  res <- dplyr::bind_rows(d[!(as.character(d$subject) %in% hi), ], tibble::as_tibble(down))
  res[order(res$subject, res$trial, res$time), names(d)[names(d) %in% names(res)]]
}

# ---- Resampling onto a uniform grid ---------------------------------------

# Put every trial on an evenly spaced grid at the participant's own rate
# (.hz) with gazer::resample_gaze(), inserting dropped samples as missing.
resample_uniform <- function(d) {
  parts <- lapply(split(d, as.character(d$subject)), function(sd) {
    hz <- sd$.hz[1]
    if (!is.finite(hz) || hz <= 0 || (1000 / hz) %% 1 != 0) return(sd)  # needs whole-ms steps
    u <- gazer::resample_gaze(sd, target_hz = hz, hz = hz)
    if ("tracker_time" %in% names(u)) u$tracker_time[u$resampled] <- NA
    u
  })
  tibble::as_tibble(dplyr::bind_rows(parts))
}

# ---- Per-participant QC report ----------------------------------------------

png_base64 <- function(plot, width = 9, height = 4) {
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, plot, width = width, height = height, dpi = 110, bg = "white")
  on.exit(unlink(f))
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size)))
}

html_table <- function(df) {
  esc <- htmltools::htmlEscape
  head <- paste0("<tr>", paste0("<th>", esc(names(df)), "</th>", collapse = ""), "</tr>")
  rows <- apply(df, 1, function(r) paste0("<tr>", paste0("<td>", esc(as.character(r)), "</td>", collapse = ""), "</tr>"))
  paste0("<table>", head, paste(rows, collapse = ""), "</table>")
}

# A self-contained HTML report (images embedded) for one participant:
# summary, exclusions, signal at each step, every trial, and parameters.
qc_report_html <- function(res, raw, subject, meta) {
  `%>%` <- dplyr::`%>%`
  sid <- as.character(subject)
  q <- res$quality[as.character(res$quality$subject) == sid, ]
  rawp <- raw[as.character(raw$subject) == sid, ]
  rawp$pupil[!is.na(rawp$pupil) & rawp$pupil <= 0] <- NA
  proc <- res$data[as.character(res$data$subject) == sid, ]
  clean <- res$cleaned[as.character(res$cleaned$subject) == sid, ]
  rate <- res$rates_original$hz[res$rates_original$subject == sid]
  pct <- function(x) sprintf("%.1f%%", 100 * x)
  kept <- q[q$reason == "kept", ]

  summary <- data.frame(
    Measure = c("Sampling rate", "Trials", "Trials kept", "Missing pupil (raw, whole recording)",
                "Blink samples (after detection, before padding)", "Missing after cleaning (analysed trials)"),
    Value = c(if (length(rate)) paste(rate[1], "Hz") else "?", nrow(q),
              sprintf("%d (%s)", nrow(kept), pct(nrow(kept) / max(1, nrow(q)))),
              pct(mean(is.na(rawp$pupil))), pct(mean(clean$blink_detected > 0, na.rm = TRUE)),
              if (nrow(kept)) pct(mean(kept$prop_missing, na.rm = TRUE)) else "–"))
  reasons <- as.data.frame(table(Reason = q$reason), responseName = "Trials")

  plots <- character()
  # Trial-level missing data
  qq <- q
  qq$status <- ifelse(qq$reason == "kept", "kept", "excluded")
  p1 <- ggplot2::ggplot(qq[!is.na(qq$prop_missing), ], ggplot2::aes(factor(trial), prop_missing, fill = status)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = res$params$missingthresh, linetype = "dashed", colour = "#c92a2a") +
    ggplot2::scale_fill_manual(values = c(kept = "#3b5bdb", excluded = "#f08c00")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(x = "Trial", y = "Missing (after cleaning)", fill = NULL,
                  title = "Missing data per trial", subtitle = "Dashed line: exclusion threshold") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6, angle = 90))
  plots <- c(plots, png_base64(p1, 9, 3.2))

  if (nrow(proc)) {
    tv <- if (!is.null(res$timevar) && res$timevar %in% names(proc)) res$timevar else "time"
    step <- 50
    mk <- function(df, col, label, t) {
      df$bin <- round(df[[t]] / step) * step
      df %>% dplyr::group_by(bin) %>% dplyr::summarise(value = mean(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(step = label)
    }
    keys <- unique(paste(proc$subject, proc$trial))
    rawk <- rawp[paste(rawp$subject, rawp$trial) %in% keys, ]
    cleank <- clean[paste(clean$subject, clean$trial) %in% keys, ]
    shift <- proc %>% dplyr::distinct(trial, .keep_all = TRUE)
    if (tv == "time_zero") {
      off <- proc %>% dplyr::group_by(trial) %>% dplyr::summarise(off = dplyr::first(time - time_zero), .groups = "drop")
      rawk$time_zero <- rawk$time - off$off[match(rawk$trial, off$trial)]
      cleank$time_zero <- cleank$time - off$off[match(cleank$trial, off$trial)]
      rng <- range(proc$time_zero)
      rawk <- rawk[rawk$time_zero >= rng[1] & rawk$time_zero <= rng[2], ]
      cleank <- cleank[cleank$time_zero >= rng[1] & cleank$time_zero <= rng[2], ]
    }
    steps <- dplyr::bind_rows(mk(rawk, "pupil", "1. Raw", tv), mk(cleank, "extendpupil", "2. Blinks/artifacts removed", tv),
                              mk(proc, "pup_interp", "3. Interpolated + smoothed", tv))
    p2 <- ggplot2::ggplot(steps, ggplot2::aes(bin, value, colour = step)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_colour_manual(values = c("#adb5bd", "#3b5bdb", "#e8590c")) +
      ggplot2::labs(x = "Time (ms)", y = "Pupil size", colour = NULL, title = "Average across kept trials at each step") +
      ggplot2::theme_minimal(base_size = 10) + ggplot2::theme(legend.position = "bottom")
    plots <- c(plots, png_base64(p2, 9, 3.6))
    p3 <- ggplot2::ggplot(mk(proc, "baselinecorrectedp", "baseline-corrected", tv), ggplot2::aes(bin, value)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey70") + ggplot2::geom_line(colour = "#2f9e44", linewidth = 0.9) +
      ggplot2::labs(x = "Time (ms)", y = "Change from baseline", title = "Baseline-corrected pupil (average of kept trials)") +
      ggplot2::theme_minimal(base_size = 10)
    plots <- c(plots, png_base64(p3, 9, 3))
    # Every trial: raw vs processed
    tr <- head(sort(unique(rawp$trial)), 60)
    rr <- rawp[rawp$trial %in% tr, c("trial", "time", "pupil")]
    pp <- proc[proc$trial %in% tr, c("trial", "time", "pup_interp")]
    p4 <- ggplot2::ggplot() +
      ggplot2::geom_line(data = rr, ggplot2::aes(time, pupil), colour = "#adb5bd", linewidth = 0.3) +
      ggplot2::geom_line(data = pp, ggplot2::aes(time, pup_interp), colour = "#e8590c", linewidth = 0.4) +
      ggplot2::facet_wrap(~ trial, scales = "free", ncol = 6) +
      ggplot2::labs(x = "Time (ms)", y = "Pupil", title = "Each trial: raw (grey) and processed (orange; kept trials only)") +
      ggplot2::theme_minimal(base_size = 7) + ggplot2::theme(axis.text = ggplot2::element_blank())
    plots <- c(plots, png_base64(p4, 10, max(3, ceiling(length(tr) / 6) * 1.1)))
  }

  prm <- res$params
  prm <- prm[!vapply(prm, is.null, logical(1))]
  params <- data.frame(Setting = names(prm), Value = vapply(prm, function(v) paste(v, collapse = ", "), ""))
  esc <- htmltools::htmlEscape
  paste0('<!doctype html><html><head><meta charset="utf-8"><title>QC report: ', esc(sid), '</title>
<style>body{font-family:system-ui,-apple-system,Segoe UI,sans-serif;max-width:1000px;margin:2rem auto;padding:0 1rem;color:#212529}
h1{font-size:1.6rem}h2{font-size:1.15rem;margin-top:2rem;border-bottom:1px solid #dee2e6;padding-bottom:.3rem}
table{border-collapse:collapse;font-size:.85rem;margin:.5rem 0}td,th{border:1px solid #dee2e6;padding:.3rem .6rem;text-align:left}
th{background:#f1f3f5}img{max-width:100%;margin:.5rem 0}.muted{color:#868e96;font-size:.85rem}</style></head><body>',
    '<h1>shiny gazeR QC report: ', esc(sid), '</h1>',
    '<p class="muted">Generated ', format(Sys.time(), "%Y-%m-%d %H:%M"), ' with shiny gazeR ', esc(meta$app_version),
    ' and gazeR ', esc(meta$gazer_version), '.</p>',
    '<h2>Summary</h2>', html_table(summary),
    '<h2>Trials by outcome</h2>', html_table(reasons),
    '<h2>Plots</h2>', paste0('<img src="', plots, '">', collapse = ""),
    '<h2>Parameters</h2>', html_table(params),
    '</body></html>')
}

# ---- BIDS-like export --------------------------------------------------------

bids_label <- function(x) gsub("[^A-Za-z0-9]", "", sub("\\.(edf|asc)(\\.gz)?$", "", as.character(x), ignore.case = TRUE))

# Write a BIDS-like derivatives folder (eyeris-style) under `root`.
write_bids <- function(root, res, raw, meta, task = "task") {
  `%>%` <- dplyr::`%>%`
  task <- bids_label(task)
  if (!nzchar(task)) task <- "task"
  deriv <- file.path(root, "derivatives", "shinygazer")
  dir.create(deriv, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(Name = "shiny gazeR pupil pre-processing", BIDSVersion = "1.9.0",
                            DatasetType = "derivative",
                            GeneratedBy = list(list(Name = "shinygazer", Version = meta$app_version,
                                                    CodeURL = "https://github.com/jgeller112/shiny-gazeR"),
                                               list(Name = "gazer", Version = meta$gazer_version))),
                       file.path(deriv, "dataset_description.json"), auto_unbox = TRUE, pretty = TRUE)
  prm <- res$params
  prm$maxgap <- if (is.infinite(prm$maxgap)) NULL else prm$maxgap
  jsonlite::write_json(list(parameters = prm, created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
                       file.path(deriv, "metadata.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  dict <- data_dictionary(list(processed = res$data))
  for (sid in unique(as.character(res$quality$subject))) {
    lab <- bids_label(sid)
    d <- file.path(deriv, paste0("sub-", lab), "eye")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    stem <- file.path(d, paste0("sub-", lab, "_task-", task))
    proc <- res$data[as.character(res$data$subject) == sid, ]
    rw <- raw[as.character(raw$subject) == sid, ]
    cl <- res$cleaned[as.character(res$cleaned$subject) == sid, ]
    if (nrow(proc)) data.table::fwrite(proc, paste0(stem, "_desc-preproc_timeseries.csv"))
    if ("message" %in% names(rw)) {
      ev <- rw[!is.na(rw$message), intersect(c("subject", "trial", "time", "tracker_time", "message"), names(rw))]
      data.table::fwrite(ev, paste0(stem, "_events.csv"))
    }
    if (nrow(cl)) {
      bl <- cl %>% dplyr::arrange(trial, time) %>% dplyr::group_by(trial) %>%
        dplyr::mutate(run = cumsum(c(1, diff(blink_detected > 0) != 0))) %>%
        dplyr::filter(blink_detected > 0) %>% dplyr::group_by(trial, run) %>%
        dplyr::summarise(start = min(time), end = max(time), .groups = "drop") %>%
        dplyr::mutate(duration_ms = end - start) %>% dplyr::select(-run)
      data.table::fwrite(bl, paste0(stem, "_blinks.csv"))
    }
    if (!is.null(res$binned) && "subject" %in% names(res$binned)) {
      b <- res$binned[as.character(res$binned$subject) == sid, ]
      if (nrow(b)) data.table::fwrite(b, paste0(stem, "_desc-binned_pupil.csv"))
    }
    data.table::fwrite(res$quality[as.character(res$quality$subject) == sid, ], paste0(stem, "_desc-quality_trials.csv"))
    rate <- res$rates$hz[res$rates$subject == sid]
    cols <- stats::setNames(as.list(dict$description), dict$column)
    jsonlite::write_json(list(SamplingFrequency = if (length(rate)) rate[1] else NULL,
                              RecordedSamplingFrequency = res$rates_original$hz[res$rates_original$subject == sid][1],
                              TaskName = task, Columns = cols),
                         paste0(stem, "_desc-preproc_timeseries.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  invisible(deriv)
}

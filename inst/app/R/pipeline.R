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

# Fill rows inserted by upsampling from the real samples either side. If
# either neighbour is missing (blink, tracker loss), the row stays NA.
fill_inserted <- function(v, time, inserted) {
  ins <- which(inserted)
  orig <- which(!inserted)
  ok <- !inserted & !is.na(v)
  if (!length(ins) || sum(ok) < 2) return(v)
  k <- findInterval(ins, orig)
  prev <- ifelse(k >= 1, orig[pmax(k, 1)], NA_integer_)
  nxt <- ifelse(k < length(orig), orig[pmin(k + 1, length(orig))], NA_integer_)
  est <- stats::approx(time[ok], v[ok], xout = time[ins])$y
  est[is.na(prev) | is.na(nxt) | is.na(v[prev]) | is.na(v[nxt])] <- NA
  v[ins] <- est
  v
}

# Upsample participants recorded below `target_hz` with gazer::upsample_gaze()
# (gazeR >= 0.2.6: original rows unchanged, messages not copied). The
# inserted rows' pupil/gaze values come back empty; fill them from the real
# samples either side.
upsample_subjects <- function(d, rates, target_hz) {
  `%>%` <- dplyr::`%>%`
  low <- rates$subject[!is.na(rates$hz) & rates$hz < target_hz]
  if (!length(low)) return(d)
  value_cols <- intersect(c("pupil", "x", "y", "pupil_left", "pupil_right",
                            "x_left", "y_left", "x_right", "y_right"), names(d))
  pupil_cols <- intersect(c("pupil", "pupil_left", "pupil_right"), value_cols)
  lo <- d[as.character(d$subject) %in% low, ]
  up <- suppressMessages(gazer::upsample_gaze(lo, pupil_cols = pupil_cols,
                                              gaze_cols = setdiff(value_cols, pupil_cols),
                                              target_hz = target_hz, upsample_pupil = TRUE,
                                              event_cols = "message"))
  up$upsampled <- up$up_sampled
  up$up_sampled <- NULL
  if ("tracker_time" %in% names(up)) up$tracker_time[up$upsampled] <- NA
  if ("sample_rate" %in% names(up)) up$sample_rate <- target_hz
  up <- up %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(value_cols), ~ fill_inserted(.x, time, upsampled))) %>%
    dplyr::ungroup()
  hi <- d[!(as.character(d$subject) %in% low), ]
  hi$upsampled <- rep(FALSE, nrow(hi))
  dplyr::bind_rows(hi, up) %>% dplyr::arrange(subject, trial, time)
}

# Run the full pipeline. Returns a list with the processed sample-level data,
# the binned data, a per-trial quality table and the parameters used.
run_pupil_pipeline <- function(data, p, log = function(...) NULL) {
  `%>%` <- dplyr::`%>%`
  n_trials <- function(d) nrow(dplyr::distinct(d, subject, trial))

  log("Starting with ", n_trials(data), " trials from ",
      length(unique(data$subject)), " subject(s)")

  # 0. Behavioural exclusions (accuracy, RT) before any processing
  behav_excl <- behaviour_exclusions(data, p)
  for (r in unique(behav_excl$reason)) {
    log("Excluding ", sum(behav_excl$reason == r), " trials: ", r)
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
  rate_summary <- function(r) paste(sprintf("%g Hz (%d)", sort(unique(r$hz)),
                                            as.integer(table(r$hz))), collapse = ", ")
  if (dplyr::n_distinct(rates$hz) > 1) {
    if (identical(p$rate_mode, "upsample")) {
      target <- max(rates$hz)
      low <- rates[rates$hz < target, ]
      log("Upsampling ", paste(sprintf("%s (%g Hz)", low$subject, low$hz), collapse = ", "),
          " to ", target, " Hz")
      d <- upsample_subjects(d, rates, target)
      rates$hz <- target
    } else {
      log("Mixed sampling rates, each participant processed at their own rate: ", rate_summary(rates))
    }
  } else {
    log("Sampling rate: ", rates$hz[1], " Hz")
  }
  d$.hz <- rates$hz[match(as.character(d$subject), rates$subject)]

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
      dplyr::mutate(.tz = gazer::onset_pupil(time, message, event = p$timelock_event)) %>%
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
                     prop_missing = mean(is.na(extendpupil[.in])),
                     valid_total = sum(!is.na(extendpupil)),
                     has_event = any(.ev),
                     .groups = "drop") %>%
    dplyr::group_by(subject) %>%
    # Subject average only over trials that will be analysed
    dplyr::mutate(subject_prop_missing = if (any(has_event)) mean(prop_missing[has_event]) else NA_real_) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(reason = dplyr::case_when(
      !has_event ~ paste0("no '", p$timelock_event, "' event"),
      valid_total < 2 ~ "no valid samples",
      prop_missing >= p$missingthresh ~ "trial missing data",
      subject_prop_missing >= p$missingthresh ~ "subject missing data",
      TRUE ~ "kept"),
      excluded = reason != "kept") %>%
    dplyr::select(-valid_total, -has_event)
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
    log("Excluding ", sum(quality$reason == r), " of ", nrow(quality), " trials: ", r,
        if (grepl("missing", r)) paste0(" (>= ", p$missingthresh * 100, "%",
                                        if (use_window) " in the analysis window" else "", ")") else "")
  }
  d <- dplyr::semi_join(d, keep, by = c("subject", "trial"))
  if (nrow(d) == 0) {
    stop(sprintf(paste0("All trials were excluded (median %.0f%% missing per trial, threshold %.0f%%). ",
                        "Raise the exclusion threshold, check the time-lock event, or pick a single eye ",
                        "instead of the average."),
                 100 * stats::median(quality$prop_missing), 100 * p$missingthresh), call. = FALSE)
  }

  # 5. Interpolate + smooth
  log("Interpolating (", p$interp_type, ") and smoothing (", p$filter, ", ",
      p$step_first, " first)")
  # hz only sets the cubic max-gap length, so run once per sampling rate
  d <- dplyr::bind_rows(lapply(split(d, d$.hz), function(di) {
    suppressWarnings(suppressMessages(
      gazer::smooth_interpolate_pupil(di, pupil = "pupil", extendpupil = "extendpupil",
                                      extendblinks = TRUE, step.first = p$step_first,
                                      filter = p$filter, maxgap = p$maxgap,
                                      type = p$interp_type, hz = di$.hz[1], n = p$ma_n)
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
    log("Baseline: median of ", p$baseline_dur, " ms before '", p$baseline_event,
        "' (", p$baseline_method, ")")
    d <- suppressMessages(
      gazer::baseline_correction_pupil_msg(d, pupil_colname = "pup_interp",
                                           baseline_dur = p$baseline_dur,
                                           event = p$baseline_event,
                                           baseline_method = p$baseline_method)
    )
    value_col <- "baselinecorrectedp"
  } else if (p$baseline == "window") {
    log("Baseline: median of ", p$baseline_window[1], "-", p$baseline_window[2],
        " ms (", p$baseline_method, ")")
    d <- suppressMessages(
      gazer::baseline_correction_pupil(d, pupil_colname = "pup_interp",
                                       baseline_window = p$baseline_window,
                                       baseline_method = p$baseline_method)
    )
    value_col <- "baselinecorrectedp"
  } else {
    d$baselinecorrectedp <- d$pup_interp
  }

  # 7. Time-lock to an event
  timevar <- "time"
  if (isTRUE(p$timelock) && "message" %in% names(d)) {
    log("Time-locking to '", p$timelock_event, "'")
    d <- d %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(time_zero = gazer::onset_pupil(time, message, event = p$timelock_event)) %>%
      dplyr::ungroup()
    n_before <- n_trials(d)
    d <- dplyr::filter(d, !is.na(time_zero))
    if (n_trials(d) < n_before) {
      log("Dropped ", n_before - n_trials(d), " trial(s) without '", p$timelock_event, "'")
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
  list(data = d, binned = binned, quality = quality, cleaned = cleaned,
       timevar = timevar, params = p, rates = rates)
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

# shinygazer 0.3.7

* Data tab, binocular (Both): the eye table shows the left-right pupil ICC
  (agreement) and repeated-measures correlation (rmcorr) per participant,
  over whole trials with trial means removed (gazer >= 0.2.14). The
  Quality tab's trial table and the Trial quality download give both per
  trial.
* Data tab: a "Resampled only" download gives the combined data with just
  the step 1 resampling applied (Resample all to, upsampling mixed rates,
  or Fill dropped samples) and no other pre-processing, without running
  the pipeline. It uses the same code as a full run.
* CSV downloads write missing values as `NA` (and `n/a` in the BIDS-like
  export) instead of empty cells, so tracker loss (which the old
  `parse_asc()` wrote as `1e+08` for gaze) is clearly marked missing.
* Reads the gzipped example data shipped with gazer >= 0.2.13 (falls back to
  the older .xls), and gzipped (.gz) delimited uploads, decompressing with
  base R.

# shinygazer 0.3.6

* Results: multilevel cluster-based permutation test (Maris & Oostenveld,
  2007). A mixed model per time bin (subject means or trials, optional item
  intercept), labels shuffled within subjects, cluster p-values; figure with
  the condition time courses and clusters underneath, cluster table,
  methods sentence and downloads. Needs lme4 (now suggested and bundled
  with the browser version).
* Eye = Both now processes the left and right eye separately, for comparing
  the eyes: identical settings, the same blink mask (either eye), optionally
  the same missing samples, and only trials that pass for both eyes. Results
  have an `eye` column; plots, the trial viewer, QC reports and the methods
  paragraph follow it.
* Binocular "Average": pupil, x and y are the monocular mean (both eyes'
  mean, or the one valid eye), as EyeLink reports the average eye, for all
  trackers (gazer >= 0.2.12). Help, data dictionary and methods paragraph
  updated.
* Data and Quality tabs: with Eye = Both, the per-eye table shows the
  left-right pupil correlation per participant (gazer::pupil_cor()), in red
  below .9.
* Tobii Pro Lab input: choose **Tobii Pro Lab (.tsv)** and upload data
  exports, read with gazer::read_tobii_prolab() (needs gazer >= 0.2.12).
  Trials follow the presented stimulus (calibration dropped) or start at
  listed events. Blinks come from Pro Lab's Blink classification (eye
  openness, where measured) plus pupil-loss gaps; a 2–9 mm range is set.
* BIDS eye-tracking input: choose **BIDS (eye-tracking)** and upload a .zip
  of a BEP020 dataset (or the *_physio.tsv.gz files with their sidecars and
  events). Read with `gazer::read_bids_eyetrack()` (needs gazer >= 0.2.11).
  Each sub/ses/task/run is a subject; optional trial-start messages split
  runs into trials. Works for EyeLink, Tobii and Pupil Labs conversions:
  pupil in mm sets the 2–9 mm range, and data without blink events switch
  to noise-based blink detection.
* Sampling rate: the box in step 1 is now "Recorded rate (Hz)" and warns
  when changed from the file's rate, since it relabels the data rather than
  resampling them. The "Resample all to" target starts at the highest
  recorded rate instead of 500 Hz.
* After a run, the Export tab selects Processed samples, the Data tab notes
  that it shows the data as loaded (and the rate of the last run), and the
  "Done" message gives the processing rate and sample count.
* Re-scaling (step 4, after baseline correction): z-score the
  baseline-corrected pupil per participant (across their kept trials) with
  gazer::zscore_pupil() (needs gazer >= 0.2.12). The z-scored `pup_z` is then
  what is binned, plotted and summarised; it is in the methods paragraph,
  data dictionary and study files.

# shinygazer 0.3.5

* QC report: self-contained HTML per participant (or a zip of all) with
  summary, exclusions, the signal at each step, every trial and parameters.
* BIDS-like export: derivatives/shinygazer/sub-*/eye/ with processed time
  series, events, blinks, binned data, trial quality and JSON metadata.
* Study file: save all settings to .json and restore them later.
* Optional resampling onto a uniform time grid at each participant's own
  rate (dropped samples inserted as missing).

* Baseline value: choose the median (default) or mean of the baseline period
  (step 4). The methods paragraph names the statistic and, for the mean,
  cites Mathôt & Vilotijević (2022). Requires gazeR >= 0.2.10.

# shinygazer 0.3.4

* Behavioural exclusions report percentages: the step 6 preview shows the
  overall and per-reason percentage of trials excluded and the range across
  participants; log lines and the methods paragraph include percentages.

# shinygazer 0.3.3

* Participants are processed one at a time and then combined, so memory use
  peaks at about one participant's data instead of the whole study (helps
  the browser version). Results are identical to processing everyone
  together.
* Intermediate columns (`interp`, `pup`, `index`) are no longer kept in the
  processed data.
* Requires gazeR >= 0.2.9, where smoothing is done within each trial (it
  previously blended the first and last samples of a trial with the
  neighbouring trial).

# shinygazer 0.3.2

* Exclusion threshold slider is now a percentage (0-100%) instead of a
  proportion (0-1), matching every table in the app.
* Files with no event messages (e.g. resting state): time-locking and
  event-based baseline are switched off automatically, with a message,
  instead of every trial being excluded as "no event".
* Clear errors when time-locking or an event baseline has no event
  selected, and when all trials are excluded (lists the actual reasons).
* Trial viewer: blink and missing-data bands have a minimum visible width
  on long trials (e.g. minutes of resting state).
* Browser version: gazeR and shinygazer are bundled from r-universe; lme4
  is no longer bundled (gazeR >= 0.2.8 makes it optional).

# shinygazer 0.3.1

* Methods paragraph (Export tab): ready-to-adapt text describing the run,
  with gazeR citation, every pre-processing setting, exclusions and the final
  sample, plus references for the methods used. Copy or download as Markdown.

* Available from r-universe (https://jgeller112.r-universe.dev). The browser
  version installs gazeR and shinygazer from there instead of building its
  own WebAssembly packages.

* Requires gazeR >= 0.2.7, where saccades is optional; shiny gazeR no longer
  depends on saccades at all (install and browser build).

# shinygazer 0.3.0

* Each participant's own sampling rate is used: read from the file header for
  EyeLink `.asc` files, otherwise estimated from the time stamps. If rates
  differ (e.g. some participants recorded at 500 Hz), step 1 lists them and
  offers to upsample everyone to the highest rate with
  `gazer::upsample_gaze()` (recommended), or to process each participant at
  their own rate. Requires gazeR >= 0.2.6.
* Download raw combined, processed or binned data for all participants or
  one, as a single CSV or one CSV per participant (Export and Data tabs).

# shinygazer 0.2.0

* The app can run in the browser with shinylive. A GitHub Actions workflow
  builds WebAssembly versions of saccades, gazeR and shinygazer and deploys
  the app to GitHub Pages.

* Requires gazeR >= 0.2.5, which now includes `read_asc()` and fixes to
  `smooth_interpolate_pupil()` (cubic + interpolate-first + Hanning) and
  `lowpass_filter()` (edge artifacts). The copies and workarounds bundled in
  shinygazer 0.1.0 have been removed; `shinygazer::read_asc()` is now a
  re-export of `gazer::read_asc()`.
* Optional low-pass filter (zero-phase Butterworth, `gazer::lowpass_filter()`)
  in step 3, applied per trial after interpolation.

# shinygazer 0.1.0

* First release: a Shiny app for gazeR's pupil pre-processing pipeline
  (`run_app()`), with EyeLink, SR Data Viewer, Tobii (Titta) and Pupil Labs
  Neon input; tracker or noise-based blink detection; behavioural joins and
  exclusions; a trial viewer with missing-data shading; quality reports; and
  time-course, raincloud and pupil-behaviour plots with within- or
  between-subject error.
* `read_asc()`: fast single-file reader for EyeLink `.asc` files (bundled
  until it is part of gazeR).
* Works around a bug in gazeR <= 0.2.4 where `smooth_interpolate_pupil()`
  with cubic interpolation, interpolate-first and Hanning smoothing filtered
  the un-interpolated signal.

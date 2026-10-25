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

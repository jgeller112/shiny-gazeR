# shinygazer 0.2.0

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

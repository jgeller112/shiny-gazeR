#' Launch the shiny gazeR app
#'
#' Opens an interactive app that runs gazeR's pupil pre-processing pipeline
#' without writing code:
#'
#' 1. **Load** EyeLink `.asc` files ([gazer::read_asc()]), SR Research Data Viewer
#'    sample reports, Tobii (Titta) exports ([gazer::merge_tobii_titta()]),
#'    Pupil Labs Neon exports ([gazer::parse_pl()]) or any long CSV, and map
#'    the columns.
#' 2. **Clean**: plausible-range limits, tracker or noise-based blink
#'    detection ([gazer::detect_blinks_noise()]), blink padding
#'    ([gazer::extend_blinks()]) and dilation-speed outliers
#'    ([gazer::speed_pupil()], [gazer::calc_mad()]).
#' 3. **Interpolate and smooth** ([gazer::smooth_interpolate_pupil()]),
#'    optionally low-pass filter ([gazer::lowpass_filter()]).
#' 4. **Baseline-correct and time-lock**
#'    ([gazer::baseline_correction_pupil_msg()], [gazer::onset_pupil()]).
#' 5. **Bin** ([gazer::downsample_gaze()]), optionally join behavioural data
#'    and exclude trials by accuracy or RT.
#'
#' The app shows raw and processed traces for every trial, missing data and
#' exclusions, and time courses (within- or between-subject error),
#' rainclouds and pupil-behaviour plots. Processed samples, binned data,
#' trial quality and a JSON file of every setting can be downloaded.
#'
#' @param ... Arguments passed to [shiny::runApp()], such as `port` or
#'   `launch.browser`.
#' @return Called for its side effect of running the app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("app", package = "shinygazer")
  if (!nzchar(app_dir)) {
    stop("Could not find the app directory. Try re-installing shinygazer.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}

# shiny gazeR <img src="man/figures/logo.svg" align="right" height="139" alt="shiny gazeR hex logo" />

Pupil pre-processing for eye-tracking data, without writing code.

**shiny gazeR** is an interactive [Shiny](https://shiny.posit.co/) app built on the
[gazeR](https://github.com/dmirman/gazer) package. Load recordings, clean them step by step,
inspect every trial, and export analysis-ready data. Every setting is saved with your results,
so the same pipeline can be re-run in R with gazeR.

## Installation

```r
# install.packages("remotes")
remotes::install_github("jgeller112/shiny-gazeR")
```

This also installs gazeR (and its `saccades` dependency) from GitHub.

## Usage

```r
shinygazer::run_app()
```

Choose **Try the example data** on the home page to look around before loading your own files.

## What it does

| Step | Options | gazeR function(s) |
|---|---|---|
| **Load** | EyeLink `.asc`, SR Data Viewer sample reports, Tobii (Titta), Pupil Labs Neon (`.zip` of the Pupil Cloud export), or any long CSV; monocular or binocular (left, right, average or both) | `read_asc()`*, `merge_tobii_titta()`, `parse_pl()` |
| **Behaviour** | Join a per-trial table (RT, accuracy, …); exclude incorrect trials, RT cutoffs, ± k SD within subject | — |
| **Blinks & artifacts** | Plausible pupil range; tracker or noise-based blinks (Hershman et al., 2018); blink padding; dilation-speed outliers | `detect_blinks_noise()`, `extend_blinks()`, `speed_pupil()`, `calc_mad()` |
| **Missing data** | Exclude trials and subjects above a threshold, measured inside the analysis window | — |
| **Interpolate & smooth** | Linear or cubic; moving average or Hanning; either order; optional zero-phase Butterworth low-pass | `smooth_interpolate_pupil()` |
| **Baseline & time-lock** | Subtractive or divisive; from an event or a fixed window; align trials to any message | `baseline_correction_pupil_msg()`, `baseline_correction_pupil()`, `onset_pupil()` |
| **Bin** | Any combination of subject, trial and condition columns | `downsample_gaze()` |

\* `read_asc()` is bundled with shiny gazeR until it is part of a gazeR release.

**Inspect and report**

- **Trial viewer:** the raw signal (both eyes and gaze), or raw vs. cleaned vs. interpolated vs. baseline-corrected, with blinks and missing samples shaded and event markers.
- **Quality:** missing data per trial and subject, plus every excluded trial with its reason.
- **Results:**
  - time courses with **within-subject (Cousineau–Morey) or between-subject** SE or 95% CI, computed across subjects after averaging trials within subject;
  - rainclouds of the mean pupil in a window;
  - pupil vs behaviour scatter plots, with optional RT markers.
- **Export:** processed samples, binned data, trial quality, plotted values, and a JSON file with every parameter and package version.

## Reading `.asc` files

`read_asc()` turns an EyeLink `.asc` file into gazeR's long format. It runs in seconds and needs no
SR Research software; convert `.edf` files with SR Research's free `edf2asc` first.

```r
d <- shinygazer::read_asc("data/sub01.asc", eye = "left")
attr(d, "asc_info")$hz
```

## Citation

If you use shiny gazeR, please cite gazeR:

> Geller, J., Winn, M. B., Mahr, T., & Mirman, D. (2020). GazeR: A package for processing gaze
> position and pupil size data. *Behavior Research Methods, 52*, 2232–2255.

## License

GPL (>= 3), matching gazeR.

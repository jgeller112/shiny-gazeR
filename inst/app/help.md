## How this app works

The app runs gazeR's pupil pre-processing pipeline on sample-level eye-tracking data. Each step in the sidebar corresponds to one or more gazeR functions, so anything you do here can be reproduced in an R script. The **Parameters (.json)** download records every setting you used, along with the shiny gazeR and gazeR versions.

Steps run in this order (functions are from gazeR unless noted):

| Step | What happens | gazeR function(s) |
|---|---|---|
| Load | Read and stack the files (`shinygazer::read_asc()` for `.asc`); rename columns to `subject`, `trial`, `time`, `pupil`, `blink`, `message` | `data.table::fread()`, as in `make_gazer()` |
| Behaviour | Trials are excluded by accuracy and RT (optional) before any processing | — |
| Blinks | Pupil values ≤ 0 and blink samples become `NA` (tracker blinks, or noise-based detection); gaps are padded back and forward | `detect_blinks_noise()`, `extend_blinks()` |
| Artifacts | Samples with implausibly fast dilation are removed | `speed_pupil()`, `calc_mad()` |
| Exclusion | Trials and subjects with too much missing data are dropped | (same logic as `count_missing_pupil()`) |
| Interpolate & smooth | Gaps are filled, then the trace is smoothed, or the reverse | `smooth_interpolate_pupil()` |
| Baseline | Median pupil size in the baseline period is subtracted or divided out | `baseline_correction_pupil_msg()`, `baseline_correction_pupil()` |
| Time-lock | Time 0 is set to an event's onset, and the trial is trimmed to a window | `onset_pupil()` |
| Bin | Samples are averaged into time bins within your grouping columns | `downsample_gaze(type = "pupil")` |

## Input data

The app needs **one row per sample** with at least a subject, trial, time (ms) and pupil column. Choose your **Eye tracker** in step 1 so the files are read correctly:

| Eye tracker | What to upload | Read with |
|---|---|---|
| EyeLink | `.asc` files, one per subject | `read_asc()` |
| SR Data Viewer | Sample reports (`.xls`/`.txt`) | `data.table::fread()` |
| Tobii (Titta) | Gaze CSVs plus matching message CSVs (same file names) | `merge_tobii_titta()` |
| Pupil Labs Neon | A `.zip` of the Pupil Cloud Time Series export (one folder per recording), or `gaze.csv`, `3d_eye_states.csv` and `events.csv` from one recording | `parse_pl()` |
| gazeR / other CSV | Any long table, e.g. output from `parse_asc()` | `data.table::fread()` |

For Pupil Labs, choose whether **any message** or only **specific messages** start a trial. With specific messages (e.g. `trial-started-light, start-dark`), in-between events such as `blank` stay inside the trial. Samples before the first trial start are dropped. Neon reports pupil diameter in mm and keeps values during blinks, so keep *Also treat tracker blink samples as missing* ticked. The plausible range defaults to 2–9 mm. Tick *Use surface (AOI) data* to add Marker Mapper surface columns. See the [Pupil Labs walkthrough](https://www.drjasongeller.com/blog/posts/2025-10-31-pupillabs/).

For Tobii, invalid samples (validity flag false) are set to missing, and gaze is in display-area units (0–1). Tobii has no blink events, so the app switches to noise-based blink detection.

In more detail:

- **EyeLink `.asc` files** (convert `.edf` with SR Research's `edf2asc`). These are read with `read_asc()`. Trials run from `TRIALID` to `TRIAL_RESULT`, and time 0 is the start of recording in each trial. Blinks come from the tracker's blink events. Event messages go in the `message` column, and Experiment Builder `TRIAL_VAR`s (your conditions) become columns. For binocular recordings, choose the left eye, the right eye, their average, or **Both**. Both keeps `pupil_left`, `pupil_right` (and x, y, blink per eye) next to the average, so you can map either eye in the column settings.
- **gazeR-format files** (csv, tsv, or space-delimited text) like the example data. These include files written by `parse_asc()`.
- **SR Research Data Viewer sample reports** (`.xls` tab-delimited text). The app adds a `sr_trial_time` column (`TIMESTAMP − IP_START_TIME`) for the time mapping. Missing values coded as `.` are read as `NA`.

If you load several files at once, they are stacked. A `source_file` column records where each row came from.

## Settings

### 1 · Load data
Check that each column is mapped correctly. The **sampling rate** is estimated from the first trial's time stamps. It is used to turn milliseconds into samples for blink extension and the cubic max-gap setting.

### 2 · Blinks & artifacts
- **Plausible range** (min/max pupil) sets out-of-range samples to missing before anything else runs, e.g. 2–9 mm for diameters in mm.
- **Blink detection.** *Tracker* uses the blinks the eye tracker flagged: EyeLink `SBLINK`/`EBLINK` events, or your blink column. *Noise-based* (Hershman, Henik & Cohen, 2018) starts from each run of missing samples and walks outward along the smoothed pupil signal. It marks the whole blink, including the eyelid closing and reopening that trackers miss, so it needs little or no extra padding (try 0–50 ms). If *Also treat tracker blink samples as missing* is ticked, both sets of blinks are removed. Detected blinks are shaded red in the trial viewer.
- **Extend back/forward** pads every gap, not only labelled blinks, because the pupil edge is unreliable just before and after a blink. 100 ms each way is a common default.
- **Dilation-speed outliers** (Kret & Sjak-Shie, 2019): for each sample, the larger of the backward and forward absolute speed is computed. Samples faster than *median + k × MAD* within the trial are removed. A larger *k* removes fewer samples.
- **Exclusion threshold** is applied after blinks and artifacts are removed. A trial is dropped if its own missing proportion reaches the threshold, or if its subject's average does. When trials are time-locked, only samples inside the analysis window count, so long rest or inter-trial periods don't get trials excluded.

### 3 · Interpolate & smooth
- **Linear** interpolation is the safer default. **Cubic spline** (Mathôt et al., 2018) gives smoother curves but can overshoot across long gaps; use *Max gap* to limit it.
- **Moving average** uses a centred window of *n* samples. **Hanning** uses an 11-point Hann window.

### 4 · Baseline & time-lock
- **Before an event**: the median of the *N* ms before the chosen message, per trial.
- **Fixed window**: the median between two time stamps, per trial for subtractive correction. Divisive correction with a fixed window uses a per-subject baseline.
- **Subtractive** gives change in tracker units; **divisive** gives proportional change. Mathôt et al. (2018) recommend subtractive.
- **Time-locking** drops trials where the event is missing. Each trial must contain the event exactly once.

### 5 · Bin & group
**Group by** sets the columns that bins are averaged within. Time bin is always included. Common choices:

- `subject` + condition: one waveform per subject and condition, the usual input for group analyses.
- `subject` + `trial`: trial-level bins, for mixed models or GAMMs.
- condition only: pooled over subjects and trials.

Each bin's value (`aggbaseline`) is the mean of the baseline-corrected pupil. In the Results plot, `subject` and `trial` are treated as units: lines show the mean across units, with ± 1 SE. Every other grouping column becomes its own line.

## Behavioural data
Step 6 joins a behavioural table onto the eye data. It should have **one row per trial** (e.g. `subject, trial, rt, acc`). Choose the join columns, usually `subject` and `trial`; their values must match the eye data, so check the Data tab (e.g. `11c` vs `11c.edf`). A status line shows how many trials matched. Behavioural columns can then be used for Group by, as plot conditions (e.g. correct vs incorrect), as RT markers on the time course, and in the pupil vs behaviour plot. If a behavioural column has the same name as an eye-data column, it gets a `_behav` suffix.

### Excluding trials by behaviour
Also in step 6, and working with behavioural columns from either the eye-tracking file or an uploaded table:
- **Accuracy.** Choose the accuracy column and which value(s) count as correct (guessed as `1`, `TRUE` or `correct`). All other trials are excluded.
- **RT.** Choose the RT column (ms). *Min* and *Max* are absolute cutoffs (e.g. 200 ms for anticipations). *± SD* removes RTs more than *k* SDs from each subject's mean, computed on trials that pass the accuracy and min/max checks. Missing RTs are excluded once an RT column is chosen.

These rules run before any pupil processing. The panel previews how many trials they will remove, and the Quality tab lists each excluded trial with its reason.

## Results
The plots are computed from the processed samples, separately from Group by, so you can change them without re-running:

1. Samples are binned (*Plot bin*) and averaged within each trial.
2. Trials are averaged **within each subject**, so subjects are the unit of analysis. With a single subject, trials are the unit.
3. The plot shows means across units.

**Plot types**
- **Time course.** Mean pupil over time per condition, with optional dashed lines at the mean of a behavioural time (e.g. RT) per condition.
- **Raincloud.** Mean pupil in a time window, per condition: half-violin (distribution), box (median and IQR), one point per unit, optionally connected within subject.
- **Pupil vs behaviour.** Mean pupil in the window against a behavioural variable, per trial or per subject mean, with a least-squares line and *r* per condition.

**Error bands**
- **Within-subject SE / CI** (Cousineau, 2005, with the Morey, 2008, correction). Each subject's mean across conditions is removed and the grand mean added back, so stable differences between people don't inflate the error. Use this for conditions everyone did. It needs at least two conditions; otherwise between-units error is shown.
- **Between-subject SE / CI.** SD across units / √n. Use this for groups that differ between people, or for a single condition.
- **95% CI** = SE × *t*(0.975, n − 1).

The *Plotted values* table and download contain exactly what is drawn.

## Outputs

| File | Contents |
|---|---|
| Processed samples | Every retained sample, with `extendpupil` (after blink and artifact removal), `pup_interp` (interpolated and smoothed), `baseline`, `baselinecorrectedp`, and `time_zero` if time-locked |
| Binned data | Your grouping columns, `timebins`, `aggbaseline` (mean corrected pupil) |
| Trial quality | Missing proportion per trial and subject, and whether the trial was excluded |
| Parameters | The column mapping, all settings and the gazeR version, as JSON |

## Citing

Cousineau, D. (2005). Confidence intervals in within-subject designs: A simpler solution to Loftus and Masson's method. *Tutorials in Quantitative Methods for Psychology, 1*, 42–45.

Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). *Tutorials in Quantitative Methods for Psychology, 4*, 61–64.

Hershman, R., Henik, A., & Cohen, N. (2018). A novel blink detection method based on pupillometry noise. *Behavior Research Methods, 50*, 107–114.

If you use gazeR, please cite:
Geller, J., Winn, M. B., Mahr, T., & Mirman, D. (2020). GazeR: A package for processing gaze position and pupil size data. *Behavior Research Methods, 52*, 2232–2255.

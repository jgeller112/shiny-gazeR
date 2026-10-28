## How this app works

The app runs gazeR's pupil pre-processing pipeline on sample-level eye-tracking data. Each step in the sidebar corresponds to one or more gazeR functions, so anything you do here can be reproduced in an R script. The **Parameters (.json)** download records every setting you used, along with the shiny gazeR and gazeR versions.

Steps run in this order (functions are from gazeR unless noted):

| Step | What happens | gazeR function(s) |
|---|---|---|
| Load | Read and stack the files (`read_asc()` for `.asc`); rename columns to `subject`, `trial`, `time`, `pupil`, `blink`, `message` | `data.table::fread()`, as in `make_gazer()` |
| Behaviour | Trials are excluded by accuracy and RT (optional) before any processing | — |
| Blinks | Pupil values ≤ 0 and blink samples become `NA` (tracker blinks, or noise-based detection); gaps are padded back and forward | `detect_blinks_noise()`, `extend_blinks()` |
| Artifacts | Samples with implausibly fast dilation are removed | `speed_pupil()`, `calc_mad()` |
| Exclusion | Trials and subjects with too much missing data are dropped | (same logic as `count_missing_pupil()`) |
| Interpolate & smooth | Gaps are filled, then the trace is smoothed, or the reverse | `smooth_interpolate_pupil()` |
| Low-pass | Zero-phase Butterworth filter per trial, if chosen as the smoothing filter | `lowpass_filter()` |
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
| Tobii Pro Lab | Data exports (`.tsv`), one or more; a file can hold several participants | `read_tobii_prolab()` |
| BIDS (eye-tracking) | A `.zip` of a BIDS dataset (BEP020), or the `*_physio.tsv.gz` files with their `.json` sidecars and `*_physioevents` files | `read_bids_eyetrack()` |
| gazeR / other CSV | Any long table, e.g. output from `parse_asc()` | `data.table::fread()` |

For Pupil Labs, choose whether **any message** or only **specific messages** start a trial. With specific messages (e.g. `trial-started-light, start-dark`), in-between events such as `blank` stay inside the trial. Samples before the first trial start are dropped. Neon reports pupil diameter in mm and keeps values during blinks, so keep *Also treat tracker blink samples as missing* ticked. The plausible range defaults to 2–9 mm. Tick *Use surface (AOI) data* to add Marker Mapper surface columns. See the [Pupil Labs walkthrough](https://www.drjasongeller.com/blog/posts/2025-10-31-pupillabs/).

For BIDS data, each `sub`/`ses`/`task`/`run` becomes a subject (e.g. `sub-01_task-rest`), left- and right-eye recordings are merged, event messages go in the `message` column and blink events become `blink`. A BIDS recording is continuous, so each run is one trial unless you list **Trial start messages** (e.g. `TRIALID`): a new trial then starts at each message beginning with one of them, and time is ms from it. The sampling rate comes from the JSON sidecar. BIDS data from any tracker can be read, including EyeLink, Tobii and Pupil Labs conversions: timestamps in s, ms, µs or ns are converted to ms. If the sidecar gives pupil size in mm (e.g. Pupil Labs), the plausible range is set to 2–9 mm, and if there are no blink events (e.g. Tobii), noise-based blink detection is turned on.

For Tobii Pro Lab, export the data (Analyze > Data Export) as `.tsv` with pupil diameter, validity, gaze point, events and the presented stimulus. Each participant (`Participant name`) becomes a subject. By default each stimulus presentation is a trial and calibration is dropped. Or list **Trial start messages** (e.g. `TrialStart`, matched against `Event` or `Event: Event value`) to start trials at events. Samples marked Invalid are set to missing and pupil is in mm (range 2–9 mm). Blinks: samples Pro Lab classified as **Blink** (`Eye movement type`) become the `blink` column, and *Blink detection: Tracker* is selected. Trackers that measure eye openness (e.g. Pro Spectrum, Pro Fusion) let Pro Lab mark the lids closing and reopening, including samples where the pupil is still valid; openness is kept as `eye_openness_left`, `eye_openness_right` and `eye_openness`. Without Blink labels, the gaps where the pupil was lost are padded as usual. The sampling rate is estimated from the time stamps (e.g. 600 Hz for a Pro Spectrum).

For Tobii, invalid samples (validity flag false) are set to missing, and gaze is in display-area units (0–1). Tobii has no blink events, so the app switches to noise-based blink detection.

In more detail:

- **EyeLink `.asc` files** (convert `.edf` with SR Research's `edf2asc`). These are read with `read_asc()`. Trials run from `TRIALID` to `TRIAL_RESULT`, and time 0 is the start of recording in each trial. Blinks come from the tracker's blink events. Event messages go in the `message` column, and Experiment Builder `TRIAL_VAR`s (your conditions) become columns. For binocular recordings, choose the left eye, the right eye, their average, or **Both**. With **Average** and **Both**, `pupil` (and `x`, `y`) is the **monocular mean**, as EyeLink reports the average eye: the mean of both eyes, or the value of the one valid eye (missing only when neither is), and `blink` is 1 if **either** eye is blinking. **Both** also keeps `pupil_left`, `pupil_right`, x/y and `blink_left`, `blink_right` for each eye, so you can map one eye in the column settings. If you do, map its matching blink column too (e.g. `pupil_left` with `blink_left`).
- **gazeR-format files** (csv, tsv, or space-delimited text) like the example data. These include files written by `parse_asc()`.
- **SR Research Data Viewer sample reports** (`.xls` tab-delimited text). The app adds a `sr_trial_time` column (`TIMESTAMP − IP_START_TIME`) for the time mapping. Missing values coded as `.` are read as `NA`.

If you load several files at once, they are stacked. A `source_file` column records where each row came from.

### Missing data and blinks by eye
The Data tab (and the Quality tab) shows, for each participant, how much of the recording is missing, as loaded and before any cleaning:

- **Missing:** samples with no valid pupil value.
- **Blink:** samples inside a tracker-flagged blink.
- **Missing outside blinks:** tracker loss (e.g. the eye wasn't found).
- **Trials >50% missing:** trials that are mostly missing.
- **L–R pupil r** (with **Both**): the correlation between the left and right pupil over samples where both are valid (`gazeR::pupil_cor()`). The two pupils move together, so this is usually above .9. A lower value (shown in red) means one eye was tracked poorly; averaging the eyes would mix in that noise, so reload with the better eye.

With **Both**, there are separate rows for the left eye, the right eye and their average. The average is the monocular mean, so it is missing only when both eyes are, and its blink is 1 if either eye blinks. With **Left**, **Right** or **Average**, there is one row for that choice. If one eye is much worse than the other (e.g. lost for whole trials), reload with the better eye.

## Settings

### 1 · Load data
Check that each column is mapped correctly.

**Sampling rate.** Each participant's rate is read from the file header for EyeLink `.asc` files, or estimated from the median time step for other files. It's used for blink padding, noise-based blink detection, the cubic max-gap setting and the low-pass filter. The **Recorded rate** box only corrects a wrongly detected rate: changing 500 to 1000 tells the app the data *are* 1000 Hz and does not add samples. To change the rate of the data, use **Resample all to** (the target starts at the highest recorded rate). Resampling happens when you click **Run**, so it shows in the processed and binned data and downloads, not in the data as loaded (the Data tab and *Raw combined* downloads). If participants were recorded at different rates (e.g. some at 500 Hz instead of 1000 Hz), step 1 lists them and offers two options:
- **Upsample to the highest rate** (recommended). Lower-rate participants get new samples in between the real ones, using `upsample_gaze()`. Each new sample's pupil and gaze values are interpolated from the real samples either side. If either neighbour is missing (a blink, tracker loss), the new sample stays missing. Event messages and the original samples are unchanged. Afterwards everyone is processed at the same rate. Upsampling assumes whole-millisecond time stamps, as EyeLink records.
- **Use each participant's own rate.** Every step uses that participant's rate. Time bins are in ms, so results still line up across participants.

The log and the parameters file record each participant's rate and which option was used.

### 2 · Blinks & artifacts
- **Plausible range** (min/max pupil) sets out-of-range samples to missing before anything else runs, e.g. 2–9 mm for diameters in mm.
- **Blink detection.** *Tracker* uses the blinks the eye tracker flagged: EyeLink `SBLINK`/`EBLINK` events, or your blink column. *Noise-based* (Hershman, Henik & Cohen, 2018) starts from each run of missing samples and walks outward along the smoothed pupil signal. It marks the whole blink, including the eyelid closing and reopening that trackers miss, so it needs little or no extra padding (try 0–50 ms). If *Also treat tracker blink samples as missing* is ticked, both sets of blinks are removed. Detected blinks are shaded red in the trial viewer.
- **Extend back/forward** pads every gap, not only labelled blinks, because the pupil edge is unreliable just before and after a blink. 100 ms each way is a common default.
- **Dilation-speed outliers** (Kret & Sjak-Shie, 2019): for each sample, the larger of the backward and forward absolute speed is computed. Samples faster than *median + k × MAD* within the trial are removed. A larger *k* removes fewer samples.
- **Exclusion threshold** (e.g. 40%) is applied after blinks and artifacts are removed, at two levels:
  - **Participant:** if more than 40% of all the participant's samples are missing, pooled across their trials, the participant is excluded and every trial is marked *subject missing data*.
  - **Trial:** otherwise, any trial with more than 40% of its samples missing is excluded (*trial missing data*).

  When trials are time-locked, only samples inside the analysis window count at both levels, so long rest or inter-trial periods don't get trials excluded. The per-eye table on the Data tab, by contrast, counts the whole recording.

### 3 · Interpolate & smooth
- **Linear** interpolation is the safer default. **Cubic spline** (Mathôt et al., 2018) gives smoother curves but can overshoot across long gaps; use *Max gap* to limit it.
- **Smoothing filter: choose one.**
  - **Moving average:** a centred window of *n* samples.
  - **Hanning:** an 11-point Hann window.
  - **Low-pass (Butterworth):** a zero-phase filter (`lowpass_filter()`, so there's no time shift) that removes fluctuations faster than the cutoff. Each trial is mean-centred and reflection-padded so its ends aren't distorted. It needs gap-free data, so it always runs after interpolation; trials that still have gaps (e.g. from a cubic *Max gap*) are left unfiltered, and the log says how many. 4 Hz is a common cutoff for pupil data, and the cutoff must be below half the sampling rate.
  - **None:** interpolation only.

  *Order* (interpolate first or smooth first) applies to moving average and Hanning.

### 4 · Baseline & time-lock

**Re-scaling: Z-score pupil** (optional) comes after baseline correction. The baseline-corrected pupil has the participant's mean subtracted and is divided by their SD, over all their kept trials. This puts participants with different pupil sizes or tracker units on one scale while keeping the differences between their trials and conditions, and results, plots and binned data are then in SD units. Z-scoring each trial separately isn't offered: it would make every trial average 0 with SD 1, erasing each trial's size of response (usually the effect of interest). `gazeR::zscore_pupil(by = "trial")` can do it in code. The z-scored values are the `pup_z` column; `baselinecorrectedp` is kept. `gazeR::zscore_pupil()`
- **Before an event**: the median or mean of the *N* ms before the chosen message, per trial.
- **Fixed window**: the baseline between two time stamps, per trial for subtractive correction. Divisive correction with a fixed window uses a per-subject baseline.
- **Baseline value: Median or Mean.** The median (gazeR's default) or the mean of the pupil in the baseline period. Mathôt & Vilotijević (2022) recommend subtractive correction, per trial, using the mean of a short baseline period.
- **Subtractive** gives change in tracker units; **divisive** gives proportional change. Mathôt et al. (2018) recommend subtractive.
- **Events:** the baseline and time-lock settings each accept **one or more** events. Each trial uses the first selected event it contains, so tasks whose trials start with different messages (e.g. `LONG_TRIAL_START` and `SHORT_TRIAL_START`) keep all their trials. The app pre-selects every start/onset-type event it finds.
- **Time-locking** drops trials that contain none of the selected events. The Quality tab gives the reason.

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

**Data by participant** (Export tab) downloads any of three datasets, for all participants or just one, as a single CSV or a `.zip` with one CSV per participant:
- **Raw combined:** the data as loaded, one row per sample, with event messages, trial variables and any behavioural columns joined, before any cleaning. For EyeLink files this is what `read_asc()` returns.
- **Processed samples:** after the full pipeline.
- **Binned:** the Group-by table.

## Reports, BIDS-like export and study files

On the Export tab:

- **QC report.** A self-contained HTML file per participant, or a zip of all of them. It contains:
  - a summary (sampling rate, trials kept, missing data and blinks);
  - trial outcomes, and missing data per trial with the exclusion threshold;
  - the average signal after each step (raw → blinks removed → interpolated and smoothed), and the baseline-corrected average;
  - a small plot of every trial;
  - all parameters.
- **BIDS-like export.** A zip of `derivatives/shinygazer/`:
  - `dataset_description.json` and `metadata.json`, with every setting;
  - for each participant, `sub-<id>/eye/sub-<id>_task-<task>_…`: processed time series (`desc-preproc_timeseries.csv` plus a JSON sidecar with sampling rate and column descriptions), `events.csv`, `blinks.csv` (start, end, duration), binned data and trial quality.
- **Study file.** *Save settings* writes every setting to a `.json` file; *Load settings* restores them. Data files aren't included, so load them again in step 1. Settings that depend on your data (column mapping, events, conditions) are applied once the data are loaded.

## Resampling
Two separate options in step 1:

- **Resample all to __ Hz** brings every participant to one sampling rate.
  - **Higher rates are downsampled** by averaging consecutive samples into bins (e.g. 2 ms bins for 500 Hz). Averaging rather than dropping samples avoids aliasing. A bin with any missing sample stays missing, a bin is a blink if any of its samples was, and every event message is kept on its bin.
  - **Lower rates are upsampled** with `upsample_gaze()`, new samples interpolated from the recorded samples either side.
  - The log and methods paragraph report it.
- **Fill dropped samples (uniform grid)** keeps each participant's rate. It puts each trial on an evenly spaced grid and inserts any samples the tracker dropped as missing. EyeLink files are normally already evenly spaced.

## Data dictionary

**Data dictionary (.csv)** on the Export tab lists every column of the data you have loaded or processed, with its type and meaning. Every per-participant `.zip` also includes `data_dictionary.csv`. Columns not listed below come from your own files: Experiment Builder trial variables, behavioural columns, or other tracker columns.

### Raw combined data
One row per sample, as loaded. No cleaning has been applied.

| Column | Meaning |
|---|---|
| `subject` | Participant label (file name without extension for EyeLink files) |
| `trial` | Trial number (one per `TRIALID` for EyeLink files) |
| `time` | ms from the start of recording in the trial |
| `pupil` | Pupil size in tracker units (EyeLink: area or diameter); `NA` = missing. **Average/Both: monocular mean (mean of both eyes, or the one valid eye)** |
| `x`, `y` | Gaze position (pixels; Tobii: 0–1). Average/Both: mean of both eyes |
| `blink` | 1 during a tracker-flagged blink. **Average/Both: 1 if either eye is blinking** |
| `message` | Event message(s) on this sample, offsets applied; `NA` otherwise |
| `tracker_time` | The tracker's own clock (ms) |
| `pupil_left`, `pupil_right`, `x_left`, `y_left`, `x_right`, `y_right` | Each eye separately (Both only) |
| `blink_left`, `blink_right` | 1 while that eye is blinking (Both only) |
| `sample_rate` | Recorded sampling rate (Hz), from the EyeLink file header |
| `eyes_recorded` | Eyes recorded in the file: left, right or both (EyeLink) |
| `source_file` | File the row came from |
| *trial variables* | One column per Experiment Builder `TRIAL_VAR` (e.g. `condition`) |

### Processed samples
The raw columns for retained trials, plus:

| Column | Meaning |
|---|---|
| `upsampled` | `TRUE` for samples added by upsampling |
| `processing_hz` | Sampling rate used for this participant (Hz) |
| `blink_detected` | 1 if removed as a blink, before padding |
| `extendpupil` | Pupil after removing blinks (padded), out-of-range values and speed outliers |
| `interp` | Pupil after interpolation, before smoothing (interpolate-first only) |
| `pup_interp` | Pupil after interpolation, smoothing and low-pass filtering |
| `baseline` | Baseline pupil size (median in the baseline period) |
| `baselinecorrectedp` | `pup_interp − baseline` (subtractive) or `(pup_interp − baseline) / baseline` (divisive) |
| `time_zero` | ms from the time-lock event |

### Binned data
| Column | Meaning |
|---|---|
| *Group-by columns* | e.g. `subject`, `trial`, condition |
| `timebins` | Start of the time bin (ms) |
| `aggbaseline` | Mean baseline-corrected pupil in the bin |

### Trial quality
| Column | Meaning |
|---|---|
| `subject`, `trial` | Trial identity |
| `samples` | Samples in the trial (inside the analysis window when time-locked) |
| `prop_missing` | Proportion of the trial's samples missing after blink and artifact removal (inside the analysis window when time-locked) |
| `subject_prop_missing` | Proportion of the participant's samples missing, pooled across trials (inside the analysis window when time-locked) |
| `reason` | Why the trial was excluded, or `kept` |
| `excluded` | `TRUE` if excluded |

### Plotted values (Results tab)
| Column | Meaning |
|---|---|
| `condition` | The plot's condition columns, joined with ` · ` |
| `timebins` | Time bin (ms), time course only |
| `mean`, `n` | Mean across units and how many units contribute |
| `se`, `half`, `lower`, `upper` | Standard error, half-width of the band (SE or 95% CI) and its edges |
| `error_type` | Which error the band shows |
| `unit`, `value` | Raincloud / behaviour plots: the unit and its mean pupil in the window |

## Methods paragraph

After a run, the Export tab shows a **methods paragraph** describing what was done, ready to adapt for a paper. It covers:

- gazeR and shiny gazeR versions and citations;
- the eye tracker, sampling rates (and upsampling) and the eye analysed;
- every cleaning step and its settings;
- the exclusion rules and how many trials and participants they removed;
- interpolation, smoothing, baseline, time-locking and binning;
- the final sample, and how much of each trial was interpolated.

It also lists references for the methods used. Copy it, or download it with references as Markdown. Check the wording: event names, for example, appear as they do in your files.

## Citing

Cousineau, D. (2005). Confidence intervals in within-subject designs: A simpler solution to Loftus and Masson's method. *Tutorials in Quantitative Methods for Psychology, 1*, 42–45.

Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). *Tutorials in Quantitative Methods for Psychology, 4*, 61–64.

Hershman, R., Henik, A., & Cohen, N. (2018). A novel blink detection method based on pupillometry noise. *Behavior Research Methods, 50*, 107–114.

If you use gazeR, please cite:
Geller, J., Winn, M. B., Mahr, T., & Mirman, D. (2020). GazeR: A package for processing gaze position and pupil size data. *Behavior Research Methods, 52*, 2232–2255.

# LoginLens - Statistical Detection of Unusual Login Activity (R / Shiny)

LoginLens is a Probability and Statistics course project. It flags **statistically unusual** login events using transparent rules, explains each flag, and evaluates the detector against injected anomalies in a **fully synthetic** dataset.

> **All data is synthetic and fictional.** Performance numbers describe a simulation, not real systems. A flag means "unusual", never "confirmed attack".

## What it does
- Generates 3,179 synthetic login events for 25 fictional users over 60 days (seed 42): a 30-day baseline and a 30-day evaluation period with 93 injected anomalies (unusual hour, high daily frequency, failed-login bursts).
- Learns per-user baselines from the baseline period only: smoothed circular login-hour probabilities and daily-login mean/standard deviation.
- Scores evaluation events with three rules (hour rarity, daily z-score, failed-login burst), combined as `S = max(ratio_hour, ratio_z, ratio_burst)`; flag if `S >= 1`. Three profiles: relaxed, balanced, strict.
- Evaluates with TP, FP, TN, FN, precision, recall, false-positive rate, specificity, accuracy and F1 (`NA` when a denominator is zero). Ground-truth labels are used only after detection.
- Shiny dashboard with four tabs: Overview, Login Explorer (filters, search, CSV export), Threshold Lab, Statistical Insights.

## Requirements
R 4.x and the packages `shiny`, `ggplot2`, `dplyr`, `lubridate`, `tidyr`, `DT`, `testthat` (plus `rmarkdown`/`knitr` and pandoc only for the optional `report/*.Rmd` files). The project was developed and verified on Linux with R 4.3.3; it has not been run on Windows by the author of these notes.

## Windows setup and commands
Run these from the project folder (`C:\Users\<you>\Loginlens`). If `Rscript` is not on your PATH, replace it by its full path, e.g. `"C:\Program Files\R\R-4.6.1\bin\Rscript.exe"`.

**PowerShell** (use `& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" ...` when using the full path):
```powershell
cd C:\Users\<you>\Loginlens
Rscript -e "install.packages(c('shiny','ggplot2','dplyr','lubridate','tidyr','DT','testthat'))"
Rscript R/generate_data.R              # optional: regenerate data/login_events.csv (seed 42)
Rscript -e "shiny::runApp('.')"        # start the app; open the printed http://127.0.0.1:PORT address
Rscript tests/run_tests.R              # run the automated tests
Rscript report/final_results.R         # recompute all reported results, CSVs and figures
```
**Command Prompt (cmd.exe)** - same commands; for the full path use `"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "shiny::runApp('.')"`.

**Linux/macOS (bash)** - identical commands.

The app reads `data/login_events.csv`, and regenerates it automatically if the file is missing.

## Reproducing the reported results
`Rscript report/final_results.R` re-runs the whole pipeline and writes `report/results/*.csv` and `report/figures/*.png`. The report and slides quote those files. Results on the committed seed-42 dataset (evaluation period, 1,667 events, 93 anomalies):

| Profile | TP | FP | TN | FN | Precision | Recall | FPR | F1 |
|---|---|---|---|---|---|---|---|---|
| relaxed | 13 | 17 | 1557 | 80 | 0.433 | 0.140 | 0.011 | 0.211 |
| balanced | 28 | 63 | 1511 | 65 | 0.308 | 0.301 | 0.040 | 0.304 |
| strict | 47 | 137 | 1437 | 46 | 0.255 | 0.505 | 0.087 | 0.339 |

`report/results/seed_study_summary.csv` gives the mean and standard deviation over 20 other seeds (same thresholds).

## Reading the dashboard
- **Profile selector** (sidebar) sets the thresholds for Overview and Explorer. *Relaxed* has the highest cut-offs (fewest flags); *strict* the lowest (most flags).
- **Threshold Lab** compares the three profiles and a custom profile (sliders for the hour ratio, z threshold and burst size) on the same evaluation data. Lowering thresholds raises recall and false alarms together; the table shows by how much on this data.
- **Login Explorer** table has a *Detection reason* for each flagged event; **Export filtered CSV** writes exactly the filtered rows (ground-truth columns are excluded).

## Tests
`Rscript tests/run_tests.R` runs the testthat suite in `tests/testthat/` (data schema, reproducibility, label separation, scoring, zero variance, insufficient history, missing/empty input, confusion-matrix metrics, thresholds, filtering/CSV export, Shiny server logic, and an HTTP start-up check that is skipped on Windows). Browser-level checks (navigation, filters, CSV download, sliders, empty-filter behaviour) were performed manually with headless Chromium and are **not** part of the automated suite.

## Repository structure
```
app.R                       Shiny UI + server
www/style.css, www/app.js   dashboard styling and sidebar navigation
R/generate_data.R           seeded synthetic data generator, CSV read/write
R/detector.R                baseline fitting, features, thresholds, scoring
R/evaluate.R                confusion matrix and metrics, filtering, CSV export
data/login_events.csv       committed synthetic dataset (seed 42)
tests/run_tests.R           one-command test runner
tests/testthat/             test files
report/final_results.R      measured results, CSVs and figures used by the documents
report/results/             measured result CSVs
report/figures/             figures; report/screenshots/ real dashboard screenshots
report/results.R, report.Rmd, slides.Rmd, report.docx, slides.pptx, viva.md
                            earlier, simpler versions of the deliverables (superseded by the files below)
```

## Deliverables
- [Final report (Word)](report/LoginLens_Final_Report.docx)
- [Final presentation (PowerPoint, 14 slides)](report/LoginLens_Final_Presentation.pptx)
- [Viva preparation (Word)](report/LoginLens_Viva_Preparation.docx)

## Limitations and responsible interpretation
- Synthetic data and self-injected anomalies: results do not show real-world performance.
- Daily login counts are skewed, over-dispersed integers, so the z-score (a normal approximation) is a heuristic.
- Assumes stable habits and a clean baseline; device and location are generated but not used; three rules combined with OR increase false alarms.
- Threshold profiles are round, hand-chosen values, not tuned on separate validation data.
- A flag is a lead for human review, never proof of an attack.

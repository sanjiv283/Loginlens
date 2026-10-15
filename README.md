# LoginLens - Statistical Detection of Unusual Login Activity (R / Shiny)

**All data is synthetic and fictional.** A flag means "statistically unusual", never "confirmed attack".

## Install (R >= 4.3)
```bash
Rscript -e 'install.packages(c("shiny","ggplot2","dplyr","lubridate","tidyr","DT","testthat","rmarkdown","knitr"))'
# Debian/Ubuntu alternative: sudo apt-get install r-base-core r-cran-shiny r-cran-ggplot2 r-cran-dplyr r-cran-lubridate r-cran-tidyr r-cran-dt r-cran-testthat r-cran-rmarkdown r-cran-knitr
```
## Run
```bash
Rscript R/generate_data.R                 # (re)generate data/login_events.csv, seed 42
Rscript -e 'shiny::runApp(".")'           # launch the app
```
## Test
```bash
Rscript tests/run_tests.R
```
## Rebuild report and slides
```bash
Rscript -e 'rmarkdown::render("report/report.Rmd"); rmarkdown::render("report/slides.Rmd")'
```
(needs pandoc). Outputs: `report/report.docx`, `report/slides.pptx`, figures in `report/figures/`, screenshot in `report/screenshots/`.

## Layout
- `R/generate_data.R` synthetic generator (baseline + evaluation periods, injected anomalies)
- `R/detector.R` hour rarity, daily z-score, failed-burst rules, combined score, threshold profiles
- `R/evaluate.R` confusion matrix/metrics (NA on zero denominators), explorer filtering, CSV export
- `app.R` Shiny app: Overview, Login Explorer, Threshold Lab, Statistical Insights
- `tests/` testthat suite; `report/` report, slides, `viva.md`

Method, formulas and limitations: see `report/report.docx` and the in-app Insights tab.

# Hybrid Time Series Forecasting under Non-Stationarity and Structural Breaks

This repository contains a reproducible Quarto-R workflow for an MPhil/PhD research project in Statistics.

## Render the report

```bash
quarto render
```

Render one format:

```bash
quarto render --to docx
quarto render --to pdf
quarto render --to html
```

## Environment control with renv

After opening the project, run:

```r
renv::init()
renv::snapshot()
```

After cloning this project on another computer, run:

```r
renv::restore()
```

## GitHub setup

```r
usethis::use_git()
usethis::use_github(private = TRUE, protocol = 'https')
```


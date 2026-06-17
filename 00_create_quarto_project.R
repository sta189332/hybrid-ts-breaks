# 00_create_quarto_project.R
# Creates a reproducible Quarto-R project for:
# Hybrid Time Series Forecasting under Non-Stationarity and Structural Breaks

project_name <- "hybrid-ts-breaks"

if (!dir.exists(project_name)) {
  dir.create(project_name)
}

old_wd <- getwd()
setwd(project_name)

dirs <- c(
  "R",
  "data/raw",
  "data/processed",
  "data/simulated",
  "outputs/figures",
  "outputs/tables",
  "outputs/models",
  "outputs/rendered",
  "logs"
)

invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

write_file <- function(path, text) {
  writeLines(text, con = path, useBytes = TRUE)
}

# RStudio project file
write_file(
  "hybrid-ts-breaks.Rproj",
  "Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: No

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: knitr
LaTeX: pdfLaTeX
"
)

# Git ignore
write_file(
  ".gitignore",
  "# R and RStudio
.Rhistory
.RData
.Ruserdata
.Rproj.user/

# renv local library
renv/library/
renv/staging/

# Quarto output
/.quarto/
/_freeze/
outputs/rendered/
*.html
*.docx
*.pdf

# Data policy
# Keep raw data local if licences or access terms restrict sharing.
data/raw/*
!data/raw/.gitkeep

# Large/generated files
outputs/models/*
outputs/figures/*
outputs/tables/*
logs/*
"
)

file.create("data/raw/.gitkeep")

# Quarto configuration
write_file(
  "_quarto.yml",
  "project:
  type: default
  output-dir: outputs/rendered
  render:
    - report.qmd

execute:
  echo: true
  warning: false
  message: false
  freeze: auto

toc: true
number-sections: true
bibliography: references.bib
csl: apa.csl

format:
  html:
    theme:
      light: flatly
    css: styles.scss
    code-fold: true
    code-tools: true
    fig-width: 8
    fig-height: 5
    df-print: paged
  docx:
    toc: true
    number-sections: true
    reference-doc: reference-doc.docx
  pdf:
    documentclass: report
    papersize: a4
    geometry:
      - margin=1in
    fig-width: 7
    fig-height: 5
"
)

# Simple SCSS theme
write_file(
  "styles.scss",
  "/* styles.scss */

$font-family-sans-serif: 'Times New Roman', Times, serif;
$font-family-serif: 'Times New Roman', Times, serif;

body {
  font-family: 'Times New Roman', Times, serif;
  font-size: 12pt;
  line-height: 1.5;
}

h1, h2, h3, h4 {
  font-family: 'Times New Roman', Times, serif;
  font-weight: 700;
}

table {
  font-size: 0.95em;
}

.caption {
  font-style: italic;
}
"
)

# Starter bibliography
write_file(
  "references.bib",
  "@book{hyndman2018forecasting,
  author    = {Hyndman, Rob J. and Athanasopoulos, George},
  title     = {Forecasting: Principles and Practice},
  edition   = {2},
  year      = {2018},
  publisher = {OTexts}
}

@article{perron1989great,
  author  = {Perron, Pierre},
  title   = {The Great Crash, the Oil Price Shock, and the Unit Root Hypothesis},
  journal = {Econometrica},
  volume  = {57},
  number  = {6},
  pages   = {1361--1401},
  year    = {1989}
}

@article{bai2003computation,
  author  = {Bai, Jushan and Perron, Pierre},
  title   = {Computation and Analysis of Multiple Structural Change Models},
  journal = {Journal of Applied Econometrics},
  volume  = {18},
  number  = {1},
  pages   = {1--22},
  year    = {2003}
}
"
)

# README
write_file(
  "README.md",
  "# Hybrid Time Series Forecasting under Non-Stationarity and Structural Breaks

This repository contains the reproducible Quarto-R workflow for an MPhil/PhD research project in Statistics.

## Main output

Render the full report with:

```bash
quarto render
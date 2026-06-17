# 00_create_quarto_project.R
# Complete project scaffold for:
# Hybrid Time Series Forecasting under Non-Stationarity and Structural Breaks
#
# Usage:
# 1. Save this file outside the target project folder.
# 2. Set your working directory to where the project folder should be created.
# 3. Run: source("00_create_quarto_project.R")
# 4. Open: hybrid-ts-breaks/hybrid-ts-breaks.Rproj
# 5. Run: renv::init(); renv::snapshot()
# 6. Render: quarto::quarto_render() or quarto render

project_name <- "hybrid-ts-breaks"

if (!dir.exists(project_name)) {
  dir.create(project_name)
}

old_wd <- getwd()
setwd(project_name)

write_file <- function(path, text) {
  writeLines(text, con = path, useBytes = TRUE)
}

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
file.create("data/raw/.gitkeep")

# -------------------------------------------------------------------------
# RStudio project file
# -------------------------------------------------------------------------

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

# -------------------------------------------------------------------------
# .gitignore
# -------------------------------------------------------------------------

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

# Quarto generated files
/.quarto/
/_freeze/
outputs/rendered/
*.html
*.docx
*.pdf

# Data policy
data/raw/*
!data/raw/.gitkeep

# Generated outputs
outputs/models/*
outputs/figures/*
outputs/tables/*
logs/*
"
)

# -------------------------------------------------------------------------
# Quarto project configuration
# -------------------------------------------------------------------------

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
  pdf:
    documentclass: report
    papersize: a4
    geometry:
      - margin=1in
    fig-width: 7
    fig-height: 5
"
)

# -------------------------------------------------------------------------
# HTML style file
# -------------------------------------------------------------------------

write_file(
  "styles.scss",
  "body {
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

# -------------------------------------------------------------------------
# Starter bibliography
# -------------------------------------------------------------------------

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

# -------------------------------------------------------------------------
# README
# -------------------------------------------------------------------------

write_file(
  "README.md",
  "# Hybrid Time Series Forecasting under Non-Stationarity and Structural Breaks

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
"
)

# -------------------------------------------------------------------------
# Package setup
# -------------------------------------------------------------------------

write_file(
  "R/00_packages_setup.R",
  "required_packages <- c(
  'tidyverse', 'lubridate', 'zoo', 'xts',
  'forecast', 'fable', 'tsibble', 'feasts',
  'urca', 'tseries', 'strucchange', 'changepoint', 'bfast',
  'rugarch', 'MSwM', 'vars',
  'glmnet', 'randomForest', 'xgboost', 'e1071',
  'caret', 'tidymodels', 'Metrics',
  'ggplot2', 'knitr', 'kableExtra',
  'renv', 'usethis', 'gh', 'gitcreds'
)

install_missing_packages <- function(pkgs = required_packages) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
}

load_required_packages <- function(pkgs = required_packages) {
  invisible(lapply(pkgs, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

install_missing_packages()
load_required_packages()
"
)

# -------------------------------------------------------------------------
# Simulation scripts
# -------------------------------------------------------------------------

write_file(
  "R/01_sim_arma_baseline.R",
  "simulate_arma_baseline <- function(n = 1000, phi = 0.6, theta = 0.3, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- as.numeric(arima.sim(model = list(ar = phi, ma = theta), n = n, sd = sigma))

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S1_ARMA_baseline',
    true_break = NA_integer_,
    break_type = 'none',
    regime = 'stable'
  )
}
"
)

write_file(
  "R/02_sim_random_walk_trend_unitroot.R",
  "simulate_random_walk <- function(n = 1000, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- cumsum(rnorm(n, 0, sigma))

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S2_random_walk',
    true_break = NA_integer_,
    break_type = 'unit_root'
  )
}

simulate_trend_stationary <- function(n = 1000, alpha = 0, beta = 0.05, phi = 0.6, sigma = 1, seed = 123) {
  set.seed(seed)
  u <- as.numeric(arima.sim(model = list(ar = phi), n = n, sd = sigma))
  time <- seq_len(n)
  y <- alpha + beta * time + u

  data.frame(
    time = time,
    y = y,
    scenario = 'S3_trend_stationary',
    true_break = NA_integer_,
    break_type = 'deterministic_trend'
  )
}

simulate_unit_root_drift <- function(n = 1000, mu = 0.05, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- numeric(n)
  eps <- rnorm(n, 0, sigma)

  for (t in 2:n) {
    y[t] <- mu + y[t - 1] + eps[t]
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S4_unit_root_drift',
    true_break = NA_integer_,
    break_type = 'unit_root_with_drift'
  )
}
"
)

write_file(
  "R/03_sim_arima_breaks.R",
  "simulate_ar_mean_break <- function(n = 1000, Tb = 600, phi = 0.6, mu1 = 0, mu2 = 2, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- numeric(n)
  eps <- rnorm(n, 0, sigma)

  for (t in 2:n) {
    mu_t <- ifelse(t <= Tb, mu1, mu2)
    y[t] <- mu_t + phi * y[t - 1] + eps[t]
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S5_AR_mean_break',
    true_break = Tb,
    break_type = 'mean',
    regime = ifelse(seq_len(n) <= Tb, 'pre_break', 'post_break')
  )
}

simulate_ar_variance_break <- function(n = 1000, Tb = 600, phi = 0.6, sigma1 = 1, sigma2 = 3, seed = 123) {
  set.seed(seed)
  y <- numeric(n)

  for (t in 2:n) {
    sigma_t <- ifelse(t <= Tb, sigma1, sigma2)
    y[t] <- phi * y[t - 1] + rnorm(1, 0, sigma_t)
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S6_AR_variance_break',
    true_break = Tb,
    break_type = 'variance',
    regime = ifelse(seq_len(n) <= Tb, 'pre_break', 'post_break')
  )
}
"
)

write_file(
  "R/04_sim_garch_breaks.R",
  "simulate_garch_volatility_break <- function(n1 = 600, n2 = 400, seed = 123) {
  if (!requireNamespace('rugarch', quietly = TRUE)) {
    stop('Package rugarch is required. Install it with install.packages(\"rugarch\").')
  }

  set.seed(seed)

  spec1 <- rugarch::ugarchspec(
    variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0)),
    fixed.pars = list(mu = 0, ar1 = 0.2, omega = 0.01, alpha1 = 0.05, beta1 = 0.90)
  )

  spec2 <- rugarch::ugarchspec(
    variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0)),
    fixed.pars = list(mu = 0, ar1 = 0.2, omega = 0.08, alpha1 = 0.12, beta1 = 0.80)
  )

  sim1 <- rugarch::ugarchpath(spec1, n.sim = n1)
  sim2 <- rugarch::ugarchpath(spec2, n.sim = n2)

  y <- c(
    as.numeric(fitted(sim1) + sigma(sim1) * rnorm(n1)),
    as.numeric(fitted(sim2) + sigma(sim2) * rnorm(n2))
  )

  n <- n1 + n2

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S7_GARCH_volatility_break',
    true_break = n1,
    break_type = 'volatility',
    regime = ifelse(seq_len(n) <= n1, 'low_volatility', 'high_volatility')
  )
}
"
)

write_file(
  "R/05_sim_markov_switching.R",
  "simulate_markov_switching <- function(n = 1000, seed = 123) {
  set.seed(seed)

  P <- matrix(c(0.95, 0.05, 0.10, 0.90), nrow = 2, byrow = TRUE)
  state <- numeric(n)
  state[1] <- 1

  for (t in 2:n) {
    state[t] <- sample(1:2, size = 1, prob = P[state[t - 1], ])
  }

  mu <- c(0, 2)
  phi <- c(0.4, 0.8)
  sigma <- c(1, 2)
  y <- numeric(n)

  for (t in 2:n) {
    s <- state[t]
    y[t] <- mu[s] + phi[s] * y[t - 1] + rnorm(1, 0, sigma[s])
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S8_Markov_switching',
    state = state,
    true_break = NA_integer_,
    break_type = 'latent_regime'
  )
}
"
)

write_file(
  "R/06_sim_nonlinear_threshold.R",
  "simulate_threshold_ar <- function(n = 1000, phi1 = 0.3, phi2 = 0.8, threshold = 0, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- numeric(n)

  for (t in 2:n) {
    phi_t <- ifelse(y[t - 1] <= threshold, phi1, phi2)
    y[t] <- phi_t * y[t - 1] + rnorm(1, 0, sigma)
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S9_threshold_AR',
    threshold = threshold,
    true_break = NA_integer_,
    break_type = 'threshold_nonlinearity'
  )
}
"
)

write_file(
  "R/07_sim_vecm_cointegration_breaks.R",
  "simulate_cointegrated_break <- function(n = 1000, Tb = 600, seed = 123) {
  set.seed(seed)

  x <- cumsum(rnorm(n))
  beta1 <- 1.0
  beta2 <- 1.8
  e <- as.numeric(arima.sim(model = list(ar = 0.5), n = n, sd = 1))
  beta_t <- ifelse(seq_len(n) <= Tb, beta1, beta2)
  y <- beta_t * x + e

  data.frame(
    time = seq_len(n),
    y = y,
    x = x,
    scenario = 'S10_cointegration_break',
    true_break = Tb,
    break_type = 'cointegration_vector',
    regime = ifelse(seq_len(n) <= Tb, 'pre_break', 'post_break')
  )
}
"
)

# -------------------------------------------------------------------------
# Data preprocessing
# -------------------------------------------------------------------------

write_file(
  "R/10_data_preprocessing.R",
  "time_ordered_split <- function(data, train_prop = 0.60, valid_prop = 0.20) {
  n <- nrow(data)
  train_end <- floor(train_prop * n)
  valid_end <- floor((train_prop + valid_prop) * n)

  list(
    train = data[1:train_end, ],
    valid = data[(train_end + 1):valid_end, ],
    test  = data[(valid_end + 1):n, ]
  )
}

compute_returns <- function(price) {
  100 * diff(log(price))
}

clean_time_series <- function(data) {
  data |>
    dplyr::arrange(time) |>
    dplyr::distinct(time, .keep_all = TRUE) |>
    tidyr::drop_na()
}

save_processed_data <- function(data, file_name) {
  readr::write_csv(data, file.path('data', 'processed', file_name))
}
"
)

# -------------------------------------------------------------------------
# Break detection
# -------------------------------------------------------------------------

write_file(
  "R/20_break_detection_methods.R",
  "detect_breaks_classical <- function(y) {
  time_index <- seq_along(y)

  bp <- tryCatch(
    strucchange::breakpoints(y ~ time_index),
    error = function(e) NULL
  )

  cusum <- tryCatch(
    strucchange::sctest(strucchange::efp(y ~ time_index, type = 'Rec-CUSUM')),
    error = function(e) NULL
  )

  cp_mean <- tryCatch(
    changepoint::cpt.mean(y, method = 'PELT'),
    error = function(e) NULL
  )

  cp_var <- tryCatch(
    changepoint::cpt.var(y, method = 'PELT'),
    error = function(e) NULL
  )

  list(
    bai_perron = bp,
    cusum = cusum,
    changepoint_mean = cp_mean,
    changepoint_variance = cp_var
  )
}

extract_changepoints <- function(cp_object) {
  if (is.null(cp_object)) return(integer(0))
  changepoint::cpts(cp_object)
}

make_break_features <- function(y, window = 20) {
  tibble::tibble(
    time = seq_along(y),
    y = as.numeric(y),
    lag1 = dplyr::lag(y, 1),
    roll_mean = zoo::rollmean(y, k = window, fill = NA, align = 'right'),
    roll_sd = zoo::rollapply(y, width = window, FUN = sd, fill = NA, align = 'right')
  ) |>
    tidyr::drop_na()
}
"
)

# -------------------------------------------------------------------------
# Existing forecasting models
# -------------------------------------------------------------------------

write_file(
  "R/30_existing_forecasting_models.R",
  "fit_arima_forecast <- function(train_y, h = 1) {
  fit <- forecast::auto.arima(train_y)
  forecast::forecast(fit, h = h)
}

fit_ets_forecast <- function(train_y, h = 1) {
  fit <- forecast::ets(train_y)
  forecast::forecast(fit, h = h)
}

make_lagged_features <- function(y, max_lag = 5) {
  lagged <- data.frame(y = as.numeric(y))
  for (i in 1:max_lag) {
    lagged[[paste0('lag', i)]] <- dplyr::lag(as.numeric(y), i)
  }
  tidyr::drop_na(lagged)
}

fit_xgboost_lag_model <- function(y, max_lag = 5, nrounds = 50) {
  lagged <- make_lagged_features(y, max_lag = max_lag)
  x <- as.matrix(lagged[, -1, drop = FALSE])
  target <- lagged$y

  xgboost::xgboost(
    data = x,
    label = target,
    nrounds = nrounds,
    objective = 'reg:squarederror',
    verbose = 0
  )
}
"
)

# -------------------------------------------------------------------------
# Proposed adaptive hybrid model
# -------------------------------------------------------------------------

write_file(
  "R/40_proposed_adaptive_hybrid_model.R",
  "fit_arima_xgboost_hybrid <- function(train_y, max_lag = 5, nrounds = 50) {
  arima_fit <- forecast::auto.arima(train_y)
  residual_values <- as.numeric(stats::residuals(arima_fit))

  residual_df <- data.frame(e = residual_values)
  for (i in 1:max_lag) {
    residual_df[[paste0('lag', i)]] <- dplyr::lag(residual_values, i)
  }
  residual_df <- tidyr::drop_na(residual_df)

  x <- as.matrix(residual_df[, -1, drop = FALSE])
  target <- residual_df$e

  xgb_fit <- xgboost::xgboost(
    data = x,
    label = target,
    nrounds = nrounds,
    objective = 'reg:squarederror',
    verbose = 0
  )

  list(
    statistical_model = arima_fit,
    residual_model = xgb_fit,
    max_lag = max_lag
  )
}

adaptive_refit_rule <- function(break_detected, full_data, recent_window = 250) {
  if (isTRUE(break_detected)) {
    tail(full_data, recent_window)
  } else {
    full_data
  }
}
"
)

# -------------------------------------------------------------------------
# Evaluation metrics
# -------------------------------------------------------------------------

write_file(
  "R/50_evaluation_metrics.R",
  "rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}

mae <- function(actual, forecast) {
  mean(abs(actual - forecast), na.rm = TRUE)
}

mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}

smape <- function(actual, forecast) {
  mean(abs(actual - forecast) / ((abs(actual) + abs(forecast)) / 2), na.rm = TRUE) * 100
}

directional_accuracy <- function(actual, forecast) {
  actual_direction <- sign(diff(actual))
  forecast_direction <- sign(diff(forecast))
  mean(actual_direction == forecast_direction, na.rm = TRUE)
}

break_detection_delay <- function(true_break, detected_break) {
  detected_break - true_break
}

forecast_metrics <- function(actual, forecast) {
  data.frame(
    RMSE = rmse(actual, forecast),
    MAE = mae(actual, forecast),
    MAPE = mape(actual, forecast),
    sMAPE = smape(actual, forecast)
  )
}
"
)

# -------------------------------------------------------------------------
# Real-world data analysis placeholder
# -------------------------------------------------------------------------

write_file(
  "R/60_real_world_data_analysis.R",
  "download_real_world_data <- function() {
  message('Add official data-download code here after verifying source access conditions.')
  message('Recommended sources: CBN, NBS, FRED, EIA, World Bank, IMF, NGX where accessible.')
}

prepare_real_world_dataset_register <- function() {
  tibble::tibble(
    dataset = c('NGN/USD exchange rate', 'Nigeria CPI/inflation', 'Brent crude oil', 'S&P 500'),
    source = c('CBN/FRED', 'NBS/CBN', 'FRED/EIA/World Bank', 'FRED/Yahoo Finance'),
    frequency = c('Daily/Monthly', 'Monthly', 'Daily/Monthly', 'Daily'),
    relevance = c(
      'Exchange-rate reforms and depreciation regimes',
      'Inflation persistence and policy shocks',
      'Oil-price volatility relevant to Nigeria',
      'External global financial benchmark'
    )
  )
}
"
)

# -------------------------------------------------------------------------
# Tables and figures
# -------------------------------------------------------------------------

write_file(
  "R/70_tables_figures.R",
  "plot_series_with_break <- function(data, title = 'Time Series with Break') {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = y)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = 'Time', y = 'Value') +
    ggplot2::theme_minimal()

  if ('true_break' %in% names(data) && any(!is.na(data$true_break))) {
    break_df <- unique(data[!is.na(data$true_break), 'true_break', drop = FALSE])
    p <- p +
      ggplot2::geom_vline(
        data = break_df,
        ggplot2::aes(xintercept = true_break),
        linetype = 'dashed'
      )
  }

  p
}

simulation_table <- function() {
  tibble::tribble(
    ~Scenario, ~DGP, ~Break_Type, ~True_Break_Known, ~Difficulty,
    'S1', 'ARMA(1,1)', 'None', 'No', 'Low',
    'S2', 'Random walk', 'Unit root', 'No', 'Moderate',
    'S3', 'Trend-stationary', 'Deterministic trend', 'No', 'Moderate',
    'S4', 'Unit root with drift', 'Drift', 'No', 'Moderate',
    'S5', 'AR mean break', 'Mean shift', 'Yes', 'Moderate-High',
    'S6', 'AR variance break', 'Variance shift', 'Yes', 'High',
    'S7', 'ARMA-GARCH break', 'Volatility break', 'Yes', 'High',
    'S8', 'Markov switching', 'Latent regime', 'Partial', 'High',
    'S9', 'Threshold AR', 'Nonlinear regime', 'Threshold known', 'High',
    'S10', 'Cointegration break', 'Long-run relation break', 'Yes', 'Very high'
  )
}
"
)

# -------------------------------------------------------------------------
# Master Quarto report
# -------------------------------------------------------------------------

write_file(
  "report.qmd",
  "---
title: 'Hybrid Time Series Forecasting under Non-Stationarity and Structural Breaks'
subtitle: 'Reproducible Quarto-R Workflow for MPhil/PhD Seminar'
author: 'Omokedi, Cornelius Oromena'
date: today
---

```{r}
#| label: setup
#| include: false
source('R/00_packages_setup.R')
source('R/01_sim_arma_baseline.R')
source('R/02_sim_random_walk_trend_unitroot.R')
source('R/03_sim_arima_breaks.R')
source('R/04_sim_garch_breaks.R')
source('R/05_sim_markov_switching.R')
source('R/06_sim_nonlinear_threshold.R')
source('R/07_sim_vecm_cointegration_breaks.R')
source('R/10_data_preprocessing.R')
source('R/20_break_detection_methods.R')
source('R/30_existing_forecasting_models.R')
source('R/40_proposed_adaptive_hybrid_model.R')
source('R/50_evaluation_metrics.R')
source('R/60_real_world_data_analysis.R')
source('R/70_tables_figures.R')

set.seed(123)
```

# Introduction

This document provides a reproducible research workflow for the study titled *Hybrid Time Series Forecasting under Non-Stationarity and Structural Breaks*. It integrates prose, R code, simulated datasets, model outputs, tables, figures and references in one Quarto document.

# Section Four: Data Organisation and Analysis

The empirical strategy combines simulated data and real-world data. Simulated data allow the true break date and data-generating process to be known, while real-world finance and economic data validate practical usefulness.

## Simulation Scenarios

```{r}
#| label: tbl-simulation-scenarios
#| tbl-cap: 'Simulation scenarios for non-stationarity and structural breaks'
knitr::kable(simulation_table())
```

## Example: AR Mean Break

```{r}
#| label: fig-ar-mean-break
#| fig-cap: 'Simulated autoregressive series with a known mean break'
sim_mean <- simulate_ar_mean_break()
plot_series_with_break(sim_mean, 'AR(1) Series with Mean Break')
```

## Example: Break Detection

```{r}
#| label: break-detection-demo
break_results <- detect_breaks_classical(sim_mean$y)
names(break_results)
```

## Forecasting Model Demonstration

```{r}
#| label: arima-demo
split_mean <- time_ordered_split(sim_mean)
fc <- fit_arima_forecast(split_mean$train$y, h = nrow(split_mean$test))
fc
```

# Section Five: Discussion and Conclusion

The discussion will interpret whether the proposed adaptive hybrid model improves forecast accuracy, reduces post-break forecast deterioration, detects structural breaks faster, and performs robustly across simulated and real-world datasets.

# References

The conceptual foundation of this work draws from classical and contemporary time series literature, including @hyndman2018forecasting, @perron1989great, and @bai2003computation.
"
)

# -------------------------------------------------------------------------
# Helper notes for renv and GitHub
# -------------------------------------------------------------------------

write_file(
  "R/98_renv_setup_notes.R",
  "# Run manually after opening the project:
# install.packages('renv')
# renv::init()
# renv::snapshot()

# On another computer after cloning:
# renv::restore()
"
)

write_file(
  "R/99_github_setup_notes.R",
  "# Run manually after opening the project in RStudio.
# Do not run blindly if Git is not installed or GitHub authentication is not configured.

# install.packages(c('usethis', 'gh', 'gitcreds'))

# usethis::use_git()

# Create token in browser:
# usethis::create_github_token()

# Store token:
# gitcreds::gitcreds_set()

# Create and push private GitHub repository:
# usethis::use_github(private = TRUE, protocol = 'https')
"
)

message("Project scaffold created successfully at: ", normalizePath(getwd()))
message("")
message("Next steps:")
message("1. Open: ", file.path(normalizePath(getwd()), "hybrid-ts-breaks.Rproj"))
message("2. In RStudio, run: install.packages('renv')")
message("3. Then run: renv::init(); renv::snapshot()")
message("4. Render with: quarto::quarto_render()")
message("5. Optional GitHub setup: read R/99_github_setup_notes.R and run commands manually.")

setwd(old_wd)

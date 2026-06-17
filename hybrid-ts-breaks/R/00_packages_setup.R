required_packages <- c(
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


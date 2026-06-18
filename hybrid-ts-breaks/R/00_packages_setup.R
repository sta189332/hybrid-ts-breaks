# -----------------------------------------------------------------------------
# hybrid-ts-breaks: package and reproducibility setup
# File: R/00_packages_setup.R
# Purpose: Provide a controlled package-management layer for a reproducible
#          PhD-level Quarto workflow on time-series non-stationarity,
#          structural breaks, regime changes, volatility, and hybrid forecasting.
# -----------------------------------------------------------------------------

# Recommended use from the project root:
#   source("R/00_packages_setup.R")
#   bootstrap_project()
#   ensure_packages()
#   attach_analysis_packages()
#   renv::snapshot()
#
# This script is intentionally not aggressive on source(). It defines package
# groups and helper functions, but it does not automatically install packages
# unless HYBRID_TS_AUTO_SETUP=true is set in the environment.

# -----------------------------------------------------------------------------
# 1. Project-level options
# -----------------------------------------------------------------------------

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  scipen = 999,
  stringsAsFactors = FALSE,
  renv.consent = TRUE
)

project_name <- "hybrid-ts-breaks"
project_version <- "0.1.0"

# -----------------------------------------------------------------------------
# 2. Package groups
# -----------------------------------------------------------------------------

reproducibility_packages <- c(
  "renv",        # project-specific library and lockfile
  "here",        # stable project-root paths
  "fs",          # portable file-system operations
  "sessioninfo", # reproducibility reporting
  "conflicted",  # explicit conflict management
  "usethis",     # project hygiene utilities
  "gh",          # GitHub integration
  "gitcreds"     # Git credential support
)

data_packages <- c(
  "tidyverse",   # data manipulation, plotting, importing, functional workflow
  "lubridate",   # date-time handling
  "readxl",      # Excel import
  "writexl",     # Excel export
  "haven",       # SPSS/Stata/SAS import where needed
  "janitor",     # data-name cleaning and tabulation helpers
  "zoo",         # irregular time-series infrastructure
  "xts"          # extensible time-series objects
)

time_series_packages <- c(
  "forecast",    # classical forecasting models and accuracy tools
  "fable",       # tidy forecasting models
  "fabletools",  # tidy model infrastructure used by fable
  "tsibble",     # tidy temporal data structures
  "feasts",      # time-series features, decomposition, diagnostics
  "slider"       # rolling-window and time-indexed operations
)

nonstationarity_packages <- c(
  "urca",        # unit-root and cointegration tests
  "tseries",     # ADF, KPSS, and other time-series tests
  "fracdiff"     # fractional differencing and long-memory support
)

structural_break_packages <- c(
  "strucchange", # structural-change tests and breakpoints
  "changepoint", # change-point detection
  "bfast",       # break detection in additive seasonal/trend decomposition
  "segmented"    # segmented and piecewise regression
)

econometric_packages <- c(
  "vars",        # VAR and related multivariate time-series models
  "rugarch",     # univariate GARCH-family volatility models
  "MSwM",        # Markov-switching models
  "lmtest",      # diagnostic tests for linear/econometric models
  "sandwich"     # robust covariance estimators
)

hybrid_ml_packages <- c(
  "tidymodels",  # modelling framework
  "caret",       # legacy comparative ML workflow
  "glmnet",      # penalised regression
  "randomForest",# random forest models
  "xgboost",     # gradient boosting
  "e1071",       # SVM and other ML routines
  "Metrics",     # forecast/error metrics
  "yardstick"    # tidy model performance metrics
)

reporting_packages <- c(
  "ggplot2",     # graphics; included in tidyverse but kept explicit
  "patchwork",   # combine ggplot figures
  "scales",      # axes, labels, transformations
  "knitr",       # tables and Quarto execution support
  "kableExtra",  # formatted tables
  "gt",          # publication-style tables
  "modelsummary",# model tables and summaries
  "rmarkdown",   # document rendering support
  "quarto"       # Quarto CLI interface from R
)

workflow_packages <- c(
  "targets",     # reproducible analysis pipelines
  "tarchetypes"  # target helpers for reports and file workflows
)

required_packages <- unique(c(
  reproducibility_packages,
  data_packages,
  time_series_packages,
  nonstationarity_packages,
  structural_break_packages,
  econometric_packages,
  hybrid_ml_packages,
  reporting_packages
))

optional_packages <- unique(c(
  workflow_packages
))

# Packages usually attached for interactive analysis. Other packages should be
# called with pkg::function() to keep scripts explicit and less conflict-prone.
analysis_attach_packages <- c(
  "tidyverse",
  "lubridate",
  "tsibble",
  "fable",
  "feasts",
  "ggplot2"
)

# -----------------------------------------------------------------------------
# 3. Internal utilities
# -----------------------------------------------------------------------------

say <- function(...) {
  message(sprintf(...))
}

pkg_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

normalise_package_vector <- function(pkgs) {
  pkgs <- as.character(pkgs)
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  unique(pkgs)
}

missing_packages <- function(pkgs = required_packages) {
  pkgs <- normalise_package_vector(pkgs)
  pkgs[!vapply(pkgs, pkg_available, logical(1))]
}

# -----------------------------------------------------------------------------
# 4. Reproducibility bootstrap
# -----------------------------------------------------------------------------

ensure_renv <- function() {
  if (!pkg_available("renv")) {
    say("Installing renv because it is required for project reproducibility.")
    install.packages("renv", dependencies = TRUE)
  }

  if (!file.exists("renv.lock")) {
    say("No renv.lock file found. Run renv::init() once from the project root, then renv::snapshot().")
  } else {
    say("renv.lock found. Use renv::restore() to reproduce the locked library on a new machine.")
  }

  invisible(TRUE)
}

restore_from_lockfile <- function(prompt = TRUE) {
  ensure_renv()

  if (!file.exists("renv.lock")) {
    stop("Cannot restore: renv.lock was not found in the project root.", call. = FALSE)
  }

  renv::restore(prompt = prompt)
  invisible(TRUE)
}

snapshot_project <- function(prompt = TRUE) {
  ensure_renv()
  renv::snapshot(prompt = prompt)
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 5. Package installation and loading
# -----------------------------------------------------------------------------

ensure_packages <- function(pkgs = required_packages, use_renv = TRUE) {
  pkgs <- normalise_package_vector(pkgs)
  missing <- missing_packages(pkgs)

  if (length(missing) == 0) {
    say("All required packages are already available.")
    return(invisible(character(0)))
  }

  say("Installing missing packages: %s", paste(missing, collapse = ", "))

  if (isTRUE(use_renv)) {
    ensure_renv()
    renv::install(missing)
  } else {
    install.packages(missing, dependencies = TRUE)
  }

  invisible(missing)
}

# Backward-compatible name retained from the original script.
install_missing_packages <- ensure_packages

attach_analysis_packages <- function(pkgs = analysis_attach_packages) {
  pkgs <- normalise_package_vector(pkgs)
  missing <- missing_packages(pkgs)

  if (length(missing) > 0) {
    stop(
      "These packages are not installed: ", paste(missing, collapse = ", "),
      ". Run ensure_packages() first.",
      call. = FALSE
    )
  }

  invisible(lapply(pkgs, function(pkg) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }))

  set_conflict_preferences()
  say("Attached analysis packages: %s", paste(pkgs, collapse = ", "))
  invisible(pkgs)
}

# Backward-compatible name retained from the original script.
load_required_packages <- attach_analysis_packages

set_conflict_preferences <- function() {
  if (!pkg_available("conflicted")) {
    return(invisible(FALSE))
  }

  suppressPackageStartupMessages(library(conflicted))

  conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("lag", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("arrange", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("mutate", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("summarise", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("summarize", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("autoplot", "ggplot2", quiet = TRUE)

  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 6. Project-folder hygiene
# -----------------------------------------------------------------------------

create_project_dirs <- function(paths = c(
  "data/raw",
  "data/processed",
  "outputs/figures",
  "outputs/tables",
  "outputs/rendered",
  "logs"
)) {
  invisible(lapply(paths, function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }))

  # Keep raw-data folder structure visible without committing raw datasets.
  raw_keep <- file.path("data", "raw", ".gitkeep")
  if (dir.exists(dirname(raw_keep)) && !file.exists(raw_keep)) {
    file.create(raw_keep)
  }

  say("Checked project directories.")
  invisible(paths)
}

write_gitignore_safeguards <- function(gitignore_path = ".gitignore") {
  safeguards <- c(
    "# Local R session files",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# Keep raw or restricted data out of GitHub",
    "data/raw/*",
    "!data/raw/.gitkeep",
    "",
    "# Rendered outputs can be regenerated",
    "outputs/rendered/*",
    "",
    "# Local logs and temporary files",
    "logs/*",
    "*.log"
  )

  existing <- character(0)
  if (file.exists(gitignore_path)) {
    existing <- readLines(gitignore_path, warn = FALSE)
  }

  additions <- safeguards[!safeguards %in% existing]

  if (length(additions) > 0) {
    cat(paste0(additions, collapse = "\n"), file = gitignore_path, append = TRUE)
    cat("\n", file = gitignore_path, append = TRUE)
    say("Updated .gitignore safeguards.")
  } else {
    say(".gitignore safeguards already present.")
  }

  invisible(additions)
}

bootstrap_project <- function(create_dirs = TRUE, update_gitignore = TRUE) {
  ensure_renv()

  if (isTRUE(create_dirs)) {
    create_project_dirs()
  }

  if (isTRUE(update_gitignore)) {
    write_gitignore_safeguards()
  }

  say("Project bootstrap complete for %s.", project_name)
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 7. Reproducibility diagnostics
# -----------------------------------------------------------------------------

package_status <- function(pkgs = required_packages) {
  pkgs <- normalise_package_vector(pkgs)

  status <- data.frame(
    package = pkgs,
    installed = vapply(pkgs, pkg_available, logical(1)),
    version = vapply(pkgs, function(pkg) {
      if (pkg_available(pkg)) {
        as.character(utils::packageVersion(pkg))
      } else {
        NA_character_
      }
    }, character(1)),
    stringsAsFactors = FALSE
  )

  status
}

write_session_info <- function(path = file.path("outputs", "session_info.txt")) {
  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (pkg_available("sessioninfo")) {
    info <- capture.output(sessioninfo::session_info())
  } else {
    info <- capture.output(utils::sessionInfo())
  }

  writeLines(info, con = path)
  say("Session information written to %s", path)
  invisible(path)
}

# -----------------------------------------------------------------------------
# 8. Optional controlled auto-setup
# -----------------------------------------------------------------------------

if (identical(Sys.getenv("HYBRID_TS_AUTO_SETUP"), "true")) {
  bootstrap_project()
  ensure_packages()
  attach_analysis_packages()
}

say("Package setup functions loaded. Recommended next call: bootstrap_project(); ensure_packages().")

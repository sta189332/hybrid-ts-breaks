# -----------------------------------------------------------------------------
# File: R/01_sim_arma_baseline.R
# Project: hybrid-ts-breaks
# Purpose: Simulate a stationary ARMA baseline series with no structural break.
#
# This script defines a pure, reproducible baseline data-generating process for
# the hybrid-ts-breaks Quarto project. It is designed as the stable reference
# scenario against which non-stationary and structural-break simulations can be
# compared. The function avoids package attachment, validates inputs, checks ARMA
# admissibility, preserves the caller's RNG state when a seed is supplied, and
# returns audit-ready metadata for downstream Monte Carlo workflows.
# -----------------------------------------------------------------------------

#' Simulate a stationary ARMA baseline series with no structural break
#'
#' @description
#' Generates a univariate ARMA(p, q) series under a stable, no-break data-
#' generating process. The output is intentionally tidy and metadata-rich so it
#' can be combined with later scenarios involving structural breaks,
#' non-stationarity, forecasting models, and Monte Carlo replications.
#'
#' @param n Positive integer scalar. Number of observations retained after
#'   burn-in.
#' @param phi Numeric vector or `NULL`. Autoregressive coefficients. Use
#'   `numeric(0)` or `NULL` for no AR component.
#' @param theta Numeric vector or `NULL`. Moving-average coefficients. Use
#'   `numeric(0)` or `NULL` for no MA component.
#' @param sigma Positive finite numeric scalar. Innovation standard deviation.
#' @param seed Non-negative integer scalar or `NULL`. If supplied, the simulated
#'   series is reproducible and the caller's previous RNG state is restored on
#'   exit.
#' @param burnin Non-negative integer scalar. Number of initial observations to
#'   discard. Burn-in reduces dependence on initial conditions for ARMA models.
#' @param scenario_id Non-empty character scalar. Scenario identifier.
#' @param replicate_id Non-missing atomic scalar. Monte Carlo replicate label.
#' @param check_stationarity Logical scalar. If `TRUE`, reject non-stationary AR
#'   coefficient vectors.
#' @param check_invertibility Logical scalar. If `TRUE`, reject non-invertible MA
#'   coefficient vectors.
#'
#' @return
#' A base R data frame with one row per time point and columns suitable for
#' scenario stacking. The object also has a `simulation_parameters` attribute
#' that records the data-generating configuration.
#'
#' @examples
#' baseline <- simulate_arma_baseline(n = 250, phi = 0.6, theta = 0.3, seed = 1)
#' head(baseline)
#' attr(baseline, "simulation_parameters")
simulate_arma_baseline <- function(n = 1000L,
                                   phi = 0.6,
                                   theta = 0.3,
                                   sigma = 1,
                                   seed = 123L,
                                   burnin = 200L,
                                   scenario_id = "S1_ARMA_baseline",
                                   replicate_id = 1L,
                                   check_stationarity = TRUE,
                                   check_invertibility = TRUE) {
  .validate_arma_baseline_inputs(
    n = n,
    phi = phi,
    theta = theta,
    sigma = sigma,
    seed = seed,
    burnin = burnin,
    scenario_id = scenario_id,
    replicate_id = replicate_id,
    check_stationarity = check_stationarity,
    check_invertibility = check_invertibility
  )

  n <- as.integer(n)
  burnin <- as.integer(burnin)
  phi <- .normalise_arma_coefficients(phi)
  theta <- .normalise_arma_coefficients(theta)

  ar_order <- length(phi)
  ma_order <- length(theta)
  dgp_label <- sprintf("ARMA(%d,0,%d)", ar_order, ma_order)

  ar_is_stationary <- .is_stationary_ar(phi)
  ma_is_invertible <- .is_invertible_ma(theta)

  if (isTRUE(check_stationarity) && !ar_is_stationary) {
    stop(
      "The supplied AR coefficients are not stationary. ",
      "For a stationary AR process, all AR polynomial roots must lie outside ",
      "the unit circle.",
      call. = FALSE
    )
  }

  if (isTRUE(check_invertibility) && !ma_is_invertible) {
    stop(
      "The supplied MA coefficients are not invertible. ",
      "For an invertible MA process, all MA polynomial roots must lie outside ",
      "the unit circle.",
      call. = FALSE
    )
  }

  restore_rng <- .preserve_rng_state(seed)
  on.exit(restore_rng(), add = TRUE)

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }

  rng_kind_used <- RNGkind()

  y <- .simulate_arma_values(
    n = n,
    phi = phi,
    theta = theta,
    sigma = sigma,
    burnin = burnin
  )

  out <- data.frame(
    time = seq_len(n),
    y = y,
    scenario = scenario_id,
    replicate_id = replicate_id,
    true_break = NA_integer_,
    break_type = "none",
    regime = "stable",
    dgp = dgp_label,
    ar_order = ar_order,
    ma_order = ma_order,
    phi = .coefficients_to_text(phi),
    theta = .coefficients_to_text(theta),
    sigma = sigma,
    seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
    burnin = burnin,
    innovation_distribution = "Gaussian",
    stringsAsFactors = FALSE
  )

  attr(out, "simulation_parameters") <- list(
    project = "hybrid-ts-breaks",
    script = "R/01_sim_arma_baseline.R",
    scenario_id = scenario_id,
    replicate_id = replicate_id,
    dgp = dgp_label,
    n = n,
    ar_order = ar_order,
    ma_order = ma_order,
    phi = phi,
    theta = theta,
    sigma = sigma,
    seed = seed,
    burnin = burnin,
    break_type = "none",
    true_break = NA_integer_,
    regime = "stable",
    innovation_distribution = "Gaussian",
    generator = if (ar_order == 0L && ma_order == 0L) "stats::rnorm" else "stats::arima.sim",
    stationarity_checked = isTRUE(check_stationarity),
    invertibility_checked = isTRUE(check_invertibility),
    ar_stationary = ar_is_stationary,
    ma_invertible = ma_is_invertible,
    rng_kind = rng_kind_used,
    r_version = R.version.string,
    stats_version = as.character(utils::packageVersion("stats"))
  )

  out
}

# -----------------------------------------------------------------------------
# Internal simulation helper
# -----------------------------------------------------------------------------

.simulate_arma_values <- function(n, phi, theta, sigma, burnin) {
  total_n <- n + burnin

  if (length(phi) == 0L && length(theta) == 0L) {
    values <- stats::rnorm(total_n, mean = 0, sd = sigma)
    return(as.numeric(utils::tail(values, n)))
  }

  model <- list(order = c(length(phi), 0L, length(theta)))

  if (length(phi) > 0L) {
    model$ar <- phi
  }

  if (length(theta) > 0L) {
    model$ma <- theta
  }

  as.numeric(
    stats::arima.sim(
      model = model,
      n = n,
      n.start = burnin,
      sd = sigma
    )
  )
}

# -----------------------------------------------------------------------------
# Internal validation helpers
# -----------------------------------------------------------------------------

.validate_arma_baseline_inputs <- function(n,
                                          phi,
                                          theta,
                                          sigma,
                                          seed,
                                          burnin,
                                          scenario_id,
                                          replicate_id,
                                          check_stationarity,
                                          check_invertibility) {
  .validate_integerish_scalar(n, "n", lower = 1L)
  .validate_coefficient_vector(phi, "phi")
  .validate_coefficient_vector(theta, "theta")
  .validate_positive_numeric_scalar(sigma, "sigma")

  if (!is.null(seed)) {
    .validate_integerish_scalar(seed, "seed", lower = 0L)
  }

  .validate_integerish_scalar(burnin, "burnin", lower = 0L)
  .validate_character_scalar(scenario_id, "scenario_id")
  .validate_atomic_scalar(replicate_id, "replicate_id")
  .validate_logical_scalar(check_stationarity, "check_stationarity")
  .validate_logical_scalar(check_invertibility, "check_invertibility")

  invisible(TRUE)
}

.validate_integerish_scalar <- function(x,
                                        name,
                                        lower = -.Machine$integer.max,
                                        upper = .Machine$integer.max) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("`%s` must be a finite numeric scalar.", name), call. = FALSE)
  }

  if (x != floor(x)) {
    stop(sprintf("`%s` must be integer-valued.", name), call. = FALSE)
  }

  if (x < lower || x > upper) {
    stop(
      sprintf(
        "`%s` must be between %s and %s.",
        name,
        format(lower, scientific = FALSE),
        format(upper, scientific = FALSE)
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.validate_coefficient_vector <- function(x, name) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be a numeric vector or `NULL`.", name), call. = FALSE)
  }

  if (length(x) > 0L && anyNA(x)) {
    stop(sprintf("`%s` must not contain missing values.", name), call. = FALSE)
  }

  if (length(x) > 0L && any(!is.finite(x))) {
    stop(sprintf("`%s` must contain only finite values.", name), call. = FALSE)
  }

  invisible(TRUE)
}

.validate_positive_numeric_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop(sprintf("`%s` must be a positive finite numeric scalar.", name), call. = FALSE)
  }

  invisible(TRUE)
}

.validate_character_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a non-empty character scalar.", name), call. = FALSE)
  }

  invisible(TRUE)
}

.validate_atomic_scalar <- function(x, name) {
  if (!is.atomic(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a non-missing atomic scalar.", name), call. = FALSE)
  }

  invisible(TRUE)
}

.validate_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be `TRUE` or `FALSE`.", name), call. = FALSE)
  }

  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Internal ARMA diagnostics
# -----------------------------------------------------------------------------

.normalise_arma_coefficients <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }

  as.numeric(x)
}

.is_stationary_ar <- function(phi, tolerance = sqrt(.Machine$double.eps)) {
  if (length(phi) == 0L) {
    return(TRUE)
  }

  roots <- polyroot(c(1, -phi))
  all(Mod(roots) > 1 + tolerance)
}

.is_invertible_ma <- function(theta, tolerance = sqrt(.Machine$double.eps)) {
  if (length(theta) == 0L) {
    return(TRUE)
  }

  roots <- polyroot(c(1, theta))
  all(Mod(roots) > 1 + tolerance)
}

.coefficients_to_text <- function(x) {
  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(format(x, digits = 15, scientific = FALSE, trim = TRUE), collapse = ",")
}

# -----------------------------------------------------------------------------
# Internal RNG helper
# -----------------------------------------------------------------------------

.preserve_rng_state <- function(seed) {
  if (is.null(seed)) {
    return(function() invisible(TRUE))
  }

  had_rng <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  if (had_rng) {
    old_rng <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  function() {
    if (had_rng) {
      assign(".Random.seed", old_rng, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }

    invisible(TRUE)
  }
}

# -----------------------------------------------------------------------------
# Optional smoke tests
# -----------------------------------------------------------------------------
# These tests are intentionally opt-in so that sourcing this file has no runtime
# side effects during Quarto rendering or package-like workflow execution.
# Run manually with:
#   Sys.setenv(HYBRID_TS_RUN_SMOKE_TESTS = "true")
#   source("R/01_sim_arma_baseline.R")

if (identical(Sys.getenv("HYBRID_TS_RUN_SMOKE_TESTS"), "true")) {
  .baseline_smoke <- simulate_arma_baseline(n = 50L, seed = 123L)

  stopifnot(
    is.data.frame(.baseline_smoke),
    nrow(.baseline_smoke) == 50L,
    all(.baseline_smoke$break_type == "none"),
    all(.baseline_smoke$regime == "stable"),
    all(is.na(.baseline_smoke$true_break)),
    !is.null(attr(.baseline_smoke, "simulation_parameters"))
  )

  rm(.baseline_smoke)
}

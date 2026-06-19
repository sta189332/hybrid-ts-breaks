# R/02_sim_random_walk_trend_unitroot.R
# ------------------------------------------------------------------------------
# Purpose:
#   Define reproducible data-generating processes (DGPs) for random walks,
#   deterministic trends, stochastic trends, and unit-root behaviour used in the
#   hybrid-ts-breaks Quarto project.
#
# Design principles:
#   - This file defines functions only; it should not create project outputs as a
#     side effect when sourced from Quarto or another R script.
#   - Simulations are deterministic by default through explicit seeds.
#   - The returned data frames share a common schema so scenarios can be combined,
#     benchmarked, plotted, or passed into later structural-break workflows.
#   - No non-base package dependency is introduced.
# ------------------------------------------------------------------------------

# ---- Internal validation helpers ---------------------------------------------

.hybrid_validate_positive_integer <- function(x, name, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop("`", name, "` must be a single finite numeric value.", call. = FALSE)
  }

  if (abs(x - round(x)) > sqrt(.Machine$double.eps)) {
    stop("`", name, "` must be an integer-valued number.", call. = FALSE)
  }

  x <- as.integer(round(x))

  if (x < minimum) {
    stop("`", name, "` must be at least ", minimum, ".", call. = FALSE)
  }

  x
}

.hybrid_validate_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop("`", name, "` must be a single finite numeric value.", call. = FALSE)
  }

  as.numeric(x)
}

.hybrid_validate_sigma <- function(sigma) {
  sigma <- .hybrid_validate_scalar(sigma, "sigma")

  if (sigma <= 0) {
    stop("`sigma` must be strictly positive.", call. = FALSE)
  }

  sigma
}

.hybrid_validate_phi <- function(phi) {
  phi <- .hybrid_validate_scalar(phi, "phi")

  if (abs(phi) >= 1) {
    stop(
      "`phi` must satisfy abs(phi) < 1 for the trend-stationary AR(1) error.",
      call. = FALSE
    )
  }

  phi
}

.hybrid_validate_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }

  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed)) {
    stop("`seed` must be NULL or a single finite integer-valued number.", call. = FALSE)
  }

  if (abs(seed - round(seed)) > sqrt(.Machine$double.eps)) {
    stop("`seed` must be integer-valued.", call. = FALSE)
  }

  as.integer(round(seed))
}

.hybrid_seed_lookup <- function(seeds, name, position) {
  if (is.null(seeds)) {
    return(NULL)
  }

  if (!is.null(names(seeds)) && name %in% names(seeds)) {
    return(.hybrid_validate_seed(seeds[[name]]))
  }

  if (length(seeds) >= position) {
    return(.hybrid_validate_seed(seeds[[position]]))
  }

  stop("`seeds` must contain a seed for `", name, "`.", call. = FALSE)
}

.hybrid_with_seed <- function(seed, expr) {
  seed <- .hybrid_validate_seed(seed)

  if (is.null(seed)) {
    return(force(expr))
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }

  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(expr)
}

.hybrid_make_simulation_frame <- function(
  time,
  y,
  innovation,
  deterministic_component,
  stochastic_component,
  scenario,
  process_class,
  break_type,
  seed,
  sigma,
  alpha = NA_real_,
  beta = NA_real_,
  phi = NA_real_,
  mu = NA_real_,
  y0 = NA_real_,
  burn_in = NA_integer_,
  model_equation
) {
  n <- length(y)

  data.frame(
    time = time,
    y = as.numeric(y),
    scenario = scenario,
    true_break = NA_integer_,
    break_type = break_type,
    process_class = process_class,
    innovation = as.numeric(innovation),
    deterministic_component = as.numeric(deterministic_component),
    stochastic_component = as.numeric(stochastic_component),
    n = as.integer(n),
    seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
    sigma = as.numeric(sigma),
    alpha = as.numeric(alpha),
    beta = as.numeric(beta),
    phi = as.numeric(phi),
    mu = as.numeric(mu),
    y0 = as.numeric(y0),
    burn_in = as.integer(burn_in),
    model_equation = model_equation,
    stringsAsFactors = FALSE
  )
}


# ---- Scenario S2: random walk / stochastic trend -----------------------------
# DGP:
#   y_t = y_{t-1} + e_t,  e_t ~ N(0, sigma^2)
#
# Interpretation:
#   A random walk is non-stationary and contains a stochastic trend. It provides
#   the simplest unit-root benchmark without deterministic drift.

simulate_random_walk <- function(n = 1000, sigma = 1, seed = 123, y0 = 0) {
  n <- .hybrid_validate_positive_integer(n, "n", minimum = 2L)
  sigma <- .hybrid_validate_sigma(sigma)
  seed <- .hybrid_validate_seed(seed)
  y0 <- .hybrid_validate_scalar(y0, "y0")

  time <- seq_len(n)
  innovation <- .hybrid_with_seed(seed, stats::rnorm(n, mean = 0, sd = sigma))
  stochastic_component <- cumsum(innovation)
  deterministic_component <- rep(y0, n)
  y <- deterministic_component + stochastic_component

  .hybrid_make_simulation_frame(
    time = time,
    y = y,
    innovation = innovation,
    deterministic_component = deterministic_component,
    stochastic_component = stochastic_component,
    scenario = "S2_random_walk",
    process_class = "I(1)_stochastic_trend_without_drift",
    break_type = "unit_root",
    seed = seed,
    sigma = sigma,
    y0 = y0,
    model_equation = "y[t] = y[t-1] + e[t]"
  )
}


# ---- Scenario S3: trend-stationary process -----------------------------------
# DGP:
#   y_t = alpha + beta * t + u_t
#   u_t = phi * u_{t-1} + e_t,  |phi| < 1
#
# Interpretation:
#   The observed series has a deterministic trend, but deviations from that
#   deterministic trend are stationary when |phi| < 1. This scenario is useful
#   for contrasting deterministic non-stationarity with unit-root behaviour.

simulate_trend_stationary <- function(
  n = 1000,
  alpha = 0,
  beta = 0.05,
  phi = 0.6,
  sigma = 1,
  seed = 123,
  burn_in = 100
) {
  n <- .hybrid_validate_positive_integer(n, "n", minimum = 2L)
  alpha <- .hybrid_validate_scalar(alpha, "alpha")
  beta <- .hybrid_validate_scalar(beta, "beta")
  phi <- .hybrid_validate_phi(phi)
  sigma <- .hybrid_validate_sigma(sigma)
  seed <- .hybrid_validate_seed(seed)
  burn_in <- .hybrid_validate_positive_integer(burn_in, "burn_in", minimum = 1L)

  time <- seq_len(n)

  innovation <- .hybrid_with_seed(
    seed,
    as.numeric(stats::arima.sim(
      model = list(ar = phi),
      n = n,
      n.start = burn_in,
      sd = sigma
    ))
  )

  deterministic_component <- alpha + beta * time
  stochastic_component <- innovation
  y <- deterministic_component + stochastic_component

  .hybrid_make_simulation_frame(
    time = time,
    y = y,
    innovation = innovation,
    deterministic_component = deterministic_component,
    stochastic_component = stochastic_component,
    scenario = "S3_trend_stationary",
    process_class = "trend_stationary_with_AR1_errors",
    break_type = "deterministic_trend",
    seed = seed,
    sigma = sigma,
    alpha = alpha,
    beta = beta,
    phi = phi,
    burn_in = burn_in,
    model_equation = "y[t] = alpha + beta*t + u[t]; u[t] = phi*u[t-1] + e[t]"
  )
}


# ---- Scenario S4: unit root with drift ---------------------------------------
# DGP:
#   y_t = y_{t-1} + mu + e_t,  e_t ~ N(0, sigma^2)
#
# Interpretation:
#   This process is non-stationary because of the unit root. The expected path
#   changes with deterministic drift, while shocks accumulate permanently.

simulate_unit_root_drift <- function(
  n = 1000,
  mu = 0.05,
  sigma = 1,
  seed = 123,
  y0 = 0
) {
  n <- .hybrid_validate_positive_integer(n, "n", minimum = 2L)
  mu <- .hybrid_validate_scalar(mu, "mu")
  sigma <- .hybrid_validate_sigma(sigma)
  seed <- .hybrid_validate_seed(seed)
  y0 <- .hybrid_validate_scalar(y0, "y0")

  time <- seq_len(n)
  innovation <- .hybrid_with_seed(seed, stats::rnorm(n, mean = 0, sd = sigma))
  stochastic_component <- cumsum(innovation)
  deterministic_component <- y0 + mu * time
  y <- y0 + cumsum(mu + innovation)

  .hybrid_make_simulation_frame(
    time = time,
    y = y,
    innovation = innovation,
    deterministic_component = deterministic_component,
    stochastic_component = stochastic_component,
    scenario = "S4_unit_root_drift",
    process_class = "I(1)_stochastic_trend_with_drift",
    break_type = "unit_root_with_drift",
    seed = seed,
    sigma = sigma,
    mu = mu,
    y0 = y0,
    model_equation = "y[t] = y[t-1] + mu + e[t]"
  )
}


# ---- Combined scenario generator --------------------------------------------
# This wrapper is optional but useful in Quarto reports because it returns a
# single long-format data frame with consistent columns across the non-stationary
# and trend-stationary benchmark scenarios.

simulate_random_walk_trend_unitroot_suite <- function(
  n = 1000,
  sigma = 1,
  seeds = c(
    random_walk = 123,
    trend_stationary = 124,
    unit_root_drift = 125
  ),
  alpha = 0,
  beta = 0.05,
  phi = 0.6,
  mu = 0.05,
  y0 = 0,
  burn_in = 100
) {
  random_walk_seed <- .hybrid_seed_lookup(seeds, "random_walk", 1L)
  trend_stationary_seed <- .hybrid_seed_lookup(seeds, "trend_stationary", 2L)
  unit_root_drift_seed <- .hybrid_seed_lookup(seeds, "unit_root_drift", 3L)

  simulations <- list(
    simulate_random_walk(
      n = n,
      sigma = sigma,
      seed = random_walk_seed,
      y0 = y0
    ),
    simulate_trend_stationary(
      n = n,
      alpha = alpha,
      beta = beta,
      phi = phi,
      sigma = sigma,
      seed = trend_stationary_seed,
      burn_in = burn_in
    ),
    simulate_unit_root_drift(
      n = n,
      mu = mu,
      sigma = sigma,
      seed = unit_root_drift_seed,
      y0 = y0
    )
  )

  output <- do.call(rbind, simulations)
  row.names(output) <- NULL
  output
}


# ---- Example use in Quarto ---------------------------------------------------
# Source this file from a Quarto document or a driver script, then call:
#
#   source("R/02_sim_random_walk_trend_unitroot.R")
#   sim_02 <- simulate_random_walk_trend_unitroot_suite(n = 1000)
#
# Keep plotting, saving, and downstream diagnostics in the calling script or
# Quarto document so this file remains reusable and side-effect free.

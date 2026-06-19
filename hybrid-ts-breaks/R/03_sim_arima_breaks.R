# 03_sim_arima_breaks.R
# hybrid-ts-breaks Quarto project
#
# Purpose:
#   Simulate ARIMA-type time-series processes with known structural breaks in
#   intercept/level, innovation variance, or both. The script is designed for
#   reproducible doctoral-level work on forecasting, non-stationarity,
#   structural breaks, and simulation-based methodological explanation.
#
# Design principles:
#   1. Keep all simulation parameters explicit and inspectable.
#   2. Preserve the true break date and regime labels in every returned data set.
#   3. Use deterministic seeds so that Quarto renders are reproducible.
#   4. Avoid hidden package dependencies. Only base R packages stats and graphics are required.
#   5. Return tidy data frames suitable for plotting, diagnostics, tables,
#      model-fitting exercises, and manuscript figures.
#
# Technical note:
#   For d = 0, the returned series y is the simulated ARMA process with a
#   possible intercept and/or variance break. For d > 0, the stationary ARMA
#   component is integrated d times. In that case, intercept changes operate as
#   drift changes in the differenced process and may induce changes in trend
#   behaviour on the observed scale.

# -----------------------------------------------------------------------------
# 0. Dependency discipline
# -----------------------------------------------------------------------------

check_arima_break_dependencies <- function() {
  required_packages <- c("stats", "graphics")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required package(s): ", paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 1. Validation helpers
# -----------------------------------------------------------------------------

check_scalar_integer <- function(x, name, lower = NULL, upper = NULL) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != as.integer(x)) {
    stop(name, " must be a single whole-number numeric value.", call. = FALSE)
  }

  if (!is.null(lower) && x < lower) {
    stop(name, " must be >= ", lower, ".", call. = FALSE)
  }

  if (!is.null(upper) && x > upper) {
    stop(name, " must be <= ", upper, ".", call. = FALSE)
  }

  as.integer(x)
}

check_scalar_numeric <- function(x, name, lower = NULL, upper = NULL, strictly_positive = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(name, " must be a single finite numeric value.", call. = FALSE)
  }

  if (strictly_positive && x <= 0) {
    stop(name, " must be strictly positive.", call. = FALSE)
  }

  if (!is.null(lower) && x < lower) {
    stop(name, " must be >= ", lower, ".", call. = FALSE)
  }

  if (!is.null(upper) && x > upper) {
    stop(name, " must be <= ", upper, ".", call. = FALSE)
  }

  x
}

check_numeric_vector <- function(x, name) {
  if (length(x) == 0L) {
    return(numeric(0))
  }

  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop(name, " must be a numeric vector with finite, non-missing values.", call. = FALSE)
  }

  as.numeric(x)
}

validate_break_point <- function(n, Tb) {
  n <- check_scalar_integer(n, "n", lower = 20)
  Tb <- check_scalar_integer(Tb, "Tb", lower = 2, upper = n - 1L)

  if (Tb < 0.1 * n || Tb > 0.9 * n) {
    warning(
      "The break point Tb is close to the sample boundary. ",
      "This is allowed, but break detection and estimation exercises may become unstable.",
      call. = FALSE
    )
  }

  list(n = n, Tb = Tb)
}

validate_ar_stationarity <- function(ar) {
  ar <- check_numeric_vector(ar, "ar")

  if (length(ar) == 0L) {
    return(invisible(TRUE))
  }

  roots <- polyroot(c(1, -ar))

  if (any(Mod(roots) <= 1)) {
    stop(
      "The AR coefficients imply a non-stationary ARMA component. ",
      "Use stationary AR coefficients before applying integration through d.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_ma_invertibility <- function(ma) {
  ma <- check_numeric_vector(ma, "ma")

  if (length(ma) == 0L) {
    return(invisible(TRUE))
  }

  roots <- polyroot(c(1, ma))

  if (any(Mod(roots) <= 1)) {
    warning(
      "The MA coefficients may imply a non-invertible MA component. ",
      "The simulation will proceed, but estimation exercises may be harder to interpret.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

make_regime_labels <- function(n, Tb) {
  ifelse(seq_len(n) <= Tb, "pre_break", "post_break")
}

integrate_series <- function(x, d = 0L) {
  d <- check_scalar_integer(d, "d", lower = 0)

  if (d == 0L) {
    return(x)
  }

  y <- x
  for (i in seq_len(d)) {
    y <- cumsum(y)
  }

  y
}

# -----------------------------------------------------------------------------
# 2. Core ARIMA structural-break simulator
# -----------------------------------------------------------------------------

simulate_arima_break <- function(n = 1000,
                                 Tb = 600,
                                 ar = 0.6,
                                 d = 0L,
                                 ma = numeric(0),
                                 intercept_pre = 0,
                                 intercept_post = 2,
                                 sigma_pre = 1,
                                 sigma_post = 1,
                                 break_type = c("intercept", "variance", "intercept_variance"),
                                 burnin = 200,
                                 seed = 123,
                                 scenario = NULL,
                                 parameterisation = "intercept_or_drift") {
  check_arima_break_dependencies()

  checked <- validate_break_point(n, Tb)
  n <- checked$n
  Tb <- checked$Tb

  ar <- check_numeric_vector(ar, "ar")
  ma <- check_numeric_vector(ma, "ma")
  d <- check_scalar_integer(d, "d", lower = 0, upper = 2)
  burnin <- check_scalar_integer(burnin, "burnin", lower = 0)
  seed <- check_scalar_integer(seed, "seed", lower = 0)

  intercept_pre <- check_scalar_numeric(intercept_pre, "intercept_pre")
  intercept_post <- check_scalar_numeric(intercept_post, "intercept_post")
  sigma_pre <- check_scalar_numeric(sigma_pre, "sigma_pre", strictly_positive = TRUE)
  sigma_post <- check_scalar_numeric(sigma_post, "sigma_post", strictly_positive = TRUE)

  validate_ar_stationarity(ar)
  validate_ma_invertibility(ma)

  break_type <- match.arg(break_type)

  if (is.null(scenario)) {
    scenario <- paste0(
      "ARIMA_", length(ar), "_", d, "_", length(ma), "_", break_type, "_break"
    )
  }

  if (!is.character(parameterisation) || length(parameterisation) != 1L || is.na(parameterisation)) {
    stop("parameterisation must be a single non-missing character value.", call. = FALSE)
  }

  total_n <- n + burnin
  adjusted_break <- Tb + burnin
  total_time <- seq_len(total_n)

  intercept_t <- rep(intercept_pre, total_n)
  sigma_t <- rep(sigma_pre, total_n)

  if (break_type %in% c("intercept", "intercept_variance")) {
    intercept_t[total_time > adjusted_break] <- intercept_post
  }

  if (break_type %in% c("variance", "intercept_variance")) {
    sigma_t[total_time > adjusted_break] <- sigma_post
  }

  set.seed(seed)
  innovation <- stats::rnorm(total_n, mean = 0, sd = sigma_t)
  arma_component <- numeric(total_n)

  p <- length(ar)
  q <- length(ma)
  start_index <- max(p, q) + 1L

  for (t in seq.int(start_index, total_n)) {
    ar_part <- if (p > 0L) {
      sum(ar * arma_component[t - seq_len(p)])
    } else {
      0
    }

    ma_part <- if (q > 0L) {
      sum(ma * innovation[t - seq_len(q)])
    } else {
      0
    }

    arma_component[t] <- intercept_t[t] + ar_part + innovation[t] + ma_part
  }

  keep <- seq.int(burnin + 1L, total_n)
  arma_kept <- arma_component[keep]
  y <- integrate_series(arma_kept, d = d)

  data.frame(
    time = seq_len(n),
    y = as.numeric(y),
    arma_component = as.numeric(arma_kept),
    innovation = as.numeric(innovation[keep]),
    scenario = scenario,
    true_break = Tb,
    break_type = break_type,
    regime = make_regime_labels(n, Tb),
    intercept = as.numeric(intercept_t[keep]),
    sigma = as.numeric(sigma_t[keep]),
    ar_order = p,
    differencing_order = d,
    ma_order = q,
    seed = seed,
    parameterisation = parameterisation,
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# 3. Backward-compatible scenario functions
# -----------------------------------------------------------------------------

simulate_ar_mean_break <- function(n = 1000,
                                   Tb = 600,
                                   phi = 0.6,
                                   mu1 = 0,
                                   mu2 = 2,
                                   sigma = 1,
                                   seed = 123,
                                   burnin = 200,
                                   mean_parameterisation = c("intercept", "unconditional_mean")) {
  mean_parameterisation <- match.arg(mean_parameterisation)
  phi <- check_scalar_numeric(phi, "phi")

  validate_ar_stationarity(phi)

  if (mean_parameterisation == "unconditional_mean") {
    intercept_pre <- mu1 * (1 - phi)
    intercept_post <- mu2 * (1 - phi)
  } else {
    intercept_pre <- mu1
    intercept_post <- mu2
  }

  out <- simulate_arima_break(
    n = n,
    Tb = Tb,
    ar = phi,
    d = 0L,
    ma = numeric(0),
    intercept_pre = intercept_pre,
    intercept_post = intercept_post,
    sigma_pre = sigma,
    sigma_post = sigma,
    break_type = "intercept",
    burnin = burnin,
    seed = seed,
    scenario = "S5_AR_mean_break",
    parameterisation = mean_parameterisation
  )

  out
}

simulate_ar_variance_break <- function(n = 1000,
                                       Tb = 600,
                                       phi = 0.6,
                                       sigma1 = 1,
                                       sigma2 = 3,
                                       seed = 123,
                                       burnin = 200) {
  simulate_arima_break(
    n = n,
    Tb = Tb,
    ar = phi,
    d = 0L,
    ma = numeric(0),
    intercept_pre = 0,
    intercept_post = 0,
    sigma_pre = sigma1,
    sigma_post = sigma2,
    break_type = "variance",
    burnin = burnin,
    seed = seed,
    scenario = "S6_AR_variance_break",
    parameterisation = "zero_intercept_variance_break"
  )
}

simulate_arima_drift_break <- function(n = 1000,
                                       Tb = 600,
                                       ar = 0.3,
                                       d = 1L,
                                       ma = numeric(0),
                                       drift1 = 0,
                                       drift2 = 0.15,
                                       sigma = 1,
                                       seed = 123,
                                       burnin = 200) {
  simulate_arima_break(
    n = n,
    Tb = Tb,
    ar = ar,
    d = d,
    ma = ma,
    intercept_pre = drift1,
    intercept_post = drift2,
    sigma_pre = sigma,
    sigma_post = sigma,
    break_type = "intercept",
    burnin = burnin,
    seed = seed,
    scenario = "S7_ARIMA_drift_break",
    parameterisation = "drift_in_differenced_process"
  )
}

simulate_arima_intercept_variance_break <- function(n = 1000,
                                                    Tb = 600,
                                                    ar = 0.5,
                                                    d = 0L,
                                                    ma = -0.3,
                                                    intercept1 = 0,
                                                    intercept2 = 1.5,
                                                    sigma1 = 1,
                                                    sigma2 = 2,
                                                    seed = 123,
                                                    burnin = 200) {
  simulate_arima_break(
    n = n,
    Tb = Tb,
    ar = ar,
    d = d,
    ma = ma,
    intercept_pre = intercept1,
    intercept_post = intercept2,
    sigma_pre = sigma1,
    sigma_post = sigma2,
    break_type = "intercept_variance",
    burnin = burnin,
    seed = seed,
    scenario = "S8_ARMA_intercept_variance_break",
    parameterisation = "intercept_and_variance_break"
  )
}

# -----------------------------------------------------------------------------
# 4. Scenario builder for Quarto chapters and simulation appendices
# -----------------------------------------------------------------------------

build_arima_break_scenarios <- function(n = 1000,
                                        Tb = 600,
                                        base_seed = 123) {
  check_scalar_integer(base_seed, "base_seed")

  scenarios <- list(
    simulate_ar_mean_break(
      n = n,
      Tb = Tb,
      phi = 0.6,
      mu1 = 0,
      mu2 = 2,
      sigma = 1,
      seed = base_seed + 1L,
      mean_parameterisation = "intercept"
    ),
    simulate_ar_variance_break(
      n = n,
      Tb = Tb,
      phi = 0.6,
      sigma1 = 1,
      sigma2 = 3,
      seed = base_seed + 2L
    ),
    simulate_arima_drift_break(
      n = n,
      Tb = Tb,
      ar = 0.3,
      d = 1L,
      ma = numeric(0),
      drift1 = 0,
      drift2 = 0.15,
      sigma = 1,
      seed = base_seed + 3L
    ),
    simulate_arima_intercept_variance_break(
      n = n,
      Tb = Tb,
      ar = 0.5,
      d = 0L,
      ma = -0.3,
      intercept1 = 0,
      intercept2 = 1.5,
      sigma1 = 1,
      sigma2 = 2,
      seed = base_seed + 4L
    )
  )

  do.call(rbind, scenarios)
}

# -----------------------------------------------------------------------------
# 5. Lightweight summaries and plotting helpers
# -----------------------------------------------------------------------------

summarise_arima_break_scenario <- function(sim_data) {
  required_columns <- c("scenario", "regime", "y", "true_break", "break_type")
  missing_columns <- setdiff(required_columns, names(sim_data))

  if (length(missing_columns) > 0L) {
    stop(
      "sim_data is missing required column(s): ", paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  split_data <- split(sim_data$y, list(sim_data$scenario, sim_data$regime), drop = TRUE)

  summary_list <- lapply(names(split_data), function(group_name) {
    values <- split_data[[group_name]]
    group_parts <- strsplit(group_name, "\\.", fixed = FALSE)[[1]]

    data.frame(
      scenario = group_parts[1],
      regime = group_parts[2],
      n = length(values),
      mean_y = mean(values),
      sd_y = stats::sd(values),
      min_y = min(values),
      max_y = max(values),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summary_list)
}

plot_arima_break_base <- function(sim_data,
                                  scenario = NULL,
                                  main = NULL,
                                  ylab = "Simulated value") {
  required_columns <- c("time", "y", "scenario", "true_break")
  missing_columns <- setdiff(required_columns, names(sim_data))

  if (length(missing_columns) > 0L) {
    stop(
      "sim_data is missing required column(s): ", paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(scenario)) {
    sim_data <- sim_data[sim_data$scenario == scenario, , drop = FALSE]

    if (nrow(sim_data) == 0L) {
      stop("No rows found for scenario: ", scenario, call. = FALSE)
    }
  }

  unique_scenarios <- unique(sim_data$scenario)
  if (length(unique_scenarios) > 1L) {
    stop(
      "plot_arima_break_base() expects one scenario at a time. ",
      "Use the scenario argument or subset the data before plotting.",
      call. = FALSE
    )
  }

  if (is.null(main)) {
    main <- unique_scenarios
  }

  graphics::plot(
    sim_data$time,
    sim_data$y,
    type = "l",
    xlab = "Time",
    ylab = ylab,
    main = main
  )

  graphics::abline(v = unique(sim_data$true_break), lty = 2)
  invisible(sim_data)
}

# -----------------------------------------------------------------------------
# 6. Example Quarto-safe usage
# -----------------------------------------------------------------------------

# The following commands are intentionally commented out so that sourcing this
# file defines functions without producing side effects during a Quarto render.
# Uncomment inside a controlled Quarto chunk when examples are needed.
#
# arima_break_data <- build_arima_break_scenarios(n = 1000, Tb = 600, base_seed = 2026)
# head(arima_break_data)
# summarise_arima_break_scenario(arima_break_data)
# plot_arima_break_base(arima_break_data, scenario = "S5_AR_mean_break")

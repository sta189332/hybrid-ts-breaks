# ==============================================================================
# R/04_sim_garch_breaks.R
# hybrid-ts-breaks
#
# Purpose:
#   Simulate AR(1)-GARCH(1,1)-type time series with an explicitly documented
#   structural break in the volatility-generating mechanism.
#
# Research use:
#   This script supports Chapter 4 Monte Carlo simulation work for the thesis
#   "Hybrid Time Series Forecasting Under Non-stationarity and Structural Breaks".
#   It is designed as a transparent benchmark generator for testing whether
#   forecasting and break-detection methods can respond to volatility-regime
#   changes without confusing conditional variance, innovations, and observed
#   returns.
#
# Reproducibility policy:
#   - The main simulator uses deterministic seed control.
#   - The seed is restored after simulation to avoid contaminating the wider
#     Quarto session.
#   - No simulation is run automatically when this file is sourced.
#   - Output writing is explicit and opt-in.
#
# Methodological note:
#   Within each regime, the conditional variance follows
#
#     h_t = omega + alpha1 * epsilon_{t-1}^2 + beta1 * h_{t-1},
#
#   with epsilon_t = sqrt(h_t) * z_t and y_t = mu + ar1 * y_{t-1} + epsilon_t.
#   The structural break is introduced by changing the GARCH parameters after a
#   known break point. The default design increases both the variance intercept
#   and short-run shock sensitivity while retaining alpha1 + beta1 < 1 in each
#   regime.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Default configuration
# ------------------------------------------------------------------------------

garch_break_config <- list(
  scenario = "S7_GARCH_volatility_break",
  n_pre_break = 600L,
  n_post_break = 400L,
  burn_in = 500L,
  seed = 123L,
  innovation = "normal",
  innovation_df = 8,

  pre_break = list(
    label = "low_volatility",
    mu = 0,
    ar1 = 0.20,
    omega = 0.010,
    alpha1 = 0.050,
    beta1 = 0.900
  ),

  post_break = list(
    label = "high_volatility",
    mu = 0,
    ar1 = 0.20,
    omega = 0.080,
    alpha1 = 0.120,
    beta1 = 0.800
  ),

  break_type = "volatility_parameter_break",
  break_description = paste(
    "Known structural break from a lower-volatility GARCH regime",
    "to a higher-volatility GARCH regime."
  ),

  reset_variance_at_break = FALSE
)


# ------------------------------------------------------------------------------
# 2. Validation utilities
# ------------------------------------------------------------------------------

assert_scalar_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(name, " must be a single finite numeric value.", call. = FALSE)
  }

  invisible(TRUE)
}


assert_positive_integer <- function(x, name, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < minimum || x != as.integer(x)) {
    stop(
      name,
      " must be a single integer greater than or equal to ",
      minimum,
      ".",
      call. = FALSE
    )
  }

  as.integer(x)
}


validate_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }

  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != as.integer(seed)) {
    stop("seed must be NULL or a single integer.", call. = FALSE)
  }

  as.integer(seed)
}


validate_innovation_settings <- function(innovation, innovation_df) {
  innovation <- match.arg(innovation, choices = c("normal", "student_t"))

  if (identical(innovation, "student_t")) {
    assert_scalar_numeric(innovation_df, "innovation_df")

    if (innovation_df <= 2) {
      stop(
        "innovation_df must be greater than 2 for a standardised Student-t innovation.",
        call. = FALSE
      )
    }
  }

  list(
    innovation = innovation,
    innovation_df = innovation_df
  )
}


validate_garch_regime <- function(regime, regime_name, allow_nonstationary = FALSE) {
  required_names <- c("label", "mu", "ar1", "omega", "alpha1", "beta1")
  missing_names <- setdiff(required_names, names(regime))

  if (length(missing_names) > 0L) {
    stop(
      regime_name,
      " is missing required field(s): ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.character(regime$label) || length(regime$label) != 1L || !nzchar(regime$label)) {
    stop(regime_name, "$label must be a single non-empty character value.", call. = FALSE)
  }

  assert_scalar_numeric(regime$mu, paste0(regime_name, "$mu"))
  assert_scalar_numeric(regime$ar1, paste0(regime_name, "$ar1"))
  assert_scalar_numeric(regime$omega, paste0(regime_name, "$omega"))
  assert_scalar_numeric(regime$alpha1, paste0(regime_name, "$alpha1"))
  assert_scalar_numeric(regime$beta1, paste0(regime_name, "$beta1"))

  if (abs(regime$ar1) >= 1) {
    stop(
      regime_name,
      "$ar1 must satisfy |ar1| < 1 for a stable AR(1) conditional mean.",
      call. = FALSE
    )
  }

  if (regime$omega <= 0) {
    stop(regime_name, "$omega must be strictly positive.", call. = FALSE)
  }

  if (regime$alpha1 < 0 || regime$beta1 < 0) {
    stop(regime_name, "$alpha1 and $beta1 must be non-negative.", call. = FALSE)
  }

  persistence <- regime$alpha1 + regime$beta1

  if (!allow_nonstationary && persistence >= 1) {
    stop(
      regime_name,
      " must satisfy alpha1 + beta1 < 1 for a covariance-stationary",
      " within-regime GARCH(1,1) benchmark. Observed persistence = ",
      round(persistence, 6),
      ".",
      call. = FALSE
    )
  }

  unconditional_variance <- if (persistence < 1) {
    regime$omega / (1 - persistence)
  } else {
    NA_real_
  }

  list(
    label = regime$label,
    mu = regime$mu,
    ar1 = regime$ar1,
    omega = regime$omega,
    alpha1 = regime$alpha1,
    beta1 = regime$beta1,
    persistence = persistence,
    unconditional_variance = unconditional_variance
  )
}


validate_garch_break_config <- function(config, allow_nonstationary = FALSE) {
  required_names <- c(
    "scenario",
    "n_pre_break",
    "n_post_break",
    "burn_in",
    "seed",
    "innovation",
    "innovation_df",
    "pre_break",
    "post_break",
    "break_type",
    "reset_variance_at_break"
  )

  missing_names <- setdiff(required_names, names(config))

  if (length(missing_names) > 0L) {
    stop(
      "The GARCH break configuration is missing required field(s): ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.character(config$scenario) || length(config$scenario) != 1L || !nzchar(config$scenario)) {
    stop("config$scenario must be a single non-empty character value.", call. = FALSE)
  }

  n_pre_break <- assert_positive_integer(config$n_pre_break, "config$n_pre_break")
  n_post_break <- assert_positive_integer(config$n_post_break, "config$n_post_break")
  burn_in <- assert_positive_integer(config$burn_in, "config$burn_in", minimum = 50L)
  seed <- validate_seed(config$seed)
  innovation_settings <- validate_innovation_settings(config$innovation, config$innovation_df)

  pre_break <- validate_garch_regime(
    regime = config$pre_break,
    regime_name = "config$pre_break",
    allow_nonstationary = allow_nonstationary
  )

  post_break <- validate_garch_regime(
    regime = config$post_break,
    regime_name = "config$post_break",
    allow_nonstationary = allow_nonstationary
  )

  if (!is.character(config$break_type) || length(config$break_type) != 1L || !nzchar(config$break_type)) {
    stop("config$break_type must be a single non-empty character value.", call. = FALSE)
  }

  if (!is.logical(config$reset_variance_at_break) || length(config$reset_variance_at_break) != 1L) {
    stop("config$reset_variance_at_break must be a single TRUE/FALSE value.", call. = FALSE)
  }

  list(
    scenario = config$scenario,
    n_pre_break = n_pre_break,
    n_post_break = n_post_break,
    burn_in = burn_in,
    seed = seed,
    innovation = innovation_settings$innovation,
    innovation_df = innovation_settings$innovation_df,
    pre_break = pre_break,
    post_break = post_break,
    break_type = config$break_type,
    break_description = config$break_description %||% NA_character_,
    reset_variance_at_break = config$reset_variance_at_break,
    allow_nonstationary = allow_nonstationary
  )
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# ------------------------------------------------------------------------------
# 3. Reproducible random-number control
# ------------------------------------------------------------------------------

with_reproducible_seed <- function(seed, code) {
  seed <- validate_seed(seed)

  if (is.null(seed)) {
    return(force(code))
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL

  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(code)
}


draw_standardised_innovations <- function(n, innovation = "normal", innovation_df = 8) {
  settings <- validate_innovation_settings(innovation, innovation_df)

  if (identical(settings$innovation, "normal")) {
    return(stats::rnorm(n))
  }

  stats::rt(n, df = settings$innovation_df) / sqrt(settings$innovation_df / (settings$innovation_df - 2))
}


# ------------------------------------------------------------------------------
# 4. Parameter metadata
# ------------------------------------------------------------------------------

make_garch_parameter_table <- function(config = garch_break_config,
                                       allow_nonstationary = FALSE) {
  cfg <- validate_garch_break_config(config, allow_nonstationary = allow_nonstationary)

  data.frame(
    scenario = cfg$scenario,
    regime = c(cfg$pre_break$label, cfg$post_break$label),
    phase = c("pre_break", "post_break"),
    mu = c(cfg$pre_break$mu, cfg$post_break$mu),
    ar1 = c(cfg$pre_break$ar1, cfg$post_break$ar1),
    omega = c(cfg$pre_break$omega, cfg$post_break$omega),
    alpha1 = c(cfg$pre_break$alpha1, cfg$post_break$alpha1),
    beta1 = c(cfg$pre_break$beta1, cfg$post_break$beta1),
    persistence = c(cfg$pre_break$persistence, cfg$post_break$persistence),
    unconditional_variance = c(
      cfg$pre_break$unconditional_variance,
      cfg$post_break$unconditional_variance
    ),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 5. Core simulator
# ------------------------------------------------------------------------------

simulate_garch_volatility_break <- function(n1 = garch_break_config$n_pre_break,
                                            n2 = garch_break_config$n_post_break,
                                            seed = garch_break_config$seed,
                                            burn_in = garch_break_config$burn_in,
                                            pre_break = garch_break_config$pre_break,
                                            post_break = garch_break_config$post_break,
                                            scenario = garch_break_config$scenario,
                                            innovation = garch_break_config$innovation,
                                            innovation_df = garch_break_config$innovation_df,
                                            break_type = garch_break_config$break_type,
                                            reset_variance_at_break =
                                              garch_break_config$reset_variance_at_break,
                                            allow_nonstationary = FALSE) {
  config <- list(
    scenario = scenario,
    n_pre_break = n1,
    n_post_break = n2,
    burn_in = burn_in,
    seed = seed,
    innovation = innovation,
    innovation_df = innovation_df,
    pre_break = pre_break,
    post_break = post_break,
    break_type = break_type,
    reset_variance_at_break = reset_variance_at_break
  )

  cfg <- validate_garch_break_config(
    config = config,
    allow_nonstationary = allow_nonstationary
  )

  total_n <- cfg$burn_in + cfg$n_pre_break + cfg$n_post_break
  retained_n <- cfg$n_pre_break + cfg$n_post_break
  break_index <- cfg$n_pre_break
  first_post_break_index <- cfg$burn_in + cfg$n_pre_break + 1L

  with_reproducible_seed(cfg$seed, {
    z <- draw_standardised_innovations(
      n = total_n,
      innovation = cfg$innovation,
      innovation_df = cfg$innovation_df
    )

    y_all <- numeric(total_n)
    epsilon_all <- numeric(total_n)
    h_all <- numeric(total_n)
    sigma_all <- numeric(total_n)
    conditional_mean_all <- numeric(total_n)
    regime_all <- character(total_n)
    phase_all <- character(total_n)

    h_previous <- cfg$pre_break$unconditional_variance
    epsilon_previous <- 0
    y_previous <- cfg$pre_break$mu / (1 - cfg$pre_break$ar1)

    for (t in seq_len(total_n)) {
      retained_time <- t - cfg$burn_in

      use_pre_break <- retained_time <= cfg$n_pre_break

      if (retained_time <= 0L) {
        active_regime <- cfg$pre_break
        regime_all[t] <- cfg$pre_break$label
        phase_all[t] <- "burn_in"
      } else if (use_pre_break) {
        active_regime <- cfg$pre_break
        regime_all[t] <- cfg$pre_break$label
        phase_all[t] <- "pre_break"
      } else {
        active_regime <- cfg$post_break
        regime_all[t] <- cfg$post_break$label
        phase_all[t] <- "post_break"
      }

      if (cfg$reset_variance_at_break && t == first_post_break_index) {
        h_previous <- cfg$post_break$unconditional_variance
        epsilon_previous <- 0
        y_previous <- cfg$post_break$mu / (1 - cfg$post_break$ar1)
      }

      h_t <- active_regime$omega +
        active_regime$alpha1 * epsilon_previous^2 +
        active_regime$beta1 * h_previous

      if (!is.finite(h_t) || h_t <= 0) {
        stop(
          "Non-positive or non-finite conditional variance encountered at internal time ",
          t,
          ". Check the GARCH parameters.",
          call. = FALSE
        )
      }

      conditional_mean_t <- active_regime$mu + active_regime$ar1 * y_previous
      epsilon_t <- sqrt(h_t) * z[t]
      y_t <- conditional_mean_t + epsilon_t

      h_all[t] <- h_t
      sigma_all[t] <- sqrt(h_t)
      conditional_mean_all[t] <- conditional_mean_t
      epsilon_all[t] <- epsilon_t
      y_all[t] <- y_t

      h_previous <- h_t
      epsilon_previous <- epsilon_t
      y_previous <- y_t
    }

    keep <- seq.int(from = cfg$burn_in + 1L, to = total_n)

    out <- data.frame(
      time = seq_len(retained_n),
      y = y_all[keep],
      epsilon = epsilon_all[keep],
      conditional_mean = conditional_mean_all[keep],
      h = h_all[keep],
      sigma = sigma_all[keep],
      z = z[keep],
      scenario = cfg$scenario,
      true_break = break_index,
      break_time = break_index + 1L,
      break_type = cfg$break_type,
      regime = regime_all[keep],
      phase = phase_all[keep],
      is_post_break = seq_len(retained_n) > break_index,
      omega = ifelse(
        seq_len(retained_n) <= break_index,
        cfg$pre_break$omega,
        cfg$post_break$omega
      ),
      alpha1 = ifelse(
        seq_len(retained_n) <= break_index,
        cfg$pre_break$alpha1,
        cfg$post_break$alpha1
      ),
      beta1 = ifelse(
        seq_len(retained_n) <= break_index,
        cfg$pre_break$beta1,
        cfg$post_break$beta1
      ),
      persistence = ifelse(
        seq_len(retained_n) <= break_index,
        cfg$pre_break$persistence,
        cfg$post_break$persistence
      ),
      unconditional_variance = ifelse(
        seq_len(retained_n) <= break_index,
        cfg$pre_break$unconditional_variance,
        cfg$post_break$unconditional_variance
      ),
      stringsAsFactors = FALSE
    )

    attr(out, "simulation_config") <- cfg
    attr(out, "regime_parameters") <- make_garch_parameter_table(config)

    out
  })
}


# ------------------------------------------------------------------------------
# 6. Diagnostics and summaries
# ------------------------------------------------------------------------------

summarise_garch_break_series <- function(sim_data) {
  required_names <- c("scenario", "regime", "phase", "y", "epsilon", "h", "sigma")

  missing_names <- setdiff(required_names, names(sim_data))

  if (length(missing_names) > 0L) {
    stop(
      "sim_data is missing required column(s): ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  retained <- sim_data[sim_data$phase %in% c("pre_break", "post_break"), , drop = FALSE]

  phase_order <- c("pre_break", "post_break")
  available_phases <- phase_order[phase_order %in% unique(retained$phase)]
  by_regime <- split(retained, retained$phase)

  summary_list <- lapply(available_phases, function(phase_name) {
    x <- by_regime[[phase_name]]

    data.frame(
      scenario = unique(x$scenario)[1],
      phase = phase_name,
      regime = unique(x$regime)[1],
      n = nrow(x),
      mean_y = mean(x$y),
      variance_y = stats::var(x$y),
      mean_epsilon = mean(x$epsilon),
      variance_epsilon = stats::var(x$epsilon),
      mean_conditional_variance = mean(x$h),
      median_conditional_variance = stats::median(x$h),
      mean_conditional_sd = mean(x$sigma),
      min_conditional_variance = min(x$h),
      max_conditional_variance = max(x$h),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summary_list)
}


check_garch_break_diagnostics <- function(sim_data) {
  required_names <- c("time", "true_break", "phase", "h", "sigma", "epsilon", "regime")

  missing_names <- setdiff(required_names, names(sim_data))

  if (length(missing_names) > 0L) {
    stop(
      "sim_data is missing required column(s): ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  true_break <- unique(sim_data$true_break)

  if (length(true_break) != 1L) {
    stop("sim_data must contain a single known true_break value.", call. = FALSE)
  }

  data.frame(
    diagnostic = c(
      "no_missing_observations",
      "positive_conditional_variance",
      "positive_conditional_sd",
      "contains_pre_break_regime",
      "contains_post_break_regime",
      "known_break_inside_sample"
    ),
    passed = c(
      !anyNA(sim_data),
      all(sim_data$h > 0 & is.finite(sim_data$h)),
      all(sim_data$sigma > 0 & is.finite(sim_data$sigma)),
      any(sim_data$phase == "pre_break"),
      any(sim_data$phase == "post_break"),
      true_break > 1L && true_break < nrow(sim_data)
    ),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7. Plotting helpers for Quarto
# ------------------------------------------------------------------------------

plot_garch_break_series <- function(sim_data) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting. Install it or use the data frame directly.",
      call. = FALSE
    )
  }

  required_names <- c("time", "y", "true_break", "scenario", "regime")

  missing_names <- setdiff(required_names, names(sim_data))

  if (length(missing_names) > 0L) {
    stop(
      "sim_data is missing required column(s): ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  break_value <- unique(sim_data$true_break)[1]

  ggplot2::ggplot(sim_data, ggplot2::aes(x = time, y = y)) +
    ggplot2::geom_line(linewidth = 0.35) +
    ggplot2::geom_vline(
      xintercept = break_value,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::labs(
      title = "Simulated AR(1)-GARCH(1,1) process with a volatility break",
      subtitle = unique(sim_data$scenario)[1],
      x = "Time",
      y = "Observed series"
    ) +
    ggplot2::theme_minimal()
}


plot_garch_conditional_variance <- function(sim_data) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting. Install it or use the data frame directly.",
      call. = FALSE
    )
  }

  required_names <- c("time", "h", "true_break", "scenario", "regime")

  missing_names <- setdiff(required_names, names(sim_data))

  if (length(missing_names) > 0L) {
    stop(
      "sim_data is missing required column(s): ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  break_value <- unique(sim_data$true_break)[1]

  ggplot2::ggplot(sim_data, ggplot2::aes(x = time, y = h)) +
    ggplot2::geom_line(linewidth = 0.35) +
    ggplot2::geom_vline(
      xintercept = break_value,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::labs(
      title = "Conditional variance path under a known GARCH volatility break",
      subtitle = unique(sim_data$scenario)[1],
      x = "Time",
      y = "Conditional variance"
    ) +
    ggplot2::theme_minimal()
}


# ------------------------------------------------------------------------------
# 8. Output-path and saving helpers
# ------------------------------------------------------------------------------

resolve_project_path <- function(..., project_root = NULL) {
  path_parts <- list(...)

  if (!is.null(project_root)) {
    return(do.call(file.path, c(list(project_root), path_parts)))
  }

  if (requireNamespace("here", quietly = TRUE)) {
    return(do.call(here::here, path_parts))
  }

  warning(
    "Package 'here' is not installed and project_root was not supplied; using getwd().",
    call. = FALSE
  )

  do.call(file.path, c(list(getwd()), path_parts))
}


save_garch_break_outputs <- function(sim_data,
                                     output_dir = resolve_project_path("data", "simulated"),
                                     file_stub = "s7_garch_volatility_break",
                                     save_summary = TRUE,
                                     save_parameters = TRUE) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  data_file <- file.path(output_dir, paste0(file_stub, ".csv"))
  utils::write.csv(sim_data, data_file, row.names = FALSE)

  written_files <- c(data = data_file)

  if (isTRUE(save_summary)) {
    summary_file <- file.path(output_dir, paste0(file_stub, "_summary.csv"))
    utils::write.csv(summarise_garch_break_series(sim_data), summary_file, row.names = FALSE)
    written_files <- c(written_files, summary = summary_file)
  }

  if (isTRUE(save_parameters)) {
    parameters <- attr(sim_data, "regime_parameters")

    if (!is.null(parameters)) {
      parameter_file <- file.path(output_dir, paste0(file_stub, "_parameters.csv"))
      utils::write.csv(parameters, parameter_file, row.names = FALSE)
      written_files <- c(written_files, parameters = parameter_file)
    }
  }

  written_files
}


# ------------------------------------------------------------------------------
# 9. Optional explicit pipeline runner
# ------------------------------------------------------------------------------

run_garch_break_pipeline <- function(config = garch_break_config,
                                     project_root = NULL,
                                     save_outputs = FALSE) {
  cfg <- validate_garch_break_config(config)

  sim_data <- simulate_garch_volatility_break(
    n1 = cfg$n_pre_break,
    n2 = cfg$n_post_break,
    seed = cfg$seed,
    burn_in = cfg$burn_in,
    pre_break = cfg$pre_break,
    post_break = cfg$post_break,
    scenario = cfg$scenario,
    innovation = cfg$innovation,
    innovation_df = cfg$innovation_df,
    break_type = cfg$break_type,
    reset_variance_at_break = cfg$reset_variance_at_break,
    allow_nonstationary = cfg$allow_nonstationary
  )

  diagnostics <- check_garch_break_diagnostics(sim_data)
  summary <- summarise_garch_break_series(sim_data)
  parameters <- attr(sim_data, "regime_parameters")

  written_files <- NULL

  if (isTRUE(save_outputs)) {
    output_dir <- resolve_project_path(
      "data",
      "simulated",
      project_root = project_root
    )

    written_files <- save_garch_break_outputs(
      sim_data = sim_data,
      output_dir = output_dir,
      file_stub = "s7_garch_volatility_break"
    )
  }

  list(
    data = sim_data,
    parameters = parameters,
    summary = summary,
    diagnostics = diagnostics,
    written_files = written_files
  )
}


# ------------------------------------------------------------------------------
# 10. Example Quarto usage
# ------------------------------------------------------------------------------

# Source this script in a Quarto setup chunk:
#
#   source(here::here("R", "04_sim_garch_breaks.R"))
#
# Then run explicitly:
#
#   garch_results <- run_garch_break_pipeline(save_outputs = FALSE)
#   garch_data <- garch_results$data
#   garch_results$summary
#   garch_results$diagnostics
#
# To write reproducible CSV outputs:
#
#   garch_results <- run_garch_break_pipeline(save_outputs = TRUE)
#
# To create figures in a Quarto chunk:
#
#   plot_garch_break_series(garch_data)
#   plot_garch_conditional_variance(garch_data)
#
# This script intentionally performs no automatic simulation at source time.
# ==============================================================================

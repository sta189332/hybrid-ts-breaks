plot_series_with_break <- function(data, title = 'Time Series with Break') {
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


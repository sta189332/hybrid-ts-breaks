fit_arima_xgboost_hybrid <- function(train_y, max_lag = 5, nrounds = 50) {
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


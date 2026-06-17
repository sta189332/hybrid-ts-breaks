fit_arima_forecast <- function(train_y, h = 1) {
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


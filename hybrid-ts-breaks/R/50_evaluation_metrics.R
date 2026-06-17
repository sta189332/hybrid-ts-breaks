rmse <- function(actual, forecast) {
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


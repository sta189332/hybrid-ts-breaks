time_ordered_split <- function(data, train_prop = 0.60, valid_prop = 0.20) {
  n <- nrow(data)
  train_end <- floor(train_prop * n)
  valid_end <- floor((train_prop + valid_prop) * n)

  list(
    train = data[1:train_end, ],
    valid = data[(train_end + 1):valid_end, ],
    test  = data[(valid_end + 1):n, ]
  )
}

compute_returns <- function(price) {
  100 * diff(log(price))
}

clean_time_series <- function(data) {
  data |>
    dplyr::arrange(time) |>
    dplyr::distinct(time, .keep_all = TRUE) |>
    tidyr::drop_na()
}

save_processed_data <- function(data, file_name) {
  readr::write_csv(data, file.path('data', 'processed', file_name))
}


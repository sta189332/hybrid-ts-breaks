detect_breaks_classical <- function(y) {
  time_index <- seq_along(y)

  bp <- tryCatch(
    strucchange::breakpoints(y ~ time_index),
    error = function(e) NULL
  )

  cusum <- tryCatch(
    strucchange::sctest(strucchange::efp(y ~ time_index, type = 'Rec-CUSUM')),
    error = function(e) NULL
  )

  cp_mean <- tryCatch(
    changepoint::cpt.mean(y, method = 'PELT'),
    error = function(e) NULL
  )

  cp_var <- tryCatch(
    changepoint::cpt.var(y, method = 'PELT'),
    error = function(e) NULL
  )

  list(
    bai_perron = bp,
    cusum = cusum,
    changepoint_mean = cp_mean,
    changepoint_variance = cp_var
  )
}

extract_changepoints <- function(cp_object) {
  if (is.null(cp_object)) return(integer(0))
  changepoint::cpts(cp_object)
}

make_break_features <- function(y, window = 20) {
  tibble::tibble(
    time = seq_along(y),
    y = as.numeric(y),
    lag1 = dplyr::lag(y, 1),
    roll_mean = zoo::rollmean(y, k = window, fill = NA, align = 'right'),
    roll_sd = zoo::rollapply(y, width = window, FUN = sd, fill = NA, align = 'right')
  ) |>
    tidyr::drop_na()
}


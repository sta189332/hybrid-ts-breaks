simulate_random_walk <- function(n = 1000, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- cumsum(rnorm(n, 0, sigma))

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S2_random_walk',
    true_break = NA_integer_,
    break_type = 'unit_root'
  )
}

simulate_trend_stationary <- function(n = 1000, alpha = 0, beta = 0.05, phi = 0.6, sigma = 1, seed = 123) {
  set.seed(seed)
  u <- as.numeric(arima.sim(model = list(ar = phi), n = n, sd = sigma))
  time <- seq_len(n)
  y <- alpha + beta * time + u

  data.frame(
    time = time,
    y = y,
    scenario = 'S3_trend_stationary',
    true_break = NA_integer_,
    break_type = 'deterministic_trend'
  )
}

simulate_unit_root_drift <- function(n = 1000, mu = 0.05, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- numeric(n)
  eps <- rnorm(n, 0, sigma)

  for (t in 2:n) {
    y[t] <- mu + y[t - 1] + eps[t]
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S4_unit_root_drift',
    true_break = NA_integer_,
    break_type = 'unit_root_with_drift'
  )
}


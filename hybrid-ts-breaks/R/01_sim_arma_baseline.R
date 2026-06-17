simulate_arma_baseline <- function(n = 1000, phi = 0.6, theta = 0.3, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- as.numeric(arima.sim(model = list(ar = phi, ma = theta), n = n, sd = sigma))

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S1_ARMA_baseline',
    true_break = NA_integer_,
    break_type = 'none',
    regime = 'stable'
  )
}


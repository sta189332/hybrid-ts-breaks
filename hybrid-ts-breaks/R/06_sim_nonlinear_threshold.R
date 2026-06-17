simulate_threshold_ar <- function(n = 1000, phi1 = 0.3, phi2 = 0.8, threshold = 0, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- numeric(n)

  for (t in 2:n) {
    phi_t <- ifelse(y[t - 1] <= threshold, phi1, phi2)
    y[t] <- phi_t * y[t - 1] + rnorm(1, 0, sigma)
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S9_threshold_AR',
    threshold = threshold,
    true_break = NA_integer_,
    break_type = 'threshold_nonlinearity'
  )
}


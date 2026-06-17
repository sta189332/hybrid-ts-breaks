simulate_ar_mean_break <- function(n = 1000, Tb = 600, phi = 0.6, mu1 = 0, mu2 = 2, sigma = 1, seed = 123) {
  set.seed(seed)
  y <- numeric(n)
  eps <- rnorm(n, 0, sigma)

  for (t in 2:n) {
    mu_t <- ifelse(t <= Tb, mu1, mu2)
    y[t] <- mu_t + phi * y[t - 1] + eps[t]
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S5_AR_mean_break',
    true_break = Tb,
    break_type = 'mean',
    regime = ifelse(seq_len(n) <= Tb, 'pre_break', 'post_break')
  )
}

simulate_ar_variance_break <- function(n = 1000, Tb = 600, phi = 0.6, sigma1 = 1, sigma2 = 3, seed = 123) {
  set.seed(seed)
  y <- numeric(n)

  for (t in 2:n) {
    sigma_t <- ifelse(t <= Tb, sigma1, sigma2)
    y[t] <- phi * y[t - 1] + rnorm(1, 0, sigma_t)
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S6_AR_variance_break',
    true_break = Tb,
    break_type = 'variance',
    regime = ifelse(seq_len(n) <= Tb, 'pre_break', 'post_break')
  )
}


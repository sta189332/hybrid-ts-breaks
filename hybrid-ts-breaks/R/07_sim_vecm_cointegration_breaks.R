simulate_cointegrated_break <- function(n = 1000, Tb = 600, seed = 123) {
  set.seed(seed)

  x <- cumsum(rnorm(n))
  beta1 <- 1.0
  beta2 <- 1.8
  e <- as.numeric(arima.sim(model = list(ar = 0.5), n = n, sd = 1))
  beta_t <- ifelse(seq_len(n) <= Tb, beta1, beta2)
  y <- beta_t * x + e

  data.frame(
    time = seq_len(n),
    y = y,
    x = x,
    scenario = 'S10_cointegration_break',
    true_break = Tb,
    break_type = 'cointegration_vector',
    regime = ifelse(seq_len(n) <= Tb, 'pre_break', 'post_break')
  )
}


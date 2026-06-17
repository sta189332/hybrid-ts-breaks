simulate_garch_volatility_break <- function(n1 = 600, n2 = 400, seed = 123) {
  if (!requireNamespace('rugarch', quietly = TRUE)) {
    stop('Package rugarch is required. Install it with install.packages("rugarch").')
  }

  set.seed(seed)

  spec1 <- rugarch::ugarchspec(
    variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0)),
    fixed.pars = list(mu = 0, ar1 = 0.2, omega = 0.01, alpha1 = 0.05, beta1 = 0.90)
  )

  spec2 <- rugarch::ugarchspec(
    variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0)),
    fixed.pars = list(mu = 0, ar1 = 0.2, omega = 0.08, alpha1 = 0.12, beta1 = 0.80)
  )

  sim1 <- rugarch::ugarchpath(spec1, n.sim = n1)
  sim2 <- rugarch::ugarchpath(spec2, n.sim = n2)

  y <- c(
    as.numeric(fitted(sim1) + sigma(sim1) * rnorm(n1)),
    as.numeric(fitted(sim2) + sigma(sim2) * rnorm(n2))
  )

  n <- n1 + n2

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S7_GARCH_volatility_break',
    true_break = n1,
    break_type = 'volatility',
    regime = ifelse(seq_len(n) <= n1, 'low_volatility', 'high_volatility')
  )
}


simulate_markov_switching <- function(n = 1000, seed = 123) {
  set.seed(seed)

  P <- matrix(c(0.95, 0.05, 0.10, 0.90), nrow = 2, byrow = TRUE)
  state <- numeric(n)
  state[1] <- 1

  for (t in 2:n) {
    state[t] <- sample(1:2, size = 1, prob = P[state[t - 1], ])
  }

  mu <- c(0, 2)
  phi <- c(0.4, 0.8)
  sigma <- c(1, 2)
  y <- numeric(n)

  for (t in 2:n) {
    s <- state[t]
    y[t] <- mu[s] + phi[s] * y[t - 1] + rnorm(1, 0, sigma[s])
  }

  data.frame(
    time = seq_len(n),
    y = y,
    scenario = 'S8_Markov_switching',
    state = state,
    true_break = NA_integer_,
    break_type = 'latent_regime'
  )
}


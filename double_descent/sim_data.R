sim_data <- function(n, A_counter = NULL) {
  Z <- rbeta(n, 0.85, 0.85)
  W1 <- 4*Z-2
  W2 <- rbinom(n, 1, 0.5)
  g_prob <- plogis(W1-2*W1*W2)

  if (!is.null(A_counter)) {
    A <- rep(A_counter, n)
  } else {
    A <- rbinom(n, 1, g_prob)
  }

  eps <- rnorm(n, 0, 0.25)
  Q <- plogis(W1-2*W1*W2)
  Y <- Q + eps

  return(data.frame(W1 = W1,
                    W2 = W2,
                    A = A,
                    Y = Y))
}

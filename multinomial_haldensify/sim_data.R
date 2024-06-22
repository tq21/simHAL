sim_data <- function(n) {
  X <- runif(n)
  mean_Y <- 2 * X + 1
  Y <- rnorm(n, mean = mean_Y, sd = 0.5)

  return(data.frame(X = X, Y = Y))
}

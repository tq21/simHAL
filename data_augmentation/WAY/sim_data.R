sim_data <- function(n, data_id) {
  # noise
  U <- rnorm(n)

  # baseline covariates
  W1 <- round(runif(n, -4, 4), 2)
  W2 <- round(runif(n, -4, 4), 2)
  W3 <- rbinom(n, 1, 0.5)

  # treatment assignment
  A <- rbinom(n, 1, plogis(0.3+0.5*W1+0.5*W2+0.7*W3))

  # outcome
  if (data_id == 1) {
    Y <- 1.5*A+W1/15-0.28*W1^2+0.5*W2+0.25*W2*W3+U
  } else if (data_id == 2) {
    Y <- 1.5*A-2*as.numeric(W1 < -3)*W3+2.5*as.numeric(W1 > -2)-
      2*as.numeric(W1 > 0)+2.5*as.numeric(W1 > 2)*X3-2.5*as.numeric(W1 > 3) +
      as.numeric(W2 > -1)-4*as.numeric(W2 > 1)*X3+2*as.numeric(W2 > 3)+U
  } else if (data_id == 3) {
    Y <- 1.5*A+W3*(4*sin(pi/2*abs(W1))*(W2 < 0)+4.1*cos(pi/2*abs(W1))*(W2 > 0))+U
  }

  return(data.frame(W1 = W1,
                    W2 = W2,
                    W3 = W3,
                    A = A,
                    Y = Y))
}

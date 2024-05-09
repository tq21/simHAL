library(MASS)

# trivariate -------------------------------------------------------------------
# smooth
sim_tri_smooth <- function(n, corr) {
  mu <- c(0, 0, 0)
  Sigma <- matrix(corr, nrow = 3, ncol = 3)
  diag(Sigma) <- 2
  data <- data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
  X1 <- data$X1
  X2 <- data$X2
  X3 <- data$X3
  data$Y <- X1/15-0.28*X1^2+0.5*X2+0.25*X2*X3+rnorm(n)

  return(data)
}

# jumps
sim_tri_jump <- function(n, corr) {
  mu <- c(0, 0, 0)
  Sigma <- matrix(corr, nrow = 3, ncol = 3)
  diag(Sigma) <- 2
  data <- data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
  X1 <- data$X1
  X2 <- data$X2
  X3 <- data$X3
  data$Y <- -2*as.numeric(X1 < -3)*X3+2.5*as.numeric(X1 > -2)-
    2*as.numeric(X1 > 0)+2.5*as.numeric(X1 > 2)*X3-2.5*as.numeric(X1 > 3) +
    as.numeric(X2 > -1)-4*as.numeric(X2 > 1)*X3+2*as.numeric(X2 > 3)+rnorm(n)

  return(data)
}

# sinusoidal
sim_tri_sin <- function(n, corr) {
  mu <- c(0, 0, 0)
  Sigma <- matrix(corr, nrow = 3, ncol = 3)
  diag(Sigma) <- 2
  data <- data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
  X1 <- data$X1
  X2 <- data$X2
  X3 <- data$X3
  data$Y <- X3*(4*sin(pi/2*abs(X1))*(X2 < 0) + 4.1*cos(pi/2*abs(X1))*(X2 > 0)) +
    rnorm(n/2)

  return(data)
}

# five-variate  ----------------------------------------------------------------
# smooth
sim_five_smooth <- function(n, corr) {
  mu <- c(0, 0, 0, 0, 0)
  Sigma <- matrix(corr, nrow = 5, ncol = 5)
  diag(Sigma) <- 2
  data <- data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
  X1 <- data$X1
  X2 <- data$X2
  X3 <- data$X3
  X4 <- data$X4
  X5 <- data$X5
  data$Y <- X1/10-0.3*X1^2+0.25*X2+0.5*X2*X3-0.5*X4+0.04*X5^2-0.1*X5+rnorm(n)

  return(data)
}

# jumps
sim_five_jump <- function(n, corr) {
  mu <- c(0, 0, 0, 0, 0)
  Sigma <- matrix(corr, nrow = 5, ncol = 5)
  diag(Sigma) <- 2
  data <- data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
  X1 <- data$X1
  X2 <- data$X2
  X3 <- data$X3
  X4 <- data$X4
  X5 <- data$X5
  data$Y <- -as.numeric(X1 < -3)*X3+0.5*as.numeric(X1 > -2)-as.numeric(X1 > 0)+
    2*as.numeric(X1 > 2)*X3-3*as.numeric(X1 > 3)+1.5*as.numeric(X2 > -1)-
    5*as.numeric(X2 > 1)*X3+2*as.numeric(X2 > 3)+2*as.numeric(X4 < 0)-
    as.numeric(X5 > 5) - as.numeric(X4 < 0)*as.numeric(X1 < 0)+2*X3+rnorm(n)

  return(data)
}

# sinusoidal
sim_five_sin <- function(n, corr) {
  mu <- c(0, 0, 0, 0, 0)
  Sigma <- matrix(corr, nrow = 5, ncol = 5)
  diag(Sigma) <- 2
  data <- data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
  X1 <- data$X1
  X2 <- data$X2
  X3 <- data$X3
  X4 <- data$X4
  X5 <- data$X5
  data$Y <- X3*(3.75*sin(pi/2*abs(X1))*(X2 < 0) + 4*cos(pi/2*abs(X1))*(X2 > 0)) +
    sin(X4*pi)*X5/10 + cos(abs(X4-X5))*X3 + rnorm(n)

  return(data)
}

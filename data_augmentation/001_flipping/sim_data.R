# univariate -------------------------------------------------------------------
# smooth
sim_uni_smooth <- function(n){
  X1 <- runif(n, -4, 4)
  Y <- 0.05*X1-0.42*X1^2+rnorm(n)
  return(data.frame(X1 = X1, Y = Y))
}

truth_uni_smooth <- function(X1){
  return(0.05*X1-0.42*X1^2)
}

# jumps
sim_uni_jump <- function(n){
  X1 <- runif(n, -4, 4)
  Y <- -2.7*as.numeric(X1 < -3)+2.5*as.numeric(X1 > -2)-
    2*as.numeric(X1 > 0)+4*as.numeric(X1 > 2)-3*as.numeric(X1 > 3)+rnorm(n)
  return(data.frame(X1 = X1, Y = Y))
}

truth_uni_jump <- function(X1){
  return(-2.7*as.numeric(X1 < -3)+2.5*as.numeric(X1 > -2)-
           2*as.numeric(X1 > 0)+4*as.numeric(X1 > 2)-3*as.numeric(X1 > 3))
}

# sinusoidal
sim_uni_sin <- function(n){
  X1 <- runif(n, -4, 4)
  Y <- 2*sin(0.5*pi*abs(X1))+2*cos(0.5*pi*abs(X1))+rnorm(n)
  return(data.frame(X1 = X1, Y = Y))
}

truth_uni_sin <- function(X1){
  return(2*sin(0.5*pi*abs(X1))+2*cos(0.5*pi*abs(X1)))
}

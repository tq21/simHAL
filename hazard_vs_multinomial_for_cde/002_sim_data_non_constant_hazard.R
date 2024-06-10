library(data.table)
sim_data <- function(n) {
  W1 <- round(runif(n, 2, 6), 2)
  W2 <- round(rnorm(n, 10, sqrt(10)), 2)

  # failure hazard
  lambda_fun <- function(a, W1, W2) {
    if (a == 5) {
      return(1)
    } else if (a == 1) {
      return(plogis(-3.1+0.3*W1))
    } else if (a == 2) {
      return(plogis(-1.2+0.1*W1))
    } else if (a == 3) {
      return(plogis(-5-W1+1.2*W2))
    } else if (a == 4) {
      return(plogis(-8+2.1*W1+2.2*W2))
    }
  }
  lambda_fun <- Vectorize(lambda_fun)

  dt <- data.table()
  dt[, `:=` (id = rep(seq(n), each = 5),
             a = rep(1:5, n),
             W1 = rep(W1, each = 5),
             W2 = rep(W2, each = 5))]
  dt[, lambda := lambda_fun(a, W1, W2)]
  dt[, surv := cumprod(1 - lambda), by = id]
  dt[, prob := lambda * shift(surv, fill = 1), by = id]
  dt[, A := rbinom(.N, 1, prob)]
  dt[, A_a := A * a]
  dt[A_a == 0, A_a := 999]
  dt[, A_a := min(A_a), by = id]
  dt[a == 1 & A_a == 999, A_a := 5]
  dt[, A := A_a]
  dt <- dt[a == 1, .(W1, W2, A)]

  return(dt)
}

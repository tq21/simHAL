library(data.table)
sim_data <- function(n) {
  W1 <- round(runif(n, 2, 6), 2)
  W2 <- round(rnorm(n, 10, sqrt(10)), 2)

  # failure hazard
  lambda_fun <- function(a, W1, W2) {
    if (a == 3) {
      return(1)
    } else if (a == 1){
      return(plogis(-3+0.3*W1+0.25*W2))
    } else if (a == 2) {
      return(plogis(0.7*W1+0.25*W2+0.5))
    }
  }
  lambda_fun <- Vectorize(lambda_fun)

  dt <- data.table()
  dt[, `:=` (id = rep(seq(n), each = 3),
             a = rep(1:3, n),
             W1 = rep(W1, each = 3),
             W2 = rep(W2, each = 3))]
  dt[, lambda := lambda_fun(a, W1, W2)]
  dt[, surv := cumprod(1 - lambda), by = id]
  dt[, prob := lambda * shift(surv, fill = 1), by = id]
  dt[, A := rbinom(.N, 1, prob)]
  dt[, A_a := A * a]
  dt[A_a == 0, A_a := 999]
  dt[, A_a := min(A_a), by = id]
  dt[a == 1 & A_a == 999, A_a := 3]
  dt[, A := A_a]
  dt <- dt[a == 1, .(W1, W2, A)]

  return(dt)
}

# tmp <- sim_data(500)
# summary(as.factor(tmp$A))

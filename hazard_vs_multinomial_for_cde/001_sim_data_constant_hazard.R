library(data.table)
sim_data <- function(n) {
  W1 <- round(runif(n, 2, 6), 2)
  W2 <- round(rnorm(n, 10, sqrt(10)), 2)

  # failure hazard
  lambda_fun <- function(a, W1, W2) {
    if (a == 5) {
      return(1)
    } else if (a %in% 1:4) {
      return(plogis(-8+0.3*W1^2+0.25*W2))
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

get_truth <- function(data) {
  rep_data <- copy(data)
  n <- rep_data[, .N]
  tau <- 5

  # prepare repeated measure data
  rep_data <- rep_data[rep(1:.N, each = tau)]
  rep_data[, `:=` (id = rep(seq(n), each = tau),
                        a = rep(seq(tau), n))]

  # convert hazard to density
  rep_data[a == 5, lambda := 1]
  rep_data[a != 5, lambda := plogis(-8+0.3*W1^2+0.25*W2)]
  rep_data[, surv := cumprod(1 - lambda), by = id]
  rep_data[, density := lambda * shift(surv, fill = 1), by = id]
  pred <- reshape(rep_data[, .(id, a, density)],
                  idvar = "id", timevar = "a", direction = "wide")
  pred <- as.data.frame(pred[, id := NULL])
  names(pred) <- seq(tau)

  return(pred)
}

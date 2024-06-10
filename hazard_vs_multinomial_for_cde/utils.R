#' Estimating the conditional density of A given W using the hazard formulation
#'
#' @param data A data.table containing the covariates and the variable of
#' interest
#' @param W A vector of the covariate names
#' @param A The name of the variable of interest
cde_hazard <- function(data, W, A) {

  n <- data[, .N]
  tau <- uniqueN(data[, ..A])

  # prepare repeated measure data
  rep_data_pred <- copy(data)
  rep_data_pred <- rep_data_pred[rep(1:.N, each = tau)]
  rep_data_pred[, `:=` (id = rep(seq(n), each = tau),
                        a = rep(seq(tau), n))]
  rep_data_pred[, A_a := as.numeric(A == a)]
  rep_data_train <- copy(rep_data_pred)
  rep_data_train <- rep_data_train[a <= A]

  # fit hazard regression using HAL
  cov_names <- c(W, "a")
  fit <- fit_hal(X = rep_data_train[, ..cov_names],
                 Y = rep_data_train[, A_a],
                 id = rep_data_train[, id],
                 smoothness_orders = 1,
                 max_degree = 3,
                 family = "binomial",
                 return_x_basis = FALSE)

  return(fit)
}

#' Predict the conditional density of A given W from a fitted HAL model
#'
#' @param fit A fitted HAL model
#' @param new_data A data.table containing the covariates and the variable of
#' interest
#' @param W A vector of the covariate names
#' @param A The name of the variable of interest
predict_cde_hazard <- function(fit, new_data, W, A) {

  n <- new_data[, .N]
  tau <- uniqueN(new_data[, ..A])

  # prepare repeated measure data
  rep_data_pred <- copy(new_data)
  rep_data_pred <- rep_data_pred[rep(1:.N, each = tau)]
  rep_data_pred[, `:=` (id = rep(seq(n), each = tau),
                        a = rep(seq(tau), n))]
  rep_data_pred[, A_a := as.numeric(A == a)]
  cov_names <- c(W, "a")

  # predict hazard
  pred <- predict(fit, new_data = rep_data_pred[, ..cov_names], type = "response")

  # convert hazard to density
  rep_data_pred[, lambda := pred]
  rep_data_pred[a == tau, lambda := 1]
  rep_data_pred[, surv := cumprod(1 - lambda), by = id]
  rep_data_pred[, density := lambda * shift(surv, fill = 1), by = id]
  pred <- reshape(rep_data_pred[, .(id, a, density)],
                  idvar = "id", timevar = "a", direction = "wide")
  pred <- as.data.frame(pred[, id := NULL])
  names(pred) <- sort(unique(new_data[[A]]))

  return(pred)
}

get_loss <- function(y_pred, y_true, family) {
  if (family == "gaussian") {
    return(sqrt(mean((y_true - y_pred)^2)))
  } else if (family == "binomial") {
    return(-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))
  }
}

get_r_square <- function(y_pred, y_true) {
  return(1 - sum((y_true-y_pred)^2)/sum((y_true-mean(y_true))^2))
}

#' Function to get the smallest eigenvalue of X^TX, where X is formed by
#' selected bases from hal9001 fit
get_smallest_eigen_hal9001 <- function(fit, X, add_main) {
  basis_list <- fit$basis_list[fit$coefs[-1] != 0]

  # whether to add main terms to final design matrix
  end_idx <- ifelse(add_main, length(basis_list) - ncol(X), length(basis_list))
  M <- as.matrix(hal9001::make_design_matrix(as.matrix(X), basis_list[1:end_idx]))

  if (add_main) {
    M <- cbind(X, M)
  }

  M <- cbind(1, M)
  M_eigen <- eigen(t(M)%*%M)

  return(M_eigen$values[length(M_eigen$values)])
}

is_binary <- function(x) {
  return(all(x %in% c(0, 1)))
}

`%+%` <- function(a, b) paste0(a, b)

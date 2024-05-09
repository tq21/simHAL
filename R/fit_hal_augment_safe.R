#' @title Fit HAL using Data Augmentation
#'
#' @description A wrapper function for `fit_hal()`. Generate augmented data
#' sets by applying Gaussian noise to the original design matrix `X`.
#'
#' @param X An input `matrix` with dimension number of observations -by-
#' number of covariates.
#' @param Y A `numeric` vector of observations of the outcome variable.
#' @param col_idx A `numeric` vector of column indices to apply noise.
#' @param noise A `numeric` value for the standard deviation of the Gaussian
#' noise.
#' @param copies A `numeric` value for the number of augmented data sets.
#' @param basis_list A `list` of basis functions generated from `X`.
#' @param ... Additional arguments to be passed to `fit_hal()`.
#'
#' @return A `list` with the following components:
#' \item{X}{An augmented `matrix` of the original design matrix `X`.}
#' \item{Y}{An augmented `numeric` vector of the outcome variable `Y`. Note
#' that this simply contains the original `Y` repeated `copies + 1` times.}
#' \item{fit}{An object returned from the function `fit_hal()`.}
fit_hal_augment_safe <- function(X,
                                 Y,
                                 basis_list,
                                 col_idx,
                                 noise,
                                 copies,
                                 family) {

  n <- nrow(X); d <- ncol(X)

  # make augmented data
  X_perturb <- map(1:copies, function(.x) {
    noise_mat <- matrix(0, nrow = n, ncol = d)
    noise_mat[, col_idx] <- rnorm(n, mean = 0, sd = noise)
    X + noise_mat
  })

  # combine the original data with the augmented, add label S
  X_aug <- do.call(rbind, X_perturb)
  colnames(X_aug) <- names(X)
  X_aug <- rbind(X, X_aug)
  labels <- c(rep(1, n), rep(0, n * copies))
  Y_all <- rep(Y, copies + 1)

  # fit HAL on the combined data, with outcome S
  fit <- fit_hal(X = X_aug,
                 Y = labels,
                 family = "binomial",
                 basis_list = basis_list,
                 smoothness_orders = 0)

  # obtain basis list with non-zero coefficients, make design matrix
  basis_list_non_zero <- fit$basis_list[fit$coef[-1] != 0]
  X_unpenalized <- as.matrix(make_design_matrix(X = as.matrix(X_aug),
                                                blist = basis_list_non_zero))

  # fit HAL on the combined data, with outcome Y
  fit <- fit_hal(X = X_aug,
                 Y = Y_all,
                 family = family,
                 basis_list = basis_list,
                 id = rep(1:n, copies + 1),
                 X_unpenalized = X_unpenalized,
                 smoothness_orders = 0)

  return(list(X = X_aug,
              Y = Y_all,
              col_idx = col_idx,
              noise = noise,
              copies = copies,
              fit = fit,
              basis_list_non_zero = basis_list_non_zero))
}

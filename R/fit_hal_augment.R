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
#' @param family A `character` string for the outcome type. Either "gaussian"
#' or "binomial".
#' @param basis_list A `list` of basis functions generated from `X`.
#' @param ... Additional arguments to be passed to `fit_hal()`.
#'
#' @importFrom purrr map
#'
#' @return A `list` with the following components:
#' \item{X}{An augmented `matrix` of the original design matrix `X`.}
#' \item{Y}{An augmented `numeric` vector of the outcome variable `Y`. Note
#' that this simply contains the original `Y` repeated `copies + 1` times.}
#' \item{fit}{An object returned from the function `fit_hal()`.}
fit_hal_augment <- function(X,
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

  # combine the original data with the augmented
  X_aug <- do.call(rbind, X_perturb)
  colnames(X_aug) <- names(X)
  X_aug <- rbind(X, X_aug)
  Y_all <- rep(Y, copies + 1)

  # fit HAL on the combined data
  fit <- fit_hal(X = X_aug,
                 Y = Y_all,
                 family = family,
                 basis_list = basis_list,
                 id = rep(1:n, copies + 1))

  return(list(X = X_aug,
              Y = Y_all,
              col_idx = col_idx,
              noise = noise,
              copies = copies,
              fit = fit))
}

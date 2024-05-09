#' @title Fit HAL using Data Augmentation (Cross-Validated)
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
#' @importFrom purrr map_vec
#' @importFrom origami make_folds
#'
#' @return A `list` with the following components:
#' \item{X}{An augmented `matrix` of the original design matrix `X`.}
#' \item{Y}{An augmented `numeric` vector of the outcome variable `Y`. Note
#' that this simply contains the original `Y` repeated `copies + 1` times.}
#' \item{fit}{An object returned from the function `fit_hal()`.}
cv_fit_hal_augment_safe <- function(X,
                                    Y,
                                    basis_list,
                                    col_idx,
                                    sd_seq,
                                    copies,
                                    family,
                                    copies_max = NULL,
                                    V_folds = 5) {

  n <- nrow(X); d <- ncol(X)

  # make CV folds for selecting optimal sd
  folds <- make_folds(n = n, V = V_folds)

  res <- mclapply(folds, function(.x) {

    # train-validation split
    X_train <- X[.x$training_set, , drop = FALSE]
    Y_train <- Y[.x$training_set]
    X_valid <- X[.x$validation_set, , drop = FALSE]
    Y_valid <- Y[.x$validation_set]

    n_train <- nrow(X_train)
    Y_train_all <- rep(Y_train, copies + 1)


    # fit augmented HAL on training set, for each sd
    loss_sd_seq <- map_vec(sd_seq, function(sd) {

      fit <- fit_hal_augment_safe(X = X_train,
                                  Y = Y_train,
                                  basis_list = basis_list,
                                  col_idx = col_idx,
                                  noise = sd,
                                  copies = copies,
                                  family = family)

      # make unpenalized design matrix on validation set
      X_unpenalized <- as.matrix(make_design_matrix(X = as.matrix(X_valid),
                                                    blist = fit$basis_list_non_zero))

      # evaluate on validation set
      pred_valid <- predict(fit$fit,
                            new_data = X_valid,
                            new_X_unpenalized = X_unpenalized,
                            type = "response")

      return(get_loss(pred_valid, Y_valid, family))
    })

    names(loss_sd_seq) <- sd_seq
    return(loss_sd_seq)

  }, mc.cores = 5)

  res <- do.call(rbind, res)

  opt_sd <- sd_seq[which.min(colMeans(res))] # optimal sd

  if (is.null(copies_max)) {

    # re-fit HAL on full data using the optimal sd
    fit <- fit_hal_augment_safe(X = X,
                                Y = Y,
                                basis_list = basis_list,
                                col_idx = col_idx,
                                noise = opt_sd,
                                copies = copies,
                                family = family)

    return(fit)

  } else {

    # re-fit HAL on full data using the optimal sd,
    # for a sequence till copies_max
    X_aug <- X
    Y_all <- Y
    fit_obj_list <- list()

    for (copies_cur in 1:copies_max) {
      # generate a single new set of perturbed data
      noise_mat <- matrix(0, nrow = n, ncol = d)
      noise_mat[, col_idx] <- rnorm(n, mean = 0, sd = opt_sd)
      new_X_perturb <- X + noise_mat

      # append this new set to the already augmented data
      X_aug <- rbind(X_aug, new_X_perturb)
      Y_all <- c(Y_all, Y)

      # fit the model on the updated augmented data
      fit <- fit_hal(X = X_aug,
                     Y = Y_all,
                     id = rep(1:n, copies_cur + 1),
                     ...)

      # store the fit, along with the current augmented data and Y
      fit_obj_list[[length(fit_obj_list) + 1]] <- list(X = X_aug,
                                                       Y = Y_all,
                                                       fit = fit,
                                                       copies = copies_cur)
    }

    return(fit_obj_list)
  }
}

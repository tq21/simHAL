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
fit_hal_augment <- function(X,
                            Y,
                            col_idx,
                            sd_seq,
                            copies,
                            family,
                            copies_max = NULL,
                            V_folds = 5,
                            ...) {

  n <- nrow(X); d <- ncol(X)

  # make CV folds for selecting optimal sd
  folds <- make_folds(n = n, V = V_folds)

  res <- future_map_dfr(folds, function(.x) {
    # train-validation split
    X_train <- X[.x$training_set, , drop = FALSE]
    Y_train <- Y[.x$training_set]
    X_valid <- X[.x$validation_set, , drop = FALSE]
    Y_valid <- Y[.x$validation_set]

    n_train <- nrow(X_train)
    Y_train_all <- rep(Y_train, copies + 1)

    # generate perturbed data (only on training set) for each sd
    loss_sd_seq <- map_vec(sd_seq, function(sd_cur) {

      # generate perturbed copies
      X_train_perturb <- map(1:copies, function(.x) {
        noise_mat <- matrix(0, nrow = n_train, ncol = d)
        noise_mat[, col_idx] <- rnorm(n_train, mean = 0, sd = sd_cur)
        X_train + noise_mat
      })

      # combine the original and perturbed data
      X_train_aug <- do.call(rbind, X_train_perturb)
      colnames(X_train_aug) <- names(X_train)
      X_train_aug <- rbind(X_train, X_train_aug)

      # fit HAL on data for each sd level
      fit <- fit_hal(X = X_train_aug,
                     Y = Y_train_all,
                     family = family,
                     id = rep(1:n_train, copies + 1),
                     ...)

      # evaluate on validation set
      pred_valid <- predict(fit, new_data = X_valid, type = "response")
      loss_sd <- get_loss(pred_valid, Y_valid, family)

      return(loss_sd)
    }, .progress = FALSE)

    names(loss_sd_seq) <- sd_seq

    return(loss_sd_seq)
  }, .options = furrr_options(seed = TRUE))
  #}, .progress = TRUE, .options = furrr_options(seed = TRUE))

  opt_sd <- sd_seq[which.min(colMeans(res))] # optimal sd

  if (is.null(copies_max)) {

    # re-fit HAL on full data using the optimal sd
    X_perturb <- map(1:copies, function(.x) {
      noise_mat <- matrix(0, nrow = n, ncol = d)
      noise_mat[, col_idx] <- rnorm(n, mean = 0, sd = opt_sd)
      X + noise_mat
    })

    X_aug <- do.call(rbind, X_perturb)
    colnames(X_aug) <- names(X)
    X_aug <- rbind(X, X_aug)
    Y_all <- rep(Y, copies + 1)

    fit <- fit_hal(X = X_aug,
                   Y = Y_all,
                   family = family,
                   id = rep(1:n, copies + 1),
                   ...)

    return(list(X = X_aug,
                Y = Y_all,
                cv_selected_sd = opt_sd,
                fit = fit))

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

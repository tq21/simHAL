fit_hal_augment <- function(X,
                            Y,
                            col_idx,
                            noise,
                            copies,
                            max_degree,
                            basis_list,
                            ...) {

  # data augmentation
  n <- nrow(X); d <- ncol(X)
  X_noise <- map(1:copies, function(.x) {
    noise_mat <- matrix(0, nrow = n, ncol = d)
    noise_mat[, col_idx] <- rnorm(n, mean = 0, sd = noise)
    X + noise_mat
  })

  X_aug <- do.call(rbind, X_noise)
  colnames(X_aug) <- names(X)
  X_aug <- rbind(X, X_aug)
  Y_all <- rep(Y, copies + 1)

  # fitting
  fit <- fit_hal(X = X_aug,
                 Y = Y_all,
                 smoothness_orders = 0,
                 basis_list = basis_list,
                 family = "gaussian",
                 max_degree = max_degree,
                 id = rep(1:n, copies + 1),
                 ...)

  return(list(X = X_aug,
              Y = Y_all,
              fit = fit))
}

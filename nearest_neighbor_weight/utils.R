library(FNN)
library(hal9001)
library(stringr)
devtools::load_all()

get_num_nn <- function(X, delta) {
  k <- nrow(X) - 1
  num_nn <- apply(X, 2, function(.x) {
    dist_mat <- get.knn(.x, k = k, algorithm = "kd_tree")$nn.dist
    dist_mat_num <- dist_mat <= delta
    rowSums(dist_mat_num)
  })

  return(num_nn)
}

# n <- 500
# d <- 5
# scenario <- "jump"
# n_noise <- 0
# n_test <- 10000

run_sim <- function(n,
                    d,
                    scenario,
                    n_noise,
                    delta,
                    n_test = 10000) {

  # simulate data --------------------------------------------------------------
  data <- NULL
  data_test <- NULL
  if (d == 1 & scenario == "smooth") {
    data <- sim_uni_smooth(n)
    data_test <- sim_uni_smooth(n_test)
  } else if (d == 1 & scenario == "jump") {
    data <- sim_uni_jump(n)
    data_test <- sim_uni_jump(n_test)
  } else if (d == 1 & scenario == "sin") {
    data <- sim_uni_sin(n)
    data_test <- sim_uni_sin(n_test)
  } else if (d == 3 & scenario == "smooth") {
    data <- sim_tri_smooth(n)
    data_test <- sim_tri_smooth(n_test)
  } else if (d == 3 & scenario == "jump") {
    data <- sim_tri_jump(n)
    data_test <- sim_tri_jump(n_test)
  } else if (d == 3 & scenario == "sin") {
    data <- sim_tri_sin(n)
    data_test <- sim_tri_sin(n_test)
  } else if (d == 5 & scenario == "smooth") {
    data <- sim_five_smooth(n)
    data_test <- sim_five_smooth(n_test)
  } else if (d == 5 & scenario == "jump") {
    data <- sim_five_jump(n)
    data_test <- sim_five_jump(n_test)
  } else if (d == 5 & scenario == "sin") {
    data <- sim_five_sin(n)
    data_test <- sim_five_sin(n_test)
  } else {
    stop("Invalid scenario")
  }

  X <- data[, 1:d]
  Y <- data[, d+1]
  X_test <- data_test[, 1:d]
  Y_test <- data_test[, d+1]

  # add noise
  if (n_noise > 0) {
    X_noise <- matrix(rnorm(n * n_noise), nrow = n)
    X <- cbind(X, X_noise)
    X_noise_test <- matrix(rnorm(n_test * n_noise), nrow = n_test)
    X_test <- cbind(X_test, X_noise_test)
  }

  basis_list <- enumerate_basis(X)

  # regular HAL ----------------------------------------------------------------
  print("fitting HAL...")
  hal_fit <- fit_hal(X = X,
                     Y = Y,
                     smoothness_orders = 0,
                     basis_list = basis_list,
                     family = "gaussian")
  pred_hal <- predict(hal_fit, new_data = X_test)
  r_squared_hal <- get_r_square(pred_hal, Y_test)
  eigen_val_hal <- get_smallest_eigen_hal9001(hal_fit, X, FALSE)

  # nearest-neighbor-weighted HAL ----------------------------------------------
  print("fitting weighted HAL...")
  weight_tab <- get_num_nn(X, delta)
  weights <- sapply(basis_list, function(.x) {
    col_idx <- .x$cols
    row_idx <- which(apply(X[, col_idx, drop = FALSE], 1, function(.y) {
      all(.y == .x$cutoffs)
    }))[1]
    max(weight_tab[row_idx, col_idx])
  })

  wt_hal_fit <- fit_hal(X = X,
                        Y = Y,
                        penalty_factor = weights,
                        smoothness_orders = 0,
                        basis_list = basis_list,
                        family = "gaussian")
  pred_wt_hal <- predict(wt_hal_fit, new_data = X_test)
  r_squared_wt_hal <- get_r_square(pred_wt_hal, Y_test)
  eigen_val_wt_hal <- get_smallest_eigen_hal9001(wt_hal_fit, X, FALSE)

  res <- data.frame(
    names = c("HAL", "Weighted HAL"),
    n = n,
    d = d,
    scenario = scenario,
    n_noise = n_noise,
    r_squared = c(r_squared_hal, r_squared_wt_hal),
    eigen_val = c(eigen_val_hal, eigen_val_wt_hal))

  return(res)
}

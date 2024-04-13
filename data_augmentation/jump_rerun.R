.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(ggplot2)
library(dplyr)
library(hal9001)
library(devtools)
library(purrr)
library(origami)
load_all()

# SIMULATION PARAMETERS --------------------------------------------------------
B <- 10
n <- 200
n_test <- 10000
d_vals <- c(1, 3, 5)
max_degree <- 3
scenarios <- c("jump")
copies <- 5
copies_max <- 10
sd_seq <- round(seq(0.01, 0.1, length.out = 20), 3)
params <- expand.grid(d = d_vals, scenario = scenarios, run = 1:B)
params$scenario <- as.character(params$scenario)
results <- data.frame(run = NULL,
                      d = NULL,
                      scenario = NULL,
                      copies = NULL,
                      mse = NULL,
                      num_selected = NULL)

# RUN SIMULATION ---------------------------------------------------------------
for (i in 1:nrow(params)) {
  param <- params[i, ]
  print("run: " %+% param$run %+%
          ", d: " %+% param$d %+%
          ", scenario: " %+% param$scenario)

  # simulate data
  if (param$d == 1 & param$scenario == "smooth") {
    data <- sim_uni_smooth(n)
    data_test <- sim_uni_smooth(n_test)
    col_idx <- c(1)
  } else if (param$d == 1 & param$scenario == "jump") {
    data <- sim_uni_jump(n)
    data_test <- sim_uni_jump(n_test)
    col_idx <- c(1)
  } else if (param$d == 1 & param$scenario == "sin") {
    data <- sim_uni_sin(n)
    data_test <- sim_uni_sin(n_test)
    col_idx <- c(1)
  } else if (param$d == 3 & param$scenario == "smooth") {
    data <- sim_tri_smooth(n)
    data_test <- sim_tri_smooth(n_test)
    col_idx <- c(1, 2)
  } else if (param$d == 3 & param$scenario == "jump") {
    data <- sim_tri_jump(n)
    data_test <- sim_tri_jump(n_test)
    col_idx <- c(1, 2)
  } else if (param$d == 3 & param$scenario == "sin") {
    data <- sim_tri_sin(n)
    data_test <- sim_tri_sin(n_test)
    col_idx <- c(1, 2)
  } else if (param$d == 5 & param$scenario == "smooth") {
    data <- sim_five_smooth(n)
    data_test <- sim_five_smooth(n_test)
    col_idx <- c(1, 2, 4, 5)
  } else if (param$d == 5 & param$scenario == "jump") {
    data <- sim_five_jump(n)
    data_test <- sim_five_jump(n_test)
    col_idx <- c(1, 2, 4, 5)
  } else if (param$d == 5 & param$scenario == "sin") {
    data <- sim_five_sin(n)
    data_test <- sim_five_sin(n_test)
    col_idx <- c(1, 2, 4, 5)
  }

  # make basis list
  basis_list <- enumerate_basis(data[, 1:param$d, drop = FALSE],
                                smoothness_orders = 0,
                                max_degree = max_degree)

  # fit regular HAL
  hal_fit <- fit_hal(X = data[, 1:param$d, drop = FALSE],
                     Y = data[, param$d+1],
                     smoothness_orders = 0,
                     max_degree = max_degree,
                     basis_list = basis_list,
                     family = "gaussian")
  hal_pred <- predict(hal_fit, new_data = data_test[, 1:param$d, drop = FALSE])
  hal_mse <- get_loss(hal_pred, data_test[, param$d+1], "gaussian")
  hal_num_selected <- length(hal_fit$basis_list[hal_fit$coefs[-1] != 0])
  res <- data.frame(run = param$run,
                    d = param$d,
                    scenario = param$scenario,
                    copies = 0,
                    mse = hal_mse,
                    num_selected = hal_num_selected)
  results <- rbind(results, res)

  # fit augmented HAL
  hal_aug_fit_obj_list <- fit_hal_augment(X = data[, 1:param$d, drop = FALSE],
                                          Y = data[, param$d+1],
                                          col_idx = col_idx,
                                          sd_seq = sd_seq,
                                          copies = copies,
                                          family = "gaussian",
                                          copies_max = copies_max,
                                          max_degree = max_degree,
                                          basis_list = basis_list)
  res_aug <- map_dfr(hal_aug_fit_obj_list, function(.x) {
    hal_aug_fit <- .x$fit
    hal_aug_pred <- predict(hal_aug_fit, new_data = data_test[, 1:param$d, drop = FALSE])
    hal_aug_mse <- get_loss(hal_aug_pred, data_test[, param$d+1], "gaussian")
    hal_aug_num_selected <- length(hal_aug_fit$basis_list[hal_aug_fit$coefs[-1] != 0])
    res <- data.frame(run = param$run,
                      d = param$d,
                      scenario = param$scenario,
                      copies = .x$copies,
                      mse = hal_aug_mse,
                      num_selected = hal_aug_num_selected)

    return(res)
  })

  results <- rbind(results, res_aug)
}

saveRDS(results, "out/jump_rerun.rds")

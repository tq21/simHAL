library(devtools)
library(hal9001)
source("sim_data.R")
load_all()

`%+%` <- function(a, b) paste0(a, b)

run_sim <- function(n,
                    seed,
                    type = "all",
                    n_test = 10000) {
  set.seed(seed)
  all_scenarios <- rep(c("smooth", "jumps", "sinusoidal"), 2)

  # trivariate
  sim_tri_smooth_dt <- sim_tri_smooth(n)
  sim_tri_smooth_dt_test <- sim_tri_smooth(n_test)

  sim_tri_jump_dt <- sim_tri_jump(n)
  sim_tri_jump_dt_test <- sim_tri_jump(n_test)

  sim_tri_sin_dt <- sim_tri_sin(n)
  sim_tri_sin_dt_test <- sim_tri_sin(n_test)

  # five-variate
  sim_five_smooth_dt <- sim_five_smooth(n)
  sim_five_smooth_dt_test <- sim_five_smooth(n_test)

  sim_five_jump_dt <- sim_five_jump(n)
  sim_five_jump_dt_test <- sim_five_jump(n_test)

  sim_five_sin_dt <- sim_five_sin(n)
  sim_five_sin_dt_test <- sim_five_sin(n_test)

  dt_list <- list()
  dt_list_test <- list()
  if (type == "trivariate") {
    dt_list <- list(sim_tri_smooth_dt, sim_tri_jump_dt, sim_tri_sin_dt)
    dt_list_test <- list(sim_tri_smooth_dt_test, sim_tri_jump_dt_test, sim_tri_sin_dt_test)
  } else if (type == "five-variate") {
    dt_list <- list(sim_five_jump_dt)
    dt_list_test <- list(sim_five_jump_dt_test)
  } else if (type == "all") {
    dt_list <- list(sim_tri_smooth_dt, sim_tri_jump_dt, sim_tri_sin_dt,
                    sim_five_smooth_dt, sim_five_jump_dt, sim_five_sin_dt)
    dt_list_test <- list(sim_tri_smooth_dt_test, sim_tri_jump_dt_test, sim_tri_sin_dt_test,
                         sim_five_smooth_dt_test, sim_five_jump_dt_test, sim_five_sin_dt_test)
  }

  res_list <- list()
  preds_list <- list()

  for (i in 1:length(dt_list)) {
    print(i %+% " out of " %+% length(dt_list))
    dt <- dt_list[[i]]
    dt_test <- dt_list_test[[i]]
    y_col_idx <- ncol(dt)
    x_col_idx <- setdiff(1:ncol(dt), ncol(dt))

    # hal9001 0-order ----------------------------------------------------------
    print("running hal9001 0-order...")
    hal9001_fit <- fit_hal(X = as.data.frame(dt[, x_col_idx]),
                           Y = dt[, y_col_idx],
                           max_degree = length(x_col_idx),
                           smoothness_orders = 0)

    # get loss
    hal9001_pred <- predict(hal9001_fit, new_data = dt_test[, x_col_idx], type = "response")
    hal9001_r_squared <- get_r_square(hal9001_pred, dt_test[, y_col_idx])
    hal9001_squared_error <- (hal9001_pred-dt_test[, y_col_idx])^2
    hal9001_order <- order(hal9001_squared_error)
    hal9001_squared_error <- sort(hal9001_squared_error, TRUE)

    # doubleHAL 0-order --------------------------------------------------------
    print("running doubleHAL 0-order...")
    hal9001_flip_fit <- fit_hal(X = -as.data.frame(dt[, x_col_idx]),
                                Y = dt[, y_col_idx],
                                max_degree = length(x_col_idx),
                                smoothness_orders = 0)

    # get loss
    hal9001_flip_pred <- predict(hal9001_flip_fit, new_data = -dt_test[, x_col_idx], type = "response")
    hal9001_double_pred <- (hal9001_pred+hal9001_flip_pred)/2
    hal9001_double_r_squared <- get_r_square(hal9001_double_pred, dt_test[, y_col_idx])
    hal9001_double_squared_error <- (hal9001_double_pred-dt_test[, y_col_idx])^2
    hal9001_double_squared_error <- hal9001_double_squared_error[hal9001_order]

    # hal9001 1-order ----------------------------------------------------------
    print("running hal9001 1-order...")
    hal9001_1_fit <- fit_hal(X = as.data.frame(dt[, x_col_idx]),
                             Y = dt[, y_col_idx],
                             max_degree = length(x_col_idx),
                             smoothness_orders = 1)

    # get loss
    hal9001_1_pred <- predict(hal9001_1_fit, new_data = dt_test[, x_col_idx], type = "response", new_X_no_basis = X_test)
    hal9001_1_r_squared <- get_r_square(hal9001_1_pred, dt_test[, y_col_idx])
    hal9001_1_squared_error <- (hal9001_1_pred-dt_test[, y_col_idx])^2
    hal9001_1_order <- order(hal9001_1_squared_error)
    hal9001_1_squared_error <- sort(hal9001_1_squared_error, TRUE)

    # doubleHAL 1-order --------------------------------------------------------
    print("running doubleHAL 1-order...")
    hal9001_1_flip_fit <- fit_hal(X = -as.data.frame(dt[, x_col_idx]),
                                  Y = dt[, y_col_idx],
                                  max_degree = length(x_col_idx),
                                  smoothness_orders = 1)

    # get loss
    hal9001_1_flip_pred <- predict(hal9001_1_flip_fit, new_data = -dt_test[, x_col_idx], type = "response")
    hal9001_1_double_pred <- (hal9001_1_pred+hal9001_1_flip_pred)/2
    hal9001_1_double_r_squared <- get_r_square(hal9001_1_double_pred, dt_test[, y_col_idx])
    hal9001_1_double_squared_error <- (hal9001_1_flip_pred-dt_test[, y_col_idx])^2
    hal9001_1_double_squared_error <- hal9001_double_squared_error[hal9001_1_order]

    res <- data.frame(algo_name = c("0th-order HAL",
                                    "0th-order doubleHAL",
                                    "1st-order HAL",
                                    "1st-order doubleHAL"),
                      r_squared = c(hal9001_r_squared,
                                    hal9001_double_r_squared,
                                    hal9001_1_r_squared,
                                    hal9001_1_double_r_squared),
                      n = n,
                      d = length(x_col_idx),
                      scenario = all_scenarios[i])

    preds <- data.frame(hal9001 = hal9001_1_squared_error,
                        hal9001_double = hal9001_double_squared_error,
                        hal9001_1 = hal9001_1_squared_error,
                        hal9001_1_double = hal9001_1_double_squared_error)

    res_list[[i]] <- res
    preds_list[[i]] <- preds
  }

  return(res_list)
}

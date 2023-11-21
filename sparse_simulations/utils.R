library(hal9001)
library(basHAL)
library(stringr)
devtools::load_all()
source("sim_data.R")

n <- 500
d <- 5
scenario <- "smooth"
n_noise <- 0
n_test <- 10000

run_sim <- function(n,
                    d,
                    scenario,
                    n_noise,
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

  # hal9001 fit ----------------------------------------------------------------
  hal_fit <- fit_hal(X = X,
                     Y = Y,
                     screen_variables = FALSE,
                     smoothness_orders = 0,
                     family = "gaussian")
  var_selected_hal <- get_selected_var_hal9001(hal_fit)
  prop_correct_hal <- sum(var_selected_hal <= d) / d
  pred_hal <- predict(hal_fit, new_data = X_test)
  r_squared_hal <- get_r_square(pred_hal, Y_test)

  # hal9001 elastic-net fit ----------------------------------------------------
  enet_fit <- fit_hal(X = X,
                      Y = Y,
                      fit_control = list(alpha = 0.9),
                      screen_variables = FALSE,
                      smoothness_orders = 0,
                      family = "gaussian")
  pred_enet <- predict(enet_fit, new_data = X_test)
  r_squared_enet <- get_r_square(pred_enet, Y_test)

  # basHAL univariate ----------------------------------------------------------
  basHAL_uni_fit <- basHAL$new(X = as.matrix(X),
                               y = Y,
                               len_final_basis_set = 1000,
                               max_rows = n,
                               max_degree = 3,
                               batch_size = 1000,
                               n_batch = 200,
                               p = 0.5,
                               seed = 123,
                               family = "gaussian",
                               n_cores = 5,
                               method = "univariate glm",
                               mixture = FALSE)
  basHAL_uni_fit$run(verbose = TRUE, plot = FALSE)
  pred_basHAL_uni <- basHAL_uni_fit$predict(newx = as.matrix(X_test), type = "response")
  get_r_square(pred_basHAL_uni, Y_test)

  # basHAL small lasso ---------------------------------------------------------
  basHAL_lasso_fit <- basHAL$new(X = as.matrix(X),
                                 y = Y,
                                 len_candidate_basis_set = 100,
                                 len_final_basis_set = 1000,
                                 max_rows = n,
                                 max_degree = 3,
                                 batch_size = 200,
                                 n_batch = 200,
                                 p = 0.5,
                                 seed = 123,
                                 family = "gaussian",
                                 n_cores = 5,
                                 method = "k-variate glmnet",
                                 mixture = FALSE)
  basHAL_lasso_fit$run(verbose = TRUE, plot = FALSE)
  pred_basHAL_lasso <- basHAL_lasso_fit$predict(newx = as.matrix(X_test), type = "response")
  get_r_square(pred_basHAL_lasso, Y_test)

  # MARS screening -------------------------------------------------------------
  mars_hal_fit <- fit_hal(X = X,
                          Y = Y,
                          screen_variables = TRUE,
                          smoothness_orders = 0,
                          family = "gaussian")
  var_selected_mars <- get_selected_var_hal9001(mars_hal_fit)
  prop_correct_mars <- sum(var_selected_mars <= d) / d
  # TODO: look at correct interactions too
  pred_mars <- predict(mars_hal_fit, new_data = X_test)
  r_squared_mars <- get_r_square(pred_mars, Y_test)

  res <- data.frame(
    names = c("basHAL univariate", "basHAL small lasso", "MARS"),
    prop_correct = c(0, 0, prop_correct_mars),
    r_squared = c(0, 0, r_squared_mars))

  return(res)
}

get_selected_var_hal9001 <- function(fit) {
  var_formula <- fit$formula
  numbers <- str_extract_all(var_formula, "X(\\d+)")[[1]]
  numbers <- as.numeric(gsub("X", "", numbers))
  return(unique(numbers))
}

fit <- basHAL_uni_fit
get_selected_var_basHAL <- function(fit) {
  fit$selected_basis_set
}



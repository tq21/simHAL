.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(hal9001)
library(devtools)
library(dplyr)
load_all()
source("sim_data_corr.R")

`%+%` <- function(a, b) paste0(a, b)

# simulation parameters --------------------------------------------------------
set.seed(123)
n_runs <- 50
n <- 2000
d_vals <- c(3, 3, 3, 5, 5, 5)
scenarios <- rep(c("smooth", "jump", "sin"), 2)
corr_vals <- seq(0.4, 0.9, 0.1)
max_degree <- 3
num_knots_quant <- c(50, 15, 5)
num_knots_kmeans <- 50
results_list <- list()

# simulation -------------------------------------------------------------------
b <- 1
for (j in 1:n_runs) {
  print("run: " %+% j)

  for (corr in corr_vals) {
    print("corr: " %+% corr)

    # simulate data ------------------------------------------------------------
    n_test <- 10000
    data_list <- list(sim_tri_smooth(n, corr), sim_tri_jump(n, corr), sim_tri_sin(n, corr),
                      sim_five_smooth(n, corr), sim_five_jump(n, corr), sim_five_sin(n, corr))
    data_test_list <- list(sim_tri_smooth(n_test, corr), sim_tri_jump(n_test, corr), sim_tri_sin(n_test, corr),
                           sim_five_smooth(n_test, corr), sim_five_jump(n_test, corr), sim_five_sin(n_test, corr))

    for (i in 1:length(data_list)) {
      data <- data_list[[i]]
      data_test <- data_test_list[[i]]
      d <- d_vals[i]
      scenario <- scenarios[i]

      print("scenario: " %+% scenario %+% ", d: " %+% d)

      # quantile HAL -----------------------------------------------------------
      basis_list_quant <- enumerate_basis(x = data[, 1:d, drop = FALSE],
                                          max_degree = max_degree,
                                          smoothness_orders = 0,
                                          num_knots = num_knots_quant,
                                          screen_knots = FALSE)
      hal_quant_fit <- fit_hal(X = data[, 1:d, drop = FALSE],
                               Y = data[, d+1],
                               smoothness_orders = 0,
                               family = "gaussian",
                               basis_list = basis_list_quant)
      pred_quant <- predict(hal_quant_fit, new_data = data_test[, 1:d, drop = FALSE])
      r_squared_quant <- get_r_square(pred_quant, data_test[, d+1])
      num_bases_quant <- length(hal_quant_fit$basis_list[hal_quant_fit$coefs[-1] != 0])
      tol_bases_quant <- length(hal_quant_fit$basis_list)
      eigen_quant <- get_smallest_eigen_hal9001(hal_quant_fit, as.matrix(data[, 1:d]), FALSE)

      # k-means HAL ------------------------------------------------------------
      basis_list_kmeans <- enumerate_basis(x = data[, 1:d, drop = FALSE],
                                           max_degree = max_degree,
                                           smoothness_orders = 0,
                                           num_knots = num_knots_kmeans,
                                           screen_knots = TRUE)
      hal_fit_kmeans <- fit_hal(X = data[, 1:d, drop = FALSE],
                                Y = data[, d+1],
                                smoothness_orders = 0,
                                family = "gaussian",
                                basis_list = basis_list_kmeans)
      pred_kmeans <- predict(hal_fit_kmeans, new_data = data_test[, 1:d, drop = FALSE])
      r_squared_kmeans <- get_r_square(pred_kmeans, data_test[, d+1])
      num_bases_kmeans <- length(hal_fit_kmeans$basis_list[hal_fit_kmeans$coefs[-1] != 0])
      tol_bases_kmeans <- length(hal_fit_kmeans$basis_list)
      eigen_kmeans <- get_smallest_eigen_hal9001(hal_fit_kmeans, as.matrix(data[, 1:d]), FALSE)

      # collect results
      results_list[[b]] <- list(n = n,
                                d = d,
                                scenario = scenario,
                                corr = corr,
                                r_squared_quant = r_squared_quant,
                                r_squared_kmeans = r_squared_kmeans,
                                num_bases_quant = num_bases_quant,
                                num_bases_kmeans = num_bases_kmeans,
                                tol_bases_quant = tol_bases_quant,
                                tol_bases_kmeans = tol_bases_kmeans,
                                eigen_quant = eigen_quant,
                                eigen_kmeans = eigen_kmeans)
      b <- b + 1
    }
  }
}

results <- do.call(rbind, lapply(results_list, data.frame, stringsAsFactors = FALSE))
saveRDS(results, "out/005_corr.rds")

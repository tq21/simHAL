.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(ggplot2)
library(dplyr)
library(hal9001)
library(devtools)
library(purrr)
library(simCadlag)
load_all()

# SIMULATION PARAMETERS --------------------------------------------------------
B <- 200
n_min <- 300
n_max <- 3000
n_test <- 10000
p_min <- 10
p_max <- 30
max_degree <- 1
num_knots <- 100
dist_names <- c("normal", "uniform", "binomial")
results_list <- list()

# RUN SIMULATION ---------------------------------------------------------------
for (b in 1:B) {
  # sample simulation parameters
  n <- sample(n_min:n_max, 1)
  p <- sample(p_min:p_max, 1)

  # generate true function
  cadlag_fun <- Cadlag$new(n_vars = p, max_degree = max_degree)
  cadlag_fun$gen_formula()
  df <- cadlag_fun$gen_samples(n)
  df_test <- cadlag_fun$gen_samples(n_test)
  print("Data: " %+% b %+% "; n = " %+% n %+% "; d = " %+% p)

  # data characteristics
  dist_factor <- factor(map_vec(cadlag_fun$rv_list, ~.x$dist_name),
                        levels = dist_names)
  dist_counts <- as.numeric(table(dist_factor))

  # fit quantile-screened HAL
  hal_quant_time_0 <- Sys.time()
  basis_list_quant <- enumerate_basis(x = df$X,
                                      max_degree = max_degree,
                                      smoothness_orders = 0,
                                      num_knots = num_knots,
                                      screen_knots = FALSE)
  hal_quant_fit <- fit_hal(X = df$X,
                           Y = df$Y,
                           smoothness_orders = 0,
                           family = "gaussian",
                           basis_list = basis_list_quant)
  hal_quant_time_1 <- Sys.time()
  hal_quant_time <- as.numeric(hal_quant_time_1 - hal_quant_time_0)
  pred_quant <- predict(hal_quant_fit, new_data = df_test$X)
  r_squared_quant <- get_r_square(pred_quant, df_test$Y)
  selected_bases_quant <- length(hal_quant_fit$basis_list[hal_quant_fit$coefs[-1] != 0])
  tol_bases_quant <- length(hal_quant_fit$basis_list)

  # fit k-means-screened HAL
  hal_kmeans_time_0 <- Sys.time()
  basis_list_kmeans <- enumerate_basis(x = df$X,
                                       max_degree = max_degree,
                                       smoothness_orders = 0,
                                       num_knots = num_knots,
                                       screen_knots = TRUE)
  hal_kmeans_fit <- fit_hal(X = df$X,
                            Y = df$Y,
                            smoothness_orders = 0,
                            family = "gaussian",
                            basis_list = basis_list_kmeans)
  hal_kmeans_time_1 <- Sys.time()
  hal_kmeans_time <- as.numeric(hal_kmeans_time_1 - hal_kmeans_time_0)
  pred_kmeans <- predict(hal_kmeans_fit, new_data = df_test$X)
  r_squared_kmeans <- get_r_square(pred_kmeans, df_test$Y)
  selected_bases_kmeans <- length(hal_kmeans_fit$basis_list[hal_kmeans_fit$coefs[-1] != 0])
  tol_bases_kmeans <- length(hal_kmeans_fit$basis_list)

  # collect results
  results_list[[b]] <- list(n = n,
                            p = p,
                            total_var_norm = cadlag_fun$total_var_norm,
                            normal = dist_counts[1],
                            uniform = dist_counts[2],
                            binomial = dist_counts[3],
                            r_squared_quant = r_squared_quant,
                            r_squared_kmeans = r_squared_kmeans,
                            selected_bases_quant = selected_bases_quant,
                            selected_bases_kmeans = selected_bases_kmeans,
                            tol_bases_quant = tol_bases_quant,
                            tol_bases_kmeans = tol_bases_kmeans,
                            hal_quant_time = hal_quant_time,
                            hal_kmeans_time = hal_kmeans_time)
}

results <- do.call(rbind, lapply(results_list, data.frame, stringsAsFactors = FALSE))
saveRDS(results, "out/001_max_degree_1.rds")

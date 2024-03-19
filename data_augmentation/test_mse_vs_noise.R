.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(ggplot2)
library(dplyr)
library(hal9001)
library(devtools)
library(purrr)
load_all()

# SIMULATION PARAMETERS --------------------------------------------------------
B <- 20
n <- 200
n_test <- 10000
d_vals <- c(1, 3, 5)
max_degree <- 3
scenarios <- c("smooth", "jump", "sin")
noise_seq <- round(seq(0.01, 0.3, length.out = 20), 3)
params <- expand.grid(d = d_vals, scenario = scenarios, noise = noise_seq, run = 1:B)
params$scenario <- as.character(params$scenario)
results_list <- list()

# RUN SIMULATION ---------------------------------------------------------------
for (i in 1:nrow(params)) {
  param <- params[i, ]
  print("run: " %+% param$run %+%
          ", d: " %+% param$d %+%
          ", scenario: " %+% param$scenario %+%
          ", noise: " %+% param$noise)

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

  # fit augmented HAL
  hal_aug_fit <- fit_hal_augment(X = data[, 1:param$d, drop = FALSE],
                                 Y = data[, param$d+1],
                                 col_idx = col_idx,
                                 noise = param$noise,
                                 copies = 5,
                                 max_degree = max_degree,
                                 basis_list = basis_list)$fit
  hal_aug_pred <- predict(hal_aug_fit, new_data = data_test[, 1:param$d, drop = FALSE])
  hal_aug_mse <- get_loss(hal_aug_pred, data_test[, param$d+1], "gaussian")
  hal_aug_num_selected <- length(hal_aug_fit$basis_list[hal_aug_fit$coefs[-1] != 0])

  # store results
  results_list[[i]] <- list(run = param$run,
                            d = param$d,
                            scenario = param$scenario,
                            noise = param$noise,
                            hal_mse = hal_mse,
                            hal_num_selected = hal_num_selected,
                            hal_aug_mse = hal_aug_mse,
                            hal_aug_num_selected = hal_aug_num_selected)
}

results <- do.call(rbind, lapply(results_list, data.frame, stringsAsFactors = FALSE))
saveRDS(results, "out/test_mse_vs_noise.rds")

# plot
results_sum <- results %>%
  summarize(avg_hal_mse = mean(hal_mse),
            avg_hal_aug_mse = mean(hal_aug_mse),
            .by = c("d", "scenario", "noise"))
plot_grid <- expand.grid(d = d_vals, scenario = scenarios)

pdf("figs/test_mse_vs_noise.pdf", width = 10, height = 8)
par(mfrow = c(3, 3))
for (i in 1:nrow(plot_grid)) {
  d_cur <- plot_grid[i, ]$d
  scenario_cur <- plot_grid[i, ]$scenario
  tmp <- filter(results_sum, d == d_cur, scenario == scenario_cur)
  plot(x = tmp$noise, y = tmp$avg_hal_aug_mse, type = "p", pch = 1,
       ylim = c(min(tmp$avg_hal_mse, tmp$avg_hal_aug_mse),
                max(tmp$avg_hal_mse, tmp$avg_hal_aug_mse)),
       xlab = "Noise sd", ylab = "Test MSE",
       main = paste0("d = ", d_cur, ", ", scenario_cur))
  lines(x = tmp$noise, y = tmp$avg_hal_aug_mse, type = "b")
  abline(h = mean(tmp$avg_hal_mse), col = "red", lty = 2)
}
dev.off()

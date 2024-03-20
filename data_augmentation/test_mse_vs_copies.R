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
copies_seq <- seq(1, 10)
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
    noise <- 0.3
  } else if (param$d == 1 & param$scenario == "jump") {
    data <- sim_uni_jump(n)
    data_test <- sim_uni_jump(n_test)
    col_idx <- c(1)
    noise <- 0.025
  } else if (param$d == 1 & param$scenario == "sin") {
    data <- sim_uni_sin(n)
    data_test <- sim_uni_sin(n_test)
    col_idx <- c(1)
    noise <- 0.163
  } else if (param$d == 3 & param$scenario == "smooth") {
    data <- sim_tri_smooth(n)
    data_test <- sim_tri_smooth(n_test)
    col_idx <- c(1, 2)
    noise <- 0.3
  } else if (param$d == 3 & param$scenario == "jump") {
    data <- sim_tri_jump(n)
    data_test <- sim_tri_jump(n_test)
    col_idx <- c(1, 2)
    noise <- 0.041
  } else if (param$d == 3 & param$scenario == "sin") {
    data <- sim_tri_sin(n)
    data_test <- sim_tri_sin(n_test)
    col_idx <- c(1, 2)
    noise <- 0.239
  } else if (param$d == 5 & param$scenario == "smooth") {
    data <- sim_five_smooth(n)
    data_test <- sim_five_smooth(n_test)
    col_idx <- c(1, 2, 4, 5)
    noise <- 0.3
  } else if (param$d == 5 & param$scenario == "jump") {
    data <- sim_five_jump(n)
    data_test <- sim_five_jump(n_test)
    col_idx <- c(1, 2, 4, 5)
    noise <- 0.041
  } else if (param$d == 5 & param$scenario == "sin") {
    data <- sim_five_sin(n)
    data_test <- sim_five_sin(n_test)
    col_idx <- c(1, 2, 4, 5)
    noise <- 0.254
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

  for (copies in copies_seq) {
    print("copies: " %+% copies)

    # fit augmented HAL
    hal_aug_fit <- fit_hal_augment(X = data[, 1:param$d, drop = FALSE],
                                   Y = data[, param$d+1],
                                   col_idx = col_idx,
                                   noise = noise,
                                   copies = copies,
                                   max_degree = max_degree,
                                   basis_list = basis_list)$fit
    hal_aug_pred <- predict(hal_aug_fit, new_data = data_test[, 1:param$d, drop = FALSE])
    hal_aug_mse <- get_loss(hal_aug_pred, data_test[, param$d+1], "gaussian")
    hal_aug_num_selected <- length(hal_aug_fit$basis_list[hal_aug_fit$coefs[-1] != 0])
    res <- data.frame(run = param$run,
                      d = param$d,
                      scenario = param$scenario,
                      copies = copies,
                      mse = hal_aug_mse,
                      num_selected = hal_aug_num_selected)
    results <- rbind(results, res)
  }
}

saveRDS(results, "out/test_mse_vs_copies.rds")

# plot
results_hal <- results %>%
  filter(copies == 0) %>%
  summarize(avg_mse = mean(mse),
            avg_num_selected = mean(num_selected),
            .by = c("d", "scenario"))
results_aug <- results %>%
  filter(copies != 0) %>%
  summarize(avg_mse = mean(mse),
            avg_num_selected = mean(num_selected),
            .by = c("d", "scenario", "copies"))
plot_grid <- expand.grid(d = d_vals, scenario = scenarios)

pdf("figs/test_mse_vs_copies.pdf", width = 10, height = 8)
par(mfrow = c(3, 3))
for (i in 1:nrow(plot_grid)) {
  d_cur <- plot_grid[i, ]$d
  scenario_cur <- plot_grid[i, ]$scenario
  tmp_hal <- filter(results_hal, d == d_cur, scenario == scenario_cur)
  tmp_aug <- filter(results_aug, d == d_cur, scenario == scenario_cur)
  plot(x = tmp_aug$copies, y = tmp_aug$avg_mse, type = "p", pch = 1,
       ylim = c(min(tmp_hal$avg_mse, tmp_aug$avg_mse),
                max(tmp_hal$avg_mse, tmp_aug$avg_mse)),
       xlab = "Noise sd", ylab = "Test MSE",
       main = paste0("d = ", d_cur, ", ", scenario_cur))
  lines(x = tmp_aug$copies, y = tmp_aug$avg_mse, type = "b")
  abline(h = mean(tmp_hal$avg_mse), col = "red", lty = 2)
}
dev.off()

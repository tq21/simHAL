.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(hal9001)
library(tmle)
library(devtools)
library(origami)
library(parallel)
library(purrr)
load_all()
source("sim_data.R")

# simulation parameters --------------------------------------------------------
date <- "0420"
B <- 500
n <- 300
data_id <- 3
results <- data.frame(B = NULL,
                      est = NULL,
                      est_lower = NULL,
                      est_upper = NULL,
                      est_aug = NULL,
                      est_lower_aug = NULL,
                      est_upper_aug = NULL)

for (b in seq(B)) {
  print("run: " %+% b)

  # simulate data
  data <- sim_data(n = n, data_id = data_id)
  data_A1 <- data; data_A1$A <- 1
  data_A0 <- data; data_A0$A <- 0
  Y <- data$Y; A <- data$A; W <- data[, c("W1", "W2", "W3")]

  # HAL basis list
  Q_basis_list <- enumerate_basis(x = data.frame(W, A = A),
                                  max_degree = 3,
                                  smoothness_orders = 0)

  # regular HAL procedure ------------------------------------------------------
  g_fit <- glm(A ~ W1 + W2 + W3, family = "binomial", data = data)
  g_est <- as.numeric(predict(g_fit, newdata = data, type = "response"))
  Q_fit <- fit_hal(X = data.frame(W, A = A),
                   Y = Y,
                   basis_list = Q_basis_list,
                   smoothness_orders = 0,
                   family = "gaussian")
  Q1_est <- predict(Q_fit, new_data = data_A1)
  Q0_est <- predict(Q_fit, new_data = data_A0)
  Q_est <- data.frame(Q0 = Q0_est, Q1 = Q1_est)
  tmle_res <- tmle(Y = Y, A = A, W = W,
                   g1W = g_est, Q = Q_est,
                   family = "gaussian")

  # augmented HAL procedure ----------------------------------------------------
  Q_aug_fit <- cv_fit_hal_augment(X = data.frame(W, A = A),
                                  Y = Y,
                                  basis_list = Q_basis_list,
                                  col_idx = c(1, 2),
                                  sd_seq = c(0.05, 0.1, 0.15, 0.2),
                                  copies = 5,
                                  family = "gaussian",
                                  copies_max = NULL)
  Q1_aug_est <- predict(Q_aug_fit$fit, new_data = data_A1)
  Q0_aug_est <- predict(Q_aug_fit$fit, new_data = data_A0)
  Q_aug_est <- data.frame(Q0 = Q0_aug_est, Q1 = Q1_aug_est)
  tmle_aug_res <- tmle(Y = Y, A = A, W = W,
                       g1W = g_est, Q = Q_aug_est,
                       family = "gaussian")

  # collect results
  results <- rbind(results,
                   data.frame(B = b,
                              est = tmle_res$estimates$ATE$psi,
                              est_lower = tmle_res$estimates$ATE$CI[1],
                              est_upper = tmle_res$estimates$ATE$CI[2],
                              est_aug = tmle_aug_res$estimates$ATE$psi,
                              est_aug_lower = tmle_aug_res$estimates$ATE$CI[1],
                              est_aug_upper = tmle_aug_res$estimates$ATE$CI[2])
  )
}

saveRDS(results, "out/run_data_" %+% data_id %+% "_" %+% date %+% ".rds")

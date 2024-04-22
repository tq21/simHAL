library(hal9001)
library(tmle)
library(devtools)
library(origami)
library(furrr)
load_all()
#source("sim_data.R")

#plan(multisession, workers = 5)

# simple WAY point treatment
sim_data <- function(n) {
  # noise
  U <- rnorm(n)

  # baseline covariates
  W1 <- round(runif(n, -4, 4), 2)
  W2 <- round(runif(n, -4, 4), 2)
  W3 <- rbinom(n, 1, 0.5)

  # treatment assignment
  A <- rbinom(n, 1, plogis(0.3+0.5*W1+0.5*W2+0.7*W3))

  # outcome
  Y <- 1.5*A+W3*(4*sin(pi/2*abs(W1))*(W2 < 0)+4.1*cos(pi/2*abs(W1))*(W2 > 0))+U

  return(data.frame(W1 = W1,
                    W2 = W2,
                    W3 = W3,
                    A = A,
                    Y = Y))
}

# simulate data
n <- 50
data <- sim_data(n)
data_A1 <- data; data_A1$A <- 1
data_A0 <- data; data_A0$A <- 0
Y <- data$Y; A <- data$A; W <- data[, c("W1", "W2", "W3")]

# HAL basis list
Q_basis_list <- enumerate_basis(x = data.frame(W, A = A),
                                max_degree = 3,
                                smoothness_orders = 0)

# regular nuisance fit
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

# augmented nuisance fit
Q_aug_fit <- fit_hal_augment_safe(X = data.frame(W, A = A),
                                  Y = Y,
                                  col_idx = c(1, 2),
                                  sd_seq = c(0.05, 0.1, 0.15, 0.2),
                                  copies = 5,
                                  family = "gaussian",
                                  copies_max = NULL,
                                  basis_list = Q_basis_list,
                                  smoothness_orders = 0)
Q1_aug_est <- predict(Q_aug_fit$fit, new_data = data_A1)
Q0_aug_est <- predict(Q_aug_fit$fit, new_data = data_A0)
Q_aug_est <- data.frame(Q0 = Q0_aug_est, Q1 = Q1_aug_est)

tmle_res <- tmle(Y = Y, A = A, W = W,
                 g1W = g_est, Q = Q_est,
                 family = "gaussian")

tmle_aug_res <- tmle(Y = Y, A = A, W = W,
                     g1W = g_est, Q = Q_aug_est,
                     family = "gaussian")

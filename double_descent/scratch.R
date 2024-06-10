library(hal9001)
library(purrr)
library(ggplot2)
source("sim_data.R")

set.seed(39074)

# simulate data
n <- 1000
data <- sim_data(n)
data_A1 <- data; data_A1$A <- 1

# plug-in HAL
lambda_seq <- exp(seq(-20, -2.5, 0.5))
hal_fit <- fit_hal(X = data[, c("W1", "W2", "A")],
                   Y = data$Y,
                   smoothness_orders = 0,
                   lambda = lambda_seq,
                   family = "gaussian",
                   return_x_basis = TRUE)
coefs <- as.vector(coef(hal_fit$lasso_fit, s = "lambda.min"))[-1]
non_zero <- which(coefs != 0)
x_basis <- hal_fit$x_basis[, non_zero]
hal_fit_relax <- glm(data$Y ~ as.matrix(x_basis), family = gaussian())
psi_plug_in <- mean(as.numeric(cbind(1, x_basis) %*% coef(hal_fit_relax)))

# undersmoothing HAL
hal_u_fit <- fit_uhal(X = data[, c("W1", "W2", "A")],
                      Y = data$Y,
                      smoothness_orders = 0,
                      lambda = lambda_seq,
                      family = "gaussian",
                      return_x_basis = TRUE)
coefs <- as.vector(coef(hal_u_fit$lasso_fit, s = "lambda.min"))[-1]
non_zero <- which(coefs != 0)
x_basis <- hal_u_fit$x_basis[, non_zero]
hal_u_fit_relax <- glm(data$Y ~ as.matrix(x_basis), family = gaussian())
psi_undersmooth <- mean(as.numeric(cbind(1, x_basis) %*% coef(hal_u_fit_relax)))

# benign overfitted HAL
#lambda_seq <- hal_fit$lasso_fit$lambda
num_predictors <- map_vec(lambda_seq, function(lambda) {
  sum(as.vector(coef(hal_fit$lasso_fit, s = lambda)) != 0)
})
psi_seq <- map_vec(lambda_seq, function(lambda) {

  # obtain coefs for given lambda
  coefs <- as.vector(coef(hal_fit$lasso_fit, s = lambda))[-1]
  non_zero <- which(coefs != 0)
  x_basis <- hal_fit$x_basis[, non_zero]
  selected_basis_list <- hal_fit$basis_list[non_zero]

  # make counterfactual design matrix
  x_basis_A1 <- make_design_matrix(as.matrix(data_A1[, c("W1", "W2", "A")]),
                                   selected_basis_list)

  if (length(selected_basis_list) > 1) {
    # fit relaxed model
    ridge_fit <- cv.glmnet(x = x_basis,
                           y = data$Y,
                           nfolds = 5,
                           relax = TRUE,
                           alpha = 0)
    beta <- as.numeric(coef(ridge_fit))
    psi <- mean(as.numeric(predict(ridge_fit, s = "lambda.min", gamma = 0, newx = x_basis_A1)))

    return(psi)
  } else {

    return(NA)
  }
})

# plot
plot_data <- data.frame(lambda = lambda_seq, psi = psi_seq)
ggplot(plot_data, aes(x = log(lambda), y = psi)) +
  geom_line() +
  geom_hline(yintercept = psi_plug_in, color = "blue", linewidth = 1) +
  geom_hline(yintercept = psi_undersmooth, color = "green", linewidth = 1) +
  geom_vline(xintercept = log(hal_fit$lasso_fit$lambda.min), color = "blue", linewidth = 2) +
  geom_vline(xintercept = log(hal_u_fit$lasso_fit$lambda), color = "green", linewidth = 2) +
  geom_hline(yintercept = 0.5, color = "red", linewidth = 2) +
  labs(x = "Log(Lambda)", y = "psi", title = "") +
  theme_minimal()




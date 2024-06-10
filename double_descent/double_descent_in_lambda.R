library(hal9001)
library(purrr)
library(ggplot2)
library(devtools)
load_all()

#set.seed(123)
set.seed(2398)
n <- 500
n_test <- 10000
data_train <- sim_tri_smooth(n = n)
data_test <- sim_tri_smooth(n = n_test)
X_train <- data_train[, 1:3]
Y_train <- data_train$Y
X_test <- data_test[, 1:3]
Y_test <- data_test$Y

lambda_seq <- exp(seq(3, -9, -0.1))

# fit HAL
basis_list <- enumerate_basis(X_train, smoothness_orders = 1)
fit <- fit_hal(X = X_train,
               Y = Y_train,
               basis_list = basis_list,
               smoothness_orders = 1,
               lambda = lambda_seq,
               family = "gaussian",
               return_x_basis = TRUE)

# make basis for test data
x_basis_test <- make_design_matrix(as.matrix(X_test), fit$basis_list)
# lambda_seq <- fit$lasso_fit$lambda

test_mse <- map_vec(lambda_seq, function(lambda) {

  # obtain coefs for given lambda
  coefs <- as.vector(coef(fit$lasso_fit, s = lambda))
  non_zero <- which(coefs != 0)[-1]
  x_basis <- fit$x_basis[, non_zero]
  x_basis <- cbind(1, x_basis)
  selected_basis_list <- fit$basis_list[non_zero]
  hal_relaxed_fit <- glm.fit(
    x = x_basis, y = Y_train, family = gaussian(), intercept = FALSE)
  beta <- coef(hal_relaxed_fit)
  good_idx <- as.numeric(which(!is.na(beta)))
  beta <- beta[good_idx]
  x_basis <- x_basis[, good_idx, drop = FALSE]

  pred <- as.vector(cbind(1, x_basis_test[]) %*% beta)

  return(mean((as.vector(Y_test) - pred)^2))
})

# plot
plot_data <- data.frame(lambda = lambda_seq, test_mse = test_mse)
plot_data <- plot_data[plot_data$test_mse < 100, ]
ggplot(plot_data, aes(x = log(lambda), y = test_mse)) +
  geom_line() +
  labs(x = "Log(Lambda)", y = "Test MSE", title = "Test MSE vs. Lambda") +
  theme_minimal()
#plot(fit$lasso_fit, xvar = "")

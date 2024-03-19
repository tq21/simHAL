library(hal9001)
library(purrr)
library(ggplot2)

# simulate a sin curve
set.seed(123)
n <- 20
n_test <- 100
n_basis <- 1000
X_train <- matrix(runif(n, -5, 5))
Y_train <- sin(X_train) + rnorm(n, 0, 0.3)
X_test <- matrix(runif(n_test, -5, 5))
Y_test <- sin(X_test) + rnorm(n_test, 0, 0.3)
X_basis <- matrix(runif(n_basis, -5, 5))
X_basis <- matrix(rbind(X_basis, X_train))

# fit HAL
basis_list <- enumerate_basis(X_basis, smoothness_orders = 1)
lambda_seq <- 2^seq(-0.00000001, -10, length.out = 100)
fit <- fit_hal(X = X_train,
               Y = Y_train,
               basis_list = basis_list,
               lambda = lambda_seq,
               smoothness_orders = 1,
               family = "gaussian")

# get test MSE for each lambda
Y_test_basis <- make_design_matrix(as.matrix(X_test), fit$basis_list)
#lambda_seq <- fit$lasso_fit$lambda
test_mse <- map_vec(lambda_seq, function(lambda) {
  pred <- predict(fit$lasso_fit$glmnet.fit,
                  newx = Y_test_basis,
                  s = lambda,
                  type = "response")
  return(mean((as.vector(Y_test) - pred)^2))
})

# plot
plot_data <- data.frame(lambda = lambda_seq, test_mse = test_mse)
ggplot(plot_data, aes(x = log(lambda), y = test_mse)) +
  geom_line() +
  labs(x = "Log(Lambda)", y = "Test MSE", title = "Test MSE vs. Lambda") +
  theme_minimal()
plot(fit$lasso_fit, xvar = "")

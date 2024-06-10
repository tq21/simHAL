library(hal9001)
library(purrr)
library(ggplot2)
library(devtools)
library(glmnet)
load_all()

set.seed(123)
n <- 500
n_test <- 10000
data_train <- sim_tri_smooth(n = n)
data_test <- sim_tri_smooth(n = n_test)
X_train <- data_train[, 1:3]
Y_train <- data_train$Y
X_test <- data_test[, 1:3]
Y_test <- data_test$Y

# fit HAL
basis_list <- enumerate_basis(X_train, smoothness_orders = 1)
fit <- fit_hal(X = X_train,
               Y = Y_train,
               basis_list = basis_list,
               smoothness_orders = 1,
               family = "gaussian",
               return_x_basis = TRUE)
og_mse <- mean((as.vector(Y_test) - predict(fit, new_data = X_test))^2)

# get test MSE for each lambda
Y_test_basis <- make_design_matrix(as.matrix(X_test), fit$basis_list)
lambda_seq <- fit$lasso_fit$lambda

test_mse <- map_vec(lambda_seq, function(lambda) {

  # obtain coefs for given lambda
  coefs <- as.vector(coef(fit$lasso_fit, s = lambda))[-1]
  non_zero <- which(coefs != 0)
  x_basis <- fit$x_basis[, non_zero]
  selected_basis_list <- fit$basis_list[non_zero]

  if (length(selected_basis_list) > 1) {
    # fit ridge
    ridge_fit <- cv.glmnet(x = x_basis,
                           y = Y_train,
                           nfolds = 5,
                           relax = TRUE,
                           alpha = 0)
    beta <- as.numeric(coef(ridge_fit))

    # make design matrix on test data
    x_basis_test <- cbind(1, make_design_matrix(as.matrix(X_test), selected_basis_list))
    pred <- as.vector(x_basis_test %*% beta)

    return(mean((as.vector(Y_test) - pred)^2))
  } else {

    return(NA)
  }
})

# plot
plot_data <- data.frame(lambda = lambda_seq, test_mse = test_mse)
plot_data <- plot_data[!is.na(plot_data$test_mse), ]
ggplot(plot_data, aes(x = log(lambda), y = test_mse)) +
  geom_line() +
  geom_hline(yintercept = og_mse, linetype = "dashed") +
  geom_vline(xintercept = log(fit$lasso_fit$lambda.min), linetype = "dashed") +
  labs(x = "Log(Lambda)", y = "Test MSE", title = "Test MSE vs. Lambda") +
  theme_minimal()
#plot(fit$lasso_fit, xvar = "")

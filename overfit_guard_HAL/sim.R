library(devtools)
library(hal9001)
load_all()

set.seed(123)
dt <- round(sim_five_smooth(1000), 2)
dt_test <- round(sim_five_smooth(10000), 2)

# An overfitted decision tree
fit <- rpart(Y ~ ., data = dt, control = rpart.control(minsplit=20, cp=0))
train_pred <- as.numeric(predict(fit, newdata = dt))
test_pred <- as.numeric(predict(fit, newdata = dt_test))
get_r_square(train_pred, dt$Y) # training R^2
get_r_square(test_pred, dt_test$Y) # test R^2

# use HAL to correct overfitting
hal_fit <- fit_hal(X = dt[, 1:5],
                   Y = dt[, 6] - train_pred,
                   max_degree = 5,
                   smoothness_orders = 0,
                   num_knots = 800)
sum(hal_fit$coefs!=0)
hal_pred <- predict(hal_fit, new_data = dt[, 1:5]) + train_pred
hal_test_pred <- predict(hal_fit, new_data = dt_test[, 1:5]) + test_pred
get_r_square(hal_pred, dt$Y) # training R^2
get_r_square(hal_test_pred, dt_test$Y) # test R^2

plot(dt$X1, dt$Y, col = "blue", pch = 20)
points(dt$X1, train_pred, col = "red")


# create a data frame with 5 variables x1 to x5,
# outcome y is a linear combination of them plus interaction of x1 and x2
set.seed(123)
n <- 500
dt <- data.frame(x1 = rnorm(n),
                 x2 = rnorm(n),
                 x3 = rnorm(n),
                 x4 = rnorm(n),
                 x5 = rnorm(n))
dt$y <- with(dt, {
  y <- x1+x2+x3+x4+x5+x1*x2*x3+rnorm(n)
})

n_test <- 10000
dt_test <- data.frame(x1 = rnorm(n_test),
                      x2 = rnorm(n_test),
                      x3 = rnorm(n_test),
                      x4 = rnorm(n_test),
                      x5 = rnorm(n_test))
dt_test$y <- with(dt_test, {
  y <- x1+x2+x3+x4+x5+x1*x2*x3+rnorm(n_test)
})

# misspecified parametric model
fit <- lm(y ~ ., data = dt)
train_pred <- as.numeric(predict(fit, newdata = dt))
test_pred <- as.numeric(predict(fit, newdata = dt_test))
get_r_square(train_pred, dt$y) # training R^2
get_r_square(test_pred, dt_test$y) # test R^2

# use HAL to correct misspecification
hal_fit <- fit_hal(X = dt[, 1:5],
                   Y = dt[, 6] - train_pred,
                   max_degree = 5,
                   smoothness_orders = 0)
sum(hal_fit$coefs!=0)
hal_pred <- predict(hal_fit, new_data = dt[, 1:5]) + train_pred
hal_test_pred <- predict(hal_fit, new_data = dt_test[, 1:5]) + test_pred
get_r_square(hal_pred, dt$y) # training R^2
get_r_square(hal_test_pred, dt_test$y) # test R^2

# just fit a HAL on data
hal_fit <- fit_hal(X = dt[, 1:5],
                   Y = dt[, 6],
                   max_degree = 5,
                   smoothness_orders = 0)
sum(hal_fit$coefs!=0)
hal_pred <- predict(hal_fit, new_data = dt[, 1:5])
hal_test_pred <- predict(hal_fit, new_data = dt_test[, 1:5])
get_r_square(hal_pred, dt$y) # training R^2
get_r_square(hal_test_pred, dt_test$y) # test R^2




library(ggplot2)
library(truncnorm)

sim_uni_sin <- function(n){
  X1 <- rtruncnorm(n, a = -4, b = 4, mean = 0, sd = 2)
  Y <- 2*sin(0.5*pi*abs(X1))+2*cos(0.5*pi*abs(X1))+rnorm(n)
  return(data.frame(X1 = X1, Y = Y))
}

data <- sim_uni_sin(500)
X <- as.matrix(data[,1])
Y <- data[,2]

basis_list <- enumerate_basis(X)

# fit regular HAL
hal_fit <- fit_hal(X = X,
                   Y = Y,
                   smoothness_orders = 0,
                   basis_list = basis_list,
                   family = "gaussian")
hal_selected_knots <- sapply(hal_fit$basis_list[hal_fit$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})

# fit nn-weighted HAL
nn_fit <- fit_hal(X = X,
                  Y = Y,
                  smoothness_orders = 0,
                  basis_list = basis_list,
                  family = "gaussian",
                  weights = get_num_nn(X = X, basis_list = basis_list, delta = 1))
nn_selected_knots <- sapply(nn_fit$basis_list[nn_fit$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})

ggplot(data, aes(x = X, y = Y)) +
  geom_point() +
  scale_x_continuous(breaks = seq(-4, 4, 0.5)) +
  geom_vline(xintercept = nn_selected_knots, color = "red", alpha = 0.5) +
  theme_bw()

# plot the data and add small verticle lines at the bottom of plot
# for the selected knots
ggplot(data, aes(x = X, y = Y)) +
  geom_point() +
  scale_x_continuous(breaks = seq(-4, 4, 0.5)) +
  geom_vline(xintercept = selected_knots, color = "red", alpha = 0.5) +
  theme_bw()







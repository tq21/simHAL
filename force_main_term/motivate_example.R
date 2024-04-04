library(hal9001)
library(ggplot2)

# generate data, y = x
n <- 100
X <- rnorm(n)
Y <- X + rnorm(n)
df <- data.frame(X = X, Y = Y)

# fit linear model
lm_fit <- lm(Y ~ X, data = df)

# fit HAL model
hal_fit <- fit_hal(X = df$X,
                   Y = df$Y,
                   smoothness_orders = 1,
                   family = "gaussian")

# fit HAL model (force in main terms)
hal_fit_main <- fit_hal(X = df$X,
                        Y = df$Y,
                        smoothness_orders = 1,
                        family = "gaussian",
                        X_unpenalized = df[, "X", drop = FALSE])

# plot fits
test_X <- data.frame(X = seq(-4, 4, 0.01))
lm_pred <- predict(lm_fit, newdata = test_X)
hal_pred <- predict(hal_fit, new_data = test_X)
plot_data <- data.frame(test_X = test_X$X,
                        lm_pred = lm_pred,
                        hal_pred = hal_pred,
                        truth = test_X$X)

ggplot(data = df, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = plot_data, aes(x = test_X, y = truth), color = "red", alpha = 0.5) +
  geom_step(data = plot_data, aes(x = test_X, y = lm_pred), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

ggplot(data = df, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = plot_data, aes(x = test_X, y = truth), color = "red", alpha = 0.5) +
  geom_step(data = plot_data, aes(x = test_X, y = hal_pred), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

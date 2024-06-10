library(ggplot2)
library(ggpubr)
source("sim_data.R")
results <- readRDS("out/results.RDS")

get_knots_from_basis <- function(basis_list) {
  map_vec(basis_list, function(.x) return(.x$cutoffs))
}

# example HAL fit
set.seed(123)
data <- sim_uni_sin(100)
basis_list <- enumerate_basis(data[, 1, drop = FALSE],
                              smoothness_orders = 0,
                              max_degree = 3)
fit <- fit_hal(X = data[, 1, drop = FALSE],
               Y = data[, 2],
               smoothness_orders = 0,
               max_degree = 3,
               basis_list = basis_list,
               family = "gaussian")
knots <- sapply(fit$basis_list[fit$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})
all_knots <- get_knots_from_basis(fit$basis_list)
X <- seq(-4, 4, 0.01)
pred <- predict(fit, new_data = c(-4, knots, 4))
dt <- data.frame(X = c(-4, knots, 4), Y = pred)
ticks <- data.frame(X = all_knots, Ymin = -6, Ymax = -5.8)
truth_Y <- truth_uni_sin(X)
truth <- data.frame(X = X, Y = truth_Y)

plt_1 <- ggplot(data, aes(x = X1, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = dt, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "") +
  geom_segment(data = ticks, aes(x = X, xend = X, y = Ymin, yend = Ymax), color = "blue", size = 0.2) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

# example flip fit
set.seed(123)
data <- sim_uni_sin(100)
basis_list <- enumerate_basis(-data[, 1, drop = FALSE],
                              smoothness_orders = 0,
                              max_degree = 3)
fit <- fit_hal(X = data[, 1, drop = FALSE],
               Y = data[, 2],
               smoothness_orders = 0,
               max_degree = 3,
               basis_list = basis_list,
               family = "gaussian")
knots <- sapply(fit$basis_list[fit$coefs[-1] != 0], function(.x) {
  -.x$cutoffs
})
all_knots <- get_knots_from_basis(fit$basis_list)
X <- seq(-4, 4, 0.01)
pred <- predict(fit, new_data = c(-4, knots, 4))
dt <- data.frame(X = c(-4, knots, 4), Y = pred)
ticks <- data.frame(X = all_knots, Ymin = -6, Ymax = -5.8)
truth_Y <- truth_uni_sin(X)
truth <- data.frame(X = X, Y = truth_Y)

plt_3 <- ggplot(data, aes(x = X1, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = dt, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "") +
  geom_segment(data = ticks, aes(x = X, xend = X, y = Ymin, yend = Ymax), color = "blue", size = 0.2) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

# plot sin scenario
sin_avg_abs_diff <- map(results, function(.x) {
  if (.x$scenario == "sin") {
    return(abs(.x$diff))
  } else {
    return(NULL)
  }
})
sin_avg_abs_diff <- Filter(Negate(is.null), sin_avg_abs_diff)
sin_avg_abs_diff <- Reduce("+", sin_avg_abs_diff) / length(sin_avg_abs_diff)

X <- seq(-4, 4, 0.01)
data <- data.frame(X = X, sin_avg_abs_diff = sin_avg_abs_diff)
plt_2 <- ggplot(data, aes(x = X, y = sin_avg_abs_diff)) +
  geom_line(linewidth = 1) +
  labs(x = "X", y = "Absolute Bias") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

plt <- ggarrange(plt_1, plt_2, ncol = 2, nrow = 1)

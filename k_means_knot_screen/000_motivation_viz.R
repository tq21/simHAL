library(ggplot2)
library(hal9001)
library(devtools)
library(ggpubr)
library(dplyr)
library(purrr)
load_all()

`%+%` <- function(a, b) paste0(a, b)

sim_sin <- function(n) {
  X <- runif(n, -4, 4)
  Y <- 4*sin(1/4*pi*X)+rnorm(n)
  return(data.frame(X = X, Y = Y))
}

get_knots_from_basis <- function(basis_list) {
  map_vec(basis_list, function(.x) return(.x$cutoffs))
}

# simulate data ----------------------------------------------------------------
set.seed(32905)
#set.seed(123)
n_sparse <- 20
n_dense <- 2000
n <- 500
n_test <- 10000
data_unif <- sim_sin(n)
data_sparse <- sim_sin(n_sparse)
data_dense <- sim_sin(n_dense) %>% filter(X > 0)
data_dense <- data_dense[sample(nrow(data_dense), n - n_sparse, replace = FALSE),]
data <- rbind(data_sparse, data_dense)
data_test <- sim_sin(n_test)
num_knots <- 100

# uniform X --------------------------------------------------------------------
# HAL
basis_list <- enumerate_basis(x = data_unif[, 1, drop = FALSE],
                              smoothness_orders = 0,
                              screen_knots = FALSE)
hal_fit <- fit_hal(X = data_unif[, 1, drop = FALSE],
                   Y = data_unif[, 2],
                   family = "gaussian",
                   smoothness_orders = 0,
                   basis_list = basis_list)
hal_pred <- predict(hal_fit, new_data = data_test[, 1, drop = FALSE])
hal_r_squared <- get_r_square(hal_pred, data_test[, 2])
hal_knots <- sapply(hal_fit$basis_list[hal_fit$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})
hal_l1_norm <- round(sum(abs(hal_fit$coefs)), 1)

# quantile HAL
quant_basis_list <- enumerate_basis(x = data_unif[, 1, drop = FALSE],
                                    smoothness_orders = 0,
                                    screen_knots = FALSE,
                                    num_knots = num_knots)
quant_fit <- fit_hal(X = data_unif[, 1, drop = FALSE],
                     Y = data_unif[, 2],
                     family = "gaussian",
                     smoothness_orders = 0,
                     basis_list = quant_basis_list)
quant_all_knots <- get_knots_from_basis(quant_fit$basis_list)
quant_pred <- predict(quant_fit, new_data = data_test[, 1, drop = FALSE])
quant_r_squared <- get_r_square(quant_pred, data_test[, 2])
quant_knots <- sapply(quant_fit$basis_list[quant_fit$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})
quant_l1_norm <- round(sum(abs(quant_fit$coefs)), 1)

# k-means HAL
kmeans_basis_list <- enumerate_basis(x = data_unif[, 1, drop = FALSE],
                                     smoothness_orders = 0,
                                     screen_knots = TRUE,
                                     num_knots = num_knots)
kmeans_fit <- fit_hal(X = data_unif[, 1, drop = FALSE],
                      Y = data_unif[, 2],
                      family = "gaussian",
                      smoothness_orders = 0,
                      basis_list = kmeans_basis_list)
kmeans_all_knots <- get_knots_from_basis(kmeans_fit$basis_list)
kmeans_pred <- predict(kmeans_fit, new_data = data_test[, 1, drop = FALSE])
kmeans_r_squared <- get_r_square(kmeans_pred, data_test[, 2])
kmeans_knots <- sapply(kmeans_fit$basis_list[kmeans_fit$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})
kmeans_l1_norm <- round(sum(abs(kmeans_fit$coefs)), 1)

# sparse X ---------------------------------------------------------------------
# HAL
basis_list_sparse <- enumerate_basis(x = data[, 1, drop = FALSE],
                                     smoothness_orders = 0,
                                     screen_knots = FALSE)
hal_fit_sparse <- fit_hal(X = data[, 1, drop = FALSE],
                          Y = data[, 2],
                          family = "gaussian",
                          smoothness_orders = 0,
                          basis_list = basis_list_sparse)
hal_pred_sparse <- predict(hal_fit_sparse, new_data = data_test[, 1, drop = FALSE])
hal_r_squared_sparse <- get_r_square(hal_pred_sparse, data_test[, 2])
hal_knots_sparse <- sapply(hal_fit_sparse$basis_list[hal_fit_sparse$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})
hal_l1_norm_sparse <- round(sum(abs(hal_fit_sparse$coefs)), 1)

# quantile HAL
quant_basis_list_sparse <- enumerate_basis(x = data[, 1, drop = FALSE],
                                           smoothness_orders = 0,
                                           screen_knots = FALSE,
                                           num_knots = num_knots)
quant_fit_sparse <- fit_hal(X = data[, 1, drop = FALSE],
                            Y = data[, 2],
                            family = "gaussian",
                            smoothness_orders = 0,
                            basis_list = quant_basis_list_sparse)
quant_all_knots_sparse <- get_knots_from_basis(quant_fit_sparse$basis_list)
quant_pred_sparse <- predict(quant_fit_sparse, new_data = data_test[, 1, drop = FALSE])
quant_r_squared_sparse <- get_r_square(quant_pred_sparse, data_test[, 2])
quant_knots_sparse <- sapply(quant_fit_sparse$basis_list[quant_fit_sparse$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})
quant_l1_norm_sparse <- round(sum(abs(quant_fit_sparse$coefs)), 1)

# k-means HAL
kmeans_basis_list_sparse <- enumerate_basis(x = data[, 1, drop = FALSE],
                                            smoothness_orders = 0,
                                            screen_knots = TRUE,
                                            num_knots = num_knots)
kmeans_fit_sparse <- fit_hal(X = data[, 1, drop = FALSE],
                             Y = data[, 2],
                             family = "gaussian",
                             smoothness_orders = 0,
                             basis_list = kmeans_basis_list)
kmeans_all_knots_sparse <- get_knots_from_basis(kmeans_fit_sparse$basis_list)
kmeans_pred_sparse <- predict(kmeans_fit_sparse, new_data = data_test[, 1, drop = FALSE])
kmeans_r_squared_sparse <- get_r_square(kmeans_pred_sparse, data_test[, 2])
kmeans_knots_sparse <- sapply(kmeans_fit_sparse$basis_list[kmeans_fit_sparse$coefs[-1] != 0], function(.x) {
  .x$cutoffs
})
kmeans_l1_norm_sparse <- round(sum(abs(kmeans_fit_sparse$coefs)), 1)

# collect results --------------------------------------------------------------
res <- data.frame(
  names = c("HAL", "quantile HAL", "kmeans HAL", "HAL", "quantile HAL", "kmeans HAL"),
  scenario = c("uniform", "uniform", "uniform", "sparse", "sparse", "sparse"),
  r_squared = c(hal_r_squared, quant_r_squared, kmeans_r_squared, hal_r_squared_sparse, quant_r_squared_sparse, kmeans_r_squared_sparse),
  l1_norm = c(hal_l1_norm, quant_l1_norm, kmeans_l1_norm, hal_l1_norm_sparse, quant_l1_norm_sparse, kmeans_l1_norm_sparse))

# get truths -------------------------------------------------------------------
truth_X <- seq(-4, 4, 0.01)
truth_Y <- 4*sin(1/4*pi*truth_X)
truth <- data.frame(X = truth_X, Y = truth_Y)

# HAL, uniform X
y_hal <- predict(hal_fit, new_data = c(-4, hal_knots, 4))
hal_dt <- data.frame(X = c(-4, hal_knots, 4), Y = y_hal)
hal_plt <- ggplot(data_unif, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = hal_dt, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "HAL (L1-norm=" %+% hal_l1_norm %+% ")") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

# HAL, sparse X
y_hal_sparse <- predict(hal_fit_sparse, new_data = c(-4, hal_knots_sparse, 4))
hal_dt_sparse <- data.frame(X = c(-4, hal_knots_sparse, 4), Y = y_hal_sparse)
hal_plt_sparse <- ggplot(data, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = hal_dt_sparse, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "HAL (L1-norm=" %+% hal_l1_norm_sparse %+% ")") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

# quantile HAL, uniform X
y_quant <- predict(quant_fit, new_data = c(-4, quant_knots, 4))
quant_dt <- data.frame(X = c(-4, quant_knots, 4), Y = y_quant)
quant_ticks <- data.frame(X = quant_all_knots, Ymin = -6, Ymax = -5.8)
quant_plt <- ggplot(data_unif, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = quant_dt, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "quantile HAL (L1-norm=" %+% quant_l1_norm %+% ")") +
  geom_segment(data = quant_ticks, aes(x = X, xend = X, y = Ymin, yend = Ymax), color = "blue", size = 0.2) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

# quantile HAL, sparse X
y_quant_sparse <- predict(quant_fit_sparse, new_data = c(-4, quant_knots_sparse, 4))
quant_dt_sparse <- data.frame(X = c(-4, quant_knots_sparse, 4), Y = y_quant_sparse)
quant_ticks_sparse <- data.frame(X = quant_all_knots_sparse, Ymin = -6, Ymax = -5.8)
quant_plt_sparse <- ggplot(data, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = quant_dt_sparse, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "quantile HAL (L1-norm=" %+% quant_l1_norm_sparse %+% ")") +
  geom_segment(data = quant_ticks_sparse, aes(x = X, xend = X, y = Ymin, yend = Ymax), color = "blue", size = 0.2) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

# k-means HAL, uniform X
y_kmeans <- predict(kmeans_fit, new_data = c(-4, kmeans_knots, 4))
kmeans_dt <- data.frame(X = c(-4, kmeans_knots, 4), Y = y_kmeans)
kmeans_ticks <- data.frame(X = kmeans_all_knots, Ymin = -6, Ymax = -5.8)
kmeans_plt <- ggplot(data_unif, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = kmeans_dt, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "k-means HAL (L1-norm=" %+% kmeans_l1_norm %+% ")") +
  geom_segment(data = kmeans_ticks, aes(x = X, xend = X, y = Ymin, yend = Ymax), color = "blue", size = 0.2) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

# k-means HAL, sparse X
y_kmeans_sparse <- predict(kmeans_fit_sparse, new_data = c(-4, kmeans_knots_sparse, 4))
kmeans_dt_sparse <- data.frame(X = c(-4, kmeans_knots_sparse, 4), Y = y_kmeans_sparse)
kmeans_ticks_sparse <- data.frame(X = kmeans_all_knots_sparse, Ymin = -6, Ymax = -5.8)
kmeans_plt_sparse <- ggplot(data, aes(x = X, y = Y)) +
  geom_point(alpha = 0.1) +
  geom_line(data = truth, aes(x = X, y = Y), color = "red", alpha = 0.5) +
  geom_step(data = kmeans_dt_sparse, aes(x = X, y = Y), color = "blue") +
  scale_x_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  labs(x = "X", y = "Y", title = "k-means HAL (L1-norm=" %+% kmeans_l1_norm_sparse %+% ")") +
  geom_segment(data = kmeans_ticks_sparse, aes(x = X, xend = X, y = Ymin, yend = Ymax), color = "blue", size = 0.2) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, size = 8),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  theme_bw()

plt <- ggarrange(hal_plt, quant_plt, kmeans_plt,
                 hal_plt_sparse, quant_plt_sparse, kmeans_plt_sparse,
                 ncol = 3, nrow = 2)
ggsave("figs/000_motivation_viz.pdf", plot = plt, device = "pdf",
       width = 10, height = 6, dpi = 300, units = "in")

library(hal9001)
library(basHAL)
library(ggpubr)
library(ggplot2)
library(plotly)
library(devtools)
load_all()

# simulate univariate X, Y is a linear combination of 3 0-order HAL bases
# with 3 random coefficients
set.seed(123)
n <- 500
dt_train <- data_gen(500)
dt_test <- data_gen(10000)

# seems larger coefficients more pronounced peak?
# normalization?

# fit hal9001
fit_9001 <- fit_hal(X = dt_train[, 1:4],
                    Y = dt_train[, 5],
                    max_degree = 2,
                    smoothness_orders = 0,
                    num_knots = n)
length(fit_9001$basis_list[fit_9001$coefs!=0])
test_pred_9001 <- as.numeric(predict(fit_9001, new_data = dt_test))
get_r_square(test_pred_9001, dt_test$Y)

# fit basHAL
fit_basHAL <- basHAL$new(X = as.matrix(X),
                         y = Y,
                         len_final_basis_set = 500,
                         max_rows = n,
                         max_degree = 2,
                         batch_size = 200,
                         n_batch = 200,
                         p = 0.5,
                         seed = 123,
                         family = "gaussian",
                         n_cores = 5,
                         method = "univariate glm",
                         mixture = FALSE)
fit_basHAL$run(verbose = TRUE, plot = FALSE)
length(fit_basHAL$selected_basis_set)
test_pred_basHAL <- fit_basHAL$predict(newx = as.matrix(dt_test[, 1:4]), type = "response")
get_r_square(test_pred_basHAL, dt_test$Y)

# plot
plt_X1 <- get_post_plt(fit_basHAL$probs, "1", -3)
plt_X2 <- get_post_plt(fit_basHAL$probs, "2", -2)
plt_X3 <- get_post_plt(fit_basHAL$probs, "3", 0)
plt_X4 <- get_post_plt(fit_basHAL$probs, "4", 2)
plt <- ggarrange(plt_X1, plt_X2, plt_X3, plt_X4,
                 nrow = 1, ncol = 4)
ggsave("plots/1012_one_way_bases.pdf", plot = plt, device = "pdf",
       width = 20, height = 4, dpi = 300, units = "in")

plt_X1X2 <- get_post_bivar_plt(fit_basHAL$probs, "1,2", c(-2, 2))
plt_X2X3 <- get_post_bivar_plt(fit_basHAL$probs, "2,3", c(-2, 2))
plt_X3X4 <- get_post_bivar_plt(fit_basHAL$probs, "3,4", c(-2, 2))
plt_X1X4 <- get_post_bivar_plt(fit_basHAL$probs, "1,4", c(-2, 2))

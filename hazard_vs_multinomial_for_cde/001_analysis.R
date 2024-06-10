library(dplyr)
library(gtools)
library(hal9001)
library(nnet)
library(devtools)
library(purrr)
library(ggplot2)
library(ggpubr)
load_all()
set.seed(123)
source("sim_data_constant_hazard.R")
source("utils.R")

# load results
res_500 <- readRDS("out/001_run_sim_n_500.rds")
res_1000 <- readRDS("out/001_run_sim_n_1000.rds")
res_1500 <- readRDS("out/001_run_sim_n_1500.rds")
res_2000 <- readRDS("out/001_run_sim_n_2000.rds")

B <- 5
n_test <- 10000
W <- c("W1", "W2")
A <- "A"

# compute log-likelihood on test data
loglik <- function(pred, A) {
  loglik <- sum(log(pred[cbind(1:nrow(pred), A)]))
  return(loglik)
}

data_test_all <- map(1:B, function(.x) {
  return(sim_data(n_test))
})

get_results <- function(res, data_test_all, sample_size) {
  res_df <- map2_dfr(res, data_test_all, function(.x, .y) {

    # glm multinomial
    glm_multinom_pred <- predict(.x$glm_multinom_fit, data = .y, type = "probs")
    glm_multinom_loglik <- loglik(glm_multinom_pred, .y$A)

    # hal hazard regression
    hal_haz_reg_pred <- predict_cde_hazard(fit = .x$hal_haz_reg_fit, new_data = .y, W = W, A = A)
    hal_haz_reg_loglik <- loglik(hal_haz_reg_pred, .y$A)

    # hal multinomial
    hal_multinom_pred <- predict(.x$hal_multinom_fit, new_data = .y[, ..W], type = "response")
    hal_multinom_loglik <- loglik(hal_multinom_pred, .y$A)

    return(data.frame(n = sample_size,
                      glm_multinom_loglik = glm_multinom_loglik,
                      hal_haz_reg_loglik = hal_haz_reg_loglik,
                      hal_multinom_loglik = hal_multinom_loglik))
  })

  return(res_df)
}

df_500 <- get_results(res_500, data_test_all, sample_size = 500)
df_1000 <- get_results(res_1000, data_test_all, sample_size = 1000)
df_1500 <- get_results(res_1500, data_test_all, sample_size = 1500)
df_2000 <- get_results(res_2000, data_test_all, sample_size = 2000)

df_500 <- df_500 %>% summarize(glm_multinom_loglik = mean(glm_multinom_loglik),
                               hal_haz_reg_loglik = mean(hal_haz_reg_loglik),
                               hal_multinom_loglik = mean(hal_multinom_loglik))
df_1000 <- df_1000 %>% summarize(glm_multinom_loglik = mean(glm_multinom_loglik),
                                 hal_haz_reg_loglik = mean(hal_haz_reg_loglik),
                                 hal_multinom_loglik = mean(hal_multinom_loglik))
df_1500 <- df_1500 %>% summarize(glm_multinom_loglik = mean(glm_multinom_loglik),
                                 hal_haz_reg_loglik = mean(hal_haz_reg_loglik),
                                 hal_multinom_loglik = mean(hal_multinom_loglik))
df_2000 <- df_2000 %>% summarize(glm_multinom_loglik = mean(glm_multinom_loglik),
                                 hal_haz_reg_loglik = mean(hal_haz_reg_loglik),
                                 hal_multinom_loglik = mean(hal_multinom_loglik))
df <- bind_rows(df_500, df_1000, df_1500, df_2000)
df$n <- c(500, 1000, 1500, 2000)

# plot
plt_hal_multi_loglik <- ggplot(df, aes(x = n)) +
  geom_line(aes(y = hal_multinom_loglik), color = "red", linewidth = 1.5) +
  labs(title = "HAL (multinomial)",
       y = "Log-likelihood",
       x = "Sample size") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5))
plt_hal_haz_reg_loglik <- ggplot(df, aes(x = n)) +
  geom_line(aes(y = hal_haz_reg_loglik), color = "blue", linewidth = 1.5) +
  labs(title = "HAL (hazard-based)",
       y = "Log-likelihood",
       x = "Sample size") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5))
plt_glm_multi_loglik <- ggplot(df, aes(x = n)) +
  geom_line(aes(y = glm_multinom_loglik), color = "purple", linewidth = 1.5) +
  labs(title = "Misspecified GLM (multinomial)",
       y = "Log-likelihood",
       x = "Sample size") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5))

plt <- ggarrange(plt_hal_multi_loglik, plt_hal_haz_reg_loglik, plt_glm_multi_loglik,
                 nrow = 1, ncol = 3, common.legend = TRUE)
ggsave(filename = "hazard_vs_multinomial.pdf", plot = plt, device = "pdf",
       path = "figs", width = 16, height = 6, dpi = 300)

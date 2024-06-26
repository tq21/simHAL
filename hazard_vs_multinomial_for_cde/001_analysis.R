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
source("001_sim_data_constant_hazard.R")
source("utils.R")

# load results
res_500 <- readRDS("out/001_run_sim_n_500.rds")
res_1000 <- readRDS("out/001_run_sim_n_1000.rds")
res_1500 <- readRDS("out/001_run_sim_n_1500.rds")
res_2000 <- readRDS("out/001_run_sim_n_2000.rds")

B <- 1
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
    hal_haz_reg_pred <- predict_cde_hazard(fit = .x$hal_haz_reg_fit, new_data = .y, W = W, tau = 5)
    hal_haz_reg_loglik <- loglik(hal_haz_reg_pred, .y$A)

    # hal multinomial
    tmp_data <- .y[rep(1:nrow(.y), each = 5),]
    tmp_data[, id := rep(seq(n_test), each = 5)]
    tmp_data$A <- rep(1:5, nrow(.y))

    suppressMessages({hal_multinom_pred <- map_dfc(1:5, function(.k) {
      tmp_data <- copy(.y)
      tmp_data$A <- .k
      hal_multinom_pred <- predict(.x$hal_multinom_fit, new_data = tmp_data[, c(..W, ..A)], type = "response")
      hal_multinom_pred <- hal_multinom_pred[, .k]

      return(data.frame(hal_multinom_pred))
    })})
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

# plot all log likelihoods on one single plot, add legend
plt <- ggplot(df, aes(x = n)) +
  geom_line(aes(y = hal_multinom_loglik, color = "HAL (multinomial)"), linewidth = 1.5) +
  geom_line(aes(y = hal_haz_reg_loglik, color = "HAL (hazard-based)"), linewidth = 1.5) +
  labs(title = "Scenario 1",
       y = "Log-likelihood",
       x = "Sample size",
       color = "Method") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5))#,
        #legend.position = "none")

saveRDS(plt, "figs/plt_scenario_1.rds")

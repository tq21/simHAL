library(dplyr)
library(gtools)
library(hal9001)
library(nnet)
library(devtools)
library(purrr)
load_all()
set.seed(123)
source("sim_data.R")
source("utils.R")

# load results
res <- readRDS("out/001_run_sim.rds")

B <- 50
n_test <- 10000
W <- c("W1", "W2")
A <- "A"

# compute log-likelihood on test data
loglik <- function(pred, A) {
  loglik <- sum(log(pred[cbind(1:nrow(pred), A)]))
  return(loglik)
}

results <- map(res, function(.x) {
  # generate test data
  data_test <- sim_data(n_test)

  # glm multinomial
  glm_multinom_pred <- predict(.x$glm_multinom_fit, data = data_test, type = "probs")
  glm_multinom_loglik <- loglik(glm_multinom_pred, data_test$A)

  # hal hazard regression
  hal_haz_reg_pred <- predict_cde_hazard(fit = .x$hal_haz_reg_fit, new_data = data_test, W = W, A = A)
  hal_haz_reg_loglik <- loglik(hal_haz_reg_pred, data_test$A)

  # hal multinomial
  hal_multinom_pred <- predict(.x$hal_multinom_fit, new_data = data_test[, ..W], type = "response")
  hal_multinom_loglik <- loglik(hal_multinom_pred, data_test$A)

  return(list(glm_multinom_loglik = glm_multinom_loglik,
              hal_haz_reg_loglik = hal_haz_reg_loglik,
              hal_multinom_loglik = hal_multinom_loglik))
})

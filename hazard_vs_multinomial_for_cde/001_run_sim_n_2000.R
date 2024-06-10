.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(dplyr)
library(gtools)
library(hal9001)
library(nnet)
library(devtools)
load_all()
set.seed(123)
source("001_sim_data_constant_hazard.R")
source("utils.R")

# SIMULATION PARAMETERS --------------------------------------------------------
B <- 50
n <- 2000
W <- c("W1", "W2")
A <- "A"
results_list <- list()

for (b in 1:B) {
  print("run " %+% b)

  # simulate data
  data <- sim_data(n)

  # multinomial logistic regression
  glm_multinom_fit <- multinom(A ~ ., data = data)

  # HAL hazard regression method
  hal_haz_reg_fit <- cde_hazard(data = data, W = W, A = A)

  # HAL multinomial regression method
  hal_multinom_fit <- fit_hal(X = data[, ..W],
                              Y = data[[A]],
                              max_degree = 3,
                              smoothness_orders = 1,
                              family = "multinomial")

  # collect results
  results_list[[b]] <- list(glm_multinom_fit = glm_multinom_fit,
                            hal_haz_reg_fit = hal_haz_reg_fit,
                            hal_multinom_fit = hal_multinom_fit)
}

saveRDS(results_list, "out/001_run_sim_n_2000.rds")

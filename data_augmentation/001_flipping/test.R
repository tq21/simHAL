.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(ggplot2)
library(dplyr)
library(hal9001)
library(devtools)
library(purrr)
library(origami)
load_all()
source("sim_data.R")

# MAKE GRID --------------------------------------------------------------------
X <- seq(-4, 4, 0.01)
truth_smooth <- truth_uni_smooth(X)
truth_sin <- truth_uni_sin(X)
truth_jump <- truth_uni_jump(X)

# SIMULATION PARAMETERS --------------------------------------------------------
B <- 500
n <- 100
d_vals <- 1
max_degree <- 3
scenarios <- c("smooth", "jump", "sin")
params <- expand.grid(d = d_vals, scenario = scenarios, run = 1:B)
params$scenario <- as.character(params$scenario)
results <- vector("list", nrow(params))

# RUN SIMULATION ---------------------------------------------------------------
for (i in 1:nrow(params)) {
  param <- params[i, ]
  print("run: " %+% param$run %+%
          ", d: " %+% param$d %+%
          ", scenario: " %+% param$scenario)

  # simulate data
  if (param$scenario == "smooth") {
    data <- sim_uni_smooth(n)
    truth <- truth_smooth
  } else if (param$scenario == "jump") {
    data <- sim_uni_jump(n)
    truth <- truth_jump
  } else if (param$scenario == "sin") {
    data <- sim_uni_sin(n)
    truth <- truth_sin
  }

  # make basis list
  basis_list <- enumerate_basis(data[, 1:param$d, drop = FALSE],
                                smoothness_orders = 0,
                                max_degree = max_degree)

  # fit regular HAL
  fit <- fit_hal(X = data[, 1:param$d, drop = FALSE],
                 Y = data[, param$d+1],
                 smoothness_orders = 0,
                 max_degree = max_degree,
                 basis_list = basis_list,
                 family = "gaussian")
  pred <- predict(fit, new_data = X)
  diff <- pred - truth

  results[[i]] <- list(scenario = param$scenario,
                       d = param$d,
                       diff = diff)
}

saveRDS(results, "out/results.RDS")

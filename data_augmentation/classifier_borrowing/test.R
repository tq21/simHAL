.libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
            .libPaths()))
library(hal9001)
library(tmle)
library(devtools)
library(origami)
library(furrr)
library(purrr)
load_all()
source("sim_data.R")

# we first train a classifier to distinguish between real and augmented data

# simulate data
data <- sim_data(n = n, data_id = data_id)
data_A1 <- data; data_A1$A <- 1
data_A0 <- data; data_A0$A <- 0
Y <- data$Y; A <- data$A; W <- data[, c("W1", "W2", "W3")]

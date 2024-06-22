library(hal9001)
library(data.table)
library(purrr)
source("sim_data.R")
source("../hazard_vs_multinomial_for_cde/utils.R")

set.seed(123)
n <- 2000
data <- as.data.table(sim_data(n))
data$S <- 1

# perturb data
Y_perturb <- 2
data_perturb <- copy(data); data_perturb$Y <- Y_perturb; data_perturb$S <- 0
data_combined <- rbind(data, data_perturb)

# train a HAL classifier
fit <- fit_hal(X = data_combined[, c("X", "Y")],
               Y = data_combined$S,
               smoothness_orders = 1,
               family = "binomial")

cutoffs <- map(fit$basis_list[fit$coefs[-1] != 0], function(.x) {
  .x$cutoffs[which(2 == .x$cols)]
})
cutoffs <- sort(unique(unlist(Filter(function(x) length(x) > 0, cutoffs))))

hist(data$Y, breaks = 20)
for (cutoff in cutoffs) {
  abline(v = cutoff, col = "red")
}

# if I use the cutoff as discretization of Y, how many observations do I have
# in each bin?
data$Y_bin <- cut(data$Y, breaks = cutoffs)
table(data$Y_bin)

# fit <- cde_hazard(data = data, W = "X", A = "Y")

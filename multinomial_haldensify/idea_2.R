# first propose an initial set of discretization cutoffs based on quantile of Y
# then discretize Y, fit multinomial HAL to learn the conditional density of Y on X
# then obtain the cutoffs of Y in the basis functions of the HAL fit with non-zero
# coefficients, use them as data-adaptively learned discretization cutoffs.
# In the final step, we renormalize the predicted densities so that under the
# data-adaptively learned discretization, the predicted density function is
# a valid density that integrates to 1 over the support of Y.
library(hal9001)
library(data.table)
library(purrr)
library(nnet)
source("sim_data.R")
source("utils.R")

set.seed(123)
n <- 500
data <- as.data.table(sim_data(n))

# initial set of discretization cutoffs
K <- 3
cutoffs <- as.numeric(quantile(data$Y, probs = seq(0, 1, length.out = K)))

# discretize Y based on the cutoffs
data$Y_bin <- as.integer(cut(data$Y, breaks = cutoffs, include.lowest = TRUE))

# fit multinomial HAL
fit <- fit_hal(X = data[, c("X", "Y")],
               Y = data$Y_bin,
               smoothness_orders = 1,
               family = "multinomial")
selected_basis_list <- unlist(map(1:(K-1), function(.k) {
  fit$basis_list[fit$coefs[.k,1][[1]][-1] != 0]
}), recursive = FALSE)

cutoffs <- map(selected_basis_list, function(.x) {
  .x$cutoffs[which(2 == .x$cols)]
})
cutoffs <- sort(unique(unlist(Filter(function(x) length(x) > 0, cutoffs))))
hist(data$Y, breaks = 100)
for (cutoff in cutoffs) {
  abline(v = cutoff, col = "red")
}
tmp <- cut(data$Y, breaks = cutoffs, include.lowest = TRUE)
table(tmp)




set.seed(123)
n <- 500
data <- as.data.table(sim_data(n))
fit <- fit_adaptive_multi_hal(data = data,
                              X = "X",
                              Y = "Y",
                              init_K = 5)




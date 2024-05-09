library(hal9001)
library(purrr)

set.seed(123)

# n grows
n_vals <- seq(100, 2000, 100)
d <- 5
n_grows_mem_use <- map_vec(n_vals, function(n) {
  X <- matrix(rnorm(n * d), nrow = n, ncol = d)
  basis_list <- enumerate_basis(x = X, max_degree = d)
  design_mat <- make_design_matrix(X = X, blist = basis_list)

  return(as.numeric(gsub(" Mb", "", format(object.size(design_mat), units = "MB"))))
})

# d grows
d_vals <- seq(1, 10)
n <- 500
d_grows_mem_use <- map_vec(d_vals, function(d) {
  X <- matrix(rnorm(n * d), nrow = n, ncol = d)
  basis_list <- enumerate_basis(x = X, max_degree = d)
  design_mat <- make_design_matrix(X = X, blist = basis_list)

  return(as.numeric(gsub(" Mb", "", format(object.size(design_mat), units = "MB"))))
})

# plot
pdf("figs/mem_plot.pdf", width = 10, height = 8)
par(mfrow = c(1, 2))

# plot n grows
plot(x = n_vals, y = n_grows_mem_use, type = "p", pch = 1,
     xlab = "Sample Size", ylab = "HAL Design Matrix Size (MB)")
lines(x = n_vals, y = n_grows_mem_use, type = "b")

# plot d grows
plot(x = d_vals, y = d_grows_mem_use, type = "p", pch = 1,
     xlab = "Number of Features", ylab = "HAL Design Matrix Size (MB)")
lines(x = d_vals, y = d_grows_mem_use, type = "b")

dev.off()




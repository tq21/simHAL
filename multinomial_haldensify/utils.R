fit_adaptive_multi_hal <- function(data,
                                   X,
                                   Y,
                                   init_K) {

  # initial discretization scheme
  cutoffs <- as.numeric(quantile(data[[Y]], probs = seq(0, 1, length.out = init_K)))
  set(data, j = "Y_bin", value = as.integer(cut(data[[Y]], breaks = cutoffs, include.lowest = TRUE)))

  # fit multinomial HAL based on initial discretization scheme
  fit <- fit_hal(X = data[, c(..X, ..Y)],
                 Y = data$Y_bin,
                 smoothness_orders = 1,
                 family = "multinomial",
                 return_x_basis = TRUE)

  # obtain data-adaptively selected cutoffs
  Y_col_idx <- length(X) + 1
  selected_basis_list <- unlist(map(1:(init_K-1), function(.k) {
    fit$basis_list[fit$coefs[.k,1][[1]][-1] != 0]
  }), recursive = FALSE)
  new_cutoffs <- map(selected_basis_list, function(.x) {
    .x$cutoffs[which(Y_col_idx == .x$cols)]
  })
  new_cutoffs <- sort(unique(unlist(Filter(function(x) length(x) > 0, new_cutoffs))))

  # discretize Y based on the data-adaptively selected cutoffs
  set(data, j = "Y_bin_new", value = as.integer(cut(data[[Y]], breaks = new_cutoffs, include.lowest = TRUE)))

  # refit multinomial logistic regression under the new discretization scheme
  selected_basis <- as.matrix(make_design_matrix(X = as.matrix(data[, c(..X, ..Y)]), selected_basis_list))
  glm_multinom_fit <- multinom(data$Y_bin_new ~ ., data = as.data.frame(selected_basis))

  return(glm_multinom_fit)
}

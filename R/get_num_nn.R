#' Function to get the number of nearest neighbors within delta distance
#' of a given list of HAL knot points.
#'
#' TODO: this function pre-computes all pairwise distances, which might be
#' less efficient compared to if we only compute the distances for the
#' knot points in the given basis list. Try benchmarking this.
#'
library(FNN)
library(hal9001)

get_num_nn <- function(X, basis_list, delta) {

  # TODO: remove binary variables
  #X_no_bin <- X[, !apply(X, 2, is_binary)]

  k <- nrow(X) - 1
  num_nn <- apply(X, 2, function(.x) {
    dist_mat <- get.knn(.x, k = k, algorithm = "kd_tree")$nn.dist
    dist_mat_num <- dist_mat <= delta
    rowSums(dist_mat_num)
  })

  weights <- sapply(basis_list, function(.x) {
    col_idx <- .x$cols
    row_idx <- which(apply(X[, col_idx, drop = FALSE], 1, function(.y) {
      all(.y == .x$cutoffs)
    }))[1]
    max(num_nn[row_idx, col_idx]) # TODO: try min/exact num.
  })

  return(weights)
}

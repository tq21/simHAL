

fit_nn <- function(X, Y, k) {
  # uniformly sample num_knots points from uniform distribution
  one_way_knots <- nearest_knots(X, k)

  n <- 1000; d <- 3
  X <- r_to_py(matrix(rnorm(n*d), nrow = n, ncol = d))
  basis_maker <- py$BasisMaker(X)
  res <- microbenchmark({
    hal_design <- basis_maker$make_design_matrix()
  }, times = 5L)
}

n <- 100
d <- 3
X <- matrix(rnorm(n*d), nrow = n, ncol = d)
k <- 5

# sample k values uniformly, then find the nearest neighbors
nearest_knots <- function(X, k) {
  X <- split(X, col(X))

  knots <- lapply(X, function(.x) {
    # sample k points uniformly
    samp_vals <- runif(k, min = min(.x), max = max(.x))

    # get nearest neighbors
    nn_idx <- sapply(samp_vals, function(.y) {
      which.min(abs(.y - .x))
    })

    sort(.x[nn_idx])
  })

  return(knots)
}

# given a list of knots, make a list of Basis objects, up to g-way
# let's not worry about duplicates right now
make_Basis_from_knots <- function(knots, max_degree) {
  # one-way
  one_way <- unlist(sapply(1:length(knots), function(i) {
    unlist(sapply(1:length(knots[[i]]), function(j) {
      Basis$new(as.numeric(names(knots))[i], knots[[i]][j])
    }))
  }))

  # up to g-way
  for (g in 2:max_degree) {
    one_way <- c(one_way, unlist(sapply(1:length(knots), function(i) {
      unlist(sapply(1:length(knots[[i]]), function(j) {
        Basis$new(as.numeric(names(knots))[i], knots[[i]][j], g)
      }))
    })))
  }

}



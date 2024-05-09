test_that("basic functionality", {
  sim_data <- function(n, data_id) {
    # noise
    U <- rnorm(n)

    # baseline covariates
    W1 <- round(runif(n, -4, 4), 2)
    W2 <- round(runif(n, -4, 4), 2)
    W3 <- rbinom(n, 1, 0.5)

    # treatment assignment
    A <- rbinom(n, 1, plogis(0.3+0.5*W1+0.5*W2+0.7*W3))

    # outcome
    if (data_id == 1) {
      Y <- 1.5*A+W1/15-0.28*W1^2+0.5*W2+0.25*W2*W3+U
    } else if (data_id == 2) {
      Y <- 1.5*A-2*as.numeric(W1 < -3)*W3+2.5*as.numeric(W1 > -2)-
        2*as.numeric(W1 > 0)+2.5*as.numeric(W1 > 2)*W3-2.5*as.numeric(W1 > 3) +
        as.numeric(W2 > -1)-4*as.numeric(W2 > 1)*W3+2*as.numeric(W2 > 3)+U
    } else if (data_id == 3) {
      Y <- 1.5*A+W3*(4*sin(pi/2*abs(W1))*(W2 < 0)+4.1*cos(pi/2*abs(W1))*(W2 > 0))+U
    }

    return(data.frame(W1 = W1,
                      W2 = W2,
                      W3 = W3,
                      A = A,
                      Y = Y))
  }

  # simulate data
  set.seed(123)
  n <- 50; data_id <- 1
  data <- sim_data(n = n, data_id = data_id)
  Y <- data$Y; A <- data$A; W <- data[, c("W1", "W2", "W3")]

  # make basis_list
  Q_basis_list <- enumerate_basis(x = data.frame(W, A = A),
                                  max_degree = 3,
                                  smoothness_orders = 0)

  Q_aug_fit <- fit_hal_augment_safe(X = data.frame(W, A = A),
                                    Y = Y,
                                    col_idx = c(1, 2),
                                    noise = 1,
                                    copies = 2,
                                    basis_list = Q_basis_list,
                                    family = "gaussian")

  expect_equal(Q_aug_fit$col_idx, c(1, 2))
  expect_equal(Q_aug_fit$noise, 1)
  expect_equal(Q_aug_fit$copies, 2)
  expect_equal(dim(Q_aug_fit$X), c(50*3, 4))
  expect_equal(length(Q_aug_fit$Y), 50*3)
})

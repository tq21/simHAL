library(origami)
library(purrr)
library(hal9001)
library(devtools)
load_all()

set.seed(123)
data <- read.csv("data/abalone/abalone.data", header = FALSE)
data$sex_f <- as.numeric(data$V1 == "F")
data$V1 <- NULL
folds <- make_folds(nrow(data), folds_vfold, V = 5)

covs <- c("sex_f", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
outcome <- "V9"

# regular HAL
losses <- map_vec(folds, function(.x) {
  data_train <- data[.x$training_set, ]
  data_valid <- data[.x$validation_set, ]

  # make basis list
  basis_list <- enumerate_basis(x = data_train[, covs],
                                smoothness_orders = 0,
                                max_degree = 2)

  # fit HAL on training set
  fit <- fit_hal(X = data_train[, covs],
                 Y = data_train[, outcome],
                 basis_list = basis_list,
                 family = "gaussian")

  pred <- predict(fit, new_data = data_valid[, outcome])
  loss <- mean((pred - data_valid[, outcome])^2)

  return(loss)
}, .progress = TRUE)

# data augmented HAL
losses_aug <- map_vec(folds, function(.x) {
  data_train <- data[.x$training_set, ]
  data_valid <- data[.x$validation_set, ]

  # make basis list
  basis_list <- enumerate_basis(x = data_train[, covs],
                                smoothness_orders = 0,
                                max_degree = 2)

  # fit HAL on training set
  fit <- fit_hal_augment(X = data_train[, covs],
                         Y = data_train[, outcome],
                         basis_list = basis_list,
                         col_idx = 2:8,
                         noise = 0.1,
                         copies = 5,
                         family = "gaussian")

  pred <- predict(fit$fit, new_data = data_valid[, outcome])
  loss <- mean((pred - data_valid[, outcome])^2)

  return(loss)
}, .progress = TRUE)

save(list = c("losses", "losses_aug"), file = "out/res.RData")

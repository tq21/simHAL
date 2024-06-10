library(origami)
library(purrr)
library(devtools)
library(sl3)
load_all()
options(sl3.verbose = TRUE)

set.seed(123)
data <- read.csv("data/abalone/abalone.data", header = FALSE)
data$sex_f <- as.numeric(data$V1 == "F")
data$V1 <- NULL
folds <- make_folds(nrow(data), folds_vfold, V = 10)

covs <- c("sex_f", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
outcome <- "V9"

losses <- map_vec(folds, function(.x) {
  data_train <- data[.x$training_set, ]
  data_valid <- data[.x$validation_set, ]

  # make sl3 task
  task <- sl3_Task$new(data = data_train,
                       covariates = covs,
                       outcome = outcome)
  task_pred <- sl3_Task$new(data = data_valid,
                            covariates = covs,
                            outcome = outcome)

  # set up learners
  learner_list <- list(
    # Lrnr_xgboost$new(max_depth = 4, nrounds = 20, verbose = 0, augment_data = FALSE),
    Lrnr_earth$new(degree = 3, augment_data = FALSE),
    # Lrnr_gam$new(augment_data = FALSE)
    Lrnr_glm$new(augment_data = FALSE)
  )
  lrnr_stack <- Stack$new(learner_list)
  lrnr <- make_learner(Pipeline,
                       Lrnr_cv$new(lrnr_stack),
                       Lrnr_cv_selector$new(loss_squared_error))

  # training
  fit <- lrnr$train(task)

  pred <- fit$predict(task_pred)
  loss <- mean((pred - data_valid[, outcome])^2)

  return(loss)
})

col_idx <- 2:8
losses_aug <- map_vec(folds, function(.x) {
  data_train <- data[.x$training_set, ]
  data_valid <- data[.x$validation_set, ]

  # make sl3 task
  task <- sl3_Task$new(data = data_train,
                       covariates = covs,
                       outcome = outcome)
  task_pred <- sl3_Task$new(data = data_valid,
                            covariates = covs,
                            outcome = outcome)

  # set up learners
  learner_list <- list(
    # Lrnr_xgboost$new(max_depth = 4, nrounds = 20, verbose = 0, augment_data = TRUE, col_idx = col_idx),
    Lrnr_earth$new(degree = 3, augment_data = TRUE, col_idx = col_idx, copies = 5, epsilon = 0.1),
    # Lrnr_gam$new(augment_data = TRUE, col_idx = col_idx, copies = 5, epsilon = 0.1)
    Lrnr_glm$new(augment_data = TRUE, col_idx = col_idx, copies = 5, epsilon = 0.1)
  )
  lrnr_stack <- Stack$new(learner_list)
  lrnr <- make_learner(Pipeline,
                       Lrnr_cv$new(lrnr_stack),
                       Lrnr_cv_selector$new(loss_squared_error))

  # training
  fit <- lrnr$train(task)

  pred <- fit$predict(task_pred)
  loss <- mean((pred - data_valid[, outcome])^2)

  return(loss)
})

mean(losses)
mean(losses_aug)

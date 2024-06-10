# Ensemble HAL hazard regression method
A_cat <- sort(unique(data$A))
A_cat_perm <- permutations(n = length(A_cat), r = length(A_cat), v = A_cat)
#all_preds <- vector(mode = "list", length = nrow(A_cat_perm))
all_preds <- vector(mode = "list", length = nrow(A_cat_perm))
for (i in 1:nrow(A_cat_perm)) {
  print(i)
  cur_perm <- A_cat_perm[i, ]
  data_tmp <- data %>% mutate(A = case_when(A == 1 ~ cur_perm[1],
                                            A == 2 ~ cur_perm[2],
                                            A == 3 ~ cur_perm[3],
                                            A == 4 ~ cur_perm[4],
                                            A == 5 ~ cur_perm[5]))
  data_test_tmp <- data_test %>% mutate(A = case_when(A == 1 ~ cur_perm[1],
                                                      A == 2 ~ cur_perm[2],
                                                      A == 3 ~ cur_perm[3],
                                                      A == 4 ~ cur_perm[4],
                                                      A == 5 ~ cur_perm[5]))

  # fit HAL hazard regression method
  hal_tmp <- fit_hal_density(W = data_tmp[, c("W1", "W2")], A = data_tmp$A)
  tau <- length(unique(data$A))
  rep_data_test <- data.table(data_test_tmp[, c("W1", "W2")], A = data_test_tmp$A)
  rep_data_test <- rep_data_test[rep(1:.N, each = tau)]
  rep_data_test$id <- rep(seq(n_test), each = tau)
  rep_data_test$a <- rep(seq(tau), n_test)
  rep_data_test <- rep_data_test[, A_a := as.numeric(A == a)]

  # predict hazard
  pred <- predict(hal_tmp$fit, new_data = rep_data_test, type = "response")

  # convert hazard to density
  rep_data_test[, lambda := pred]
  rep_data_test[a == tau, lambda := 1]
  rep_data_test <- rep_data_test[, surv := cumprod(1 - lambda), by = id]
  rep_data_test <- rep_data_test[, density := lambda * shift(surv, fill = 1), by = id]

  hal_tmp_pred <- data.frame("1" = rep_data_test[a == 1, density],
                             "2" = rep_data_test[a == 2, density],
                             "3" = rep_data_test[a == 3, density],
                             "4" = rep_data_test[a == 4, density],
                             "5" = rep_data_test[a == 5, density])

  all_preds[[i]] <- as.matrix(hal_tmp_pred)
}

hal_3_pred <- Reduce("+", all_preds) / length(all_preds)
rowSums(hal_3_pred)
sum(log(hal_3_pred[cbind(1:nrow(hal_3_pred), data_test$A)]))










# Ensemble HAL multinomial logistic regression
A_cat <- sort(unique(data$A))
A_cat_perm <- permutations(n = length(A_cat), r = length(A_cat), v = A_cat)
#all_preds <- vector(mode = "list", length = nrow(A_cat_perm))
all_preds <- vector(mode = "list", length = nrow(A_cat_perm))
for (i in 1:nrow(A_cat_perm)) {
  print(i)
  cur_perm <- A_cat_perm[i, ]
  data_tmp <- data %>% mutate(A = case_when(A == 1 ~ cur_perm[1],
                                            A == 2 ~ cur_perm[2],
                                            A == 3 ~ cur_perm[3],
                                            A == 4 ~ cur_perm[4],
                                            A == 5 ~ cur_perm[5]))
  data_test_tmp <- data_test %>% mutate(A = case_when(A == 1 ~ cur_perm[1],
                                                      A == 2 ~ cur_perm[2],
                                                      A == 3 ~ cur_perm[3],
                                                      A == 4 ~ cur_perm[4],
                                                      A == 5 ~ cur_perm[5]))

  # fit HAL hazard regression method
  hal_tmp <- fit_hal_density(W = data_tmp[, c("W1", "W2")], A = data_tmp$A)
  tau <- length(unique(data$A))
  rep_data_test <- data.table(data_test_tmp[, c("W1", "W2")], A = data_test_tmp$A)
  rep_data_test <- rep_data_test[rep(1:.N, each = tau)]
  rep_data_test$id <- rep(seq(n_test), each = tau)
  rep_data_test$a <- rep(seq(tau), n_test)
  rep_data_test <- rep_data_test[, A_a := as.numeric(A == a)]

  # predict hazard
  pred <- predict(hal_tmp$fit, new_data = rep_data_test, type = "response")

  # convert hazard to density
  rep_data_test[, lambda := pred]
  rep_data_test[a == tau, lambda := 1]
  rep_data_test <- rep_data_test[, surv := cumprod(1 - lambda), by = id]
  rep_data_test <- rep_data_test[, density := lambda * shift(surv, fill = 1), by = id]

  hal_tmp_pred <- data.frame("1" = rep_data_test[a == 1, density],
                             "2" = rep_data_test[a == 2, density],
                             "3" = rep_data_test[a == 3, density],
                             "4" = rep_data_test[a == 4, density],
                             "5" = rep_data_test[a == 5, density])

  all_preds[[i]] <- as.matrix(hal_tmp_pred)
}

hal_3_pred <- Reduce("+", all_preds) / length(all_preds)
rowSums(hal_3_pred)
sum(log(hal_3_pred[cbind(1:nrow(hal_3_pred), data_test$A)]))











# simulate data: W ~ U[-4, 4] and A|W ~ N(mu = W, sd = 0.25)
n <- 500
W <- data.frame(W = rnorm(n))
get_prob <- function(W) {
  p0 <- plogis(-1+0.2*W)
  p1 <- plogis(0.2*W)
  p2 <- plogis(0.3*W)
  p3 <- plogis(1+0.1*W)
  total_p <- p0 + p1 + p2 + p3
  p0 <- p0 / total_p
  p1 <- p1 / total_p
  p2 <- p2 / total_p
  p3 <- p3 / total_p

  return(cbind(p0, p1, p2, p3))
}

true_probs <- t(apply(as.matrix(W), 1, get_prob))
A <- apply(true_probs, 1, function(p) sample(1:4, 1, prob = p))
data <- data.frame(W, A)




mean(rowSums((hal_1_pred - true_probs)^2))
mean(rowSums((hal_2_pred - true_probs)^2))





A_cat <- c(1, 2, 3, 4)
A_cat_perm <- permutations(n = length(A_cat), r = length(A_cat), v = A_cat)
all_preds <- vector(mode = "list", length = 2)
for (i in c(1,3)) {
  print(i)
  cur_perm <- A_cat_perm[i, ]
  data_tmp <- data %>% mutate(A = case_when(A == 1 ~ cur_perm[1],
                                            A == 2 ~ cur_perm[2],
                                            A == 3 ~ cur_perm[3],
                                            A == 4 ~ cur_perm[4]))

  # fit multinomial HAL
  hal_fit <- fit_hal(X = data_tmp$W,
                     Y = data_tmp$A,
                     max_degree = 3,
                     smoothness_orders = 0,
                     family = "multinomial")
  hal_pred <- predict(hal_fit, new_data = data_tmp$W, type = "response")
  all_preds[[i]] <- hal_pred[, cur_perm]

  # compute log-likelihood loss

  # fit haldensify
  # fit <- haldensify(
  #   A = data_tmp$A, W = data_tmp$W,
  #   n_bins = 3, grid_type = "equal_range",
  #   lambda_seq = exp(seq(-1, -10, length = 100)),
  #   max_degree = 3
  # )

  # pred <- predict(fit, new_A = c(0, 1, 2, 3), new_W = c(0, 0, 0, 0))
}



tmp <- vector(mode = "list", length = 2)
tmp[[1]] <- all_preds[[1]]
tmp[[2]] <- all_preds[[24]]

res <- Reduce("+", all_preds) / 2
rowSums(res)



mean(rowSums((pred - true_probs)^2))
mean(rowSums((res - true_probs)^2))

hist(rowSums(abs(res - true_probs)))
predict(fit, new_A = data$A_cat, new_W = data$W)

hist(rowSums(log(true_probs/pred)*true_probs))
hist(rowSums(log(true_probs/res)*true_probs))

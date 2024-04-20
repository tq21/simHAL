library(atmle)
library(hal9001)
library(origami)
library(purrr)
library(devtools)
load_all()

# RCT DGP ----------------------------------------------------------------------
sim_data <- function(n, ate, g_rct) {
  # noise
  U <- rnorm(n)

  # baseline covariates
  W1 <- rnorm(n)
  W2 <- rnorm(n)
  W3 <- rnorm(n)

  # treatment assignment
  A <- rbinom(n, 1, g_rct)

  # outcome
  Y <- 2.5+ate*A+0.9*W1+1.1*W2+2.7*W3+U

  return(data.frame(W1 = W1,
                    W2 = W2,
                    W3 = W3,
                    A = A,
                    Y = Y))
}

# SIMULATION PARAMETERS --------------------------------------------------------
B <- 500
n <- 500
g_rct <- 0.5
n_copies <- 5
noise <- 0.01
ate <- 1.5
nuisance_method <- "glm"
working_model_method <- "glmnet"
results <- data.frame(run = NULL,
                      tmle_est = NULL,
                      tmle_lower = NULL,
                      tmle_upper = NULL,
                      tmle_cover = NULL,
                      atmle_est = NULL,
                      atmle_lower = NULL,
                      atmle_upper = NULL,
                      atmle_cover = NULL,
                      atmle_rct_est = NULL,
                      atmle_rct_lower = NULL,
                      atmle_rct_upper = NULL,
                      atmle_rct_cover = NULL)

for (b in 1:B) {
  print("run: " %+% b)

  # simulate data
  data_rct <- sim_data(n, ate, g_rct); data_rct$S <- 1
  Y <- data_rct$Y; A <- data_rct$A; W <- data_rct[, c("W1", "W2", "W3")]

  # TMLE on RCT alone
  tmle_res <- tmle(Y = Y, A = A, W = W,
                   Q.SL.library = c("SL.glm"),
                   g.SL.library = c("SL.glm"),
                   family = "gaussian")

  # A-TMLE using synthetic external data
  data_ext_all <- map_dfr(seq(n_copies), function(i) {
    data_ext <- data_rct
    data_ext[, c("W1", "W2", "W3")] <- data_ext[, c("W1", "W2", "W3")] + rnorm(n, 0, noise)
    data_ext$Y <- data_ext$Y*2
    data_ext$S <- 0
    return(data_ext)
  })

  atmle_res <- atmle(data = rbind(data_rct, data_ext_all),
                     S_node = 6,
                     W_node = 1:3,
                     A_node = 4,
                     Y_node = 5,
                     controls_only = FALSE,
                     family = "gaussian",
                     atmle_pooled = TRUE,
                     theta_method = nuisance_method,
                     Pi_method = nuisance_method,
                     g_method = nuisance_method,
                     theta_tilde_method = nuisance_method,
                     bias_working_model = working_model_method,
                     pooled_working_model = working_model_method,
                     g_rct = g_rct,
                     verbose = FALSE)

  # A-TMLE using RCT alone
  folds <- make_folds(n = n, V = 5)
  g <- rep(g_rct, n)
  theta_tilde <- atmle:::learn_theta_tilde(W = W,
                                           Y = Y,
                                           delta = rep(1, n),
                                           method = nuisance_method,
                                           folds = folds,
                                           family = "gaussian",
                                           theta_bounds = NULL)
  T_working <- atmle:::learn_T(W = W,
                               A = A,
                               Y = Y,
                               g = g,
                               delta = rep(1, n),
                               weights = rep(1, n),
                               theta_tilde = theta_tilde,
                               method = working_model_method,
                               min_working_model = FALSE,
                               v_folds = length(folds))
  atmle_rct_res <- list()
  atmle_rct_res$est <- mean(T_working$pred)
  psi_tilde_eic <- atmle:::get_eic_psi_tilde(T_working,
                                             g,
                                             theta_tilde,
                                             Y,
                                             A,
                                             n,
                                             rep(1, n))
  psi_tilde_se <- sqrt(var(psi_tilde_eic, na.rm = TRUE)/n)
  atmle_rct_res$lower <- atmle_rct_res$est-1.96*psi_tilde_se
  atmle_rct_res$upper <- atmle_rct_res$est+1.96*psi_tilde_se

  cover <- atmle_res$lower < ate & ate < atmle_res$upper
  print("cover: " %+% cover)

  # compile results
  res <- data.frame(run = b,
                    tmle_est = tmle_res$estimates$ATE$psi,
                    tmle_lower = tmle_res$estimates$ATE$CI[1],
                    tmle_upper = tmle_res$estimates$ATE$CI[2],
                    tmle_cover = tmle_res$estimates$ATE$CI[1] < ate & ate < tmle_res$estimates$ATE$CI[2],
                    atmle_est = atmle_res$est,
                    atmle_lower = atmle_res$lower,
                    atmle_upper = atmle_res$upper,
                    atmle_cover = atmle_res$lower < ate & ate < atmle_res$upper,
                    atmle_rct_est = atmle_rct_res$est,
                    atmle_rct_lower = atmle_rct_res$lower,
                    atmle_rct_upper = atmle_rct_res$upper,
                    atmle_rct_cover = atmle_rct_res$lower < ate & ate < atmle_rct_res$upper)
  results <- rbind(results, res)
}

# SAVE RESULTS -----------------------------------------------------------------
saveRDS(results, "out/atmle_synthetic_external_0418.rds")

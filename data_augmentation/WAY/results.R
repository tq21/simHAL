
res_1 <- readRDS("out/run_data_1_0419.rds")
res_2 <- readRDS("out/run_data_2_0419.rds")
res_3 <- readRDS("out/run_data_3_0419.rds")

plot_fun <- function(res, truth, title) {
  bias_tmle <- mean(res$est - truth)
  bias_tmle_aug <- mean(res$est_aug - truth)
  var_tmle <- var(res$est)
  var_tmle_aug <- var(res$est_aug)
  mse_tmle <- bias_tmle^2+var_tmle
  mse_tmle_aug <- bias_tmle_aug^2+var_tmle_aug

  res_df <- data.frame(bias = c(bias_tmle, bias_tmle_aug),
                       var = c(var_tmle, var_tmle_aug),
                       mse = c(mse_tmle, mse_tmle_aug),
                       method = c("TMLE (HAL)", "TMLE (Augmented HAL)"))

  p_bias <- ggplot(res_df, aes(x = n, y = bias, color = estimator)) +
    geom_point(size = 1.5) +
    geom_line(linewidth = 1) +
    labs(title = "",
         x = "n",
         y = "mse") +
    theme_minimal() +
    theme(text = element_text(size = 16),
          legend.position = "none")

  return(p)
}

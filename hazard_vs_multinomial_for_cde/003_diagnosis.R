library(plotly)
library(data.table)
library(hal9001)
source("utils.R")
set.seed(9823475)
n <- 500
W <- c("W1", "W2")
A <- "A"

# expand grid
W1 <- seq(2, 6, 0.1)
W2 <- seq(0, 20, 0.1)
data_plot <- as.data.table(expand.grid(W1 = W1, W2 = W2))

# scenario 1
source("001_sim_data_constant_hazard.R")
data_1 <- sim_data(n)
hal_haz_reg_fit_1 <- cde_hazard(data = data_1, W = W, A = A)
hal_haz_reg_pred_1 <- predict_cde_hazard(fit = hal_haz_reg_fit_1, new_data = data_plot, W = W, tau = 5)
hal_multinom_fit_1 <- fit_hal(X = data_1[, ..W],
                              Y = data_1[[A]],
                              max_degree = 3,
                              smoothness_orders = 1,
                              family = "multinomial")
hal_multinom_pred_1 <- predict(hal_multinom_fit_1, new_data = data_plot[, ..W], type = "response")
true_density_1 <- get_truth(data_plot)

# scenario 2
source("002_sim_data_non_constant_hazard.R")
data_2 <- sim_data(n)
hal_haz_reg_fit_2 <- cde_hazard(data = data_2, W = W, A = A)
hal_haz_reg_pred_2 <- predict_cde_hazard(fit = hal_haz_reg_fit_2, new_data = data_plot, W = W, tau = 5)
hal_multinom_fit_2 <- fit_hal(X = data_2[, ..W],
                              Y = data_2[[A]],
                              max_degree = 3,
                              smoothness_orders = 1,
                              family = "multinomial")
hal_multinom_pred_2 <- predict(hal_multinom_fit_2, new_data = data_plot[, ..W], type = "response")
true_density_2 <- get_truth(data_plot)

get_plot <- function(scenario, k) {

  if (scenario == 1) {
    hal_haz_reg_pred <- hal_haz_reg_pred_1
    hal_multinom_pred <- hal_multinom_pred_1
    true_density <- true_density_1
  } else if (scenario == 2) {
    hal_haz_reg_pred <- hal_haz_reg_pred_2
    hal_multinom_pred <- hal_multinom_pred_2
    true_density <- true_density_2
  }

  plt <- plot_ly() %>%
    add_trace(
      data = as.data.frame(data_plot), x = ~W1, y = ~W2, z = ~hal_haz_reg_pred[, k],
      type = 'scatter3d', mode = 'markers',
      marker = list(size = 3, color = 'blue', opacity = 0.5),
      name = 'HAL (hazard-based)'
    ) %>%
    add_trace(
      data = as.data.frame(data_plot), x = ~W1, y = ~W2, z = ~hal_multinom_pred[, k],
      type = 'scatter3d', mode = 'markers',
      marker = list(size = 3, color = 'red', opacity = 0.5),
      name = 'HAL (multinomial)'
    ) %>%
    add_trace(
      data = as.data.frame(data_plot), x = ~W1, y = ~W2, z = ~true_density[, k],
      type = 'scatter3d', mode = 'markers',
      marker = list(size = 3, color = 'green', opacity = 0.5),
      name = 'Truth'
    ) %>%
    layout(
      title = paste("3D Plot for Category", k),
      scene = list(
        xaxis = list(title = "W1"),
        yaxis = list(title = "W2"),
        zaxis = list(title = "Predicted Probability")
      )
    )

  return(plt)
}

get_plot(scenario = 2, k = 3)

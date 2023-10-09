library(ggplot2)
library(ggpubr)
library(purrr)

`%+%` <- function(a, b) paste0(a, b)

n_500 <- readRDS("out/1005_500.RDS")
n_1000 <- readRDS("out/1005_1000.RDS")
n_2000 <- readRDS("out/1005_2000.RDS")
final_res <- map_dfr(list(n_500, n_1000, n_2000), function(.x) return(.x))
final_res <- final_res[final_res$algo_name %in% c("0th-order HAL",
                                                  "0th-order doubleHAL",
                                                  "1st-order HAL",
                                                  "1st-order doubleHAL"),]
final_res$algo_name <- c("0-HAL", "0-doubleHAL", "1-HAL", "1-doubleHAL")
final_res$algo_name <- as.factor(final_res$algo_name)
final_res$algo_name <- factor(final_res$algo_name, levels = c("0-HAL", "0-doubleHAL", "1-HAL", "1-doubleHAL"))
final_res$n <- as.factor(final_res$n)

# r square plot
plot_r_square <- function(dt, d, scenario, y_min, y_max) {
  p <- ggplot(dt[dt$scenario == scenario & dt$d == d, ],
              aes(x = algo_name, y = r_squared, fill = n)) +
    geom_point(size = 3, shape = 24) +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
    ylim(y_min, y_max) +
    theme_minimal() +
    labs(x = "", y = "R-squared", title = "d = " %+% d %+% ", " %+% scenario) +
    scale_fill_manual(values = c("500" = "#00000011",
                                 "1000" = "#00000055",
                                 "2000" = "#00000099")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 1, size = 8),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))

  return(p)
}

p1_r_square <- plot_r_square(final_res, 3, "smooth", 0.7, 0.81)
p2_r_square <- plot_r_square(final_res, 3, "jumps", 0.65, 0.81)
p3_r_square <- plot_r_square(final_res, 3, "sinusoidal", 0.5, 0.81)
p4_r_square <- plot_r_square(final_res, 5, "smooth", 0.7, 0.81)
p5_r_square <- plot_r_square(final_res, 5, "jumps", 0.5, 0.81)
p6_r_square <- plot_r_square(final_res, 5, "sinusoidal", 0.35, 0.81)

plt_r_square <- ggarrange(p1_r_square, p2_r_square, p3_r_square, p4_r_square, p5_r_square, p6_r_square,
                          nrow = 2, ncol = 3,
                          common.legend = TRUE, legend = "right")

ggsave("plots/r_square_1005.pdf", plot = plt_r_square, device = "pdf",
       width = 12, height = 8, dpi = 300, units = "in")



# Load necessary libraries
library(tidyverse)

# Convert the data to long format
data_long <- preds %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "Variable", values_to = "Value")

# Function to plot spikes
plot_spikes <- function(df, var_name) {
  df <- df %>% filter(Variable == var_name)

  ggplot(df, aes(x = id, xend = id, y = 0, yend = ifelse(Variable %in% c("B", "D"), -Value, Value))) +
    geom_segment(size = 0.5) +
    coord_flip() +
    labs(
      title = paste("Spikes for", var_name),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

# Plot spikes for each variable
p1 <- plot_spikes(data_long, "A")
p2 <- plot_spikes(data_long, "B")
p3 <- plot_spikes(data_long, "C")
p4 <- plot_spikes(data_long, "D")

# Arrange the plots in one grid
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 1)

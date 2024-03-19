library(ggplot2)
library(ggpubr)

jump <- readRDS("out/1120_jump.RDS")
smooth <- readRDS("out/1120_smooth.RDS")
sin <- readRDS("out/1120_sin.RDS")

jump[1,]$delta <- 11
smooth[1,]$delta <- 11
sin[1,]$delta <- 11

jump_plt <- ggplot(jump, aes(x = delta, y = r_squared)) +
  geom_point() +
  geom_point(data = jump[1,], color = "red") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.01), limits = c(0.71, 0.76)) +
  labs(x = "delta", y = "R^2", title = "Scenario: jumps") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

smooth_plt <- ggplot(smooth, aes(x = delta, y = r_squared)) +
  geom_point() +
  geom_point(data = smooth[1,], color = "red") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.01), limits = c(0.72, 0.76)) +
  labs(x = "delta", y = "R^2", title = "Scenario: smooth") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sin_plt <- ggplot(sin, aes(x = delta, y = r_squared)) +
  geom_point() +
  geom_point(data = sin[1,], color = "red") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.01), limits = c(0.32, 0.47)) +
  labs(x = "delta", y = "R^2", title = "Scenario: sin") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# smallest eigenvalue plots
jump_eigen_plt <- ggplot(jump, aes(x = delta, y = eigen_val)) +
  geom_point() +
  geom_point(data = jump[1,], color = "red") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(x = "delta", y = "smallest eigenvalue", title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

smooth_eigen_plt <- ggplot(smooth, aes(x = delta, y = eigen_val)) +
  geom_point() +
  geom_point(data = smooth[1,], color = "red") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(x = "delta", y = "smallest eigenvalue", title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

sin_eigen_plt <- ggplot(sin, aes(x = delta, y = eigen_val)) +
  geom_point() +
  geom_point(data = sin[1,], color = "red") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(x = "delta", y = "smallest eigenvalue", title = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# plot all together
ggarrange(jump_plt, smooth_plt, sin_plt,
          jump_eigen_plt, smooth_eigen_plt, sin_eigen_plt,
          ncol = 3, nrow = 2)

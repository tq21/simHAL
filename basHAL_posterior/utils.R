`%+%` <- function(a, b) paste0(a, b)

data_gen <- function(n) {
  X1 <- runif(n, -4, 4)
  X2 <- runif(n, -4, 4)
  X3 <- runif(n, -4, 4)
  X4 <- runif(n, -4, 4)
  Y <- 2.1+2*as.numeric(X1 >= -3)+
    1.8*as.numeric(X2 >= -2)+
    1.7*as.numeric(X3 >= 0)+
    2.1*as.numeric(X4 >= 2)+
    2.4*as.numeric(X1 >= -2)*as.numeric(X2 >= 2)+
    2.7*as.numeric(X2 >= -2)*as.numeric(X3 >= 2)+
    1.6*as.numeric(X3 >= -2)*as.numeric(X4 >= 2)+
    1.9*as.numeric(X1 >= -2)*as.numeric(X4 >= 2)+
    rnorm(n, 0, 1)
  return(data.frame(X1 = X1, X2 = X2, X3 = X3, X4 = X4, Y = Y))
}

get_post_plt <- function(all_probs, var_name, truth) {
  # one-way
  all_one_way_idx <- which(!grepl(",", names(all_probs)))
  all_one_way <- all_probs[all_one_way_idx]
  one_way_knot_idx <- grep("^" %+% var_name, names(all_one_way))
  one_way_knot <- as.numeric(sub(".*_", "", names(all_one_way)[one_way_knot_idx]))
  one_way_knot_probs <- as.numeric(all_one_way[one_way_knot_idx])

  res_df <- data.frame(knots = one_way_knot, probs = one_way_knot_probs)

  p <- ggplot(res_df, aes(x = knots, y = probs)) +
    geom_point(shape = 20, color = "dodgerblue", size = 2, alpha = 0.4) +
    geom_vline(xintercept = truth, linetype = "dashed", color = "red", size = 1.5) +
    geom_smooth(method = "gam", se = FALSE, color = "black", size = 1.5) +
    labs(x = "X" %+% var_name %+% " knot points",
         y = "Probability") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
      axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 20))
    )

  return(p)
}

get_post_bivar_plt <- function(all_probs, var_names, truth) {

  # two-way
  all_two_way_idx <- grep(",", names(all_probs))
  all_two_way <- all_probs[all_two_way_idx]
  two_way_knot_idx <- grep("^" %+% var_names, names(all_two_way))
  two_way_knot_both <- sub(".*_", "", names(all_two_way)[two_way_knot_idx])
  first_knot <- as.numeric(sub(",.*", "", two_way_knot_both))
  second_knot <- as.numeric(sub(".*,", "", two_way_knot_both))
  probs <- as.numeric(all_two_way[two_way_knot_idx])

  res_df <- data.frame(first_knot = first_knot, second_knot = second_knot, probs = probs)

  p <- plot <- plot_ly() %>%
    add_trace(data = res_df,
              x = ~first_knot,
              y = ~second_knot,
              z = ~probs,
              type = "scatter3d",
              mode = "markers",
              marker = list(size = 5, opacity = 0.5)) %>%
    add_trace(x = c(truth[1], truth[1]),
              y = c(truth[2], truth[2]),
              z = c(0, max(res_df$probs)),
              type = "scatter3d",
              mode = "lines",
              line = list(color = "red", width = 10)) %>%
    layout(scene = list(xaxis = list(title = "First knot point"),
                        yaxis = list(title = "Second knot point"),
                        zaxis = list(title = "Probability")))

  return(p)
}

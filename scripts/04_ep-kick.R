# Run from project root. Standalone (loads GAM from data/).
library(mgcv)
library(tidyverse)

###############################
### KICKING EXPECTED POINTS ###
###############################

# GAM Model for Kick Percentage

saved_gam <- readRDS("data/gam_model.rds")

exp_points_on_miss <- 0.76
exp_points_on_success <- 3

# Pitch grid values
x_vals <- seq(-35, 35, by = 1)
y_vals <- seq(5, 60, by = 1)
grid <- expand.grid(x = x_vals, y = y_vals)

post_half_width <- 2.81

compute_kicking_angle <- function(x, y, post_half_width = 2.81) {
  left_post_x  <- -post_half_width
  right_post_x <-  post_half_width
  
  # angle from kicker to each post
  angle_left  <- atan2(left_post_x - x, y)
  angle_right <- atan2(right_post_x - x, y)
  
  # angular width of target
  angle_width <- angle_right - angle_left
  angle_width
}

grid <- grid %>%
  mutate(
    kicking_angle = compute_kicking_angle(x, y, post_half_width),
    distance_to_center = sqrt(x^2 + y^2)
  )

grid$prob <- predict(saved_gam,
                     newdata = grid %>% select(kicking_angle, distance_to_center),
                     type = "response")

grid <- grid %>%
  mutate(expected_points = prob * exp_points_on_success + (1 - prob) * exp_points_on_miss)

# Plotting probability of successful penalty kick

thresholds <- c(0.8, 0.6, 0.4, 0.2)

kick_prob_plot <- ggplot(grid, aes(x = x, y = y, fill = prob)) +
  geom_tile() +
  geom_contour(aes(x = x, y = y, z = prob), inherit.aes = FALSE,
               breaks = thresholds,
               color = "white",
               linewidth = 0.8,
               linetype = "dashed") +
  scale_fill_viridis_c(option = "magma", name = "Expected Prob") +
  coord_fixed() +
  labs(
    title = "Success Probability of a Kick",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

ggsave("plots/kick_prob_plot.png", kick_prob_plot, width = 10, height = 8, dpi = 300)

# Expected Points of a Penalty Kick

kick_ep_plot <- ggplot(grid, aes(x = x, y = y, fill = expected_points)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Expected Points") +
  coord_fixed() +
  labs(
    title = "Expected Points of a Penalty Kick",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

ggsave("plots/kick_ep_plot.png", kick_ep_plot,
       width = 10, height = 8, dpi = 300)

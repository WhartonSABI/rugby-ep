library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)


penalty_data = read_csv("data/Modeled_Penalty_Percentage_Data.csv")

head(penalty_data)

# Convert row/column labels to numeric midpoints
row_labels <- penalty_data$`Distance from Try line`
y_k <- sapply(strsplit(row_labels, "-"), function(x) mean(as.numeric(x)))
col_labels <- names(penalty_data)[-1]
x_k <- sapply(strsplit(col_labels, "-"), function(x) mean(as.numeric(x)))

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

penalty_long <- penalty_data %>%
  mutate(y = y_k) %>%
  pivot_longer(cols = -c(`Distance from Try line`, y),
               names_to = "x_label", values_to = "predicted_prob") %>%
  mutate(
    x_centered = sapply(strsplit(x_label, "-"), function(z) mean(as.numeric(z))) - 35,
    kicking_angle = compute_kicking_angle(x_centered, y),
    distance_to_center = sqrt(x_centered^2 + y^2)  # add distance to middle of posts
  ) %>%
  select(x = x_centered, y, distance_to_center, kicking_angle, predicted_prob) %>%
  drop_na(predicted_prob)

head(penalty_long)

ggplot(penalty_long, aes(x = x, y = y, fill = kicking_angle)) +
  geom_tile(color = "white") +           # each cell as a tile
  scale_fill_viridis_c(option = "plasma") +  # nice color scale
  coord_fixed(ratio = 1) +               # square grid
  labs(
    x = "Distance from Touchline (m)",
    y = "Distance from Try Line (m)",
    fill = "Kicking Angle (deg)",
    title = "Kicking Angle Across the Pitch"
  ) +
  theme_minimal()

penalty_long <- penalty_long %>%
  mutate(pred_prob_frac = predicted_prob / 100)

logit_model <- glm(pred_prob_frac ~ kicking_angle + distance_to_center,
                   data = penalty_long,
                   family = binomial(link = "logit"))

summary(logit_model)

penalty_long <- penalty_long %>%
  mutate(predicted_prob_fit = predict(logit_model, type = "response"))

ggplot(penalty_long, aes(x = pred_prob_frac, y = predicted_prob_fit)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Actual Probability",
    y = "Predicted Probability",
    title = "Actual vs Predicted Probabilities for Penalty Kicks"
  ) +
  theme_minimal()


pitch_width <- 70     # meters (touchline to touchline)
y_min <- 5           # minimum distance from try line
y_max <- 60          # maximum distance from try line
x_seq <- seq(0, pitch_width, by = 1)      # 1 m increments across width
y_seq <- seq(y_min, y_max, by = 1)        # 1 m increments from try line


grid <- expand.grid(x = x_seq, y = y_seq) %>%
  mutate(
    x_centered = x - 35,
    distance_to_center = sqrt(x_centered^2 + y^2),
    kicking_angle = compute_kicking_angle(x_centered, y)
  ) %>%
  # now that variables exist, predict
  mutate(prob = predict(logit_model, newdata = ., type = "response"))


thresholds <- c(0.5, 0.6, 0.7, 0.8, 0.9)

ggplot(grid, aes(x = x, y = y, fill = prob)) +
  geom_tile() +
  geom_contour(aes(z = prob),
               breaks = thresholds,
               color = "white",
               linewidth = 0.8,
               linetype = "dashed") +
  scale_fill_viridis_c(option = "magma", name = "Expected Prob") +
  coord_fixed() +
  labs(
    title = "Expected Probability of Kick Success (Symmetric)",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()


### Smoothing Function

library(mgcv)

gam_model <- gam(pred_prob_frac ~ te(kicking_angle, distance_to_center),
                 data = penalty_long,
                 family = quasibinomial(link = "logit"),
                 method = "REML")

summary(gam_model)

penalty_long <- penalty_long %>% mutate(predicted_gam = predict(gam_model, type = "response"))

ggplot(penalty_long, aes(x = pred_prob_frac, y = predicted_gam)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Actual prob", y = "GAM predicted prob", title = "Actual vs GAM predicted")

# Predict on grid (use same grid you created)
grid <- grid %>%
  mutate(prob_gam = predict(gam_model, newdata = ., type = "response"))

thresholds <- c(0.5, 0.6, 0.7, 0.8, 0.9)
ggplot(grid, aes(x = x, y = y, fill = prob_gam)) +
  geom_tile() +
  geom_contour(aes(z = prob_gam), breaks = thresholds, color = "white", linewidth = 0.8, linetype = "dashed") +
  scale_fill_viridis_c(option = "magma", name = "Expected Prob") +
  coord_fixed() + theme_minimal() +
  labs(title = "GAM smoothed probability surface", x = "Lateral position (m)", y = "Distance from goal line (m)")

saved_gam <- "gam_model.rds"
saveRDS(gam_model, saved_gam)


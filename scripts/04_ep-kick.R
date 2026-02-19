# run from project root; standalone (loads GAM from data/)
library(mgcv)
library(tidyverse)
library(tidyr)
library(dplyr)

###############################
### KICKING EXPECTED POINTS ###
###############################

# Load in Penalty Estimated Success Prob

kick_estimates <- read.csv("data/Modeled_Kick_Estimates.csv")

# Set proper column names from row 1
colnames(kick_estimates) <- c("try_line", kick_estimates[1, -1])
kick_estimates <- kick_estimates[-1, ] %>%
  pivot_longer(cols = -try_line, names_to = "touchline", values_to = "estimate") %>%
  filter(estimate != "" & !is.na(estimate) & estimate != " ") %>%
  mutate(
    try_line  = gsub("^0", "", try_line),
    touchline = gsub("^0", "", touchline),
    # Fix 0-5 band which becomes "-5" after stripping leading zero
    touchline = ifelse(touchline == "-5", "0-5", touchline),
    try_line  = ifelse(try_line  == "-5", "0-5", try_line),
    prob    = as.numeric(trimws(gsub("\\s*\\(.*", "", estimate))),
    ci_low  = as.numeric(gsub(".*\\((\\d+)-.*", "\\1", estimate)),
    ci_high = as.numeric(gsub(".*-(\\d+)\\).*", "\\1", estimate)),
    y = sapply(strsplit(try_line,  "-"), function(x) (as.numeric(x[1]) + as.numeric(x[2])) / 2),
    x = sapply(strsplit(touchline, "-"), function(x) (as.numeric(x[1]) + as.numeric(x[2])) / 2)
  ) %>%
  select(try_line, touchline, prob, ci_low, ci_high, x, y)

# --- Plot 1: Tile map with modelled probability ---
p1 <- ggplot(kick_estimates, aes(x = x, y = y)) +
  geom_tile(aes(fill = prob), colour = "white", width = 4.8, height = 4.8) +
  geom_text(aes(label = paste0(prob, "%")), 
            size = 2.5, colour = "white") +
  scale_fill_gradientn(
    colours = c("#d73027", "#fc8d59", "#fee090", "#91cf60", "#1a9850"),
    limits = c(0, 100),
    name = "Modelled\nprobability (%)"
  ) +
  scale_x_continuous(breaks = seq(2.5, 67.5, by = 5),
                     labels = c("0-5","5-10","10-15","15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50","50-55","55-60","60-65","65-70")) +
  scale_y_reverse(breaks = seq(7.5, 62.5, by = 5),
                  labels = c("5-10","10-15","15-20","20-25","25-30","30-35",
                             "35-40","40-45","45-50","50-55","55-60","60-65")) +
  labs(x = "Distance from left-hand touchline (m)",
       y = "Distance from try line (m)",
       title = "Penalty kick success probability",
       subtitle = "Modelled probability (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1

# --- Plot 2: CI plot faceted by try line band ---
p2 <- ggplot(kick_estimates, aes(x = x, y = prob)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#91cf60", alpha = 0.3) +
  geom_line(colour = "#1a9850", linewidth = 0.8) +
  geom_point(colour = "#1a9850", size = 1.5) +
  facet_wrap(~ try_line, ncol = 3) +
  scale_x_continuous(breaks = seq(2.5, 67.5, by = 10)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(x = "Distance from left-hand touchline (m)",
       y = "Success probability",
       title = "Modelled probability with 90% CI",
       subtitle = "Line = modelled probability, ribbon = 90% CI") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

p2


#####################
### Previous Code ###
#####################

# expected points assumptions
exp_points_on_miss <- 0.76
exp_points_on_success <- 3

# pitch grid
x_vals <- seq(-35, 35, by = 1)
y_vals <- seq(5, 60, by = 1)
grid <- expand.grid(x = x_vals, y = y_vals)

# post half-width in meters
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
# derive geometric features for GAM prediction
  mutate(
    kicking_angle = compute_kicking_angle(x, y, post_half_width),
    distance_to_center = sqrt(x^2 + y^2)
  )

# predict kick success probability
grid$prob <- predict(saved_gam,
                     newdata = grid %>% select(kicking_angle, distance_to_center),
                     type = "response")

# convert probability to expected points
grid <- grid %>%
  mutate(expected_points = prob * exp_points_on_success + (1 - prob) * exp_points_on_miss)

# kick success probability

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

# expected points for penalty kick

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

source("scripts/01_data-prep.R")

# run from project root
library(mgcv)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

###############################
### KICKING EXPECTED POINTS ###
###############################

# Load in Penalty Estimated Success Prob

kick_data <- read.csv("data/Goal kicking data.csv")

# Defining Variables

# X1.Metres = distance from left touchline
# Y1.Metres = distance from kicker's team goalline

# Renaming to x and y

kick_data <- kick_data %>%
  rename(x = X1.Metres) %>%
  mutate(
    y = 100 - Y1.Metres
  )

# Filtering for penalty kicks
  
kick_data <- kick_data %>%
  filter(Type == 2)

# Defining makes or misses
kick_data <- kick_data %>%
  mutate(make = ifelse(Quality == 1, 1, 0))

# Filtering for columns that I need

kick_data_small <- kick_data %>%
  select(x, y, make, Distance, PlayerID)

# Computing angle of each kick

kick_data_small <- kick_data_small %>%
  mutate(
    angle = atan2(abs(x - 35), y) * (180 / pi)
  )

# Simple Model

model <- glm(make ~ angle + Distance, 
             family = binomial(link = "logit"), 
             data = kick_data_small)

summary(model)

# Confidence intervals for coefficients
confint(model)

# Predicted probabilities with uncertainty
pred <- predict(model, type = "response", se.fit = TRUE)
kick_data$fitted_prob <- pred$fit
kick_data$lower <- pred$fit - 1.96 * pred$se.fit
kick_data$upper <- pred$fit + 1.96 * pred$se.fit


# Grid of x and y values
grid <- expand.grid(
  x = seq(0, 70, length.out = 200),
  y = seq(5, 65, length.out = 200)
)

# Compute angle and distance to posts (x=35, y=0)
grid <- grid %>%
  mutate(
    angle = atan2(abs(x - 35), y) * (180 / pi),
    Distance = sqrt((x - 35)^2 + y^2)
  )

# Predict probabilities
grid$prob <- predict(model, newdata = grid, type = "response")

# Turn into Expected Points
grid$expected_points <- 3*grid$prob + 0.6*(1-grid$prob)

# Contour thresholds
thresholds <- c(0.2, 0.4, 0.6, 0.8)

# Plot
kick_prob_plot <- ggplot(grid, aes(x = x, y = y, fill = prob)) +
  geom_tile() +
  geom_contour(aes(z = prob),
               breaks = thresholds,
               color = "white",
               linewidth = 0.8,
               linetype = "dashed") +
  scale_fill_viridis_c(option = "magma", name = "Kick Probability",
                       limits = c(0, 1)) +
  geom_point(aes(x = 35, y = 0), color = "white", size = 3, shape = 4) +
  coord_fixed() +
  labs(
    title = "Kick Success Probability",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

kick_prob_plot

ggsave("plots/kick_prob_plot.png", kick_prob_plot,
       width = 10, height = 8, dpi = 300)

# Showing confidence interval

# Cross section: straight in front of posts (x = 35), vary distance
cross_section <- data.frame(
  y = seq(5, 65, length.out = 200),
  x = 35
) %>%
  mutate(
    angle = atan2(abs(x - 35), y) * (180 / pi),
    Distance = sqrt((x - 35)^2 + y^2)
  )

# Predict with standard errors (on log-odds scale, then transform)
pred <- predict(model, newdata = cross_section, type = "link", se.fit = TRUE)

cross_section <- cross_section %>%
  mutate(
    fit_logit = pred$fit,
    se = pred$se.fit,
    prob = plogis(fit_logit),
    lower = plogis(fit_logit - 1.96 * se),
    upper = plogis(fit_logit + 1.96 * se)
  )

# Plot
cross_section_plot <- ggplot(cross_section, aes(x = y)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = prob), color = "steelblue", linewidth = 1) +
  labs(
    title = "Kick Success Probability — Straight in Front (x = 35)",
    x = "Distance from goal line (m)",
    y = "Probability of success"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggsave("plots/cross_section_plot.png", cross_section_plot,
       width = 10, height = 8, dpi = 300)

# Cross section: fix y = 20, vary x from 0 to 70

cross_section_angle <- data.frame(
  x = seq(0, 70, length.out = 200),
  y = 20
) %>%
  mutate(
    angle = atan2(abs(x - 35), y) * (180 / pi),
    Distance = sqrt((x - 35)^2 + y^2)
  )

# Predict with standard errors (on log-odds scale, then transform)
pred_angle <- predict(model, newdata = cross_section_angle, type = "link", se.fit = TRUE)

cross_section_angle <- cross_section_angle %>%
  mutate(
    fit_logit = pred_angle$fit,
    se = pred_angle$se.fit,
    prob = plogis(fit_logit),
    lower = plogis(fit_logit - 1.96 * se),
    upper = plogis(fit_logit + 1.96 * se)
  )

# Plot
cross_section_angle_plot <- ggplot(cross_section_angle, aes(x = x)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = prob), color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = 35, linetype = "dashed", color = "gray50") +
  annotate("text", x = 36, y = 0.1, label = "centre of posts", 
           hjust = 0, color = "gray50", size = 3) +
  labs(
    title = "Kick Success Probability — Fixed Distance (y = 20m)",
    x = "Distance from left touchline (m)",
    y = "Probability of success"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

cross_section_angle_plot

ggsave("plots/cross_section_angle_plot.png", cross_section_angle_plot,
       width = 10, height = 8, dpi = 300)

#################################
### Expected Points of a Miss ###
#################################

phase_data_restarts = read_csv("data/phase_2018-19.csv")

phase_data_restarts <- phase_data_restarts %>%
  mutate(
    # extract signed points
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # replace missing points with 0
    points = ifelse(is.na(points), 0, points)
  )

# Filtering out restarts with point changes (scores) and restarts if halfs
phase_data_restarts <- phase_data_restarts %>%
  group_by(Round, Home, Away) %>%
  filter(Phase == 1) %>%
  mutate(
    Points_Diff_Change = abs(Points_Difference) - abs(lag(Points_Difference)),
    is_first_row = row_number() == 1,
    is_second_half_start = Seconds_Remaining < 2400 & lag(Seconds_Remaining) >= 2400
  ) %>%
  filter(
    Play_Start == "Restart Kick",
    is.na(Points_Diff_Change) | Points_Diff_Change == 0,
    !is_first_row,
    !is.na(is_second_half_start) & !is_second_half_start
  ) %>%
  ungroup()

unique(phase_data_restarts$Location)

ep_by_zone <- phase_data_restarts %>%
  group_by(Location) %>%
  summarise(
    n = n(),
    avg_ep = round(mean(points), 2)
  ) %>%
  ungroup()

# add overall average row
overall <- phase_data_restarts %>%
  summarise(
    Location = "Overall Average",
    n = n(),
    avg_ep = round(mean(points), 2)
  )

ep_table <- bind_rows(ep_by_zone, overall)

print(ep_table)

# Data set to bootstrap

kick_miss_ep <- phase_data_restarts %>%
  select(points, ID)

# number of bootstrap replicates
B <- 2000

# one bootstrap replicate
one_boot_ep <- function(b) {
  kick_miss_ep %>%
    slice_sample(n = nrow(kick_miss_ep), replace = TRUE) %>%
    group_by(ID) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    summarise(mean_ep = mean(points)) %>%
    pull(mean_ep)
}

# run bootstrap
boot_ep <- sapply(seq_len(B), one_boot_ep)

# 95% percentile CI
ep_ci <- quantile(boot_ep, probs = c(0.025, 0.975))

cat("Mean EP:", mean(boot_ep), "\n")
cat("95% CI: [", ep_ci[1], ",", ep_ci[2], "]\n")

boot_df <- data.frame(ep = boot_ep)

# Missed Kick Bootstrap
missed_bootstrap <- ggplot(boot_df, aes(x = ep)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 40) +
  geom_vline(xintercept = mean(boot_ep), color = "steelblue", 
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = ep_ci[1], color = "red", 
             linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = ep_ci[2], color = "red", 
             linewidth = 1, linetype = "dashed") +
  annotate("text", x = mean(boot_ep), y = Inf, 
           label = sprintf("Mean = %.2f", mean(boot_ep)),
           hjust = -0.1, vjust = 2, color = "steelblue") +
  annotate("text", x = ep_ci[1], y = Inf, 
           label = sprintf("2.5%% = %.2f", ep_ci[1]),
           hjust = 1.1, vjust = 2, color = "red") +
  annotate("text", x = ep_ci[2], y = Inf, 
           label = sprintf("97.5%% = %.2f", ep_ci[2]),
           hjust = -0.1, vjust = 2, color = "red") +
  labs(
    title = "Bootstrap Distribution of Mean Expected Points",
    x = "Mean Expected Points",
    y = "Count"
  ) +
  theme_minimal()

missed_bootstrap

ggsave("plots/missed_bootstrap.png", missed_bootstrap,
       width = 10, height = 8, dpi = 300)

# run from project root; standalone (loads GAM from data/)
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
  y = seq(5, 70, length.out = 200)
)

# Compute angle and distance to posts (x=35, y=0)
grid <- grid %>%
  mutate(
    angle = atan2(abs(x - 35), y) * (180 / pi),
    Distance = sqrt((x - 35)^2 + y^2)
  )

# Predict probabilities
grid$prob <- predict(model, newdata = grid, type = "response")

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

# Showing confidence interval

# Cross section: straight in front of posts (x = 35), vary distance
cross_section <- data.frame(
  y = seq(5, 70, length.out = 200),
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
ggplot(cross_section, aes(x = y)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = prob), color = "steelblue", linewidth = 1) +
  labs(
    title = "Kick Success Probability — Straight in Front (x = 35)",
    x = "Distance from goal line (m)",
    y = "Probability of success"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

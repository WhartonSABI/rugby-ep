##################
### REFERENCES ###
##################

## Links to Article
# https://www.data-ruck.com/blog/predicting-kicks-outcome/
# https://journals.sagepub.com/doi/10.1177/22150218251365220

## Data Download Link
# https://zenodo.org/api/records/13851563/files-archive

# Goal Kicking Data
# https://www.sciencedirect.com/science/article/pii/S1440244014000255


################
### PACKAGES ###
################

# install.packages("patchwork")
library(patchwork)

# install.packages("stringr")
library(stringr)

# install.packages("tidyverse")
library(tidyverse)

# install.packages("dplyr")
library(dplyr)

# GAM
library(mgcv)

#################
### DATA LOAD ###
#################

# run from project directory (or within .Rproj)
phase_data = read_csv("data/phase_2018-19.csv")
# preview data
head(phase_data)

########################
### DATA EXPLORATION ###
#######################

# location where the phase began, determined determined by the metre lines dividing the pitch
unique(phase_data$Location)

# period between subsequent rucks
unique(phase_data$Phase)

# location on the pitch where the phase began, determined by the lineout lines dividing the pitch
unique(phase_data$Side)

# how did the play start?
unique(phase_data$Play_Start)

# upon scoring, the event was assigned to all phases since the previous scoring event
unique(phase_data$Outcome)

# points difference (I'm assuming for whoever has possession of the ball)
## oftentimes this is from perspective of the home team, but if it flips back and forth w/ possession then you're right
unique(phase_data$Points_Difference)

# remove instances when cards occur
phase_data <- phase_data %>%
  filter(
    Red_Cards_Own == 0,
    Red_Cards_Opp == 0,
    Yellow_Cards_Own == 0,
    Yellow_Cards_Opp == 0
  ) %>%
  select(-Red_Cards_Own, -Red_Cards_Opp, -Yellow_Cards_Own, -Yellow_Cards_Opp)

# get restarts
restarts <- phase_data %>%
  filter(Play_Start == "Restart Kick", Phase ==1)

# list of unique restart locations
unique(restarts$Location)

# expected points of lineout in different locations

phase_data <- phase_data %>%
  mutate(
    # extract signed numbers inside parentheses: (+3), (-3), etc.
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # handle cases with no number (e.g. "no score", turnovers)
    points = ifelse(is.na(points), 0, points)
  )

zones <- c(
  "5m-Goal (opp)",
  "22m-5m (opp)",
  "10m-22m (opp)",
  "Half-10m (opp)",
  "10m-Half (own)",
  "22m-10m (own)",
  "5m-22m (own)",
  "Goal-5m (own)"
)

lineouts_clean <- phase_data %>%
  filter(
    Play_Start == "Lineout",
    Location %in% zones,
    Phase == 1
  ) %>%
  arrange(Round, Home, Away, Team_In_Poss, desc(Seconds_Remaining)) %>%
  group_by(Round, Home, Away, Team_In_Poss, Points_Difference, Outcome) %>%
  mutate(time_gap = lag(Seconds_Remaining) - Seconds_Remaining,
         too_close = !is.na(time_gap) & time_gap < 200) %>%
  slice_head(n = 1) %>%
  ungroup()

expected_points_by_zone <- lineouts_clean %>%
  group_by(Location) %>%
  summarise(
    n_phases = n(),
    avg_points = mean(points, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    .groups = "drop"
  )

print(expected_points_by_zone)

# Expected Points of Lineout All Areas of the pitch

zones_order <- c(
  "5m-Goal (opp)",
  "22m-5m (opp)",
  "10m-22m (opp)",
  "Half-10m (opp)",
  "10m-Half (own)",
  "22m-10m (own)",
  "5m-22m (own)",
  "Goal-5m (own)"
)

expected_points_by_zone <- lineouts_clean %>%
  group_by(Location) %>%
  summarise(
    n_phases = n(),
    avg_points = mean(points, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Location = factor(Location, levels = zones_order)) %>%
  arrange(Location)

# Print Table
print(expected_points_by_zone)

# Plot of Meteres vs Expected Points of Lineout

zone_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)

expected_points_by_zone$Location <- factor(expected_points_by_zone$Location, levels = zones_order)
expected_points_by_zone$meter_x <- zone_meters[match(as.character(expected_points_by_zone$Location), zones_order)]

lineout_plot <- ggplot(expected_points_by_zone, aes(x = meter_x, y = avg_points)) +
  geom_point(size = 2) +
  geom_text(aes(label = round(avg_points, 2)), vjust = -1.2, size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = 0.8) +
  scale_x_continuous(
    breaks = zone_meters,
    labels = zones_order,
    expand = expansion(add = c(3, 3)),
    limits = c(0, 100)  # ensures full field range
  ) +
  labs(
    title = "Average Points by Lineout Location (Quadratic Fit, Increasing Distance from Opposition Goal Line)",
    x = "Field Position (meters)",
    y = "Average Expected Points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 8))
  )

ggsave("plots/lineout_plot.png", lineout_plot, width = 10, height = 6, dpi = 300)

df <- tibble::tibble(
  y_mid = c(31, 13.5, 2.5, 45),
  avg_points = c(1.25, 2.04, 3.74, 0.879)
)

p2 <- ggplot(df, aes(x = y_mid, y = avg_points)) +
  geom_point(size = 3, color = "red") +
  geom_line(group = 1, color = "blue") +
  labs(
    title = "Average Expected Points by Zone",
    x = "Distance from goal line (m, zone midpoint)",
    y = "Average Points"
  ) +
  theme_minimal()

ggsave("plots/expected_points_by_zone.png", p2, width = 10, height = 6, dpi = 300)





# GAM Model for Kick Percentage

saved_gam <- "gam_model.rds"

exp_points_on_miss <- 0.604
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
    kicking_angle = compute_kicking_angle(x_vals, y, post_half_width),
    distance_to_center = sqrt(x^2 + y^2)
  )

grid$prob <- predict(gam_model,
                     newdata = grid %>% select(kicking_angle, distance_to_center),
                     type = "response")

grid <- grid %>%
  mutate(expected_points = prob * exp_points_on_success + (1 - prob) * exp_points_on_miss)


summary(grid$prob)

library(ggplot2)
thresholds <- c(0.5, 0.6, 0.7, 0.8, 0.9)
ggplot(grid, aes(x = x + 35, y = y, fill = prob)) +   # +35 to convert centered x back to 0-70 if desired
  geom_tile() +
  geom_contour(aes(z = prob), breaks = thresholds, color = "white", linewidth = 0.6, linetype = "dashed") +
  scale_fill_viridis_c(option = "magma", name = "GAM Prob") +
  coord_fixed() + theme_minimal() +
  labs(title = "GAM predicted probability of success", x = "Lateral position (m)", y = "Distance from goal line (m)")


# Expected Points after Penalty miss (drop kick from opposition 22m)

all_restarts <- restarts

head(all_restarts)
table(all_restarts$Location)

# Need to Remove All Restarts After Tries and Penalties

restarts_not_after_score <- phase_data %>%
  mutate(
    prev_ID = lag(ID),
    prev_Points = lag(Points_Difference),
    prev_Play = lag(Play_Start),
    prev_Team_In_Poss = lag(Team_In_Poss)
  ) %>%
  filter(
    Phase == 1,
    Play_Start == "Restart Kick",
    !is.na(prev_ID),
    abs(Points_Difference - prev_Points) == 0,
    Seconds_Remaining < 4795,
    !between(Seconds_Remaining, 2390, 2400)
  )



head(restarts_not_after_score)
table(restarts_not_after_score$Location)


# Filtering out unreasonable locations

avg_points_by_location_restarts <- restarts_not_after_score %>%
  group_by(Location) %>%
  summarise(
    n = n(),
    avg_expected_points = mean(points, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_expected_points))

print(avg_points_by_location_restarts)

overall_avg_points_restarts <- restarts_not_after_score %>%
  summarise(overall_avg_expected_points = mean(points, na.rm = TRUE))

print(overall_avg_points_restarts)


# lineout expected points

expected_points_by_zone <- tibble(
  Location = c("10m-22m (opp)", "22m-5m (opp)", "5m-Goal (opp)", "Half-10m (opp)"),
  y_mid = c(31, 13.5, 2.5, 45),
  avg_points = c(1.25, 2.04, 3.74, 0.879)
)

# fit quadratic function
quad_fit <- lm(avg_points ~ poly(y_mid, 2, raw = TRUE), data = expected_points_by_zone)

y_dense <- seq(min(expected_points_by_zone$y_mid),
               max(expected_points_by_zone$y_mid), length.out = 500)

fitted_dense <- predict(quad_fit, newdata = data.frame(y_mid = y_dense))

p5 <- ggplot() +
  geom_point(data = expected_points_by_zone, aes(x = y_mid, y = avg_points), color = "blue") +
  geom_line(aes(x = y_dense, y = fitted_dense), color = "red", linewidth = 1) +
  labs(title = "Quadratic Fit to Average Points by Zone",
       x = "y_mid",
       y = "Average Points") +
  theme_minimal()

ggsave("plots/quadratic_fit.png", p5, width = 10, height = 6, dpi = 300)

interpolate_quad <- function(x) {
  predict(quad_fit, newdata = data.frame(y_mid = x))
}

grid <- grid %>%
  mutate(
    avg_points_interp = interpolate_quad(y),
    point_diff = expected_points - avg_points_interp
  )

# graph of if the lineout takes place at the same distance from goal as kick
p6 <- ggplot(grid, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",    # darker blue for negative
    mid = "white",       # zero
    high = "#E76F51",   # darker orange/red for positive
    midpoint = 0,
    name = "Kick vs Lineout"
  ) +
  coord_fixed() +
  labs(
    title = "Expected Points Difference: Kick vs Lineout",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

ggsave("plots/kick_vs_lineout_comparison.png", p6, width = 12, height = 10, dpi = 300)

# with a y shift

y_shifts <- c(0, -5, -10, -15, -20)

plots <- lapply(y_shifts, function(shift) {
  
  grid_shifted <- grid %>%
    mutate(
      avg_points_interp = pmin(interpolate_quad(y + shift), 3.74),  # cap at 3.74
      point_diff = expected_points - avg_points_interp
    )
  
  p <- ggplot(grid_shifted, aes(x = x, y = y, fill = point_diff)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#457B9D",    # darker blue for negative
      mid = "white",       # zero
      high = "#E76F51",   # darker orange/red for positive
      midpoint = 0,
      name = "Kick vs Lineout"
    ) +
    coord_fixed() +
    labs(
      title = paste("Expected Points Difference: Kick vs Lineout (Lineout", shift, "m)"),
      x = "Lateral position (m)",
      y = "Distance from goal line (m)"
    ) +
    theme_minimal()
  
  # Save each plot
  filename <- paste0("plots/kick_vs_lineout_shift_", abs(shift), "m.png")
  ggsave(filename, p, width = 12, height = 10, dpi = 300)
  
  p
})



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

# successfulpenalty kicks and locations
successful_penalty_kicks <- phase_data %>%
  arrange(ID) %>% 
  mutate(
    prev_location = lag(Location),          # previous play location
    prev_team = lag(Team_In_Poss),          # previous team in possession
    prev_side = lag(Side),                  # previous side
    score_change = Points_Difference - lag(Points_Difference),
    
    # flip certain "own" zones to "opp"
    prev_location = case_when(
      prev_location == "Goal-5m (own)"   ~ "5m-Goal (opp)",
      prev_location == "5m-22m (own)"    ~ "22m-5m (opp)",
      prev_location == "22m-10m (own)"   ~ "10m-22m (opp)",
      TRUE ~ prev_location
    )
  ) %>%
  # isolate for 3-point scores
  filter(
    abs(score_change) == 3
  ) %>%
  select(ID, Round, Home, Away, Location, prev_location, prev_team, prev_side, Points_Difference, Outcome)
# preview data
print(successful_penalty_kicks)


# mapping successful penalty kicks

zone_levels <- c("Goal-5m (own)", "5m-22m (own)", "22m-10m (own)", "10m-Half (own)",
                 "Half-10m (opp)", "10m-22m (opp)", "22m-5m (opp)", "5m-Goal (opp)")

successful_penalty_kicks <- successful_penalty_kicks %>%
  mutate(
    prev_location = factor(prev_location, levels = zone_levels),
    prev_side = factor(prev_side, levels = c("Left", "Centre", "Right"))
  )

field_counts <- successful_penalty_kicks %>%
  count(prev_location, prev_side)

p1 <- ggplot(field_counts, aes(x = prev_side, y = prev_location, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Successful Penalty Kicks by Field Position",
       x = "Side of Field",
       y = "Field Zone") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("plots/penalty_kicks_by_position.png", p1, width = 10, height = 8, dpi = 300)

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

field_lines <- c(5, 22, 40, 50, 60, 78, 95)
line_names <- c("5m (opp)", "22m (opp)", "10m (opp)", "Half", "10m (own)", "22m (own)", "5m (own)")

zone_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)

expected_points_by_zone$Location <- factor(expected_points_by_zone$Location, levels = zones_order)
expected_points_by_zone$meter_x <- zone_meters[match(as.character(expected_points_by_zone$Location), zones_order)]

# Computing Quadratic Minimum

quad_fit_tmp <- lm(avg_points ~ poly(meter_x, 2), data = expected_points_by_zone)
grid_x <- seq(min(expected_points_by_zone$meter_x, na.rm = TRUE),
              max(expected_points_by_zone$meter_x, na.rm = TRUE),
              length.out = 2000)
pred_tmp <- predict(quad_fit_tmp, newdata = data.frame(meter_x = grid_x))

cap_x <- grid_x[which.min(pred_tmp)]

lineout_plot <- ggplot(expected_points_by_zone, aes(x = meter_x, y = avg_points)) +
  geom_point(size = 2) +
  geom_text(aes(label = round(avg_points, 2)), vjust = -1.2, size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", formula = y ~ poly(pmin(x,  cap_x), 2), se = FALSE, linewidth = 0.8) +
  scale_x_continuous(
    breaks = field_lines,
    labels = line_names,
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
    axis.title.x = element_text(margin = ggplot2::margin(t = unit(8, "pt")))
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

# coefficients from logistic regression (REPLICATE THIS)
beta_angle <- 0.45
beta_distance <- -0.022
intercept <- -1.78

x_vals <- seq(-35, 35, by = 1)
y_vals <- seq(5, 60, by = 1)
grid <- expand.grid(x = x_vals, y = y_vals)

# SET THIS AS DEFAULT VALUE, CORRECT IF NOT SO
post_half_width <- 2.81

# logistic function
logit_prob <- function(angle, distance) {
  1 / (1 + exp(-(beta_angle * angle + beta_distance * distance + intercept)))
}

compute_expected_points <- function(x, y, post_half_width = 2.81) {
  # positions of the posts along the x-axis
  left_post_x  <- -post_half_width
  right_post_x <-  post_half_width
  
  # distances to the posts from kicker at (x, y)
  angle_left  <- atan2(x + left_post_x, y)
  angle_right <- atan2(x - right_post_x, y)
  
  # angular width of target
  angle <- angle_right - angle_left
  
  # logistic probability
  distance <- sqrt(x^2 + y^2)
  prob_raw <- logit_prob(angle, distance)
  
  prob_raw
}

grid <- grid %>%
  mutate(
    prob_raw_pos = compute_expected_points(x, y),
    prob_raw_neg = compute_expected_points(-x, y),  # mirror kicker position
    prob_raw_max = pmax(prob_raw_pos, prob_raw_neg)
  ) %>%
  mutate(
    prob = prob_raw_max / max(prob_raw_max),
    expected_points = prob * 3 + (1-prob)*0.604    # 6.04 obtained from expected points following a 22 meter drop out
  )


# plot symmetric heatmap
thresholds <- c(0.8, 0.6, 0.4, 0.2)

p3 <- ggplot(grid, aes(x = x, y = y, fill = prob)) +
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

ggsave("plots/kick_success_probability.png", p3, width = 12, height = 10, dpi = 300)

p4 <- ggplot(grid, aes(x = x, y = y, fill = expected_points)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Expected Points") +
  coord_fixed() +
  labs(
    title = "Expected Points of Kick Success (Symmetric)",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

ggsave("plots/expected_points_heatmap.png", p4, width = 12, height = 10, dpi = 300)



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

y_shifts <- c(0, -5, -10, -15, -20, -25)

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

# Specific y shift and decision marker

y_shift <- -15

marker_x <- -20
marker_y <- 30

grid_shifted <- grid %>%
  mutate(
    avg_points_interp = pmin(interpolate_quad(y + y_shift), 3.74),
    point_diff = expected_points - avg_points_interp
  )

decision_marker <- ggplot(grid_shifted, aes(x = x, y = y, fill = point_diff)) +
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
    title = paste("Expected Points Difference: Kick vs Lineout (Lineout", y_shift, "m)"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal() +
  # Add your marker
  geom_point(aes(x = marker_x, y = marker_y), color = "black", size = 4, shape = 4, stroke = 1.2)

ggsave("plots/kick_vs_lineout_shift_15m_marker.png", decision_marker, width = 12, height = 10, dpi = 300)

decision_marker

marker_value <- grid_shifted %>%
  filter(x == marker_x, y == marker_y) %>%
  select(expected_points, avg_points_interp, point_diff)

marker_value <- marker_value %>%
  mutate(
    expected_points = round(expected_points, 2),
    avg_points_interp = round(avg_points_interp, 2),
    point_diff = round(point_diff, 2)
  )

marker_value


# All Blacks vs South Africa Game

sep_game_data = read_csv("data/All Blacks vs South Africa Game Sep 16th.csv")

sep_game_data <- sep_game_data %>%
  rename(
    x = `x location`,
    y = `y location`
  ) %>%
  mutate(
    x = x - 35
  )
  
sep_game_data <- sep_game_data %>%
  left_join(grid, by = c("x", "y"))

sep_game_data <- sep_game_data %>%
  mutate(
    optimal_decision = if_else(point_diff > 0, "lineout", "kick")
  )

decision_tbl <- sep_game_data %>%
  select(Decision, optimal_decision, point_diff)
print(decision_tbl)

decision_tbl <- decision_tbl %>%
  mutate(
    missed_points = if_else(Decision == optimal_decision, 0, point_diff)
  )
print(decision_tbl)

sum(abs(decision_tbl$missed_points))

# Graphing Decision Boundary

marker_x <- -20
marker_y <- 30
y_shifts <- seq(0, -25, by = -1)

shift_results <- lapply(y_shifts, function(shift) {
  grid_shifted <- grid %>%
    mutate(
      avg_points_interp = pmin(interpolate_quad(y + shift), 3.74),
      point_diff = expected_points - avg_points_interp
    )
  
  marker_val <- grid_shifted %>%
    mutate(dist = sqrt((x - marker_x)^2 + (y - marker_y)^2)) %>%
    slice_min(dist, n = 1) %>%
    select(expected_points, avg_points_interp)
  
  tibble(
    y_shift = shift,
    kick_EP = marker_val$expected_points,
    lineout_EP = marker_val$avg_points_interp
  )
}) %>%
  bind_rows()

shift_results_long <- shift_results %>%
  pivot_longer(
    cols = c(kick_EP, lineout_EP),
    names_to = "Option",
    values_to = "Expected_Points"
  )

delta_intercept <- ggplot(shift_results_long, aes(x = y_shift, y = Expected_Points, color = Option)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = paste("Expected Points vs Lineout Shift at (", marker_x, ",", marker_y, ")"),
    x = "Yards Gained for Lineout (m)",
    y = "Expected Points",
    color = "Option"
  )

ggsave("plots/delta_intercept_graph.png", delta_intercept, width = 12, height = 10, dpi = 300)


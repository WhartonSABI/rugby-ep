library(dplyr)
library(stringr)

setwd("/Users/kennywatts/Downloads")

phase_data <- read.csv("Premiership Rugby 2018-19 Phase Data.csv")

## Links to Data
# https://www.data-ruck.com/blog/predicting-kicks-outcome/?utm_source=chatgpt.com
# https://journals.sagepub.com/doi/10.1177/22150218251365220

head(phase_data)

# Investigating Data

unique(phase_data$Location)
# The location on the pitch where the phase began, determined by the metre lines dividing the rugby union pitch.

unique(phase_data$Phase)
# Period between subsequent rucks

unique(phase_data$Side)
# The location on the pitch where the phase began, determined by the lineout lines dividing the rugby union pitch.

unique(phase_data$Play_Start)
# Type of way the play started

unique(phase_data$Outcome)
# Upon scoring, the event (e.g., tries, penalty kicks, drop goals or end of the half) was assigned to all phases since the previous scoring event.

unique(phase_data$Points_Difference)
# The points difference (I'm assuming for whoever has possession of the ball)

phase_data <- phase_data %>%
  select(-Red_Cards_Own, -Red_Cards_Opp, -Yellow_Cards_Own, -Yellow_Cards_Opp)

restarts <- phase_data %>%
  filter(Play_Start == "Restart Kick", Phase ==1)

unique(restarts$Location)

# Finding Penalty Kicks and Their locations

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
  filter(
    abs(score_change) == 3
  ) %>%
  select(ID, Round, Home, Away, Location, prev_location, prev_team, prev_side, Points_Difference, Outcome)

print(successful_penalty_kicks)


# Mapping Successful Penalty Kicks

library(ggplot2)

zone_levels <- c("Goal-5m (own)", "5m-22m (own)", "22m-10m (own)", "10m-Half (own)",
                 "Half-10m (opp)", "10m-22m (opp)", "22m-5m (opp)", "5m-Goal (opp)")

successful_penalty_kicks <- successful_penalty_kicks %>%
  mutate(
    prev_location = factor(prev_location, levels = zone_levels),
    prev_side = factor(prev_side, levels = c("Left", "Centre", "Right"))
  )

field_counts <- successful_penalty_kicks %>%
  count(prev_location, prev_side)

ggplot(field_counts, aes(x = prev_side, y = prev_location, fill = n)) +
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

# Expected Points of lineout in different locations

phase_data <- phase_data %>%
  mutate(
    # Extract signed numbers inside parentheses: (+3), (-3), etc.
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # Handle cases with no number (e.g. "No Score", turnovers)
    points = ifelse(is.na(points), 0, points)
  )

zones <- c("Half-10m (opp)", "10m-22m (opp)", "22m-5m (opp)", "5m-Goal (opp)")

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

df <- tibble::tibble(
  y_mid = c(31, 13.5, 2.5, 45),
  avg_points = c(1.25, 2.04, 3.74, 0.879)
)

ggplot(df, aes(x = y_mid, y = avg_points)) +
  geom_point(size = 3, color = "red") +
  geom_line(group = 1, color = "blue") +
  labs(
    title = "Average Expected Points by Zone",
    x = "Distance from goal line (m, zone midpoint)",
    y = "Average Points"
  ) +
  theme_minimal()

####

library(ggplot2)
library(dplyr)

# Coefficients from logistic regression
beta_angle <- 0.45
beta_distance <- -0.022
intercept <- -1.78

library(dplyr)

x_vals <- seq(-35, 35, by = 1)
y_vals <- seq(5, 60, by = 1)
grid <- expand.grid(x = x_vals, y = y_vals)

post_half_width <- 2.81

# Logistic function
logit_prob <- function(angle, distance) {
  1 / (1 + exp(-(beta_angle * angle + beta_distance * distance + intercept)))
}

compute_expected_points <- function(x, y, post_half_width) {
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
    prob_raw_pos = compute_expected_points(x, y, k),
    prob_raw_neg = compute_expected_points(-x, y, k),  # mirror kicker position
    prob_raw_max = pmax(prob_raw_pos, prob_raw_neg)
  ) %>%
  mutate(
    prob = prob_raw_max / max(prob_raw_max),
    expected_points = prob * 3
  )


# Plot symmetric heatmap
thresholds <- c(0.8, 0.6, 0.4, 0.2)

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


ggplot(grid, aes(x = x, y = y, fill = expected_points)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Expected Points") +
  coord_fixed() +
  labs(
    title = "Expected Points of Kick Success (Symmetric)",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()


# Including Lineout Expected Points
library(dplyr)
library(ggplot2)

expected_points_by_zone <- tibble(
  Location = c("10m-22m (opp)", "22m-5m (opp)", "5m-Goal (opp)", "Half-10m (opp)"),
  y_mid = c(31, 13.5, 2.5, 45),
  avg_points = c(1.25, 2.04, 3.74, 0.879)
)

# Fit quadratic function
quad_fit <- lm(avg_points ~ poly(y_mid, 2, raw = TRUE), data = expected_points_by_zone)

y_dense <- seq(min(expected_points_by_zone$y_mid),
               max(expected_points_by_zone$y_mid), length.out = 500)

fitted_dense <- predict(quad_fit, newdata = data.frame(y_mid = y_dense))

ggplot() +
  geom_point(data = expected_points_by_zone, aes(x = y_mid, y = avg_points), color = "blue") +
  geom_line(aes(x = y_dense, y = fitted_dense), color = "red", linewidth = 1) +
  labs(title = "Quadratic Fit to Average Points by Zone",
       x = "y_mid",
       y = "Average Points") +
  theme_minimal()

interpolate_quad <- function(x) {
  predict(quad_fit, newdata = data.frame(y_mid = x))
}

grid <- grid %>%
  mutate(
    avg_points_interp = interpolate_quad(y),
    point_diff = expected_points - avg_points_interp
  )

# Graph of if the lineout takes place at the same distance from goal as kick
ggplot(grid, aes(x = x, y = y, fill = point_diff)) +
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

# With a y shift

library(patchwork)

y_shifts <- c(0, -5, -10, -15, -20)

plots <- lapply(y_shifts, function(shift) {
  
  grid_shifted <- grid %>%
    mutate(
      avg_points_interp = pmin(interpolate_quad(y + shift), 3.74),  # cap at 3.74
      point_diff = expected_points - avg_points_interp
    )
  
  ggplot(grid_shifted, aes(x = x, y = y, fill = point_diff)) +
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
})

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]


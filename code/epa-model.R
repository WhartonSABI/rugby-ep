##################
### REFERENCES ###
##################

# setwd("/Users/kennywatts/Documents/GitHub/Rugby-Expected-Points/")

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
########################

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

# phase data including cards

phase_data_cards <- phase_data

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
  arrange(Round, Home, Away, Team_In_Poss, desc(Seconds_Remaining))

expected_points_by_zone <- lineouts_clean %>%
  group_by(Location) %>%
  summarise(
    n_phases = n(),
    avg_points = mean(points, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    .groups = "drop"
)

print(expected_points_by_zone)

#####
# Expected points using linear regression
#####

last_play <- phase_data_cards %>%
  group_by(Round, Home, Away) %>%
  filter(ID == max(ID)) %>%
  ungroup() %>%
  mutate(
    last_play_points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    last_play_points = ifelse(is.na(last_play_points), 0, last_play_points),
    
    Score_Change = last_play_points,
    
    Final_Points_Difference = Points_Difference + Score_Change,
    Final_Points_Diff_Home = if_else(Team_In_Poss == "Home",
                                     Final_Points_Difference,
                                     -Final_Points_Difference)
)

# making win percentage column

matches <- last_play %>%
  select(Round, Home, Away, Final_Points_Diff_Home) %>%
  distinct()

team_games <- matches %>%
  pivot_longer(cols = c(Home, Away),
               names_to = "Side",
               values_to = "Team") %>%
  mutate(
    Win = case_when(
      Side == "Home" & Final_Points_Diff_Home > 0 ~ 1,
      Side == "Home" & Final_Points_Diff_Home <= 0 ~ 0,
      Side == "Away" & Final_Points_Diff_Home < 0 ~ 1,
      Side == "Away" & Final_Points_Diff_Home >= 0 ~ 0
    ),
    Round = as.numeric(Round)
  ) %>%
  arrange(Team, Round)

team_games <- team_games %>%
  group_by(Team) %>%
  arrange(Round) %>%
  mutate(
    Games_Played = lag(row_number(), default = 0),
    Wins_Before = lag(cumsum(Win), default = 0),
    WinPct_Before = if_else(Games_Played == 0, 0.5, Wins_Before / Games_Played)
  ) %>%
  ungroup()

phase_data_cards <- phase_data_cards %>%
  mutate(
    Team_for_join = case_when(
      Team_In_Poss == "Home" ~ Home,
      Team_In_Poss == "Away" ~ Away
    )
  )

phase_data_cards <- phase_data_cards %>%
  left_join(
    team_games %>% select(Team, Round, WinPct_Before),
    by = c("Team_for_join" = "Team", "Round" = "Round")
  )

regression_lineouts <- phase_data_cards %>%
  filter(
    Play_Start == "Lineout",
    Location %in% zones,
    Phase == 1
  )

# Home and away column

regression_lineouts <- regression_lineouts %>%
  mutate(
    Home_Attack = if_else(Team_In_Poss == "Home", 1, 0)
  )

regression_lineouts <- regression_lineouts %>%
  mutate(
    Opponent = if_else(Team_In_Poss == "Home", Away, Home)
  ) %>%
  left_join(
    team_games %>% select(Team, Round, WinPct_Before) %>%
      rename(Opponent = Team,
             Opponent_WinPct = WinPct_Before),
    by = c("Opponent", "Round")
  )

# Win percent differential

regression_lineouts <- regression_lineouts %>%
  mutate(
    WinPct_Diff = WinPct_Before - Opponent_WinPct
  )

# Card differential

regression_lineouts <- regression_lineouts %>%
  mutate(
    Card_Diff = (Yellow_Cards_Opp + Red_Cards_Opp) - (Yellow_Cards_Own + Red_Cards_Own)
  )

# Seconds remaining in half

regression_lineouts <- regression_lineouts %>%
  mutate(
    Seconds_Remaining_Half = if_else(
      Seconds_Remaining > 2400,             # first half
      Seconds_Remaining - 2400,             # seconds remaining in first half
      Seconds_Remaining                     # second half: already less than 2400
    )
  )

regression_lineouts <- regression_lineouts %>%
  mutate(
    Less_Than_2_Min = if_else(Seconds_Remaining_Half < 120, 1, 0)
  )

regression_lineouts <- regression_lineouts %>%
  mutate(
  points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
  points = ifelse(is.na(points), 0, points))

# Weighting lineouts in consecutive possessions with same outcome

regression_lineouts <- regression_lineouts %>%
  arrange(Round, Home, Away, ID) %>%
  group_by(Round, Home, Away) %>%
  mutate(
    run_id = cumsum(c(TRUE, diff(Points_Difference) != 0 | Outcome[-1] != Outcome[-n()])),
    n_same = ave(run_id, run_id, FUN = length),          # count observations in each run
    weighted_points = points / n_same                   # divide points by number in run
  ) %>%
  ungroup()

lineouts_by_zone <- split(regression_lineouts, regression_lineouts$Location)

zone_regressions <- list()

for (zone_name in names(lineouts_by_zone)) {
  df <- lineouts_by_zone[[zone_name]]
  
  lm_zone <- lm(weighted_points ~ Less_Than_2_Min + Home_Attack + WinPct_Diff + Card_Diff, data = df)
  
  zone_regressions[[zone_name]] <- lm_zone
}

# Expected points for each zone using regression

library(broom)

zone_coefficients <- lapply(names(zone_regressions), function(zone) {
  broom::tidy(zone_regressions[[zone]]) %>%
    mutate(Location = zone)
}) %>%
  bind_rows()

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

intercepts <- zone_coefficients %>%
  filter(term == "(Intercept)")

intercepts <- intercepts %>%
  mutate(Location = factor(Location, levels = zones_order))

reg_plot <- ggplot(intercepts, aes(x = Location, y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_text(aes(label = round(estimate, 2)),
            nudge_y = 0.5,   # move labels slightly above the bars
            hjust = -0.5) +   # center horizontally
  labs(
    x = "Zone",
    y = "Intercept (Expected Points at baseline)",
    title = "Intercept Coefficients for Each Lineout Zone"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/reg_plot.png", reg_plot, width = 10, height = 6, dpi = 300)

# Standard Scenario Plot

Less_Than_2_Min_val <- 0
Home_Attack_val <- 1
WinPct_Diff_val <- 0
Card_Diff_val <- 0

zone_coeff_summary <- zone_coefficients %>%
  select(Location, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate, values_fn = mean)

EP_standard <- zone_coeff_summary %>%
  mutate(
    EP_standard = `(Intercept)` +
      Less_Than_2_Min * Less_Than_2_Min_val +
      Home_Attack * Home_Attack_val +
      WinPct_Diff * WinPct_Diff_val +
      Card_Diff * Card_Diff_val
  ) %>%
  select(Location, EP_standard) %>%
  pivot_longer(
    cols = EP_standard,
    names_to = "Condition",
    values_to = "Expected_Points"
  ) %>%
  mutate(Location = factor(Location, levels = zones_order))

reg_plot_standard <- ggplot(EP_standard, aes(x = Location, y = Expected_Points)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Expected_Points, 2)),
            nudge_y = 0.5, 
            hjust = -0.5) +
  labs(
    x = "Zone",
    y = "Expected Points (Standard Scenario)",
    title = "Expected Points for Each Lineout Zone (Standard Scenario)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/standard_coef_reg_plot.png", reg_plot_standard, width = 10, height = 6, dpi = 300)

# Expected Points of Lineout All Areas of the pitch

expected_points_by_zone <- lineouts_weighted %>%
  group_by(Location) %>%
  summarise(
    n_phases = n(),
    avg_points = mean(weighted_points, na.rm = TRUE),
    total_points = sum(weighted_points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Location = factor(Location, levels = zones_order)) %>%
  arrange(Location)

expected_points_regression <- EP_standard %>%
  arrange(factor(Location, levels = zones_order)) %>%
  select(Location, Expected_Points) %>%
  mutate(Location = factor(Location, levels = zones_order))

# Print Table
print(expected_points_by_zone)

# Plot of Meteres vs Expected Points of Lineout

field_lines <- c(5, 22, 40, 50, 60, 78, 95)
line_names <- c("5m (opp)", "22m (opp)", "10m (opp)", "Half", "10m (own)", "22m (own)", "5m (own)")
zone_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)

expected_points_by_zone$Location <- factor(expected_points_by_zone$Location, levels = zones_order)
expected_points_by_zone$meter_x <- zone_meters[match(as.character(expected_points_by_zone$Location), zones_order)]

expected_points_regression$Location <- factor(expected_points_regression$Location, levels = zones_order)
expected_points_regression$meter_x <- zone_meters[match(as.character(expected_points_regression$Location), zones_order)]

# Computing Quadratic Minimum

quad_fit_tmp <- lm(avg_points ~ poly(meter_x, 2), data = expected_points_by_zone)
quad_fit_reg <- lm(Expected_Points ~ poly(meter_x, 2), data = expected_points_regression)


grid_x <- seq(min(expected_points_by_zone$meter_x, na.rm = TRUE),
              max(expected_points_by_zone$meter_x, na.rm = TRUE),
              length.out = 2000)

pred_tmp <- predict(quad_fit_tmp, newdata = data.frame(meter_x = grid_x))
pred_reg <- predict(quad_fit_reg, newdata = data.frame(meter_x = grid_x))

cap_x <- grid_x[which.min(pred_tmp)]
cap_x_reg <- grid_x[which.min(pred_reg)]

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
  avg_points = c(1.36, 2.4, 4.17, 0.66)
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

# Regression quad plot

lineout_plot_reg <- ggplot(expected_points_regression, aes(x = meter_x, y = Expected_Points)) +
  geom_point(size = 2) +
  geom_text(aes(label = round(Expected_Points, 2)), vjust = -1.2, size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", formula = y ~ poly(pmin(x,  cap_x_reg), 2), se = FALSE, linewidth = 0.8) +
  scale_x_continuous(
    breaks = field_lines,
    labels = line_names,
    expand = expansion(add = c(3, 3)),
    limits = c(0, 100)  # ensures full field range
  ) +
  labs(
    title = "Average Points by Lineout Location",
    x = "Field Position (meters)",
    y = "Average Expected Points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = unit(8, "pt")))
  )

ggsave("plots/lineout_plot_reg.png", lineout_plot_reg, width = 10, height = 6, dpi = 300)

df_reg <- tibble::tibble(
  y_mid = c(31, 13.5, 2.5, 45),
  estimate = c(1.36, 2.4, 4.17, 0.66)
)

p2_reg <- ggplot(df_reg, aes(x = y_mid, y = estimate)) +
  geom_point(size = 3, color = "red") +
  geom_line(group = 1, color = "blue") +
  labs(
    title = "Average Expected Points by Zone",
    x = "Distance from goal line (m, zone midpoint)",
    y = "Average Points"
  ) +
  theme_minimal()

ggsave("plots/expected_points_regression.png", p2_reg, width = 10, height = 6, dpi = 300)

# coefficients from logistic regression (REPLICATE THIS)
beta_angle <- 0.45
beta_distance <- -0.022
intercept <- -1.78

x_vals <- seq(-35, 35, by = 1)
y_vals <- seq(5, 65, by = 1)
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
    expected_points = prob * 3 + (1-prob)*0.74    # 0.74 obtained from expected points following a 22 meter drop out
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

avg_points_by_location_restarts <- avg_points_by_location_restarts %>% 
  dplyr::slice(1:5)

print(avg_points_by_location_restarts)

overall_avg_points_restarts <- weighted.mean(
  avg_points_by_location_restarts$avg_expected_points,
  avg_points_by_location_restarts$n
)


y_dense <- seq(min(expected_points_by_zone$meter_x),
               max(expected_points_by_zone$meter_x), length.out = 500)
y_dense_reg <- seq(min(expected_points_regression$meter_x),
               max(expected_points_regression$meter_x), length.out = 500)

fitted_dense <- predict(quad_fit_tmp, newdata = data.frame(meter_x = y_dense))
fitted_dense_reg <- predict(quad_fit_reg, newdata = data.frame(meter_x = y_dense_reg))

p5 <- ggplot() +
  geom_point(data = expected_points_by_zone, aes(x = meter_x, y = avg_points), color = "blue") +
  geom_line(aes(x = y_dense, y = fitted_dense), color = "red", linewidth = 1) +
  labs(title = "Quadratic Fit to Average Points by Zone",
       x = "meter_x",
       y = "Average Points") +
  theme_minimal()

ggsave("plots/quadratic_fit.png", p5, width = 10, height = 6, dpi = 300)

p5_reg <- ggplot() +
  geom_point(data = expected_points_regression, aes(x = meter_x, y = Expected_Points), color = "blue") +
  geom_line(aes(x = y_dense_reg, y = fitted_dense_reg), color = "red", linewidth = 1) +
  labs(title = "Quadratic Fit to Average Points by Zone",
       x = "meter_x",
       y = "Average Points") +
  theme_minimal()

ggsave("plots/quadratic_fit_reg.png", p5, width = 10, height = 6, dpi = 300)

interpolate_quad <- function(x) {
  predict(quad_fit_tmp, newdata = data.frame(meter_x = x))
}

interpolate_quad_reg <- function(x) {
  predict(quad_fit_reg, newdata = data.frame(meter_x = x))
}

grid <- grid %>%
  mutate(
    avg_points_interp = interpolate_quad(y),
    point_diff = expected_points - avg_points_interp
  )

grid_reg <- grid %>%
  mutate(
    avg_points_interp = interpolate_quad_reg(y),
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


p6_reg <- ggplot(grid_reg, aes(x = x, y = y, fill = point_diff)) +
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
    title = "Expected Points Difference (Regression): Kick vs Lineout",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

ggsave("plots/kick_vs_lineout_comparison_reg.png", p6_reg, width = 12, height = 10, dpi = 300)

# with a y shift

y_shifts <- c(0, -5, -10, -15, -20, -25)

plots <- lapply(y_shifts, function(shift) {
  
  grid_shifted <- grid %>%
    mutate(
      avg_points_interp = pmin(interpolate_quad(y + shift), 4.17),
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

plots_reg <- lapply(y_shifts, function(shift) {
  
  grid_shifted_reg <- grid_reg %>%
    mutate(
      avg_points_interp = pmin(interpolate_quad_reg(y + shift), 4.96),
      point_diff = expected_points - avg_points_interp
    )
  
  p_reg <- ggplot(grid_shifted_reg, aes(x = x, y = y, fill = point_diff)) +
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
      title = paste("Expected Points Difference (Regression): Kick vs Lineout (Lineout", shift, "m)"),
      x = "Lateral position (m)",
      y = "Distance from goal line (m)"
    ) +
    theme_minimal()
  
  # Save each plot
  filename <- paste0("plots/kick_vs_lineout_reg_shift_", abs(shift), "m.png")
  ggsave(filename, p_reg, width = 12, height = 10, dpi = 300)
  
  p_reg
})

# Specific y shift and decision marker

y_shift <- -20

marker_x <- 19
marker_y <- 40

grid_shifted <- grid %>%
  mutate(
    avg_points_interp = pmin(interpolate_quad(y + y_shift), 4.17),
    point_diff = expected_points - avg_points_interp
  )

grid_shifted_reg <- grid %>%
  mutate(
    avg_points_interp = pmin(interpolate_quad_reg(y + y_shift), 4.96),
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

decision_marker_reg <- ggplot(grid_shifted_reg, aes(x = x, y = y, fill = point_diff)) +
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
    title = paste("Expected Points Difference (Regression): Kick vs Lineout (Lineout", y_shift, "m)"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal() +
  # Add your marker
  geom_point(aes(x = marker_x, y = marker_y), color = "black", size = 4, shape = 4, stroke = 1.2)

ggsave("plots/kick_vs_lineout_shift_15m_marker_reg.png", decision_marker_reg, width = 12, height = 10, dpi = 300)

decision_marker_reg

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

marker_value_reg <- grid_shifted_reg %>%
  filter(x == marker_x, y == marker_y) %>%
  select(expected_points, avg_points_interp, point_diff)

marker_value_reg <- marker_value_reg %>%
  mutate(
    expected_points = round(expected_points, 2),
    avg_points_interp = round(avg_points_interp, 2),
    point_diff = round(point_diff, 2)
  )

marker_value_reg


# Graphing Decision Boundary

marker_x <- 19
marker_y <- 40

y_shifts <- seq(0, -35, by = -1)

shift_results <- lapply(y_shifts, function(shift) {
  grid_shifted <- grid %>%
    mutate(
      avg_points_interp = pmin(interpolate_quad(y + shift), 4.17),
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

shift_results_reg <- lapply(y_shifts, function(shift) {
  grid_shifted_reg <- grid_reg %>%
    mutate(
      avg_points_interp = pmin(interpolate_quad_reg(y + shift), 4.17),
      point_diff = expected_points - avg_points_interp
    )
  
  marker_val_reg <- grid_shifted_reg %>%
    mutate(dist = sqrt((x - marker_x)^2 + (y - marker_y)^2)) %>%
    slice_min(dist, n = 1) %>%
    select(expected_points, avg_points_interp)
  
  tibble(
    y_shift = shift,
    kick_EP = marker_val_reg$expected_points,
    lineout_EP = marker_val_reg$avg_points_interp
  )
}) %>%
  bind_rows()

shift_results_long <- shift_results %>%
  mutate(y_shift_plot = abs(y_shift)) %>%
  pivot_longer(
    cols = c(kick_EP, lineout_EP),
    names_to = "Option",
    values_to = "Expected_Points"
  )

shift_results_long_reg <- shift_results_reg %>%
  mutate(y_shift_plot = abs(y_shift)) %>%
  pivot_longer(
    cols = c(kick_EP, lineout_EP),
    names_to = "Option",
    values_to = "Expected_Points"
  )

f <- function(shift) interpolate_quad(marker_y + shift) - shift_results$kick_EP[1]

zero_shift <- -uniroot(f, c(-50, 0))$root

zero_shift

f_reg <- function(shift) interpolate_quad_reg(marker_y + shift) - shift_results_reg$kick_EP[1]

zero_shift_reg <- -uniroot(f_reg, c(-50, 0))$root

zero_shift_reg


delta_intercept <- ggplot(shift_results_long, aes(x = y_shift_plot, y = Expected_Points, color = Option)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = abs(zero_shift), linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = abs(zero_shift) + 1,  # shift label slightly right (adjust as needed)
    y = max(shift_results_long$Expected_Points),
    label = sprintf("Intercept = %.2f m", abs(zero_shift)),
    vjust = -0.5,
    hjust = 0
  ) +
  theme_minimal() +
  labs(
    title = paste("Expected Points vs Lineout Shift at (", marker_x, ",", marker_y, ")"),
    x = "Yards Gained for Lineout (m)",
    y = "Expected Points",
    color = "Option"
  )

ggsave("plots/delta_intercept_graph.png", delta_intercept, width = 12, height = 10, dpi = 300)


delta_intercept_reg <- ggplot(shift_results_long_reg, aes(x = y_shift_plot, y = Expected_Points, color = Option)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = abs(zero_shift_reg), linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = abs(zero_shift_reg) + 1,  # shift label slightly right (adjust as needed)
    y = max(shift_results_long_reg$Expected_Points),
    label = sprintf("Intercept = %.2f m", abs(zero_shift_reg)),
    vjust = -0.5,
    hjust = 0
  ) +
  theme_minimal() +
  labs(
    title = paste("Expected Points vs Lineout Shift at (", marker_x, ",", marker_y, ")"),
    x = "Yards Gained for Lineout (m)",
    y = "Expected Points",
    color = "Option"
  )

ggsave("plots/delta_intercept_graph_reg.png", delta_intercept_reg, width = 12, height = 10, dpi = 300)


# Shifting Variables in Regression Equation

# Set scenario values
Less_Than_2_Min_Coef <- 0
Home_Attack_Coef <- 1
WinPct_Diff_Coef <- 0

EP_given_card <- zone_coefficients %>%
  select(Location, term, estimate) %>%
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) %>%
  mutate(
    EP_no_card = `(Intercept)` +
      Less_Than_2_Min * Less_Than_2_Min_Coef +
      Home_Attack * Home_Attack_Coef +
      WinPct_Diff * WinPct_Diff_Coef,
    EP_card    = EP_no_card + Card_Diff
  )

EP_given_card

EP_long <- EP_given_card %>%
  select(Location, EP_no_card, EP_card) %>%
  pivot_longer(
    cols = c(EP_no_card, EP_card),
    names_to = "Condition",
    values_to = "Expected_Points"
  ) %>%
  mutate(
    Location = factor(Location, levels = zones_order)
  )

yellow_card_plot <- ggplot(EP_long, aes(x = Location, y = Expected_Points, color = Condition)) +
  geom_point(size = 2) +
  geom_text(aes(label = round(Expected_Points, 2)), 
            vjust = -1, nudge_x = 0.4, size = 3, check_overlap = TRUE) +
  labs(
    title = "Expected Points by Zone With and Without Yellow Card",
    x = "Zone",
    y = "Expected Points",
    color = "Condition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/yellow_card_plot.png", yellow_card_plot, width = 12, height = 10, dpi = 300)


# Changing win percentage

Less_Than_2_Min_Coef <- 0
Home_Attack_Coef <- 1
Card_Diff_val <- 0
WinPct_Diff_vals <- c(-0.25, -0.125, 0, 0.125, 0.25)

EP_scenarios <- expand.grid(
  Location = unique(zone_coefficients$Location),
  WinPct_Diff_val = WinPct_Diff_vals
) %>%
  left_join(
    zone_coefficients %>%
      select(Location, term, estimate) %>%
      pivot_wider(names_from = term, values_from = estimate),
    by = "Location"
  ) %>%
  mutate(
    EP_no_card = `(Intercept)` +
      Less_Than_2_Min * Less_Than_2_Min_Coef +
      Home_Attack * Home_Attack_Coef +
      WinPct_Diff * WinPct_Diff_val +
      Card_Diff * Card_Diff_val
  ) %>%
  select(Location, WinPct_Diff_val, EP_no_card) %>%
  mutate(Location = factor(Location, levels = zones_order))

win_percent_plot <- ggplot(EP_scenarios, aes(x = Location, y = EP_no_card, color = as.factor(WinPct_Diff_val), group = WinPct_Diff_val)) +
  geom_point(size = 2) +
  geom_line() +
  geom_text(aes(label = round(EP_no_card, 2)), 
            vjust = -1, size = 3, check_overlap = FALSE,
            position = position_dodge(width = 0.5)) +
  labs(
    title = "Expected Points by Zone Without Yellow Card\nfor Different Win Percentage Differences",
    x = "Zone",
    y = "Expected Points",
    color = "Win % Diff"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/win_percent_plot.png", win_percent_plot, width = 12, height = 10, dpi = 300)

# Standard Scenario


# Scenario values
Less_Than_2_Min_val <- 0
Home_Attack_val <- 1
WinPct_Diff_val <- 0
Card_Diff_val <- 0

# Reshape to wide so each term and its std.error is a column
EP_standard <- zone_coefficients %>%
  pivot_wider(
    id_cols = Location,
    names_from = term,
    values_from = c(estimate, std.error),
    names_glue = "{term}_{.value}"
  ) %>%
  mutate(
    EP_standard = `(Intercept)_estimate` +
      Less_Than_2_Min_estimate * Less_Than_2_Min_val +
      Home_Attack_estimate * Home_Attack_val +
      WinPct_Diff_estimate * WinPct_Diff_val +
      Card_Diff_estimate * Card_Diff_val,
    SE_EP_standard = sqrt(
      `(Intercept)_std.error`^2 +
        (Less_Than_2_Min_val^2) * (Less_Than_2_Min_std.error^2) +
        (Home_Attack_val^2) * (Home_Attack_std.error^2) +
        (WinPct_Diff_val^2) * (WinPct_Diff_std.error^2) +
        (Card_Diff_val^2) * (Card_Diff_std.error^2)
    )
  ) %>%
  mutate(Location = factor(Location, levels = zones_order))

zones_to_plot <- zones_order[1:6]

EP_standard_subset <- EP_standard %>%
  filter(Location %in% zones_to_plot) %>%
  mutate(Location = factor(Location, levels = zones_to_plot))

reg_plot_standard <- ggplot(EP_standard_subset, aes(x = Location, y = EP_standard)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = EP_standard - SE_EP_standard, 
                    ymax = EP_standard + SE_EP_standard),
                width = 0.2) +
  geom_text(aes(label = round(EP_standard, 2)),
            nudge_y = 0.5, 
            hjust = -0.5) +
  labs(
    x = "Zone",
    y = "Expected Points (Standard Scenario)",
    title = "Expected Points for Each Lineout Zone (Standard Scenario)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/reg_plot_standard.png", reg_plot_standard, width = 12, height = 10, dpi = 300)


# All Blacks vs South Africa Game #This is wrong

sep_game_data_csv = read_csv("data/All Blacks vs South Africa Game Sep 16th.csv")

sep_game_data_csv <- sep_game_data_csv %>%
  rename(
    x = `x location`,
    y = `y location`
  ) %>%
  mutate(
    x = x - 35,
    y = y
  )

intercept_fit <- lm(estimate ~ poly(zone_meters, 2), data = intercepts)

sep_game_data_csv <- sep_game_data_csv %>%
  mutate(
    predicted_intercept = predict(intercept_fit,
                                  newdata = data.frame(
                                    zone_meters = distance_from_try_after_shift
                                  ))
  )

coeffs_zone_wide <- zone_coefficients %>%
  select(term, estimate, Location) %>%
  pivot_wider(
    names_from = term,
    values_from = estimate
  )

match_with_coeffs <- sep_game_data_csv %>%
  left_join(coeffs_zone_wide, by = "Location")

match_with_coeffs <- match_with_coeffs %>%
  mutate(
    EP_lineout =
      predicted_intercept +
      Less_Than_2_Min * Less_Than_2_Min_val +
      Home_Attack * Home_Attack_val +
      WinPct_Diff * WinPct_Diff_val +
      Card_Diff * Card_Diff_val
  )

match_with_coeffs <- match_with_coeffs %>%
  left_join(
    grid %>% select(x, y, expected_points),
    by = c("x", "y")
  ) %>%
  rename(EP_kick = expected_points)

# Comparing optimal to actual decisions

match_with_coeffs <- match_with_coeffs %>%
  mutate(
    optimal_decision = if_else(EP_kick >= EP_lineout, "kick", "lineout"),
    
    EP_optimal = pmax(EP_kick, EP_lineout),
    
    EP_actual = if_else(Decision == "kick", EP_kick, EP_lineout),
    
    EP_lost = EP_optimal - EP_actual
  )

decision_table <- match_with_coeffs %>%
  select(Team, Decision, optimal_decision, EP_actual, EP_optimal, EP_lost)

decision_table

team_EP_lost <- match_with_coeffs %>%
  group_by(Team) %>%
  summarise(
    total_EP_lost = sum(EP_lost, na.rm = TRUE)
  )

team_EP_lost

# Final score ABs won 24-17


# Scenario Plot

Less_Than_2_Min_val <- 0
Home_Attack_val <- 1
WinPct_Diff_val <- 0
Card_Diff_val <- -1
shift <- -20

zone_coeff_summary <- zone_coefficients %>%
  select(Location, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate, values_fn = mean)

EP_scenario <- zone_coeff_summary %>%
  mutate(
    EP_scenario = `(Intercept)` +
      Less_Than_2_Min * Less_Than_2_Min_val +
      Home_Attack * Home_Attack_val +
      WinPct_Diff * WinPct_Diff_val +
      Card_Diff * Card_Diff_val
  ) %>%
  select(Location, EP_scenario) %>%
  pivot_longer(
    cols = EP_scenario,
    names_to = "Condition",
    values_to = "Expected_Points"
  ) %>%
  mutate(Location = factor(Location, levels = zones_order))

expected_points_regression_scenario <- EP_scenario %>%
  arrange(factor(Location, levels = zones_order)) %>%
  select(Location, Expected_Points) %>%
  mutate(Location = factor(Location, levels = zones_order))

expected_points_regression_scenario$Location <- factor(expected_points_regression_scenario$Location, levels = zones_order)
expected_points_regression_scenario$meter_x <- zone_meters[match(as.character(expected_points_regression_scenario$Location), zones_order)]

quad_fit_reg_scenario <- lm(Expected_Points ~ poly(meter_x, 2), data = expected_points_regression_scenario)

grid_x <- seq(min(expected_points_by_zone$meter_x, na.rm = TRUE),
              max(expected_points_by_zone$meter_x, na.rm = TRUE),
              length.out = 2000)

y_dense_reg_scenario <- seq(min(expected_points_regression_scenario$meter_x),
                   max(expected_points_regression_scenario$meter_x), length.out = 500)

interpolate_quad_reg_scenario <- function(x) {
  predict(quad_fit_reg_scenario, newdata = data.frame(meter_x = x))
}

grid_reg <- grid %>%
  mutate(
    avg_points_interp = interpolate_quad_reg_scenario(y),
    point_diff = expected_points - avg_points_interp
  )

grid_shifted_reg <- grid_reg %>%
  mutate(
    avg_points_interp = pmin(interpolate_quad_reg_scenario(y + shift), 4.96),
    point_diff = expected_points - avg_points_interp
  )

x_marker <- 19
y_marker <- 40

scenario <- ggplot(grid_shifted_reg, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Kick vs Lineout"
  ) +
  #geom_text(aes(x = x_marker, y = y_marker), label = "X", 
            #size = 5, fontface = "bold", color = "black") +
  coord_fixed() +
  labs(
    title = paste("Expected Points Difference: Kick vs Lineout (Lineout", shift, "m, home, -1 card)"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

ggsave("plots/20m_home_-1card.png", scenario, width = 12, height = 10, dpi = 300)


#############
### SETUP ###
#############

# Packages

# install.packages("patchwork")
library(patchwork)
# install.packages("stringr")
library(stringr)
# install.packages("tidyverse")
library(tidyverse)

# Seed for randomization
set.seed(11-19-2025)

#######################
### PHASE DATA LOAD ###
#######################

# run from project directory (or within .Rproj)
phase_data = read_csv("data/phase_2018-19.csv")
# preview data
head(phase_data)

#########################
### LINEOUT DATA PREP ###
#########################

phase_data <- phase_data %>%
  mutate(
    # extract signed numbers inside parentheses: (+3), (-3), etc.
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # handle cases with no number (e.g. "no score", turnovers)
    points = ifelse(is.na(points), 0, points)
  )

# Keeping just first phases beginning with lineouts
phase_data <- phase_data %>%
  filter(
    Play_Start == "Lineout",
    Phase == 1
  )

# Getting final score of the match
last_play <- phase_data %>%
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

last_play <- last_play %>%
  select(Round, Home, Away, Final_Points_Diff_Home) %>%
  distinct()

last_play <- last_play %>%
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

# Get running win percentages
last_play <- last_play %>%
  group_by(Team) %>%
  arrange(Round) %>%
  mutate(
    Games_Played = lag(row_number(), default = 0),
    Wins_Before = lag(cumsum(Win), default = 0),
    WinPct_Before = if_else(Games_Played == 0, 0.5, Wins_Before / Games_Played)
  ) %>%
  ungroup()

# Get teams in possession
phase_data <- phase_data %>%
  mutate(
    Team_for_join = case_when(
      Team_In_Poss == "Home" ~ Home,
      Team_In_Poss == "Away" ~ Away
    )
  ) %>%
# Add in win percentage for team in possession
  left_join(
    last_play %>% select(Team, Round, WinPct_Before),
    by = c("Team_for_join" = "Team", "Round" = "Round")
  )

# Define which team is in possession
phase_data <- phase_data %>%
  mutate(
    Home_Attack = if_else(Team_In_Poss == "Home", 1, 0)
  )

# Get opponent data
phase_data <- phase_data %>%
  mutate(
    Opponent = if_else(Team_In_Poss == "Home", Away, Home)
  ) %>%
    left_join(
      last_play %>% select(Team, Round, WinPct_Before) %>%
        rename(Opponent = Team,
               Opponent_WinPct = WinPct_Before),
      by = c("Opponent", "Round")
    ) %>%
  mutate(
    # Win percent differential
    WinPct_Diff = WinPct_Before - Opponent_WinPct,
    # Card differential
    Card_Diff = (Yellow_Cards_Opp + Red_Cards_Opp) - (Yellow_Cards_Own + Red_Cards_Own)
  )



# Seconds remaining in half
phase_data <- phase_data %>%
  mutate(
    Seconds_Remaining_Half = if_else(
      Seconds_Remaining > 2400,             # first half
      Seconds_Remaining - 2400,             # seconds remaining in first half
      Seconds_Remaining                     # second half: already less than 2400
    )
  )

# Binary less than 2 mins

phase_data <- phase_data %>%
  mutate(
    Less_Than_2_Min = if_else(Seconds_Remaining_Half < 120, 1, 0)
  )

# Binary if lineout part of consecutive plays with same outcome without change in possesion
phase_data <- phase_data %>%
  arrange(Round, Home, Away, ID) %>%
  group_by(Round, Home, Away) %>%
  mutate(
    run_id = cumsum(c(TRUE, diff(Points_Difference) != 0 | Outcome[-1] != Outcome[-n()])),
    n_same = ave(run_id, run_id, FUN = length), # count observations in each run
  ) %>%
  ungroup()

# Adding meter line of play start
location_names <- c("5m-Goal (opp)", "22m-5m (opp)", "10m-22m (opp)",
                    "Half-10m (opp)","10m-Half (own)", "22m-10m (own)",
                    "5m-22m (own)", "Goal-5m (own)")
location_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)

lookup <- setNames(location_meters, location_names)

phase_data$meter_line <- lookup[phase_data$Location]

# Sampling one random observation from consecutive lineouts within a single possession
sampled_phase_data <- phase_data %>%
  arrange(Round, Home, Away, ID) %>%
  group_by(Round, Home, Away, run_id) %>%
  slice_sample(n = 1) %>% 
  ungroup()

#############
### PLOTS ###
#############

# Meter line
meter_line_marginal <- ggplot(sampled_phase_data, aes(x = meter_line, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs Meter Line (Marginal Relationship)",
    x = "Meter Line",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/meter_line_marginal.png", meter_line_marginal,
       width = 10, height = 8, dpi = 300)

# Seconds remaining in half
seconds_remaining_marginal <- ggplot(sampled_phase_data, aes(x = Seconds_Remaining_Half, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs Seconds Remaining in Half (Marginal Relationship)",
    x = "Seconds_Remaining_Half",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/seconds_remaining_marginal.png", seconds_remaining_marginal,
       width = 10, height = 8, dpi = 300)

# Home lineout possession
home_marginal <- ggplot(sampled_phase_data, aes(x = factor(Home_Attack), y = points)) +
  geom_boxplot(fill = "grey80") +
  labs(
    title = "Points by Home Attack",
    x = "Home Attack (0 = Away Attack, 1 = Home Attack)",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/home_marginal.png", home_marginal,
       width = 10, height = 8, dpi = 300)

# Win Percent Differential
win_per_marginal <- ggplot(sampled_phase_data, aes(x = WinPct_Diff, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs WinPct_Diff (Marginal Relationship)",
    x = "WinPct_Diff",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/win_per_marginal.png", win_per_marginal,
       width = 10, height = 8, dpi = 300)

# Card Differential
card_dif_marginal <- ggplot(sampled_phase_data, aes(x = factor(Card_Diff), y = points)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Points by Card Differential",
    x = "Card Differential (Own - Opponent)",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/card_dif_marginal.png", card_dif_marginal,
       width = 10, height = 8, dpi = 300)


regression <- lm(points ~ meter_line + Home_Attack + Card_Diff + WinPct_Diff,
                 data = sampled_phase_data)

summary(regression)


# Plotting EP of Lineout

# Regression coefficients
intercept <- 2.72591
coef_meter <- -0.05784
coef_home <- 1.00477
coef_card <- 0.84649
coef_win_per <- 0.77876

meter_seq <- seq(0, 100, by = 1)

expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 1 + coef_card * 0 + coef_win_per * 0

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

ep_by_meter_line <- ggplot(plot_data, aes(x = meter_line, y = expected_points)) +
  geom_line(size = 1.2, color = "blue") +
  labs(
    title = "Expected Points by Meter Line",
    subtitle = "Assuming Home_Attack = 1 and Card_Diff = 0",
    x = "Meter Line",
    y = "Expected Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/ep_by_meter_line.png", ep_by_meter_line,
       width = 10, height = 8, dpi = 300)

###############################
### KICKING EXPECTED POINTS ###
###############################

# GAM Model for Kick Percentage

saved_gam <- readRDS("gam_model.rds")

exp_points_on_miss <- 0.74
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

grid$prob <- predict(saved_gam,
                     newdata = grid %>% select(kicking_angle, distance_to_center),
                     type = "response")

grid <- grid %>%
  mutate(expected_points = prob * exp_points_on_success + (1 - prob) * exp_points_on_miss)

# Plotting probability of successful penalty kick

thresholds <- c(0.8, 0.6, 0.4, 0.2)

kick_prob_plot <- ggplot(grid, aes(x = x, y = y, fill = prob)) +
  geom_tile() +
  geom_contour(aes(z = prob),
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

ggsave("clean_plots/kick_prob_plot.png", kick_prob_plot,
       width = 10, height = 8, dpi = 300)

# Expected Points of a Penalty Kick

kick_ep_plot <- ggplot(grid, aes(x = x, y = y, fill = expected_points)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Expected Prob") +
  coord_fixed() +
  labs(
    title = "Expected Points of a Penalty Kick",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()

ggsave("clean_plots/kick_ep_plot.png", kick_ep_plot,
       width = 10, height = 8, dpi = 300)


#########################
### MAKING A DECISION ###
#########################

# Plotting Difference in Expected Points Across Pitch

plot_data <- plot_data %>%
  rename(lineout_ep = expected_points)

grid <- grid %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(kick_ep = expected_points) %>%
  mutate(
    point_diff = lineout_ep - kick_ep
)

# EP of Lineout minus EP of penalty kick 
# Assumes all coeffs = 0 expect meter_line, home, and intercept
# Also assumes no advancing of lineout from penalty location

no_shift_plot <- ggplot(grid, aes(x = x, y = y, fill = point_diff)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = "Point Differential: Lineout EP – Kick EP",
    x = "Field Width (x)",
    y = "Field Length (y)",
    fill = "Point Diff"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/no_shift_plot.png", no_shift_plot,
       width = 10, height = 8, dpi = 300)

# Shifting location of lineout forward

max_lineout_ep <- max(grid$lineout_ep)

y_shifts <- c(0, -5, -10, -15, -20, -25)

plots <- lapply(y_shifts, function(shift) {
  
  # Shift y and lookup shifted EP
  grid_shifted <- grid %>%
    mutate(y_shifted = y + shift) %>%
    
    left_join(
      grid %>%
        select(x, y, lineout_ep) %>%
        rename(y_shifted = y, lineout_ep_shifted = lineout_ep),
      by = c("x", "y_shifted")
    ) %>%
    
    # If y_shifted < min(y), then lineout_ep_shifted becomes NA — replace with max
    mutate(
      lineout_ep_shifted = ifelse(
        is.na(lineout_ep_shifted),
        max_lineout_ep,
        lineout_ep_shifted
      ),
      point_diff_shifted = lineout_ep_shifted - kick_ep
    )
  
  p <- ggplot(grid_shifted, aes(x = x, y = y, fill = point_diff_shifted)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradient2(
      low = "#457B9D",
      mid = "white",      
      high = "#E76F51",
      midpoint = 0,
      name = "Lineout - Kick"
    ) +
    coord_fixed() +
    labs(
      title = paste0("Point Differential (y shift = ", shift, ")"),
      x = "Lateral position (m)",
      y = "Distance from goal line (m)"
    ) +
    theme_minimal(base_size = 14)
  
  filename <- paste0("clean_plots/kick_vs_lineout_shift_", abs(shift), "m.png")
  ggsave(filename, p, width = 12, height = 10, dpi = 300)
  
  p
})

######################
### LINEOUT SHIFTS ###
######################

marker_x <- 19
marker_y <- 40

y_shifts <- seq(0, -30, by = -1)

max_lineout_ep <- max(grid$lineout_ep)   # EP at y = 5 (the min y)

shift_results <- lapply(y_shifts, function(shift) {
  
  grid_shifted <- grid %>%
    mutate(
      y_shifted = y + shift
    ) %>%
    left_join(
      grid %>%
        select(x, y, lineout_ep) %>%
        rename(y_shifted = y, lineout_ep_shifted = lineout_ep),
      by = c("x", "y_shifted")
    ) %>%
    mutate(
      lineout_ep_shifted = ifelse(is.na(lineout_ep_shifted),
                                  max_lineout_ep,
                                  lineout_ep_shifted)
    )
  
  marker_val <- grid_shifted %>%
    mutate(dist = sqrt((x - marker_x)^2 + (y - marker_y)^2)) %>%
    slice_min(dist, n = 1) %>%
    select(kick_ep, lineout_ep_shifted)
  
  tibble(
    y_shift = shift,
    kick_EP = marker_val$kick_ep,
    lineout_EP = marker_val$lineout_ep_shifted
  )
}) %>%
  bind_rows()

shift_results_long <- shift_results %>%
  tidyr::pivot_longer(
    cols = c(kick_EP, lineout_EP),
    names_to = "Option",
    values_to = "Expected_Points"
  ) %>%
  mutate(
    Option = recode(Option,
                    kick_EP = "Kick",
                    lineout_EP = "Lineout")
  )

shift_results_long <- shift_results_long %>%
  mutate(y_shift_plot = -y_shift)

shift_intercept <- -with(shift_results,
                   y_shifts[which.min(abs(kick_EP - lineout_EP))])

delta_plot <- ggplot(shift_results_long,
                     aes(x = y_shift_plot, y = Expected_Points, color = Option)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = shift_intercept, linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = shift_intercept + 1,
    y = max(shift_results_long$Expected_Points),
    label = sprintf("Break-even shift = %.1f m", shift_intercept),
    vjust = -0.5,
    hjust = 0
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = paste0("Expected Points vs Lineout Shift at (", marker_x, ", ", marker_y, ")"),
    x = "Shift in Y (metres gained from kick to touch)",
    y = "Expected Points",
    color = "Option"
  )

ggsave("clean_plots/delta_plot.png", delta_plot,
       width = 10, height = 8, dpi = 300)

##########################
### SITUATION ANALYSIS ###
##########################

# Scenario 1 - Home vs Away

y_shift <- -20

meter_seq <- seq(0, 100, by = 1)

expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 1 + coef_card * 0 + coef_win_per * 0

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

scenario_max_lineout_ep <- max(plot_data$expected_points)

grid_scenario <- grid %>%
  select(-lineout_ep) %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(lineout_ep = expected_points)

grid_scenario <- grid_scenario %>%
  mutate(
    lineout_ep_shifted = pmin(lineout_ep, max(lineout_ep)),
    lineout_ep_shifted = approx(y, lineout_ep, xout = y + y_shift, rule = 2)$y,
    
    point_diff = lineout_ep_shifted - kick_ep
  )

# Home team
home_points_diff <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/home_points_diff.png", home_points_diff,
       width = 10, height = 8, dpi = 300)

expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 0 + coef_card * 0 + coef_win_per * 0

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

scenario_max_lineout_ep <- max(plot_data$expected_points)

grid_scenario <- grid %>%
  select(-lineout_ep) %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(lineout_ep = expected_points)

grid_scenario <- grid_scenario %>%
  mutate(
    lineout_ep_shifted = pmin(lineout_ep, max(lineout_ep)),
    lineout_ep_shifted = approx(y, lineout_ep, xout = y + y_shift, rule = 2)$y,
    
    point_diff = lineout_ep_shifted - kick_ep
  )

# Away team
away_points_diff <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/away_points_diff.png", away_points_diff,
       width = 10, height = 8, dpi = 300)


# Scenario 2 - Yellow Cards

expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 0 + coef_card * 1 + coef_win_per * 0

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

scenario_max_lineout_ep <- max(plot_data$expected_points)

grid_scenario <- grid %>%
  select(-lineout_ep) %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(lineout_ep = expected_points)

grid_scenario <- grid_scenario %>%
  mutate(
    lineout_ep_shifted = pmin(lineout_ep, max(lineout_ep)),
    lineout_ep_shifted = approx(y, lineout_ep, xout = y + y_shift, rule = 2)$y,
    
    point_diff = lineout_ep_shifted - kick_ep
  )

# Opponent has yellow card
opponent_yellow <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/opponent_yellow.png", opponent_yellow,
       width = 10, height = 8, dpi = 300)


expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 0 + coef_card * 0 + coef_win_per * 0

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

scenario_max_lineout_ep <- max(plot_data$expected_points)

grid_scenario <- grid %>%
  select(-lineout_ep) %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(lineout_ep = expected_points)

grid_scenario <- grid_scenario %>%
  mutate(
    lineout_ep_shifted = pmin(lineout_ep, max(lineout_ep)),
    lineout_ep_shifted = approx(y, lineout_ep, xout = y + y_shift, rule = 2)$y,
    
    point_diff = lineout_ep_shifted - kick_ep
  )

# No difference in yellow cards
no_yellow <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/no_yellow.png", no_yellow,
       width = 10, height = 8, dpi = 300)



expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 0 + coef_card * -1 + coef_win_per * 0

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

scenario_max_lineout_ep <- max(plot_data$expected_points)

grid_scenario <- grid %>%
  select(-lineout_ep) %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(lineout_ep = expected_points)

grid_scenario <- grid_scenario %>%
  mutate(
    lineout_ep_shifted = pmin(lineout_ep, max(lineout_ep)),
    lineout_ep_shifted = approx(y, lineout_ep, xout = y + y_shift, rule = 2)$y,
    
    point_diff = lineout_ep_shifted - kick_ep
  )

# You have a yellow card
own_yellow <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/own_yellow.png", own_yellow,
       width = 10, height = 8, dpi = 300)


# Scenario 3 - Team Quality

expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 0 + coef_card * 0 + coef_win_per * -0.25

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

scenario_max_lineout_ep <- max(plot_data$expected_points)

grid_scenario <- grid %>%
  select(-lineout_ep) %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(lineout_ep = expected_points)

grid_scenario <- grid_scenario %>%
  mutate(
    lineout_ep_shifted = pmin(lineout_ep, max(lineout_ep)),
    lineout_ep_shifted = approx(y, lineout_ep, xout = y + y_shift, rule = 2)$y,
    
    point_diff = lineout_ep_shifted - kick_ep
  )

# Bad team
bad_team <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/bad_team.png", bad_team,
       width = 10, height = 8, dpi = 300)



expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 0 + coef_card * 0 + coef_win_per * 0.25

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

scenario_max_lineout_ep <- max(plot_data$expected_points)

grid_scenario <- grid %>%
  select(-lineout_ep) %>%
  left_join(plot_data, by = c("y" = "meter_line")) %>%
  rename(lineout_ep = expected_points)

grid_scenario <- grid_scenario %>%
  mutate(
    lineout_ep_shifted = pmin(lineout_ep, max(lineout_ep)),
    lineout_ep_shifted = approx(y, lineout_ep, xout = y + y_shift, rule = 2)$y,
    
    point_diff = lineout_ep_shifted - kick_ep
  )

# Good team
good_team <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#457B9D",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("clean_plots/good_team.png", good_team,
       width = 10, height = 8, dpi = 300)


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
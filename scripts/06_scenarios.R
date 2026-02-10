# run from project root; depends on 05_decisions.R
source("scripts/05_decisions.R")

##########################
### SITUATION ANALYSIS ###
##########################

# scenario 1: no card or win percentage difference

y_shift <- -20

# build baseline shifted lineout minus kick surface
grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# baseline scenario
baseline_points_diff <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    subtitle = "Baseline: Card_Diff = 0, WinPct_Diff = 0",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/baseline_points_diff.png", baseline_points_diff,
       width = 10, height = 8, dpi = 300)

# baseline scenario with marker

marker_x <- 20
marker_y <- 30

baseline_with_marker <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  geom_point(aes(x = marker_x, y = marker_y), shape = 4, color = "black", size = 4, stroke = 1.5)+
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
    midpoint = 0,
    name = "Lineout - Kick"
  ) +
  coord_fixed() +
  labs(
    title = paste("Point Difference (Lineout - Kick) with Y-shift =", abs(y_shift), "m"),
    subtitle = "Baseline: Card_Diff = 0, WinPct_Diff = 0",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/baseline_with_marker.png", baseline_with_marker,
       width = 10, height = 8, dpi = 300)

# inspect ep values at marker point
marker_values <- grid_scenario %>%
  filter(x == marker_x, y == marker_y) %>%
  select(point_diff, kick_ep, lineout_ep_shifted)

marker_values

# scenario 2: yellow cards

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 1, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 1, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# opponent yellow card
opponent_yellow <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
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

ggsave("plots/opponent_yellow.png", opponent_yellow,
       width = 10, height = 8, dpi = 300)


# reset to neutral card state
grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# no yellow card difference
no_yellow <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
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

ggsave("plots/no_yellow.png", no_yellow,
       width = 10, height = 8, dpi = 300)


# set own yellow card disadvantage

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = -1, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = -1, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# own yellow card
own_yellow <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
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

ggsave("plots/own_yellow.png", own_yellow,
       width = 10, height = 8, dpi = 300)


# scenario 3: team quality

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = -0.25),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = -0.25),
    point_diff = lineout_ep_shifted - kick_ep
  )

# lower-quality team
bad_team <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
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

ggsave("plots/bad_team.png", bad_team,
       width = 10, height = 8, dpi = 300)


# set higher win percentage differential

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0.25),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0.25),
    point_diff = lineout_ep_shifted - kick_ep
  )

# higher-quality team
good_team <- ggplot(grid_scenario, aes(x = x, y = y, fill = point_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
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

ggsave("plots/good_team.png", good_team,
       width = 10, height = 8, dpi = 300)

####################################
### SOUTH AFRICA VS. NEW ZEALAND ###
####################################

sep_game_data_csv = read_csv("data/All Blacks vs South Africa Game Sep 16th.csv")

# align game coordinates with model grid
sep_game_data_csv <- sep_game_data_csv %>%
  rename(
    x = `x location`,
    y = `y location`
  ) %>%
  mutate(
    x = x - 35,
    y = y
  )

# rows with Card_Diff = 0 and WinPct_Diff = 0

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

data_with_ep <- sep_game_data_csv %>%
  # join lineout ep on shifted y
  left_join(
    grid_scenario %>% select(x, y, lineout_ep),
    by = c("x" = "x", "distance_from_try_after_shift" = "y")
  ) %>%
  # join kick ep on original y
  left_join(
    grid_scenario %>% select(x, y, kick_ep),
    by = c("x" = "x", "y" = "y")
  ) %>%
  mutate(
    # lineout minus kick
    point_diff = lineout_ep - kick_ep
  )

table_ep <- data_with_ep %>%
  mutate(
    # optimal decision: lineout if point_diff > 0, else kick
    optimal_decision = ifelse(point_diff > 0, "lineout", "kick")
  ) %>%
  select(
    Team,             # team in possession
    lineout_ep,       # expected points for lineout
    kick_ep,          # expected points for kick
    Decision,         # actual decision
    optimal_decision  # ep-optimal decision
  ) %>%
  mutate(
    # ep for optimal and actual decisions
    EP_optimal = ifelse(optimal_decision == "lineout", lineout_ep, kick_ep),
    EP_actual  = ifelse(Decision == "lineout", lineout_ep, kick_ep),

    # absolute difference: optimal vs actual
    ep_diff = abs(EP_optimal - EP_actual)
  )

table_ep <- table_ep %>%
  mutate(
    Team = recode(Team, "AB" = "NZ")
  )

# summarize decision efficiency
table_ep

summary_metrics <- table_ep %>%
  summarise(
    total_delta_ep = sum(ep_diff),
    prop_optimal = mean(Decision == optimal_decision)
  )

print(summary_metrics)


##################
### REFERENCES ###
##################

# article links
# https://www.data-ruck.com/blog/predicting-kicks-outcome/
# https://journals.sagepub.com/doi/10.1177/22150218251365220

# data download link
# https://zenodo.org/api/records/13851563/files-archive

# goal kicking data
# https://www.sciencedirect.com/science/article/pii/S1440244014000255
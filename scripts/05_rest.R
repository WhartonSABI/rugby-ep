# Run from project root. Depends on 02_ep-lineout.R and 04_ep-kick.R.
source("scripts/02_ep-lineout.R")
source("scripts/04_ep-kick.R")

#########################
### MAKING A DECISION ###
#########################

# Plotting Difference in Expected Points Across Pitch

plot_data <- plot_data %>%
  rename(lineout_ep = expected_points)

lineout_ep_at_y <- function(y_new, card_diff = 0, win_pct_diff = 0) {
  smoother <- build_lineout_smoother(card_diff = card_diff, win_pct_diff = win_pct_diff)
  y_clipped <- pmin(pmax(y_new, min(meter_lines)), max(meter_lines))
  as.numeric(smoother(y_clipped))
}

grid <- grid %>%
  rename(kick_ep = expected_points) %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0),
    point_diff = lineout_ep - kick_ep
  )

no_shift_plot <- ggplot(grid, aes(x = x, y = y, fill = point_diff)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_gradient2(
    low = "#E76F51",
    mid = "white",
    high = "#457B9D",
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

ggsave("plots/no_shift_plot.png", no_shift_plot,
       width = 10, height = 8, dpi = 300)

# Shifting location of lineout forward

max_lineout_ep <- max(grid$lineout_ep)

y_shifts <- c(0, -5, -10, -15, -20, -25)

for (shift in y_shifts) {
  grid_shifted <- grid %>%
    mutate(y_shifted = y + shift) %>%
    left_join(
      grid %>%
        select(x, y, lineout_ep) %>%
        rename(y_shifted = y, lineout_ep_shifted = lineout_ep),
      by = c("x", "y_shifted")
    ) %>%
    mutate(
      lineout_ep_shifted = ifelse(is.na(lineout_ep_shifted),
                                  max_lineout_ep,
                                  lineout_ep_shifted),
      point_diff = lineout_ep_shifted - kick_ep
    )

  p <- ggplot(grid_shifted, aes(x = x, y = y, fill = point_diff)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradient2(
      low = "#E76F51",
      mid = "white",
      high = "#457B9D",
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

  ggsave(paste0("plots/kick_vs_lineout_shift_", abs(shift), "m.png"),
         p, width = 10, height = 8, dpi = 300)
}

######################
### LINEOUT SHIFTS ###
######################

marker_x <- 20
marker_y <- 30

y_shifts <- seq(0, -30, by = -1)

shift_results <- lapply(y_shifts, function(shift) {
  grid_shifted <- grid %>%
    mutate(y_shifted = y + shift) %>%
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
  geom_line(linewidth = 1.2) +
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

ggsave("plots/delta_plot.png", delta_plot,
       width = 10, height = 8, dpi = 300)

##########################
### SITUATION ANALYSIS ###
##########################

# Scenario 1 - No Card or Win Percentage Difference

y_shift <- -20

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# Baseline scenario
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

# Baseline scenario with marker

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

marker_values <- grid_scenario %>%
  filter(x == marker_x, y == marker_y) %>%
  select(point_diff, kick_ep, lineout_ep_shifted)

marker_values

# Scenario 2 - Yellow Cards

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 1, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 1, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# Opponent has yellow card
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


grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# No difference in yellow cards
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



grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = -1, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = -1, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

# You have a yellow card
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


# Scenario 3 - Team Quality

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = -0.25),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = -0.25),
    point_diff = lineout_ep_shifted - kick_ep
  )

# Bad team
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



grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0.25),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0.25),
    point_diff = lineout_ep_shifted - kick_ep
  )

# Good team
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

#####################################
### South Africa v.s. New Zealand ###
#####################################

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

# Rows without cards (Card_Diff = 0, WinPct_Diff = 0)

grid_scenario <- grid %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0),
    lineout_ep_shifted = lineout_ep_at_y(y + y_shift, card_diff = 0, win_pct_diff = 0),
    point_diff = lineout_ep_shifted - kick_ep
  )

data_with_ep <- sep_game_data_csv %>%
  # Join lineout EP based on shifted y
  left_join(
    grid_scenario %>% select(x, y, lineout_ep),
    by = c("x" = "x", "distance_from_try_after_shift" = "y")
  ) %>%
  # Join kick EP based on original y
  left_join(
    grid_scenario %>% select(x, y, kick_ep),
    by = c("x" = "x", "y" = "y")
  ) %>%
  mutate(
    # difference between lineout and kick, including card differential effect
    point_diff = lineout_ep - kick_ep
  )

table_ep <- data_with_ep %>%
  mutate(
    # Determine optimal decision: lineout if point_diff > 0, else kick
    optimal_decision = ifelse(point_diff > 0, "lineout", "kick")
  ) %>%
  select(
    Team,             # team in possession
    lineout_ep,       # expected points if choosing lineout
    kick_ep,          # expected points if choosing kick
    Decision,         # actual decision taken
    optimal_decision  # optimal decision based on EP
  ) %>%
  mutate(
    # EP for optimal and actual decision
    EP_optimal = ifelse(optimal_decision == "lineout", lineout_ep, kick_ep),
    EP_actual  = ifelse(Decision == "lineout", lineout_ep, kick_ep),

    # Difference: optimal - actual
    ep_diff = abs(EP_optimal - EP_actual)
  )

table_ep <- table_ep %>%
  mutate(
    Team = recode(Team, "AB" = "NZ")
  )

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

## Links to Article
# https://www.data-ruck.com/blog/predicting-kicks-outcome/
# https://journals.sagepub.com/doi/10.1177/22150218251365220

## Data Download Link
# https://zenodo.org/api/records/13851563/files-archive

# Goal Kicking Data
# https://www.sciencedirect.com/science/article/pii/S1440244014000255

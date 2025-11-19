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
    Location %in% zones,
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

last_play <- last_play %>%
  group_by(Team) %>%
  arrange(Round) %>%
  mutate(
    Games_Played = lag(row_number(), default = 0),
    Wins_Before = lag(cumsum(Win), default = 0),
    WinPct_Before = if_else(Games_Played == 0, 0.5, Wins_Before / Games_Played)
  ) %>%
  ungroup()

phase_data <- phase_data %>%
  mutate(
    Team_for_join = case_when(
      Team_In_Poss == "Home" ~ Home,
      Team_In_Poss == "Away" ~ Away
    )
  )

phase_data <- phase_data %>%
  left_join(
    last_play %>% select(Team, Round, WinPct_Before),
    by = c("Team_for_join" = "Team", "Round" = "Round")
  )

# Home and away column

phase_data <- phase_data %>%
  mutate(
    Home_Attack = if_else(Team_In_Poss == "Home", 1, 0)
  )

# Win percent differential

phase_data <- phase_data %>%
  mutate(
    Opponent = if_else(Team_In_Poss == "Home", Away, Home)
  ) %>%
    left_join(
      team_games %>% select(Team, Round, WinPct_Before) %>%
        rename(Opponent = Team,
               Opponent_WinPct = WinPct_Before),
      by = c("Opponent", "Round")
    )

phase_data <- phase_data %>%
  mutate(
    WinPct_Diff = WinPct_Before - Opponent_WinPct
  )

# Card differential

phase_data <- phase_data %>%
  mutate(
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

# Binary if lineout part of consecutive plays with same outcome without change
# in possession

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

# Sampling one random observation from consecutive lineouts

sampled_phase_data <- phase_data %>%
  arrange(Round, Home, Away, ID) %>%
  group_by(Round, Home, Away, run_id) %>%
  slice_sample(n = 1) %>% 
  ungroup()

# Plotting Marginals

# Meter line
ggplot(sampled_phase_data, aes(x = meter_line, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs Meter Line (Marginal Relationship)",
    x = "Meter Line",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

# Seconds remaining in half
ggplot(sampled_phase_data, aes(x = Seconds_Remaining_Half, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs Seconds Remaining in Half (Marginal Relationship)",
    x = "Seconds_Remaining_Half",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

# Home lineout possession
ggplot(sampled_phase_data, aes(x = factor(Home_Attack), y = points)) +
  geom_boxplot(fill = "grey80") +
  labs(
    title = "Points by Home Attack",
    x = "Home Attack (0 = Away Attack, 1 = Home Attack)",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

# Win Percent Differential
ggplot(sampled_phase_data, aes(x = WinPct_Diff, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs WinPct_Diff (Marginal Relationship)",
    x = "WinPct_Diff",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

# Card Differential
ggplot(sampled_phase_data, aes(x = factor(Card_Diff), y = points)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Points by Card Differential",
    x = "Card Differential (Own - Opponent)",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)


regression <- lm(points ~ meter_line + Home_Attack + Card_Diff,
                 data = sampled_phase_data)

summary(regression)


# Plotting EP of Lineout

# Regression coefficients
intercept <- 2.838467
coef_meter <- -0.060002
coef_home <- 0.925027
coef_card <- 0.979793

meter_seq <- seq(0, 100, by = 1)

expected_points <- intercept + coef_meter * meter_seq + 
  coef_home * 1 + coef_card * 0

plot_data <- data.frame(
  meter_line = meter_seq,
  expected_points = expected_points
)

ggplot(plot_data, aes(x = meter_line, y = expected_points)) +
  geom_line(size = 1.2, color = "blue") +
  labs(
    title = "Expected Points by Meter Line",
    subtitle = "Assuming Home_Attack = 1 and Card_Diff = 0",
    x = "Meter Line",
    y = "Expected Points"
  ) +
  theme_minimal(base_size = 14)

############################
### Kick Expected Points ###
############################

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

# Plotting kicking heatmap

thresholds <- c(0.8, 0.6, 0.4, 0.2)

ggplot(grid, aes(x = x, y = y, fill = expected_points)) +
  geom_tile() +
  geom_contour(aes(z = expected_points),
               breaks = thresholds,
               color = "white",
               linewidth = 0.8,
               linetype = "dashed") +
  scale_fill_viridis_c(option = "magma", name = "Expected Prob") +
  coord_fixed() +
  labs(
    title = "Expected Prooints of Kick",
    x = "Lateral position (m)",
    y = "Distance from goal line (m)"
  ) +
  theme_minimal()



#############
### SETUP ###
#############

# load libraries

# install.packages("patchwork")
library(patchwork)
# install.packages("stringr")
library(stringr)
# install.packages("tidyverse")
library(tidyverse)

# set seed
set.seed(11-19-2025)

#######################
### PHASE DATA LOAD ###
#######################

# run from project root
phase_data = read_csv("data/phase_2018-19.csv")
# preview data
head(phase_data)

#########################
### LINEOUT DATA PREP ###
#########################

phase_data <- phase_data %>%
  mutate(
    # extract signed points
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # replace missing points with 0
    points = ifelse(is.na(points), 0, points)
  )

# final score by match
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

# keep first-phase lineouts
phase_data <- phase_data %>%
  filter(
    Play_Start == "Lineout",
    Phase == 1
  )

# build win percentage

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

# running win percentage
last_play <- last_play %>%
  group_by(Team) %>%
  arrange(Round) %>%
  mutate(
    Games_Played = lag(row_number(), default = 0),
    Wins_Before = lag(cumsum(Win), default = 0),
    WinPct_Before = if_else(Games_Played == 0, 0.5, Wins_Before / Games_Played)
  ) %>%
  ungroup()

# team in possession
phase_data <- phase_data %>%
  mutate(
    Team_for_join = case_when(
      Team_In_Poss == "Home" ~ Home,
      Team_In_Poss == "Away" ~ Away
    )
  ) %>%
# join team win percentage
  left_join(
    last_play %>% select(Team, Round, WinPct_Before),
    by = c("Team_for_join" = "Team", "Round" = "Round")
  )

# flag home attack
phase_data <- phase_data %>%
  mutate(
    Home_Attack = if_else(Team_In_Poss == "Home", 1, 0)
  )

# opponent data
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
    # win percentage differential
    WinPct_Diff = WinPct_Before - Opponent_WinPct,
    # card differential
    Card_Diff = (Yellow_Cards_Opp + Red_Cards_Opp) - (Yellow_Cards_Own + Red_Cards_Own)
  )



# seconds remaining in half
phase_data <- phase_data %>%
  mutate(
    Seconds_Remaining_Half = if_else(
      Seconds_Remaining > 2400,             # first half
      Seconds_Remaining - 2400,             # first-half seconds remaining
      Seconds_Remaining                     # second half
    )
  )

# flag under 2 minutes

phase_data <- phase_data %>%
  mutate(
    Less_Than_2_Min = if_else(Seconds_Remaining_Half < 120, 1, 0)
  )

# flag repeated outcomes in a possession run
phase_data <- phase_data %>%
  arrange(Round, Home, Away, ID) %>%
  group_by(Round, Home, Away) %>%
  mutate(
    run_id = cumsum(c(TRUE, diff(Points_Difference) != 0 | Outcome[-1] != Outcome[-n()])),
    n_same = ave(run_id, run_id, FUN = length), # run size
  ) %>%
  ungroup()

# map location to meter line
location_names <- c("5m-Goal (opp)", "22m-5m (opp)", "10m-22m (opp)",
                    "Half-10m (opp)","10m-Half (own)", "22m-10m (own)",
                    "5m-22m (own)", "Goal-5m (own)")
location_meters <- c(2.5, 13.5, 31, 45, 55, 69, 86.5, 97.5)

# zone boundaries in meters from opponent try line
zone_boundaries_m <- c(0, 5, 22, 40, 50, 60, 78, 95, 100)

lookup <- setNames(location_meters, location_names)

phase_data$meter_line <- lookup[phase_data$Location]

# draw one sample per possession run
sampled_phase_data <- phase_data %>%
  arrange(Round, Home, Away, ID) %>%
  group_by(Round, Home, Away, run_id) %>%
  slice_sample(n = 1) %>% 
  ungroup()

#############
### PLOTS ###
#############

# meter line
meter_line_marginal <- ggplot(sampled_phase_data, aes(x = meter_line, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs Meter Line (Marginal Relationship)",
    x = "Meters from Try Line",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/meter_line_marginal.png", meter_line_marginal,
       width = 10, height = 8, dpi = 300)

# seconds remaining in half
seconds_remaining_marginal <- ggplot(sampled_phase_data, aes(x = Seconds_Remaining_Half, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs Seconds Remaining in Half (Marginal Relationship)",
    x = "Seconds_Remaining_Half",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/seconds_remaining_marginal.png", seconds_remaining_marginal,
       width = 10, height = 8, dpi = 300)

# win percentage differential
win_per_marginal <- ggplot(sampled_phase_data, aes(x = WinPct_Diff, y = points)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Points vs WinPct_Diff (Marginal Relationship)",
    x = "WinPct_Diff",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/win_per_marginal.png", win_per_marginal,
       width = 10, height = 8, dpi = 300)

# card differential
card_dif_marginal <- ggplot(sampled_phase_data, aes(x = factor(Card_Diff), y = points)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Points by Card Differential",
    x = "Card Differential (Opponent - Own)",
    y = "Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/card_dif_marginal.png", card_dif_marginal,
       width = 10, height = 8, dpi = 300)

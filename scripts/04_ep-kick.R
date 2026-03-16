source("scripts/01_data-prep.R")

# run from project root
library(mgcv)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

# project-wide reproducibility seed (override via EP_SEED)
ep_seed <- suppressWarnings(as.integer(Sys.getenv("EP_SEED", "20260313")))
if (is.na(ep_seed)) {
  warning("Invalid EP_SEED; defaulting to 20260313")
  ep_seed <- 20260313L
}
set.seed(ep_seed)

add_kick_features <- function(df) {
  df %>%
    mutate(
      angle = atan2(abs(x - 35), y) * (180 / pi),
      Distance = sqrt((x - 35)^2 + y^2)
    )
}

location_center_lookup <- c(
  "5m-Goal (opp)" = 5,
  "22m-5m (opp)" = 13.5,
  "10m-22m (opp)" = 31.0,
  "Half-10m (opp)" = 45.0,
  "10m-Half (own)" = 55.0,
  "22m-10m (own)" = 69.0,
  "5m-22m (own)" = 86.5,
  "Goal-5m (own)" = 97.5
)

# Map y (meters from opposition try line) to phase-data location zones.
zone_from_y <- function(y) {
  zone_levels <- c(
    "5m-Goal (opp)",
    "22m-5m (opp)",
    "10m-22m (opp)",
    "Half-10m (opp)",
    "10m-Half (own)",
    "22m-10m (own)",
    "5m-22m (own)",
    "Goal-5m (own)"
  )
  as.character(cut(
    y,
    breaks = c(0, 5, 22, 40, 50, 60, 78, 95, Inf),
    labels = zone_levels,
    include.lowest = TRUE,
    right = TRUE
  ))
}

build_miss_lookup <- function(restart_df) {
  by_zone <- restart_df %>%
    group_by(Location) %>%
    summarise(
      n = n(),
      avg_ep = mean(points),
      .groups = "drop"
    ) %>%
    mutate(
      center_y = unname(location_center_lookup[Location])
    ) %>%
    arrange(center_y)

  miss_spline <- NULL
  if (nrow(by_zone) >= 4 && all(!is.na(by_zone$center_y))) {
    miss_spline <- try(
      stats::smooth.spline(
        x = by_zone$center_y,
        y = by_zone$avg_ep,
        w = pmax(by_zone$n, 1),
        spar = 0.6
      ),
      silent = TRUE
    )
    if (inherits(miss_spline, "try-error")) {
      miss_spline <- NULL
    }
  }

  list(
    by_zone = by_zone,
    lookup = setNames(by_zone$avg_ep, by_zone$Location),
    overall = mean(restart_df$points),
    spline = miss_spline,
    y_min = min(by_zone$center_y, na.rm = TRUE),
    y_max = max(by_zone$center_y, na.rm = TRUE)
  )
}

miss_ep_from_y <- function(y, miss_lookup, method = c("smooth", "step")) {
  method <- match.arg(method)

  if (method == "smooth" && !is.null(miss_lookup$spline)) {
    y_clip <- pmin(pmax(y, miss_lookup$y_min), miss_lookup$y_max)
    return(as.numeric(predict(miss_lookup$spline, x = y_clip)$y))
  }

  zones <- zone_from_y(y)
  zone_vals <- unname(miss_lookup$lookup[zones])
  zone_vals[is.na(zone_vals)] <- miss_lookup$overall
  as.numeric(zone_vals)
}

predict_kick_ep <- function(kick_model, newdata, miss_lookup, miss_method = "smooth") {
  nd <- newdata
  if (!("angle" %in% names(nd)) || !("Distance" %in% names(nd))) {
    nd <- add_kick_features(nd)
  }

  p_make <- as.numeric(predict(kick_model, newdata = nd, type = "response"))
  ep_miss <- miss_ep_from_y(nd$y, miss_lookup, method = miss_method)
  3 * p_make + (1 - p_make) * ep_miss
}

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

kick_data_small <- add_kick_features(kick_data_small)

# Empirical 5m x 5m make-rate surface
kick_empirical <- kick_data_small %>%
  mutate(
    x_bin = pmin(67.5, floor(x / 5) * 5 + 2.5),
    y_bin = pmin(97.5, floor(y / 5) * 5 + 2.5)
  ) %>%
  group_by(x_bin, y_bin) %>%
  summarise(
    n = n(),
    make_rate = mean(make),
    .groups = "drop"
  )

kick_empirical_heatmap <- ggplot(kick_empirical, aes(x = x_bin, y = y_bin, fill = make_rate)) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(0, 1),
    name = "Empirical\nmake rate"
  ) +
  coord_fixed() +
  labs(
    title = "Empirical penalty make rate (5m x 5m grid)",
    x = "Lateral position (m)",
    y = "Distance from opposition try line (m)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "plots/kick_empirical_heatmap.png",
  kick_empirical_heatmap,
  width = 10,
  height = 8,
  dpi = 300
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
  y = seq(5, 65, length.out = 200)
)

# Compute angle and distance to posts (x=35, y=0)
grid <- add_kick_features(grid)

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

ggsave("plots/kick_prob_plot.png", kick_prob_plot,
       width = 10, height = 8, dpi = 300)

# Showing confidence interval

# Cross section: straight in front of posts (x = 35), vary distance
cross_section <- data.frame(
  y = seq(5, 65, length.out = 200),
  x = 35
) %>%
  add_kick_features()

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
cross_section_plot <- ggplot(cross_section, aes(x = y)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = prob), color = "steelblue", linewidth = 1) +
  labs(
    title = "Kick Success Probability - Straight in Front (x = 35)",
    x = "Distance from goal line (m)",
    y = "Probability of success"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

ggsave("plots/cross_section_plot.png", cross_section_plot,
       width = 10, height = 8, dpi = 300)

# Cross section: fix y = 20, vary x from 0 to 70

cross_section_angle <- data.frame(
  x = seq(0, 70, length.out = 200),
  y = 20
) %>%
  add_kick_features()

# Predict with standard errors (on log-odds scale, then transform)
pred_angle <- predict(model, newdata = cross_section_angle, type = "link", se.fit = TRUE)

cross_section_angle <- cross_section_angle %>%
  mutate(
    fit_logit = pred_angle$fit,
    se = pred_angle$se.fit,
    prob = plogis(fit_logit),
    lower = plogis(fit_logit - 1.96 * se),
    upper = plogis(fit_logit + 1.96 * se)
  )

# Plot
cross_section_angle_plot <- ggplot(cross_section_angle, aes(x = x)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = prob), color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = 35, linetype = "dashed", color = "gray50") +
  annotate("text", x = 36, y = 0.1, label = "centre of posts", 
           hjust = 0, color = "gray50", size = 3) +
  labs(
    title = "Kick Success Probability - Fixed Distance (y = 20m)",
    x = "Distance from left touchline (m)",
    y = "Probability of success"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

cross_section_angle_plot

ggsave("plots/cross_section_angle_plot.png", cross_section_angle_plot,
       width = 10, height = 8, dpi = 300)

#################################
### Expected Points of a Miss ###
#################################

phase_data_restarts = read_csv("data/phase_2018-19.csv")

phase_data_restarts <- phase_data_restarts %>%
  mutate(
    # extract signed points
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    
    # replace missing points with 0
    points = ifelse(is.na(points), 0, points)
  )

# Filtering out restarts with point changes (scores) and restarts if halfs
phase_data_restarts <- phase_data_restarts %>%
  group_by(Round, Home, Away) %>%
  filter(Phase == 1) %>%
  mutate(
    Points_Diff_Change = abs(Points_Difference) - abs(lag(Points_Difference)),
    is_first_row = row_number() == 1,
    is_second_half_start = Seconds_Remaining < 2400 & lag(Seconds_Remaining) >= 2400
  ) %>%
  filter(
    Play_Start == "Restart Kick",
    is.na(Points_Diff_Change) | Points_Diff_Change == 0,
    !is_first_row,
    !is.na(is_second_half_start) & !is_second_half_start
  ) %>%
  ungroup()

unique(phase_data_restarts$Location)

miss_lookup <- build_miss_lookup(phase_data_restarts)

ep_by_zone <- miss_lookup$by_zone %>%
  transmute(
    Location,
    n,
    avg_ep = round(avg_ep, 2)
  )

# add overall average row
overall <- tibble(
  Location = "Overall Average",
  n = nrow(phase_data_restarts),
  avg_ep = round(miss_lookup$overall, 2)
)

ep_table <- bind_rows(ep_by_zone, overall)

print(ep_table)

# Continuation-aware EP surface for kicks.
grid$expected_points <- predict_kick_ep(model, grid, miss_lookup)

run_miss_component_bootstrap <- tolower(Sys.getenv("EP_RUN_MISS_COMPONENT_BOOTSTRAP", "false")) %in%
  c("1", "true", "yes")

if (run_miss_component_bootstrap) {
  # Data set to bootstrap
  kick_miss_ep <- phase_data_restarts %>%
    select(points, ID)

  # number of bootstrap replicates
  B <- 2000L

  # one bootstrap replicate
  one_boot_ep <- function(b) {
    idx <- sample.int(nrow(kick_miss_ep), size = nrow(kick_miss_ep), replace = TRUE)
    mean(kick_miss_ep$points[idx])
  }

  # run bootstrap
  boot_ep <- sapply(seq_len(B), one_boot_ep)

  # 95% percentile CI
  ep_ci <- quantile(boot_ep, probs = c(0.025, 0.975))

  cat("Mean EP:", mean(boot_ep), "\n")
  cat("95% CI: [", ep_ci[1], ",", ep_ci[2], "]\n")

  boot_df <- data.frame(ep = boot_ep)

  # Optional diagnostic plot for continuation-only uncertainty.
  missed_bootstrap <- ggplot(boot_df, aes(x = ep)) +
    geom_histogram(fill = "steelblue", color = "white", bins = 40) +
    geom_vline(xintercept = mean(boot_ep), color = "steelblue",
               linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = ep_ci[1], color = "red",
               linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = ep_ci[2], color = "red",
               linewidth = 1, linetype = "dashed") +
    annotate("text", x = mean(boot_ep), y = Inf,
             label = sprintf("Mean = %.2f", mean(boot_ep)),
             hjust = -0.1, vjust = 2, color = "steelblue") +
    annotate("text", x = ep_ci[1], y = Inf,
             label = sprintf("2.5%% = %.2f", ep_ci[1]),
             hjust = 1.1, vjust = 2, color = "red") +
    annotate("text", x = ep_ci[2], y = Inf,
             label = sprintf("97.5%% = %.2f", ep_ci[2]),
             hjust = -0.1, vjust = 2, color = "red") +
    labs(
      title = "Bootstrap Distribution of Mean Expected Points",
      x = "Mean Expected Points",
      y = "Count"
    ) +
    theme_minimal()

  missed_bootstrap

  ggsave("plots/missed_bootstrap.png", missed_bootstrap,
         width = 10, height = 8, dpi = 300)
} else {
  message("Skipping continuation-only bootstrap. Set EP_RUN_MISS_COMPONENT_BOOTSTRAP=true to enable it.")
}

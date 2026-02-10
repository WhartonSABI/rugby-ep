# run from project root; depends on 01_data-prep.R
source("scripts/01_data-prep.R")

##############################
### CATEGORICAL REGRESSION ###
##############################

# format variables
sampled_phase_data <- sampled_phase_data %>%
  mutate(
    points_factor = factor(points),
    meter_line_factor = factor(meter_line)
  )

# multinomial regression
multinomial_model <- nnet::multinom(
  points_factor ~ meter_line_factor + Card_Diff + WinPct_Diff,
  data = sampled_phase_data,
  trace = FALSE
)

summary(multinomial_model)

# prediction grid by meter line
meter_lines <- sort(unique(sampled_phase_data$meter_line))
pred_data <- tibble(
  meter_line = meter_lines,
  meter_line_factor = factor(meter_line, levels = levels(sampled_phase_data$meter_line_factor)),
  Card_Diff = mean(sampled_phase_data$Card_Diff, na.rm = TRUE),
  WinPct_Diff = mean(sampled_phase_data$WinPct_Diff, na.rm = TRUE)
)

point_levels <- levels(sampled_phase_data$points_factor)
point_values <- as.numeric(as.character(point_levels))

# helper: return class probabilities across meter lines
get_probs_by_meter <- function(card_diff = 0, win_pct_diff = 0) {
  pred <- tibble(
    meter_line = meter_lines,
    meter_line_factor = factor(meter_line, levels = levels(sampled_phase_data$meter_line_factor)),
    Card_Diff = card_diff,
    WinPct_Diff = win_pct_diff
  )
  pr <- predict(multinomial_model, newdata = pred, type = "probs")
  if (is.vector(pr)) pr <- matrix(pr, nrow = 1)
  pr
}

# helper: return expected points from predicted probabilities
predict_lineout_ep <- function(meter_line, card_diff = 0, win_pct_diff = 0, model = multinomial_model) {
  new_data <- tibble(
    meter_line = meter_line,
    meter_line_factor = factor(meter_line, levels = levels(sampled_phase_data$meter_line_factor)),
    Card_Diff = card_diff,
    WinPct_Diff = win_pct_diff
  )

  probs <- predict(model, newdata = new_data, type = "probs")
  if (is.vector(probs)) {
    probs <- matrix(probs, nrow = 1)
  }

  as.vector(probs %*% point_values)
}

# predicted probabilities
probs_multi <- predict(multinomial_model, newdata = pred_data, type = "probs")

# convert to matrix if needed
if (is.vector(probs_multi)) {
  probs_multi <- matrix(probs_multi, nrow = 1)
}

# expected points
expected_points_multi <- probs_multi %*% point_values

results_multi <- data.frame(
  meter_line = pred_data$meter_line,
  expected_points = as.vector(expected_points_multi),
  probs_multi
)

print("Expected Points by Meter Line (point estimate):")
print(results_multi)

# baseline ep at zone centers
# zone centers for interpolation
plot_data <- tibble(
  meter_line = meter_lines,
  expected_points = predict_lineout_ep(meter_lines, card_diff = 0, win_pct_diff = 0)
)

build_lineout_smoother <- function(card_diff = 0, win_pct_diff = 0) {
# helper: monotone smoother over zone-level ep
  ep_zone <- predict_lineout_ep(
    meter_lines,
    card_diff = card_diff,
    win_pct_diff = win_pct_diff,
    model = multinomial_model
  )

  # non-increasing in meter line
  iso_fit <- isoreg(meter_lines, -ep_zone)
  splinefun(iso_fit$x, -iso_fit$yf, method = "monoH.FC")
}

# interpolated ep by meter line
ep_by_meter_line_data <- plot_data %>%
  mutate(expected_points = build_lineout_smoother()(meter_line))

ep_by_meter_line <- ggplot(ep_by_meter_line_data, aes(x = meter_line, y = expected_points)) +
  geom_line(linewidth = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  labs(
    title = "Expected Points by Field Zone",
    subtitle = "Interpolated lineout EP. Card_Diff = 0, WinPct_Diff = 0",
    x = "Distance from try line (m)",
    y = "Expected Points"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/ep_by_meter_line.png", ep_by_meter_line,
       width = 10, height = 8, dpi = 300)

# run from project root; depends on 02_ep-lineout.R and 04_ep-kick.R
source("scripts/02_ep-lineout.R")
source("scripts/04_ep-kick.R")

#########################
### MAKING A DECISION ###
#########################

# expected point difference across pitch

plot_data <- plot_data %>%
  rename(lineout_ep = expected_points)

# lineout ep lookup with clipping to observed meter range
# smoother is built once per (card_diff, win_pct_diff) pair and cached to avoid
# re-fitting inside mutate(), which caused a subscript-out-of-bounds error in
# splinefun when called row-wise on a large grid
lineout_ep_smoother_cache <- list()

lineout_ep_at_y <- function(y_new, card_diff = 0, win_pct_diff = 0, less_than_2_min = 0) {
  cache_key <- paste(card_diff, win_pct_diff, less_than_2_min, sep = "_")
  if (is.null(lineout_ep_smoother_cache[[cache_key]])) {
    lineout_ep_smoother_cache[[cache_key]] <<- build_lineout_smoother(
      card_diff = card_diff,
      win_pct_diff = win_pct_diff,
      less_than_2_min = less_than_2_min
    )
  }
  smoother <- lineout_ep_smoother_cache[[cache_key]]
  y_clipped <- pmin(pmax(y_new, min(meter_lines)), max(meter_lines))
  as.numeric(smoother(y_clipped))
}

grid <- grid %>%
  rename(kick_ep = expected_points) %>%
  mutate(
    lineout_ep = lineout_ep_at_y(y, card_diff = 0, win_pct_diff = 0, less_than_2_min = 0),
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

# evaluate fixed y-shift scenarios
# shift lineout location forward

max_lineout_ep <- max(grid$lineout_ep)

y_shifts <- c(0, -5, -10, -15, -20, -25)

# generate point-difference heatmaps for each shift
for (shift in y_shifts) {
  grid_shifted <- grid %>%
    mutate(
      lineout_ep_shifted = lineout_ep_at_y(y + shift, card_diff = 0, win_pct_diff = 0, less_than_2_min = 0),
      kick_ep = predict_kick_ep(model, pick(everything()), miss_lookup),
      point_diff = lineout_ep_shifted - kick_ep
    )
  
  p <- ggplot(grid_shifted, aes(x = x, y = y, fill = point_diff)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradient2(
      low = "#E76F51", mid = "white", high = "#457B9D",
      midpoint = 0, name = "Lineout - Kick"
    ) +
    coord_fixed() +
    labs(
      title = paste0("Point Differential (y shift = ", shift, ")"),
      x = "Lateral position (m)",
      y = "Distance from goal line (m)"
    ) +
    theme_minimal(base_size = 14)

  p
  
  if (shift != 0) {
    p <- p + theme(legend.position = "none")
  }
  
  ggsave(paste0("plots/kick_vs_lineout_shift_", abs(shift), "m.png"),
         plot = p, width = 10, height = 8, dpi = 300)
}

######################
### LINEOUT SHIFTS ###
######################

marker_x <- 20
marker_y <- 30

y_shifts <- seq(0, -30, by = -1)

# get kick_ep at marker location
marker_kick_ep <- predict_kick_ep(
  model,
  data.frame(x = marker_x, y = marker_y),
  miss_lookup
)

shift_results <- lapply(y_shifts, function(shift) {
  tibble(
    y_shift = shift,
    kick_EP = marker_kick_ep,  # fixed — kick is always from marker location
    lineout_EP = pmin(
      lineout_ep_at_y(marker_y + shift, card_diff = 0, win_pct_diff = 0, less_than_2_min = 0),
      max_lineout_ep
    )
  )
}) %>%
  bind_rows()

# rest of code unchanged
shift_results_long <- shift_results %>%
  tidyr::pivot_longer(
    cols = c(kick_EP, lineout_EP),
    names_to = "Option",
    values_to = "Expected_Points"
  ) %>%
  mutate(Option = recode(Option, kick_EP = "Kick", lineout_EP = "Lineout"))

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

ggsave("plots/delta_plot.png", delta_plot, width = 10, height = 8, dpi = 300)

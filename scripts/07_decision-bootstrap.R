#!/usr/bin/env Rscript

# run from project root; depends on 05_decisions.R
source("scripts/05_decisions.R")

library(dplyr)
library(tidyr)
library(ggplot2)

###############################
### JOINT DECISION BOOTSTRAP ###
###############################

boot_B <- as.integer(Sys.getenv("EP_DECISION_BOOT_B", unset = "1000"))
n_cores <- as.integer(Sys.getenv("EP_DECISION_BOOT_CORES", unset = NA_character_))
if (is.na(n_cores)) n_cores <- min(12, max(1L, parallel::detectCores() - 1L))
message("Decision bootstrap: B = ", boot_B, ", cores = ", n_cores)

set.seed(20260313)

# marker location used in case-study section
marker_x <- 20
marker_y <- 30
y_shifts <- seq(0, -30, by = -1)
d_touch_vals <- -y_shifts

# lineout bootstrap ingredients
match_keys <- phase_data %>%
  distinct(Round, Home, Away)
n_matches <- nrow(match_keys)
meter_line_levels <- levels(sampled_phase_data$meter_line_factor)

predict_lineout_ep_boot <- function(meter_line, boot_model, card_diff = 0, win_pct_diff = 0) {
  new_data <- tibble(
    meter_line = meter_line,
    meter_line_factor = factor(meter_line, levels = meter_line_levels),
    Card_Diff = card_diff,
    WinPct_Diff = win_pct_diff
  )

  probs <- predict(boot_model, newdata = new_data, type = "probs")
  if (is.vector(probs)) probs <- matrix(probs, nrow = 1)
  as.vector(probs %*% point_values)
}

build_lineout_smoother_boot <- function(boot_model, card_diff = 0, win_pct_diff = 0) {
  ep_zone <- predict_lineout_ep_boot(
    meter_lines,
    boot_model,
    card_diff = card_diff,
    win_pct_diff = win_pct_diff
  )
  iso_fit <- isoreg(meter_lines, -ep_zone)
  splinefun(iso_fit$x, -iso_fit$yf, method = "hyman")
}

one_joint_boot <- function(b) {
  # 1) lineout module: clustered match bootstrap + one row per run_id
  sampled_match_idx <- sample.int(n_matches, size = n_matches, replace = TRUE)
  sampled_matches <- match_keys[sampled_match_idx, ] %>%
    mutate(boot_match_id = row_number())

  boot_phase <- sampled_matches %>%
    left_join(phase_data, by = c("Round", "Home", "Away")) %>%
    group_by(boot_match_id, run_id) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(
      points_factor = factor(points, levels = point_levels),
      meter_line_factor = factor(meter_line, levels = meter_line_levels)
    )

  boot_lineout_model <- try(
    nnet::multinom(
      points_factor ~ meter_line_factor + Card_Diff + WinPct_Diff,
      data = boot_phase,
      trace = FALSE
    ),
    silent = TRUE
  )

  # 2) kick-make module bootstrap
  boot_kick <- kick_data_small %>%
    slice_sample(n = nrow(kick_data_small), replace = TRUE)

  boot_kick_model <- try(
    glm(make ~ angle + Distance, family = binomial(link = "logit"), data = boot_kick),
    silent = TRUE
  )

  # 3) continuation module bootstrap
  boot_restarts <- phase_data_restarts %>%
    slice_sample(n = nrow(phase_data_restarts), replace = TRUE)

  boot_miss_lookup <- build_miss_lookup(boot_restarts)

  if (inherits(boot_lineout_model, "try-error") || inherits(boot_kick_model, "try-error")) {
    return(list(
      lineout_ep = rep(NA_real_, length(y_shifts)),
      p_make = NA_real_,
      miss_ep = NA_real_,
      kick_ep = NA_real_,
      delta_ep = rep(NA_real_, length(y_shifts))
    ))
  }

  lineout_smoother <- build_lineout_smoother_boot(boot_lineout_model, card_diff = 0, win_pct_diff = 0)
  lineout_y <- pmin(pmax(marker_y + y_shifts, min(meter_lines)), max(meter_lines))
  lineout_ep <- as.numeric(lineout_smoother(lineout_y))

  marker_point <- tibble(x = marker_x, y = marker_y)
  marker_with_features <- add_kick_features(marker_point)
  p_make <- as.numeric(predict(boot_kick_model, newdata = marker_with_features, type = "response"))
  miss_ep <- miss_ep_from_y(marker_y, boot_miss_lookup)
  kick_ep <- as.numeric(3 * p_make + (1 - p_make) * miss_ep)

  delta_ep <- lineout_ep - kick_ep

  list(
    lineout_ep = lineout_ep,
    p_make = as.numeric(p_make),
    miss_ep = as.numeric(miss_ep),
    kick_ep = as.numeric(kick_ep),
    delta_ep = delta_ep
  )
}

boot_results <- parallel::mclapply(
  seq_len(boot_B),
  one_joint_boot,
  mc.cores = n_cores
)

lineout_ep_boot <- do.call(rbind, lapply(boot_results, `[[`, "lineout_ep"))
delta_ep_boot <- do.call(rbind, lapply(boot_results, `[[`, "delta_ep"))
kicking_boot <- tibble(
  p_make = as.numeric(sapply(boot_results, `[[`, "p_make")),
  miss_ep = as.numeric(sapply(boot_results, `[[`, "miss_ep")),
  kick_ep = as.numeric(sapply(boot_results, `[[`, "kick_ep"))
)

valid_delta <- !is.na(delta_ep_boot[, 1])
lineout_ep_boot <- lineout_ep_boot[valid_delta, , drop = FALSE]
delta_ep_boot <- delta_ep_boot[valid_delta, , drop = FALSE]
kicking_boot <- kicking_boot[valid_delta, , drop = FALSE]

if (nrow(delta_ep_boot) == 0) {
  stop("All bootstrap replicates failed. Check model stability.")
}

component_summary <- tibble(
  component = c("p_make", "miss_ep", "kick_ep", "lineout_ep_20m_shift", "delta_ep_20m_shift"),
  mean = c(
    mean(kicking_boot$p_make, na.rm = TRUE),
    mean(kicking_boot$miss_ep, na.rm = TRUE),
    mean(kicking_boot$kick_ep, na.rm = TRUE),
    mean(lineout_ep_boot[, d_touch_vals == 20], na.rm = TRUE),
    mean(delta_ep_boot[, d_touch_vals == 20], na.rm = TRUE)
  ),
  lo_2_5 = c(
    quantile(kicking_boot$p_make, 0.025, na.rm = TRUE),
    quantile(kicking_boot$miss_ep, 0.025, na.rm = TRUE),
    quantile(kicking_boot$kick_ep, 0.025, na.rm = TRUE),
    quantile(lineout_ep_boot[, d_touch_vals == 20], 0.025, na.rm = TRUE),
    quantile(delta_ep_boot[, d_touch_vals == 20], 0.025, na.rm = TRUE)
  ),
  hi_97_5 = c(
    quantile(kicking_boot$p_make, 0.975, na.rm = TRUE),
    quantile(kicking_boot$miss_ep, 0.975, na.rm = TRUE),
    quantile(kicking_boot$kick_ep, 0.975, na.rm = TRUE),
    quantile(lineout_ep_boot[, d_touch_vals == 20], 0.975, na.rm = TRUE),
    quantile(delta_ep_boot[, d_touch_vals == 20], 0.975, na.rm = TRUE)
  )
)

shift_summary <- tibble(
  d_touch = d_touch_vals,
  delta_mean = apply(delta_ep_boot, 2, mean, na.rm = TRUE),
  delta_lo_2_5 = apply(delta_ep_boot, 2, quantile, probs = 0.025, na.rm = TRUE),
  delta_hi_97_5 = apply(delta_ep_boot, 2, quantile, probs = 0.975, na.rm = TRUE),
  pr_lineout_better = apply(delta_ep_boot, 2, function(x) mean(x > 0, na.rm = TRUE))
)

cat("\nJoint bootstrap marker summary:\n")
print(component_summary)
cat("\nShift summary (first rows):\n")
print(head(shift_summary, 10))

boot_outputs <- list(
  B = boot_B,
  n_cores = n_cores,
  marker = list(x = marker_x, y = marker_y),
  d_touch = d_touch_vals,
  y_shifts = y_shifts,
  lineout_ep_boot = lineout_ep_boot,
  kicking_boot = kicking_boot,
  delta_ep_boot = delta_ep_boot,
  component_summary = component_summary,
  shift_summary = shift_summary
)

saveRDS(boot_outputs, "data/decision_bootstrap.rds")
write.csv(component_summary, "data/decision_bootstrap_marker_summary.csv", row.names = FALSE)
write.csv(shift_summary, "data/decision_bootstrap_shift_summary.csv", row.names = FALSE)

# Plot 1: full DeltaEP distribution at d_touch = 20m
delta_20 <- delta_ep_boot[, d_touch_vals == 20]
delta_20_ci <- quantile(delta_20, c(0.025, 0.975), na.rm = TRUE)
delta_20_mean <- mean(delta_20, na.rm = TRUE)
pr_lineout_20 <- mean(delta_20 > 0, na.rm = TRUE)

delta_boot_plot <- ggplot(tibble(delta_ep = delta_20), aes(x = delta_ep)) +
  geom_histogram(fill = "#457B9D", color = "white", bins = 45) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = delta_20_mean, linetype = "dashed", color = "#1D3557", linewidth = 1) +
  geom_vline(xintercept = delta_20_ci[1], linetype = "dashed", color = "#E76F51", linewidth = 1) +
  geom_vline(xintercept = delta_20_ci[2], linetype = "dashed", color = "#E76F51", linewidth = 1) +
  labs(
    title = expression(paste("Joint Bootstrap Distribution of ", Delta, "EP at ", d[touch], " = 20m")),
    subtitle = sprintf("Mean = %.2f, 95%% CI = [%.2f, %.2f], Pr(lineout better) = %.2f",
                       delta_20_mean, delta_20_ci[1], delta_20_ci[2], pr_lineout_20),
    x = expression(Delta * "EP (Lineout - Kick)"),
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

ggsave("plots/delta_bootstrap_marker.png", delta_boot_plot, width = 10, height = 7, dpi = 300)

# Plot 2: DeltaEP vs meters gained with 95% joint-bootstrap band
point_est_curve <- shift_results %>%
  transmute(
    d_touch = -y_shift,
    delta_point_est = lineout_EP - kick_EP
  )

delta_shift_plot <- shift_summary %>%
  left_join(point_est_curve, by = "d_touch") %>%
  ggplot(aes(x = d_touch, y = delta_mean)) +
  geom_ribbon(aes(ymin = delta_lo_2_5, ymax = delta_hi_97_5), fill = "#A8DADC", alpha = 0.5) +
  geom_line(color = "#1D3557", linewidth = 1.2) +
  geom_line(aes(y = delta_point_est), color = "#E76F51", linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  labs(
    title = expression(paste("Decision Curve with Joint Uncertainty (", Delta, "EP)")),
    subtitle = "Solid: bootstrap mean, band: 95% CI, dashed: point estimate",
    x = "Meters gained to touch",
    y = expression(Delta * "EP (Lineout - Kick)")
  ) +
  theme_minimal(base_size = 13)

ggsave("plots/delta_plot_bootstrap.png", delta_shift_plot, width = 10, height = 7, dpi = 300)


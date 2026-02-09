# Run from project root. Depends on 02_ep-lineout.R.
source("scripts/02_ep-lineout.R")

#########################
### Cluster Bootstrap ###
#########################

# Brill et al. (2025) style clustered bootstrap:
# 1) resample matches with replacement
# 2) within each sampled match, sample exactly one row per run_id
boot_B <- as.integer(Sys.getenv("EP_BOOT_B", unset = "2000"))
n_cores <- as.integer(Sys.getenv("EP_BOOT_CORES", unset = NA_character_))
if (is.na(n_cores)) n_cores <- min(12, max(1L, parallel::detectCores() - 1L))
message("Bootstrap: B = ", boot_B, ", cores = ", n_cores)

match_keys <- phase_data %>%
  distinct(Round, Home, Away)

n_matches <- nrow(match_keys)
meter_line_levels <- levels(sampled_phase_data$meter_line_factor)

# One bootstrap replicate (runs in parallel workers)
one_boot <- function(b) {
  sampled_match_idx <- sample.int(n_matches, size = n_matches, replace = TRUE)
  sampled_matches <- match_keys[sampled_match_idx, ] %>%
    mutate(boot_match_id = row_number())

  boot_sample <- sampled_matches %>%
    left_join(phase_data, by = c("Round", "Home", "Away")) %>%
    group_by(boot_match_id, run_id) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(
      points_factor = factor(points, levels = point_levels),
      meter_line_factor = factor(meter_line, levels = meter_line_levels)
    )

  boot_fit <- try(
    nnet::multinom(
      points_factor ~ meter_line_factor + Card_Diff + WinPct_Diff,
      data = boot_sample,
      trace = FALSE
    ),
    silent = TRUE
  )

  if (inherits(boot_fit, "try-error")) {
    return(list(coef = NULL, ep_row = rep(NA_real_, nrow(pred_data))))
  }

  boot_probs <- predict(boot_fit, newdata = pred_data, type = "probs")
  if (is.vector(boot_probs)) {
    boot_probs <- matrix(boot_probs, nrow = 1)
  }
  ep_row <- as.vector(boot_probs %*% point_values)
  list(coef = coef(boot_fit), ep_row = ep_row)
}

boot_results <- parallel::mclapply(
  seq_len(boot_B),
  one_boot,
  mc.cores = n_cores
)

coef_boot <- lapply(boot_results, `[[`, "coef")
ep_boot <- do.call(rbind, lapply(boot_results, `[[`, "ep_row"))
colnames(ep_boot) <- as.character(pred_data$meter_line)

ep_ci <- apply(
  ep_boot,
  2,
  quantile,
  probs = c(0.025, 0.975),
  na.rm = TRUE
)

results_multi_ci <- tibble(
  meter_line = pred_data$meter_line,
  expected_points = as.vector(expected_points_multi),
  ep_lo_2_5 = ep_ci[1, ],
  ep_hi_97_5 = ep_ci[2, ]
)

print("Expected Points by Meter Line with 95% Bootstrap CI:")
print(results_multi_ci)

bootstrap_outputs <- list(
  point_estimate_model = multinomial_model,
  bootstrap_coefficients = coef_boot,
  expected_points_bootstrap = ep_boot,
  expected_points_summary = results_multi_ci
)

saveRDS(bootstrap_outputs, "data/bootstrap.rds")

# Plotting EP with uncertainty at each zone (discrete; no continuous curve)
multi_ep_plot <- ggplot(results_multi_ci, aes(x = meter_line, y = expected_points)) +
  geom_linerange(aes(ymin = ep_lo_2_5, ymax = ep_hi_97_5), linewidth = 1.2, color = "steelblue") +
  geom_point(size = 3.5, color = "steelblue") +
  labs(
    title = "Expected Points by Field Zone (Multinomial + Cluster Bootstrap)",
    subtitle = "Estimates at discrete zones only. 95% CI from match-level resampling.",
    x = "Zone (meters from try line)",
    y = "Expected Points"
  ) +
  theme_minimal()

ggsave("plots/multinomial_bootstrap.png", multi_ep_plot,
       width = 10, height = 8, dpi = 300)

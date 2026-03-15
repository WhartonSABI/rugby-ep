#!/usr/bin/env Rscript

# run from project root
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(nnet)
  library(mgcv)
})

# project-wide reproducibility seed (override via EP_SEED)
ep_seed <- suppressWarnings(as.integer(Sys.getenv("EP_SEED", "20260313")))
if (is.na(ep_seed)) {
  warning("Invalid EP_SEED; defaulting to 20260313")
  ep_seed <- 20260313L
}
set.seed(ep_seed)

############################
### METRIC CALCULATIONS  ###
############################

clip_prob <- function(p, eps = 1e-9) pmin(pmax(p, eps), 1 - eps)

binary_metrics <- function(p, y) {
  p <- clip_prob(as.numeric(p))
  y <- as.numeric(y)
  tibble(
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
    brier = mean((p - y)^2)
  )
}

multiclass_metrics <- function(probs, y, class_levels) {
  if (is.vector(probs)) probs <- matrix(probs, nrow = 1)

  probs <- as.data.frame(probs)
  for (cl in class_levels) {
    if (!(cl %in% names(probs))) probs[[cl]] <- 0
  }
  probs <- as.matrix(probs[, class_levels, drop = FALSE])
  probs <- clip_prob(probs)
  probs <- probs / rowSums(probs)

  y_fac <- factor(as.character(y), levels = class_levels)
  y_idx <- as.integer(y_fac)
  ll <- -mean(log(probs[cbind(seq_along(y_idx), y_idx)]))

  y_mat <- matrix(0, nrow = length(y_idx), ncol = length(class_levels))
  y_mat[cbind(seq_along(y_idx), y_idx)] <- 1
  br <- mean(rowSums((y_mat - probs)^2))

  tibble(log_loss = ll, brier = br)
}

sample_one_per_run <- function(df) {
  df %>%
    group_by(Round, Home, Away, run_id) %>%
    slice_sample(n = 1) %>%
    ungroup()
}

############################
### LINEOUT DATA PREP    ###
############################

phase_raw <- read_csv("data/phase_2018-19.csv", show_col_types = FALSE)

phase_points <- phase_raw %>%
  mutate(
    points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    points = ifelse(is.na(points), 0, points)
  )

last_play <- phase_points %>%
  group_by(Round, Home, Away) %>%
  filter(ID == max(ID)) %>%
  ungroup() %>%
  mutate(
    last_play_points = str_extract(Outcome, "[-+]?\\d+") %>% as.numeric(),
    last_play_points = ifelse(is.na(last_play_points), 0, last_play_points),
    score_change = last_play_points,
    final_points_diff = Points_Difference + score_change,
    final_points_diff_home = if_else(
      Team_In_Poss == "Home",
      final_points_diff,
      -final_points_diff
    )
  ) %>%
  select(Round, Home, Away, final_points_diff_home) %>%
  distinct() %>%
  pivot_longer(
    cols = c(Home, Away),
    names_to = "Side",
    values_to = "Team"
  ) %>%
  mutate(
    Win = case_when(
      Side == "Home" & final_points_diff_home > 0 ~ 1,
      Side == "Home" & final_points_diff_home <= 0 ~ 0,
      Side == "Away" & final_points_diff_home < 0 ~ 1,
      Side == "Away" & final_points_diff_home >= 0 ~ 0
    ),
    Round = as.numeric(Round)
  ) %>%
  arrange(Team, Round) %>%
  group_by(Team) %>%
  mutate(
    Games_Played = lag(row_number(), default = 0),
    Wins_Before = lag(cumsum(Win), default = 0),
    WinPct_Before = if_else(Games_Played == 0, 0.5, Wins_Before / Games_Played)
  ) %>%
  ungroup()

lineout_full <- phase_points %>%
  filter(Play_Start == "Lineout", Phase == 1) %>%
  mutate(
    Team_for_join = case_when(
      Team_In_Poss == "Home" ~ Home,
      Team_In_Poss == "Away" ~ Away
    ),
    Opponent = if_else(Team_In_Poss == "Home", Away, Home)
  ) %>%
  left_join(
    last_play %>% select(Team, Round, WinPct_Before),
    by = c("Team_for_join" = "Team", "Round" = "Round")
  ) %>%
  left_join(
    last_play %>%
      select(Team, Round, WinPct_Before) %>%
      rename(Opponent = Team, Opponent_WinPct = WinPct_Before),
    by = c("Opponent", "Round")
  ) %>%
  mutate(
    WinPct_Diff = WinPct_Before - Opponent_WinPct,
    Card_Diff = (Yellow_Cards_Opp + Red_Cards_Opp) - (Yellow_Cards_Own + Red_Cards_Own),
    Seconds_Remaining_Half = if_else(
      Seconds_Remaining > 2400,
      Seconds_Remaining - 2400,
      Seconds_Remaining
    ),
    Less_Than_2_Min = if_else(Seconds_Remaining_Half < 120, 1, 0)
  ) %>%
  arrange(Round, Home, Away, ID) %>%
  group_by(Round, Home, Away) %>%
  mutate(
    run_id = cumsum(c(TRUE, diff(Points_Difference) != 0 | Outcome[-1] != Outcome[-n()])),
    n_same = ave(run_id, run_id, FUN = length)
  ) %>%
  ungroup()

location_names <- c(
  "5m-Goal (opp)", "22m-5m (opp)", "10m-22m (opp)", "Half-10m (opp)",
  "10m-Half (own)", "22m-10m (own)", "5m-22m (own)", "Goal-5m (own)"
)
location_meters <- c(5, 13.5, 31, 45, 55, 69, 86.5, 97.5)
lookup <- setNames(location_meters, location_names)

lineout_full <- lineout_full %>%
  mutate(
    meter_line = as.numeric(lookup[Location]),
    match_id = paste(Round, Home, Away, sep = "__")
  )

class_levels <- sort(unique(lineout_full$points))
meter_levels <- sort(unique(lineout_full$meter_line))

lineout_models <- list(
  list(
    key = "lineout_baseline",
    label = "Baseline: meter + Card + WinPct + <2min",
    formula = points_factor ~ meter_line_factor + Card_Diff + WinPct_Diff + Less_Than_2_Min
  ),
  list(
    key = "lineout_plus_interaction",
    label = "Baseline + Card x WinPct",
    formula = points_factor ~ meter_line_factor + Card_Diff * WinPct_Diff + Less_Than_2_Min
  ),
  list(
    key = "lineout_separate_strengths",
    label = "Baseline with separate team strengths",
    formula = points_factor ~ meter_line_factor + Card_Diff + WinPct_Before + Opponent_WinPct + Less_Than_2_Min
  ),
  list(
    key = "lineout_plus_time",
    label = "Baseline + continuous time",
    formula = points_factor ~ meter_line_factor + Card_Diff + WinPct_Diff + Less_Than_2_Min + Seconds_Remaining_Half
  )
)

evaluate_lineout_model <- function(df, model_spec, k_folds = 5) {
  match_keys <- df %>%
    distinct(match_id) %>%
    mutate(fold = sample(rep(seq_len(k_folds), length.out = n())))

  df_fold <- df %>%
    left_join(match_keys, by = "match_id")

  fold_scores <- lapply(seq_len(k_folds), function(k) {
    train_full <- df_fold %>% filter(fold != k)
    test_full <- df_fold %>% filter(fold == k)

    train_df <- sample_one_per_run(train_full) %>%
      mutate(
        points_factor = factor(points, levels = class_levels),
        meter_line_factor = factor(meter_line, levels = meter_levels)
      )

    test_df <- sample_one_per_run(test_full) %>%
      mutate(
        points_factor = factor(points, levels = class_levels),
        meter_line_factor = factor(meter_line, levels = meter_levels)
      )

    fit <- try(
      nnet::multinom(
        formula = model_spec$formula,
        data = train_df,
        trace = FALSE
      ),
      silent = TRUE
    )
    if (inherits(fit, "try-error")) return(NULL)

    pr <- try(predict(fit, newdata = test_df, type = "probs"), silent = TRUE)
    if (inherits(pr, "try-error")) return(NULL)

    m <- multiclass_metrics(
      probs = pr,
      y = test_df$points_factor,
      class_levels = as.character(class_levels)
    )

    tibble(
      model_key = model_spec$key,
      model_label = model_spec$label,
      fold = k,
      log_loss = m$log_loss,
      brier = m$brier
    )
  })

  bind_rows(fold_scores)
}

lineout_cv_raw <- bind_rows(lapply(lineout_models, evaluate_lineout_model, df = lineout_full, k_folds = 5))

lineout_cv_summary <- lineout_cv_raw %>%
  group_by(model_key, model_label) %>%
  summarise(
    cv_log_loss = mean(log_loss, na.rm = TRUE),
    cv_brier = mean(brier, na.rm = TRUE),
    folds_used = n(),
    .groups = "drop"
  ) %>%
  mutate(
    component = "Lineout multinomial",
    delta_log_loss_vs_baseline = cv_log_loss - cv_log_loss[model_key == "lineout_baseline"],
    model_order = case_when(
      model_key == "lineout_baseline" ~ 1L,
      model_key == "lineout_plus_interaction" ~ 2L,
      model_key == "lineout_separate_strengths" ~ 3L,
      model_key == "lineout_plus_time" ~ 4L,
      TRUE ~ 99L
    )
  )

lineout_ref <- sample_one_per_run(lineout_full) %>%
  mutate(
    points_factor = factor(points, levels = class_levels),
    meter_line_factor = factor(meter_line, levels = meter_levels)
  )

lineout_point_values <- as.numeric(as.character(class_levels))

predict_lineout_ep <- function(fit, newdata) {
  pr <- predict(fit, newdata = newdata, type = "probs")
  if (is.vector(pr)) pr <- matrix(pr, nrow = 1)
  pr <- as.data.frame(pr)
  class_char <- as.character(class_levels)
  for (cl in class_char) {
    if (!(cl %in% names(pr))) pr[[cl]] <- 0
  }
  pr <- as.matrix(pr[, class_char, drop = FALSE])
  as.numeric(pr %*% lineout_point_values)
}

lineout_fits <- lapply(lineout_models, function(model_spec) {
  fit <- nnet::multinom(
    formula = model_spec$formula,
    data = lineout_ref,
    trace = FALSE
  )
  list(
    model_key = model_spec$key,
    model_label = model_spec$label,
    fit = fit
  )
})

ep_eval_grid <- tidyr::expand_grid(
  meter_line = meter_levels,
  Card_Diff = c(-1, 0, 1),
  WinPct_Diff = c(-0.25, 0, 0.25)
) %>%
  mutate(
    meter_line_factor = factor(meter_line, levels = meter_levels),
    WinPct_Before = 0.5 + WinPct_Diff / 2,
    Opponent_WinPct = 0.5 - WinPct_Diff / 2,
    Less_Than_2_Min = 0,
    Seconds_Remaining_Half = 1200
  )

baseline_fit <- lineout_fits[[which(vapply(lineout_fits, function(x) x$model_key == "lineout_baseline", logical(1)))]]$fit
baseline_ep <- predict_lineout_ep(baseline_fit, ep_eval_grid)

lineout_ep_sensitivity <- bind_rows(lapply(lineout_fits, function(model_obj) {
  ep <- predict_lineout_ep(model_obj$fit, ep_eval_grid)
  tibble(
    model_key = model_obj$model_key,
    model_label = model_obj$model_label,
    ep_mean_abs_diff_vs_baseline = mean(abs(ep - baseline_ep)),
    ep_max_abs_diff_vs_baseline = max(abs(ep - baseline_ep))
  )
}))

############################
### KICK MODEL CHECKS    ###
############################

kick_data <- read.csv("data/Goal kicking data.csv") %>%
  rename(x = X1.Metres) %>%
  mutate(
    y = 100 - Y1.Metres
  ) %>%
  filter(Type == 2) %>%
  mutate(
    make = ifelse(Quality == 1, 1, 0),
    angle = atan2(abs(x - 35), y) * (180 / pi),
    Distance = sqrt((x - 35)^2 + y^2)
  ) %>%
  select(x, y, make, angle, Distance, PlayerID)

kick_models <- list(
  list(
    key = "kick_logit",
    label = "Logistic: angle + distance",
    fit_fun = function(df) glm(make ~ angle + Distance, family = binomial(), data = df)
  ),
  list(
    key = "kick_gam",
    label = "GAM: s(angle) + s(distance)",
    fit_fun = function(df) mgcv::gam(make ~ s(angle, k = 8) + s(Distance, k = 8), family = binomial(), data = df, method = "REML")
  )
)

evaluate_kick_model <- function(df, model_spec, k_folds = 5) {
  df <- df %>%
    mutate(fold = sample(rep(seq_len(k_folds), length.out = n())))

  fold_scores <- lapply(seq_len(k_folds), function(k) {
    train_df <- df %>% filter(fold != k)
    test_df <- df %>% filter(fold == k)

    fit <- try(model_spec$fit_fun(train_df), silent = TRUE)
    if (inherits(fit, "try-error")) return(NULL)

    pr <- try(predict(fit, newdata = test_df, type = "response"), silent = TRUE)
    if (inherits(pr, "try-error")) return(NULL)

    m <- binary_metrics(pr, test_df$make)
    tibble(
      model_key = model_spec$key,
      model_label = model_spec$label,
      fold = k,
      log_loss = m$log_loss,
      brier = m$brier
    )
  })

  bind_rows(fold_scores)
}

kick_cv_raw <- bind_rows(lapply(kick_models, evaluate_kick_model, df = kick_data, k_folds = 5))

kick_cv_summary <- kick_cv_raw %>%
  group_by(model_key, model_label) %>%
  summarise(
    cv_log_loss = mean(log_loss, na.rm = TRUE),
    cv_brier = mean(brier, na.rm = TRUE),
    folds_used = n(),
    .groups = "drop"
  ) %>%
  mutate(
    component = "Kick success",
    delta_log_loss_vs_baseline = cv_log_loss - cv_log_loss[model_key == "kick_logit"],
    model_order = case_when(
      model_key == "kick_logit" ~ 1L,
      model_key == "kick_gam" ~ 2L,
      TRUE ~ 99L
    )
  )

marker_kick <- tibble(
  x = 20,
  y = 30,
  angle = atan2(abs(20 - 35), 30) * (180 / pi),
  Distance = sqrt((20 - 35)^2 + 30^2)
)

kick_fits <- lapply(kick_models, function(model_spec) {
  fit <- model_spec$fit_fun(kick_data)
  tibble(
    model_key = model_spec$key,
    model_label = model_spec$label,
    marker_p_make = as.numeric(predict(fit, newdata = marker_kick, type = "response"))
  )
}) %>%
  bind_rows()

############################
### EXPORTS              ###
############################

model_sensitivity <- bind_rows(
  lineout_cv_summary %>% select(component, model_key, model_label, cv_log_loss, cv_brier, delta_log_loss_vs_baseline, folds_used, model_order),
  kick_cv_summary %>% select(component, model_key, model_label, cv_log_loss, cv_brier, delta_log_loss_vs_baseline, folds_used, model_order)
) %>%
  arrange(component, model_order) %>%
  mutate(
    cv_log_loss = round(cv_log_loss, 4),
    cv_brier = round(cv_brier, 4),
    delta_log_loss_vs_baseline = round(delta_log_loss_vs_baseline, 4)
  ) %>%
  select(-model_order)

write.csv(lineout_cv_raw, "data/lineout_model_sensitivity_folds.csv", row.names = FALSE)
write.csv(kick_cv_raw, "data/kick_model_sensitivity_folds.csv", row.names = FALSE)
write.csv(model_sensitivity, "data/model_sensitivity_summary.csv", row.names = FALSE)
write.csv(lineout_ep_sensitivity, "data/lineout_model_ep_sensitivity.csv", row.names = FALSE)
write.csv(kick_fits, "data/kick_model_marker_sensitivity.csv", row.names = FALSE)

table_rows <- apply(model_sensitivity, 1, function(r) {
  sprintf(
    "%s & %s & %.4f & %.4f & %.4f \\\\",
    r[["component"]],
    r[["model_label"]],
    as.numeric(r[["cv_log_loss"]]),
    as.numeric(r[["cv_brier"]]),
    as.numeric(r[["delta_log_loss_vs_baseline"]])
  )
})

table_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Out-of-sample sensitivity checks for lineout and kick model specifications (grouped 5-fold CV).}",
  "\\label{tab:model-sensitivity}",
  "\\resizebox{\\linewidth}{!}{%",
  "\\begin{tabular}{llccc}",
  "\\toprule",
  "Component & Model & CV log loss & CV Brier & $\\Delta$ log loss vs baseline \\\\",
  "\\midrule",
  table_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\vspace{0.4em}",
  "\\begin{minipage}{0.95\\linewidth}",
  "\\footnotesize Baselines are: lineout multinomial with meter-line factor + card differential + win-percentage differential + under-2-minute indicator, and kick logistic regression with angle + distance.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(table_lines, "paper/appendix_model_sensitivity.tex")

cat("\nModel sensitivity summary:\n")
print(model_sensitivity)
cat("\nWrote:\n")
cat("- data/model_sensitivity_summary.csv\n")
cat("- data/lineout_model_ep_sensitivity.csv\n")
cat("- data/kick_model_marker_sensitivity.csv\n")
cat("- paper/appendix_model_sensitivity.tex\n")

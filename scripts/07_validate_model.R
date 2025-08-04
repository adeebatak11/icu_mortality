# -----------------------------------------------------------------------------
# 01. Model Validation Script: Calibration & Performance (LASSO Model)
# -----------------------------------------------------------------------------
# This script:
#   - Loads model results and predictions
#   - Computes calibration metrics (CITL, slope)
#   - Plots calibration curve (deciles)
#   - Calculates Brier score (if added)
#   - Summarizes risk stratification
#   - Runs decision curve analysis
#   - Produces publication-ready tables and plots
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 02. Load Dependencies and Model Results
# -----------------------------------------------------------------------------
source("scripts/05_run_lasso_model.R")  # Loads df_model and predictions
set.seed(123)

# -----------------------------------------------------------------------------
# 03. Calibration Metrics: CITL & Slope
# -----------------------------------------------------------------------------
boot_citl_slope <- function(data, indices) {
  d <- data[indices, ]
  y <- as.numeric(as.character(d$icumortality))
  p <- d$pred_lasso
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  # CITL
  fit_citl <- glm(y ~ offset(lp), family = binomial())
  # Slope
  fit_slope <- glm(y ~ lp, family = binomial())
  c(CITL = coef(fit_citl)[1], Slope = coef(fit_slope)[2])
}

citl_ci  <- quantile(boot_res$t[, 1], probs = c(0.025, 0.975), na.rm = TRUE)
slope_ci <- quantile(boot_res$t[, 2], probs = c(0.025, 0.975), na.rm = TRUE)
# Run bootstrap for calibration metrics
df_boot <- df_model %>% mutate(pred_lasso = as.vector(pred_lasso))
boot_res <- boot(data = df_boot, statistic = boot_citl_slope, R = 1000)

# Extract confidence intervals
citl_ci  <- quantile(boot_res$t[, 1], probs = c(0.025, 0.975), na.rm = TRUE)
slope_ci <- quantile(boot_res$t[, 2], probs = c(0.025, 0.975), na.rm = TRUE)

# Print results
cat(sprintf("CITL:  %.3f (95%% CI: %.3f to %.3f)\n", boot_res$t0[1], citl_ci[1], citl_ci[2]))
cat(sprintf("Slope: %.3f (95%% CI: %.3f to %.3f)\n", boot_res$t0[2], slope_ci[1], slope_ci[2]))

# -----------------------------------------------------------------------------
# 04. Calibration Plot (Deciles)
# -----------------------------------------------------------------------------
calib_bin <- df_model %>%
  mutate(
    y = as.numeric(as.character(icumortality)),
    pred = pred_lasso,
    decile = ntile(pred, 10)
  ) %>%
  group_by(decile) %>%
  summarise(
    N = n(),
    Mean_Pred = mean(pred),
    Observed = mean(y),
    Events = sum(y)
  ) %>%
  rowwise() %>%
  mutate(CI = list(binom.confint(Events, N, methods = "wilson"))) %>%
  unnest_wider(CI) %>%
  select(decile, Mean_Pred, Observed, lower, upper) %>%
  mutate(
    Mean_Pred = Mean_Pred * 100,
    Observed = Observed * 100,
    lower = lower * 100,
    upper = upper * 100
  )

p2 <- ggplot(calib_bin, aes(x = Mean_Pred, y = Observed)) +
  geom_point(size = 3.5, shape = 21, fill = "black", color = "white", stroke = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.7, color = "black") +
  geom_line(linewidth = 1.2, color = "black", aes(group = 1)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray40", linewidth = 1) +
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 35)) +
  labs(
    title = "Calibration Curve (Deciles of Predicted Risk)",
    x = "Mean Predicted Mortality (%)",
    y = "Observed Mortality (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  )
# ggsave("docs/images/calibration_curve.png", plot = p2, bg = "white", width = 6, height = 6.5, dpi = 600)

# -----------------------------------------------------------------------------
# 05. Risk Stratification Summary Table
# -----------------------------------------------------------------------------
boot_group_rate <- function(data, indices) {
  yb <- as.numeric(as.character(data$icumortality[indices]))
  grpb <- pred_risk_group[indices]
  tapply(yb, grpb, mean, na.rm = TRUE)
}

# Run bootstrap for risk group rates
boot_rates <- boot(data = df_model, statistic = boot_group_rate, R = 1000)
ci_group <- round(100 * apply(boot_rates$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE), 1)

# Build summary table
calib_summary <- tibble(
  `Risk Group` = levels(pred_risk_group),
  N = group_sizes,
  Deaths = group_deaths,
  ObservedRatePct = round(group_rates, 1),
  CI_Lower = ci_group[1, ],
  CI_Upper = ci_group[2, ]
) %>% 
  mutate(
    `Observed Mortality (%)` = sprintf("%.1f", ObservedRatePct),
    `95% CI (%)` = paste0(sprintf("%.1f", CI_Lower), "–", sprintf("%.1f", CI_Upper))
  ) %>% 
  select(`Risk Group`, N, Deaths, `Observed Mortality (%)`, `95% CI (%)`)

# Format risk group summary table
table1 <- calib_summary %>%
  gt() %>%
  tab_options(
    table.font.size = 16,
    table.align     = "center",
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    table.border.top.style    = "solid",
    table.border.top.width    = px(2),
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.left.style   = "solid",
    table.border.left.width   = px(2),
    table.border.right.style  = "solid",
    table.border.right.width  = px(2),
    table_body.vlines.style   = "solid",
    table_body.vlines.width   = px(1),
    column_labels.vlines.style = "solid",
    column_labels.vlines.width = px(1),
    stub.border.style = "solid",
    stub.border.width = px(1)
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_caption("Observed Mortality by Risk Group")

gtsave(
  data = table1,
  filename = "docs/images/risk_group_table.png",  
  expand = 10,                                     
  vwidth = 1000,                                   
  vheight = 800                                    
)
# -----------------------------------------------------------------------------
# 06. Decision Curve Analysis (Net Benefit)
# -----------------------------------------------------------------------------
# Prepare input
dca_df <- df_model %>%
  mutate(
    risk = pred_lasso,
    outcome = as.numeric(as.character(icumortality))
  )

# Check input validity
stopifnot(
  all(!is.na(dca_df$outcome)),
  all(!is.na(dca_df$risk)),
  all(dca_df$risk >= 0 & dca_df$risk <= 1)
)

# Run rmda decision curve
dca_rmda <- decision_curve(
  outcome ~ risk,
  data        = dca_df,
  thresholds  = seq(0.01, 0.50, by = 0.01),
  bootstraps  = 1000,
  study.design= "cohort",
  policy      = "opt-in"
)

# Helper: Calculate net benefit for each threshold
net_benefit_tbl <- function(y, p, thresholds) {
  n <- length(y)
  prev <- mean(y)
  map_dfr(thresholds, function(t) {
    pred <- p >= t
    tp <- sum(pred & y == 1)
    fp <- sum(pred & y == 0)
    nb_model <- (tp/n) - (fp/n) * (t / (1 - t))
    nb_all   <- prev - (1 - prev) * (t / (1 - t))
    tibble(
      threshold = t,
      net_benefit = nb_model,
      all         = nb_all,
      none        = 0,
      ia_per_100  = (nb_model - nb_all) / (t / (1 - t)) * 100
    )
  })
}

# Estimate net benefit and confidence intervals via bootstrapping
dca_tbl <- net_benefit_tbl(y = dca_df$outcome, p = dca_df$risk, thresholds = seq(0.01, 0.50, 0.01))

nb_mat <- replicate(1000, {
  idx <- sample.int(nrow(dca_df), replace = TRUE)
  net_benefit_tbl(dca_df$outcome[idx], dca_df$risk[idx], seq(0.01, 0.50, 0.01))$net_benefit
})

dca_tbl <- dca_tbl %>%
  mutate(
    nb_lo = apply(nb_mat, 1, quantile, 0.025),
    nb_hi = apply(nb_mat, 1, quantile, 0.975),
    model = "LASSO"
  )

# Plot decision curve analysis
p_dca <- ggplot(dca_tbl) +
  geom_ribbon(aes(x = threshold, ymin = nb_lo, ymax = nb_hi), fill = "gray70", alpha = 0.3) +
  geom_line(aes(x = threshold, y = net_benefit, color = model), linewidth = 1.2) +
  geom_line(aes(x = threshold, y = all, color = "Treat All"), linetype = "dashed", linewidth = 1) +
  geom_hline(aes(yintercept = 0, color = "Treat None"), linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = c(0.05, 0.20), linetype = "dotted", color = "blue", linewidth = 0.5) +
  labs(
    title = "Decision Curve Analysis (LASSO model)",
    x = "Threshold Probability",
    y = "Net Benefit",
    color = "Strategy"
  ) +
  scale_color_manual(values = c(
    "LASSO" = "black",
    "Treat All" = "darkred",
    "Treat None" = "lightgreen"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.75, 0.15),
    legend.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  )

# -----------------------------------------------------------------------------
# 07. Threshold-Based Net Benefit Summary Table
# -----------------------------------------------------------------------------
# Helper: Summarize DCA at selected thresholds
report_thresholds <- function(tbl, t_vec = c(0.05, 0.20)) {
  purrr::map_dfr(t_vec, function(tstar) {
    row <- tbl[which.min(abs(tbl$threshold - tstar)), ]
    tibble(
      threshold = tstar,
      net_benefit = round(row$net_benefit * 100),
      nb_lo = round(row$nb_lo * 100, 1),
      nb_hi = round(row$nb_hi * 100, 1),
      ia_per_100 = round(row$ia_per_100),
      pct_flagged = round(mean(dca_df$risk >= tstar) * 100, 1)
    )
  })
}

# Build summary table for clinical interpretation
dca_summary <- report_thresholds(dca_tbl, c(0.05, 0.20)) %>%
  mutate(
    `Threshold (%)` = threshold * 100,
    `Net Benefit†` = net_benefit,
    `95% CI` = paste0(nb_lo, "–", nb_hi),
    `Escalations Avoided‡` = ia_per_100,
    `% Patients Flagged` = sprintf("%.1f", pct_flagged)
  ) %>%
  select(
    `Threshold (%)`,
    `Net Benefit†`,
    `95% CI`,
    `Escalations Avoided‡`,
    `% Patients Flagged`
  )

# Format the gt table for publication
table_dca <- dca_summary %>%
  gt() %>%
  tab_header(
    title = md("**Clinical Interpretation at 5% and 20% Thresholds**")
  ) %>%
  tab_source_note(
    md("† Additional correct decisions per 100 patients vs *Treat None*  \n‡ Fewer unnecessary escalations per 100 patients vs *Treat All*")
  ) %>%
  tab_options(
    table.font.size = 16,
    table.align     = "center",
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    table.border.top.style    = "solid",
    table.border.top.width    = px(2),
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.left.style   = "solid",
    table.border.left.width   = px(2),
    table.border.right.style  = "solid",
    table.border.right.width  = px(2),
    table_body.vlines.style   = "solid",
    table_body.vlines.width   = px(1),
    column_labels.vlines.style = "solid",
    column_labels.vlines.width = px(1),
    stub.border.style = "solid",
    stub.border.width = px(1)
  ) %>%
  cols_align(align = "center", columns = everything())

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
#source("scripts/04_run_glm_model.R")  # Loads df_model and predictions
source("scripts/05_run_lasso_model.R")  # Loads df_model and predictions
set.seed(123)

# -----------------------------------------------------------------------------
# 03. Calibration Metrics: CITL & Slope
# -----------------------------------------------------------------------------
df_boot <- df_model %>% mutate(pred_lasso = as.vector(pred_lasso))


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

# Run bootstrap for calibration metrics
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

ann_txt <- sprintf(
  "Calibration\nCITL %.2f (%.2f–%.2f)\nSlope %.2f (%.2f–%.2f)",
  boot_res$t0[1], citl_ci[1], citl_ci[2],
  boot_res$t0[2], slope_ci[1], slope_ci[2]
)

p2 <- ggplot(calib_bin, aes(x = Mean_Pred, y = Observed)) +
  # CI bars first (subtle)
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.5, linewidth = 0.45, color = "grey45") +
  # Decile points and connecting path
  geom_point(size = 3.0, shape = 21, fill = "white", color = "black", stroke = 0.9) +
  geom_path(linewidth = 1.0, color = "black") +
  # Dotted reference line (perfect calibration)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey60", linewidth = 0.8) +
  # Square-ish frame & bounds
  coord_equal(xlim = c(0, 25), ylim = c(0, 35), expand = FALSE) +
  # % on ticks; titles without (%)
  scale_x_continuous(breaks = seq(0, 25, 5),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  scale_y_continuous(breaks = seq(0, 35, 5),
                     labels = percent_format(accuracy = 1, scale = 1)) +
  labs(x = "Mean Predicted Mortality",
       y = "Observed Mortality") +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    plot.margin  = margin(6, 8, 6, 6)
  )+
  theme(
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )+
  geom_vline(xintercept = 5,  linetype = "dotted", linewidth = 0.6, color = "#d78b00") + # 5% Watchlist
  geom_vline(xintercept = 20, linetype = "dotted", linewidth = 0.6, color = "#c23a3a")   # 20% Escalate


ggsave("docs/clinician_insights/www/calibration_curve.png", plot = p2, bg = "white", width = 5, height = 7, units  = "in", dpi = 600)

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

pred_risk_group <- cut(
  dca_df$risk,
  breaks = c(-Inf, 0.05, 0.20, Inf),
  right = FALSE,                 # left-closed, right-open: [a, b)
  include.lowest = TRUE,
  labels = c("Low", "Moderate", "High")
)

# Counts and rates from the SAME population
group_sizes  <- as.integer(table(pred_risk_group)[c("Low","Moderate","High")])
group_deaths <- tapply(dca_df$outcome, pred_risk_group, sum, na.rm = TRUE)[c("Low","Moderate","High")]
group_rates  <- 100 * tapply(dca_df$outcome, pred_risk_group, mean, na.rm = TRUE)[c("Low","Moderate","High")]

# Sanity checks: bins vs threshold rules must match exactly
n_flag_5  <- sum(dca_df$risk >= 0.05)           # ≥5% = Moderate + High
n_flag_20 <- sum(dca_df$risk >= 0.20)           # ≥20% = High
stopifnot(sum(group_sizes[2:3]) == n_flag_5)    # Moderate+High == flagged at 5%
stopifnot(group_sizes[3]      == n_flag_20)     # High == flagged at 20%

# (Re)build the risk-strat table (ordered High, Moderate, Low for display)
ci_group <- round(100 * apply(boot_rates$t, 2, quantile, c(0.025, 0.975), na.rm = TRUE), 1)  # you already computed this

calib_summary <- tibble::tibble(
  `Risk Group` = factor(c("High","Moderate","Low"), levels = c("High","Moderate","Low")),
  N       = rev(group_sizes),                        # reorder to High, Moderate, Low
  Deaths  = rev(as.integer(group_deaths)),
  `Observed Mortality (%)` = sprintf("%.1f", rev(group_rates)),
  `95% CI (%)`             = paste0(sprintf("%.1f", ci_group[1, ]), "–", sprintf("%.1f", ci_group[2, ]))
)

# Format risk group summary table
table1 <- calib_summary %>%
  mutate(`Risk Group` = factor(`Risk Group`, levels = c("High", "Moderate", "Low"))) %>%
  arrange(`Risk Group`) %>%
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
  cols_align(align = "center", columns = everything())

# gtsave(
#   data = table1,
#   filename = "docs/images/risk_group_table.png",  
#   expand = 10,                                     
#   vwidth = 1000,                                   
#   vheight = 800                                    
# )
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
  n <- length(y); prev <- mean(y)
  purrr::map_dfr(thresholds, function(t) {
    pred <- p >= t
    tp <- sum(pred & y == 1); fp <- sum(pred & y == 0)
    fn <- sum(!pred & y == 1); tn <- sum(!pred & y == 0)
    nb_model <- (tp/n) - (fp/n) * (t/(1-t))
    nb_all   <-  prev     - (1-prev) * (t/(1-t))
    tibble(
      threshold = t,
      n_flagged = tp + fp,
      n_events  = tp + fn,
      tp = tp, fp = fp, fn = fn, tn = tn,
      net_benefit = nb_model,
      nb_all = nb_all,
      ia_per_100  = (nb_model - nb_all) / (t/(1-t)) * 100  # fewer escalations per 100 vs Treat All
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
library(ggplot2)
library(scales)

# Interpolate net benefit at 5% and 20% thresholds
nb5  <- approx(dca_tbl$threshold, dca_tbl$net_benefit, xout = 0.05)$y
nb20 <- approx(dca_tbl$threshold, dca_tbl$net_benefit, xout = 0.20)$y

p_dca <- ggplot(dca_tbl) +
  # Orange shading for 5–20% (Watchlist zone)
  annotate("rect", xmin = 0.05, xmax = 0.20, ymin = -Inf, ymax = Inf,
           fill = "orange", alpha = 0.03) +
  
  # Light red shading for 20–50% (Escalate zone)
  annotate("rect", xmin = 0.20, xmax = 0.50, ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.03) +
  
  # Uncertainty band around LASSO model
  geom_ribbon(aes(x = threshold, ymin = nb_lo, ymax = nb_hi),
              fill = "grey70", alpha = 0.25) +
  
  # Net benefit curve for LASSO model
  geom_line(aes(x = threshold, y = net_benefit, color = "LASSO"),
            linewidth = 1.2) +
  
  # Treat All strategy
  geom_line(aes(x = threshold, y = nb_all, color = "Treat All"),
            linewidth = 1, linetype = "dashed") +
  
  # Treat None reference (net benefit = 0)
  geom_hline(aes(yintercept = 0, color = "Treat None"),
             linewidth = 1, linetype = "dashed") +
  
  # Threshold indicators
  geom_vline(xintercept = 0.05, linetype = "dotted",
             color = "orange", linewidth = 0.6) +
  
  geom_vline(xintercept = 0.20, linetype = "dotted",
             color = "red", linewidth = 0.6) +
  
  # Axis labels and manual color scheme
  labs(
    x = "Threshold probability",
    y = "Net benefit",
    color = "Strategy"
  ) +
  
  scale_color_manual(values = c(
    "LASSO"      = "black",
    "Treat All"  = "darkblue",
    "Treat None" = "grey55"
  )) +
  
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.5, by = 0.05),
                     limits = c(0, 0.5), expand = expansion(mult = 0)) +
  
  scale_y_continuous(limits = c(-0.02, 0.05),
                     breaks = seq(-0.02, 0.05, by = 0.01),
                     sec.axis = sec_axis(~ . * 100,
                                         breaks = -2:4,
                                         name = "Standardized net benefit (per 100 patients)")) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 16),
    plot.subtitle   = element_text(size = 13, margin = margin(b = 10)),
    axis.title      = element_text(),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Render
p_dca
ggsave("docs/clinician_insights/www/decision_curve.png",
       plot = p_dca,
       bg = "white",
       width = 6.8,         # Wider for label room
       height = 5.5,        # Shorter for slide/pdf layout
       dpi = 600,           # High-res PNG
       units = "in")
# -----------------------------------------------------------------------------
# 07. Threshold-Based Net Benefit Summary Table
# -----------------------------------------------------------------------------
# Helper: Summarize DCA at selected thresholds
report_thresholds <- function(tbl, y, p, t_vec = c(0.05, 0.20)) {
  n <- length(y); n_deaths <- sum(y == 1)
  purrr::map_dfr(t_vec, function(tstar) {
    row <- tbl[which.min(abs(tbl$threshold - tstar)), ]
    pred <- p >= tstar
    tp <- sum(pred & y == 1); fp <- sum(pred & y == 0)
    fn <- sum(!pred & y == 1); # tn not needed below
    flagged <- tp + fp
    
    tibble(
      threshold_pct = tstar * 100,
      nb            = round(row$net_benefit * 100, 1),   # per 100 patients
      nb_lo         = round(row$nb_lo       * 100, 1),
      nb_hi         = round(row$nb_hi       * 100, 1),
      ia_per_100    = round(row$ia_per_100),             # per 100 vs Treat All
      flagged_disp  = sprintf("%.1f (≈%s/%s)", 100*flagged/n,
                              format(flagged, big.mark=","), format(n, big.mark=",")),
      deaths_cap_disp = sprintf("%.0f (≈%s/%s)", 100*tp/n_deaths,
                                format(tp, big.mark=","), format(n_deaths, big.mark=","))
    )
  })
}

#helper 2
metrics_at_threshold <- function(y, p, t) {
  pred <- p >= t
  tp <- sum(pred & y == 1); fp <- sum(pred & y == 0)
  fn <- sum(!pred & y == 1); tn <- sum(!pred & y == 0)
  n  <- length(y)
  tibble::tibble(
    threshold      = t,
    n              = n,
    tp = tp, fp = fp, fn = fn, tn = tn,
    flagged_pct    = 100 * (tp + fp) / n,
    deaths_captured_pct = 100 * tp / (tp + fn),         # Sensitivity
    specificity_pct      = 100 * tn / (tn + fp),
    ppv_pct        = ifelse(tp + fp > 0, 100 * tp / (tp + fp), NA_real_),
    npv_pct        = ifelse(tn + fn > 0, 100 * tn / (tn + fn), NA_real_)
  )
}


# Build summary table for clinical interpretation
dca_summary <- report_thresholds(dca_tbl, dca_df$outcome, dca_df$risk, c(0.05, 0.20)) %>%
  transmute(
    `Threshold (%)`            = threshold_pct,
    `Net Benefit†`             = nb,
    `95% CI`                   = paste0(nb_lo, "–", nb_hi),
    `Escalations Avoided‡`     = ia_per_100,
    `% Patients Flagged`       = flagged_disp,
    `Deaths Captured (%)`      = deaths_cap_disp
  )

## 1) threshold metrics (PPV etc.)
thr_metrics <- dplyr::bind_rows(
  metrics_at_threshold(dca_df$outcome, dca_df$risk, 0.05),
  metrics_at_threshold(dca_df$outcome, dca_df$risk, 0.20)
) %>%
  dplyr::mutate(`Threshold (%)` = threshold * 100) %>%
  dplyr::transmute(
    `Threshold (%)`,
    `PPV (%)`              = sprintf("%.1f", ppv_pct),
    `% Patients Flagged`   = sprintf("%.1f (≈%s/%s)",
                                     100 * (tp + fp) / n,
                                     format(tp + fp, big.mark=","), format(n, big.mark=",")),
    `Deaths Captured (%)`  = sprintf("%.0f (≈%s/%s)",
                                     100 * tp / (tp + fn),
                                     format(tp, big.mark=","), format(tp + fn, big.mark=","))
  )

## 2) net benefit + CI + escalations avoided (per 100) from dca_tbl
dca_summary_nb <- report_thresholds(dca_tbl, dca_df$outcome, dca_df$risk, c(0.05, 0.20)) %>%
  dplyr::transmute(
    `Threshold (%)` = threshold_pct,
    `Net benefit [95% CI]` = paste0(nb, " (", nb_lo, "–", nb_hi, ")"),
    `Escalations Avoided‡` = ia_per_100
  )
## 3) merge and order columns
dca_summary_final <- thr_metrics %>%
  dplyr::left_join(dca_summary_nb, by = "Threshold (%)") %>%
  dplyr::select(
    `Threshold (%)`,
    `% Patients Flagged`,
    `Deaths Captured (%)`,
    `PPV (%)`,
    `Net benefit [95% CI]`,
    `Escalations Avoided‡`
  ) %>%
  dplyr::arrange(`Threshold (%)`)

## 4) transposed view (Metric | ≥5% | ≥20%)
op_tbl_wide <- dca_summary_final %>%
  dplyr::mutate(thresh = ifelse(`Threshold (%)` == 5, "≥5% (Watchlist)", "≥20% (Escalate)")) %>%
  tidyr::pivot_longer(-c(`Threshold (%)`, thresh), names_to = "Metric", values_to = "Value",
                      values_transform = list(Value = as.character)) %>%
  dplyr::select(Metric, thresh, Value) %>%
  tidyr::pivot_wider(names_from = thresh, values_from = Value) %>%
  dplyr::mutate(
    Metric = factor(Metric, levels = c(
      "% Patients Flagged", "Deaths Captured (%)", "PPV (%)",
      "Net benefit [95% CI]", "Escalations Avoided‡"
    ))
  ) %>%
  dplyr::arrange(Metric)

## show the two tables in the console
dca_summary_final
op_tbl_wide

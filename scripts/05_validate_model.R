# -------------------------------------------------------------------------
# 01. Load Previous Script
source("scripts/04_run_model.R")  # Ensure df_model is ready

# -------------------------------------------------------------------------
# 02. ROC-AUC comparison between GLM and LASSO
## AUC Function for GLM
glm_boot_auc <- function(data, indices) {
  d <- data[indices, ]
  fit <- glm(icumortality ~ ., data = d, family = binomial)
  pred <- predict(fit, newdata = d, type = "response")
  roc_d <- roc(d$icumortality, pred, levels = c(0,1), direction = "<")
  as.numeric(auc(roc_d))
}

# GLM AUC 95% CI
boot_glm_auc <- boot(data = df_model, statistic = glm_boot_auc, R = 100)
mean_glm_auc <- mean(boot_glm_auc$t, na.rm = TRUE)
ci_glm_trimmed   <- quantile(boot_glm_auc$t, probs = c(0.05, 0.95), na.rm = TRUE)

## AUC Function for LASSO (fixed lambda)
lasso_boot_auc <- function(data, indices) {
  d <- data[indices, ]
  X_boot <- model.matrix(icumortality ~ ., data = d)[, -1]
  y_boot <- as.numeric(as.character(d$icumortality))
  fit <- glmnet(X_boot, y_boot, family = "binomial", alpha = 1, lambda = best_lambda)
  pred <- as.vector(predict(fit, newx = X_boot, type = "response"))
  roc_d <- roc(y_boot, pred, levels = c(0,1), direction = "<")
  as.numeric(auc(roc_d))
}

# LASSO AUC 95% CI
boot_lasso_auc <- boot(data = df_model, statistic = lasso_boot_auc, R = 100)
mean_lasso_auc <- mean(boot_lasso_auc$t, na.rm = TRUE)
ci_lasso_trimmed <- quantile(boot_lasso_auc$t, probs = c(0.05, 0.95), na.rm = TRUE)

# Visualize AUC comparison
auc_plot_df <- data.frame(
  Model = c("GLM", "LASSO"),
  MeanAUC = c(mean_glm_auc, mean_lasso_auc),
  CI_Lower = c(ci_glm_trimmed[1], ci_lasso_trimmed[1]),
  CI_Upper = c(ci_glm_trimmed[2], ci_lasso_trimmed[2])
) %>% 
  mutate(
    AUC_Label = sprintf("%.3f \u00B1 %.3f", 
                        MeanAUC, 
                        (CI_Upper - CI_Lower) / 2)
  )

p1 <- ggplot(auc_plot_df, aes(x = Model, y = MeanAUC)) +
  geom_point(size = 4.5, shape = 21, fill = "black", color = "black", stroke = 1.2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.3, linewidth = 1) +
  geom_text(aes(label = AUC_Label),
            position = position_nudge(x = 0.07), 
            size = 5, fontface = "bold", hjust = 0) +
  labs(
    subtitle = "Model Discrimination (AUC) with Trimmed 90% CIs",
    y = "Area Under ROC Curve (AUC)",
    x = NULL,
    caption = "Estimated via bootstrap resampling (n = 1,000)"
  ) +
  ylim(0.75, 0.93) +
  theme_bw(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.caption = element_text(size = 11, hjust = 0.5, margin = margin(t = 12)),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    axis.line = element_line(color = "black")
  )
#ggsave("docs/images/auc_ci_comparison.png", plot = p1, width = 6, height = 4, dpi = 300)

# Visualize ROC-curve comparison
#png("docs/images/roc_plot_comparison.png", width = 800, height = 600)
plot(roc_glm, col = "#1f77b4", lwd = 2, main = "ROC Curves: GLM vs LASSO")
plot(roc_lasso, col = "#ff7f0e", lwd = 2, add = TRUE)

legend("bottomright",
       legend = c(sprintf("GLM (AUC = %.3f)", auc(roc_glm)),
                  sprintf("LASSO (AUC = %.3f)", auc(roc_lasso))),
       col = c("#1f77b4", "#ff7f0e"), lwd = 2, bty = "n")

#dev.off()


# -------------------------------------------------------------------------
# 03. CITL + Slope

boot_citl_slope <- function(data, indices) {
  d <- data[indices, ]
  y <- as.numeric(as.character(d$icumortality))
  p <- d$pred_lasso
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  
  # CITL
  fit_citl <- glm(y ~ offset(lp), family = binomial())
  citl <- coef(fit_citl)[1]
  
  # Slope
  fit_slope <- glm(y ~ lp, family = binomial())
  slope <- coef(fit_slope)[2]
  
  return(c(CITL = citl, Slope = slope))
}

# Run bootstrap
boot_res <- boot(data = df_boot, statistic = boot_citl_slope, R = 100)


# 95% percentile confidence intervals
citl_ci   <- quantile(boot_res$t[, 1], probs = c(0.025, 0.975), na.rm = TRUE)
slope_ci  <- quantile(boot_res$t[, 2], probs = c(0.025, 0.975), na.rm = TRUE)

# Report
cat(sprintf("CITL:  %.3f (95%% CI: %.3f to %.3f)\n", boot_res$t0[1], citl_ci[1], citl_ci[2]))
cat(sprintf("Slope: %.3f (95%% CI: %.3f to %.3f)\n", boot_res$t0[2], slope_ci[1], slope_ci[2]))



# -------------------------------------------------------------------------
# 04. Decile Plot

#Calculate summary stats per decile
calib_bin <- df_model %>%
  mutate(
    y = y_full,
    pred = pred_lasso,
    decile = ntile(pred, 10)  # divide into 10 equal groups
  ) %>%
  group_by(decile) %>%
  summarise(
    N = n(),
    Mean_Pred = mean(pred),
    Observed = mean(y),
    Events = sum(y)
  ) %>%
  rowwise() %>%
  mutate(
    CI = list(binom.confint(Events, N, methods = "wilson"))
  ) %>%
  unnest_wider(CI) %>%
  select(decile, Mean_Pred, Observed, lower, upper) %>%
  mutate(
    Mean_Pred = Mean_Pred * 100,
    Observed = Observed * 100,
    lower = lower * 100,
    upper = upper * 100
  )

ggplot(calib_bin, aes(x = Mean_Pred, y = Observed)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.8, color = "black") +
  geom_line(color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Calibration Curve (Deciles of Predicted Risk)",
    x = "Mean Predicted Mortality (%)",
    y = "Observed Mortality (%)"
  ) +
  coord_equal() +
  theme_minimal(base_size = 14)


# -------------------------------------------------------------------------
# 05. Brier score 
boot_brier <- function(data, indices) {
  d <- data[indices, ]
  y <- y_full
  p <- d$pred_lasso
  p_bar <- mean(y, na.rm = TRUE)
  brier <- mean((y - p)^2, na.rm = TRUE)
  brier_null <- p_bar * (1 - p_bar)
  brier_scaled <- 1 - brier / brier_null
  return(c(brier, brier_scaled))
}
boot_score <- boot(data = df_boot, statistic = boot_brier, R = 100)

# Extract estimates and CIs
brier_est <- boot_score$t0[1]
scaled_est <- boot_score$t0[2]

brier_ci <- quantile(boot_score$t[, 1], c(0.025, 0.975), na.rm = TRUE)
scaled_ci <- quantile(boot_score$t[, 2], c(0.025, 0.975), na.rm = TRUE)

brier_summary <- tibble(
  Metric = c("Brier Score", "Scaled Brier Score"),
  Estimate = c(brier_est, scaled_est),
  CI_Lower = c(brier_ci[1], scaled_ci[1]),
  CI_Upper = c(brier_ci[2], scaled_ci[2])
) %>%
  mutate(across(where(is.numeric), round, 4))

# -------------------------------------------------------------------------
# 06. Risk Stratification
boot_group_rate <- function(data, indices) {
  yb <- as.numeric(as.character(data$icumortality[indices]))
  grpb <- pred_risk_group[indices]
  tapply(yb, grpb, mean, na.rm = TRUE)
}

boot_rates <- boot(data = df_model, statistic = boot_group_rate, R = 1000)
ci_group <- round(100 * apply(boot_rates$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE), 1) # convert to percentage

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
    `95% CI (%)` = paste0(
      sprintf("%.1f", CI_Lower),
      "–",
      sprintf("%.1f", CI_Upper)
    )
  ) %>%
  select(
    `Risk Group`,
    N,
    Deaths,
    `Observed Mortality (%)`,
    `95% CI (%)`
  )

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
  tab_caption("Table 1. Observed Mortality by Risk Group")

#gtsave(table1, filename = "docs/images/calibration_table.png",zoom  = 2) 

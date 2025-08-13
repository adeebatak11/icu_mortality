# -------------------------------------------------------------------------
# 01. Model Comparison Script: GLM vs LASSO
# -------------------------------------------------------------------------
# This script:
#   - Loads results from GLM and LASSO models
#   - Compares AUC-ROC, parsimony, interpretability, and risk group separation
#   - Provides publication-ready visuals
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# 02. Load Model Results
# -------------------------------------------------------------------------

source("scripts/04_run_glm_model.R")
source("scripts/05_run_lasso_model.R")
set.seed(123)
# -------------------------------------------------------------------------
# 03. ROC-AUC Comparison (Bootstrap)
# -------------------------------------------------------------------------

glm_boot_auc <- function(data, indices) {
  d <- data[indices, ]
  fit <- glm(icumortality ~ ., data = d, family = binomial)
  pred <- predict(fit, newdata = d, type = "response")
  roc_d <- roc(d$icumortality, pred, levels = c(0,1), direction = "<")
  as.numeric(auc(roc_d))
}

boot_glm_auc <- boot(data = df_model, statistic = glm_boot_auc, R = 1000)
mean_glm_auc <- mean(boot_glm_auc$t, na.rm = TRUE)
ci_glm <- quantile(boot_glm_auc$t, probs = c(0.025, 0.975), na.rm = TRUE)

lasso_boot_auc <- function(data, indices) {
  d <- data[indices, ]
  X_boot <- model.matrix(icumortality ~ ., data = d)[, -1]
  y_boot <- as.numeric(as.character(d$icumortality))
  fit <- glmnet(X_boot, y_boot, family = "binomial", alpha = 1, lambda = best_lambda)
  pred <- as.vector(predict(fit, newx = X_boot, type = "response"))
  roc_d <- roc(y_boot, pred, levels = c(0,1), direction = "<")
  as.numeric(auc(roc_d))
}

boot_lasso_auc <- boot(data = df_model, statistic = lasso_boot_auc, R = 1000)
mean_lasso_auc <- mean(boot_lasso_auc$t, na.rm = TRUE)
ci_lasso <- quantile(boot_lasso_auc$t, probs = c(0.025, 0.975), na.rm = TRUE)

# -------------------------------------------------------------------------
# 04. Visualize AUC Comparison
# -------------------------------------------------------------------------

auc_plot_df <- data.frame(
  Model = c("GLM", "LASSO"),
  MeanAUC = c(mean_glm_auc, mean_lasso_auc),
  CI_Lower = c(ci_glm[1], ci_lasso[1]),
  CI_Upper = c(ci_glm[2], ci_lasso[2])
) %>% 
  mutate(
    AUC_Label = sprintf("%.3f \u00B1 %.3f", MeanAUC, (CI_Upper - CI_Lower) / 2)
  )

p1 <- ggplot(auc_plot_df, aes(x = Model, y = MeanAUC)) +
  geom_point(size = 4.5, shape = 21, fill = "black", color = "black", stroke = 1.2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.3, linewidth = 1) +
  geom_text(aes(label = AUC_Label), position = position_nudge(x = 0.07), size = 5, fontface = "bold", hjust = 0) +
  labs(
    subtitle = "Model Discrimination (AUC) with Trimmed 90% CIs",
    y = "Area Under ROC Curve (AUC)",
    x = NULL,
    caption = "Estimated via bootstrap resampling (n = 1,000)"
  ) +
  ylim(0.5, 1) +
  theme_bw(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.caption = element_text(size = 11, hjust = 0.5, margin = margin(t = 12)),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    axis.line = element_line(color = "black")
  )
#ggsave("docs/images/auc_ci_comparison.png", plot = p1, width = 6, height = 4, dpi = 300)

# -------------------------------------------------------------------------
# 05. Visualize ROC Curve Comparison
# -------------------------------------------------------------------------

#png("docs/images/roc_plot_comparison.png", width = 800, height = 600)
plot(roc_glm, col = "#1f77b4", lwd = 2, main = "ROC Curves: GLM vs LASSO")
plot(roc_lasso, col = "#ff7f0e", lwd = 2, add = TRUE)

legend("bottomright",
       legend = c(sprintf("GLM (AUC = %.3f)", auc(roc_glm)),
                  sprintf("LASSO (AUC = %.3f)", auc(roc_lasso))),
       col = c("#1f77b4", "#ff7f0e"), lwd = 2, bty = "n")
#dev.off()

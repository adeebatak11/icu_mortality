# -------------------------------------------------------------------------
# 01. Load Model Objects
# -------------------------------------------------------------------------
source("scripts/04_run_model.R")  # Ensure df_model is ready

# -------------------------------------------------------------------------
# 02. Internal Validation via Bootstrapping
# -------------------------------------------------------------------------

## ROC-AUC comparison between GLM and LASSO

## AUC Function for GLM
glm_boot_auc <- function(data, indices) {
  d <- data[indices, ]
  fit <- glm(icumortality ~ ., data = d, family = binomial)
  pred <- predict(fit, newdata = d, type = "response")
  roc_d <- roc(d$icumortality, pred, levels = c(0,1), direction = "<")
  as.numeric(auc(roc_d))
}
# GLM AUC 95% CI
boot_glm_auc <- boot(data = df_model, statistic = glm_boot_auc, R = 1000)
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
boot_lasso_auc <- boot(data = df_model, statistic = lasso_boot_auc, R = 1000)
mean_lasso_auc <- mean(boot_lasso_auc$t, na.rm = TRUE)
ci_lasso_trimmed <- quantile(boot_lasso_auc$t, probs = c(0.05, 0.95), na.rm = TRUE)

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

# Visualize AUC comparison
ggplot(auc_plot_df, aes(x = Model, y = MeanAUC)) +
  geom_point(size = 4.5, shape = 21, fill = "black", color = "black", stroke = 1.2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.3, linewidth = 1) +
  geom_text(aes(label = AUC_Label),
            position = position_nudge(x = 0.07), 
            size = 5, fontface = "bold", hjust = 0) +
  labs(
    subtitle = "Model Discrimination (AUC) with Trimmed 90% CIs",
    y = "Area Under ROC Curve (AUC))",
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
# Visualize ROC-curve comparison
plot(roc_glm, col = "#1f77b4", lwd = 2, main = "ROC Curves: GLM vs LASSO")
plot(roc_lasso, col = "#ff7f0e", lwd = 2, add = TRUE)

legend("bottomright",
       legend = c(sprintf("GLM (AUC = %.3f)", auc(roc_glm)),
                  sprintf("LASSO (AUC = %.3f)", auc(roc_lasso))),
       col = c("#1f77b4", "#ff7f0e"), lwd = 2, bty = "n")


lasso_boot_freq <- function(data, indices) {
  d <- data[indices, ]
  X <- model.matrix(icumortality ~ ., data = d)[, -1]
  y <- as.numeric(as.character(d$icumortality))
  fit <- glmnet(X, y, family = "binomial", lambda = best_lambda)
  coefs <- as.vector(coef(fit))[-1]  # Drop intercept
  as.integer(coefs != 0)  # 1 = selected, 0 = not selected
}

lasso_boot_brier <- function(data, indices) {
  d <- data[indices, ]
  X <- model.matrix(icumortality ~ ., data = d)[, -1]
  y <- as.numeric(as.character(d$icumortality))
  fit <- glmnet(X, y, family = "binomial", lambda = best_lambda)
  pred <- as.vector(predict(fit, newx = X, type = "response"))
  # Ensure y is 0/1 for BrierScore
  y_bin <- ifelse(y %in% c(0, 1), y, NA)
  if (any(is.na(y_bin))) stop("Non-binomial outcome detected in BrierScore calculation.")
  BrierScore(pred, y_bin)
}

lasso_boot_calibration <- function(data, indices) {
  d <- data[indices, ]
  X <- model.matrix(icumortality ~ ., data = d)[, -1]
  y <- as.numeric(as.character(d$icumortality))
  fit <- glmnet(X, y, family = "binomial", lambda = best_lambda)
  pred <- as.vector(predict(fit, newx = X, type = "response"))
  logit_pred <- log(pred / (1 - pred))
  cal_model <- glm(y ~ logit_pred, family = binomial)
  c(intercept = coef(cal_model)[1], slope = coef(cal_model)[2])
}

lasso_boot_risk_groups <- function(data, indices) {
  d <- data[indices, ]
  X <- model.matrix(icumortality ~ ., data = d)[, -1]
  y <- as.numeric(as.character(d$icumortality))
  fit <- glmnet(X, y, family = "binomial", lambda = best_lambda)
  pred <- as.vector(predict(fit, newx = X, type = "response"))
  group <- cut(pred, breaks = c(0, 0.05, 0.20, 1), labels = c("Low", "Moderate", "High"))
  tapply(y, group, mean)  # Mortality rate per group
}

set.seed(101)
boot_brier      <- boot(data = df_model, statistic = lasso_boot_brier,      R = 1000)
boot_freq       <- boot(data = df_model, statistic = lasso_boot_freq,       R = 1000)
boot_cal        <- boot(data = df_model, statistic = lasso_boot_calibration,R = 1000)
boot_riskgroup  <- boot(data = df_model, statistic = lasso_boot_risk_groups,R = 1000)



# Brier Score 95% CI
boot.ci(boot_brier, type = "perc")

# Variable selection frequency
var_names <- colnames(model.matrix(icumortality ~ ., data = df_model)[, -1])
coef_freq <- colMeans(boot_freq$t)
coef_freq_df <- data.frame(Variable = var_names, SelectionRate = coef_freq)

# Calibration slope & intercept summary
calib_df <- as.data.frame(boot_cal$t)
colnames(calib_df) <- c("Intercept", "Slope")
summary(calib_df)

# Risk group mortality CI
# Use trimmed quantiles for CI (e.g., trim 5% from each tail)
group_mortality_df <- as.data.frame(boot_riskgroup$t)
colnames(group_mortality_df) <- c("Low", "Moderate", "High")
trim <- 0.05
ci_low     <- quantile(group_mortality_df$Low, probs = c(trim, 1-trim), na.rm = TRUE)
ci_moderate<- quantile(group_mortality_df$Moderate, probs = c(trim, 1-trim), na.rm = TRUE)
ci_high    <- quantile(group_mortality_df$High, probs = c(trim, 1-trim), na.rm = TRUE)

means <- colMeans(group_mortality_df, na.rm = TRUE)

risk_summary <- data.frame(
  Group       = c("Low", "Moderate", "High"),
  MeanMortality = round(100 * means, 1),
  CI_Lower    = round(100 * c(ci_low[1], ci_moderate[1], ci_high[1]), 1),
  CI_Upper    = round(100 * c(ci_low[2], ci_moderate[2], ci_high[2]), 1)
)

print(risk_summary)

# -------------------------------------------------------------------------
# 03. Compare ROC Curves (Visual)
# -------------------------------------------------------------------------

roc_glm   <- roc(df_model$icumortality, pred_glm)
roc_lasso <- roc(df_model$icumortality, pred_lasso)

plot(roc_glm,   col = "#0072B2", lwd = 2, main = "GLM vs LASSO ROC Curve")
plot(roc_lasso, col = "#D55E00", lwd = 2, add = TRUE)
legend("bottomright", legend = c("GLM", "LASSO"),
       col = c("#0072B2", "#D55E00"), lwd = 2)

# -------------------------------------------------------------------------
# 04. Summarize Key Outputs
# -------------------------------------------------------------------------

auc_glm   <- auc(roc_glm)
auc_lasso <- auc(roc_lasso)

cat(sprintf("GLM AUC:   %.3f\n", auc_glm))
cat(sprintf("LASSO AUC: %.3f\n", auc_lasso))

print(coefs_lasso_df)

# -------------------------------------------------------------------------
# 05. (Optional) Risk Group Table (from earlier)
# -------------------------------------------------------------------------

risk_group <- cut(
  pred_lasso,
  breaks = c(0, 0.05, 0.20, 1),
  labels = c("Low", "Moderate", "High"),
  right = FALSE
)

cat("Risk Group vs Actual Mortality:\n")
print(table(risk_group, df_model$icumortality))

risk_summary <- data.frame(
  group = levels(risk_group),
  n = as.numeric(table(risk_group)),
  deaths = tapply(as.numeric(as.character(df_model$icumortality)), risk_group, sum, na.rm = TRUE),
  observed_mortality = 100 * tapply(as.numeric(as.character(df_model$icumortality)), risk_group, mean, na.rm = TRUE)
)
print(risk_summary)

library(ggplot2)

ggplot(risk_summary, aes(x = Group, y = MeanMortality)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  labs(title = "Observed Mortality by Risk Group",
       y = "Observed Mortality (%)",
       x = "Risk Group") +
  theme_minimal()

source("scripts/04_run_model.R")
# 1. Create decile groups based on predicted probabilities
df_model$decile <- cut(
  pred_lasso,
  breaks = quantile(pred_lasso, probs = seq(0, 1, 0.1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = paste0("D", 1:10)
)

# 2. Calculate mean predicted and observed mortality for each decile
calib_decile <- aggregate(
  cbind(pred = pred_lasso, obs = as.numeric(as.character(df_model$icumortality))) ~ decile,
  data = df_model,
  FUN = mean
)
calib_decile$pred <- calib_decile$pred * 100
calib_decile$obs <- calib_decile$obs * 100

# 3. Plot calibration curve
ggplot(calib_decile, aes(x = pred, y = obs)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Calibration Curve (Deciles of Predicted Risk)",
    x = "Mean Predicted Mortality (%)",
    y = "Observed Mortality (%)"
  ) +
  theme_bw()

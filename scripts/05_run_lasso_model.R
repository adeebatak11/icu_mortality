# -------------------------------------------------------------------------
# 01. Penalized Logistic Regression (LASSO) Script
# -------------------------------------------------------------------------
# This script:
#   - Loads selected features and modeling dataset
#   - Fits a penalized logistic regression (LASSO) for ICU mortality
#   - Selects features via regularization
#   - Predicts probabilities and assigns risk groups
#   - Summarizes mortality by risk group
#   - Evaluates model performance with ROC curve
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# 02. Load Data and Feature Engineering
# -------------------------------------------------------------------------

source("scripts/03_feature_selection.R")

# -------------------------------------------------------------------------
# 03. Fit Penalized Logistic Regression (LASSO)
# -------------------------------------------------------------------------

set.seed(42)

# Prepare design matrix for glmnet
X_full <- model.matrix(icumortality ~ ., data = df_model)[, -1]
y_full <- as.numeric(as.character(df_model$icumortality))

# Cross-validated LASSO (alpha = 1)
cvfit <- cv.glmnet(
  X_full,
  y_full,
  family = "binomial",
  alpha = 1,
  nfolds = 10,
  type.measure = "deviance"
)
best_lambda <- cvfit$lambda.min

# -------------------------------------------------------------------------
# 04. Extract Nonzero Coefficients
# -------------------------------------------------------------------------

coefs_lasso <- coef(cvfit, s = "lambda.min")
coefs_lasso <- data.frame(
  Variable    = rownames(coefs_lasso)[which(as.numeric(coefs_lasso) != 0)],
  Coefficient = as.numeric(coefs_lasso)[which(as.numeric(coefs_lasso) != 0)]
)

# -------------------------------------------------------------------------
# 05. Predict Probabilities and Assign Risk Groups
# -------------------------------------------------------------------------

pred_lasso <- as.vector(predict(cvfit, newx = X_full, s = "lambda.min", type = "response"))
roc_lasso <- roc(
  response = y_full,
  predictor = pred_lasso,
  levels = c(0, 1),
  direction = "<"
)

pred_risk_group <- cut(
  pred_lasso,
  breaks = c(0, 0.05, 0.20, 1),
  labels = c("Low", "Moderate", "High")
)
group_sizes <- as.numeric(table(pred_risk_group))
group_deaths <- tapply(as.numeric(as.character(df_model$icumortality)), pred_risk_group, sum, na.rm = TRUE)
group_rates <- 100 * tapply(as.numeric(as.character(df_model$icumortality)), pred_risk_group, mean, na.rm = TRUE)

# -------------------------------------------------------------------------
# 01. Load Data and Feature Engineering
# -------------------------------------------------------------------------

source("scripts/03_feature_selection.R")  # Ensure df_model is ready

# -------------------------------------------------------------------------
# 02. Standard Logistic Regression (Full Model)
# -------------------------------------------------------------------------

# Fit full logistic regression model
formula_glm <- icumortality ~ .
model_glm   <- glm(formula_glm, data = df_model, family = binomial)

# Predict probabilities
pred_glm <- predict(model_glm, type = "response")

roc_glm <- roc(response = df_model$icumortality, predictor = pred_glm, levels = c(0,1), direction = "<")
# -------------------------------------------------------------------------
# 03. Penalized Logistic Regression (LASSO)
# -------------------------------------------------------------------------

set.seed(42)

# Prepare design matrix
X_full <- model.matrix(icumortality ~ ., data = df_model)[, -1]
y_full <- as.numeric(as.character(df_model$icumortality))

# Cross-validated LASSO (alpha = 1 = LASSO)
cvfit <- cv.glmnet(X_full, y_full, family = "binomial", alpha = 1, nfolds = 10, type.measure = "deviance")
best_lambda <- cvfit$lambda.min

# Get nonzero coefficients
coefs_lasso <- coef(cvfit, s = "lambda.min")
coefs_lasso_df <- data.frame(
  Variable    = rownames(coefs_lasso)[which(as.numeric(coefs_lasso) != 0)],
  Coefficient = as.numeric(coefs_lasso)[which(as.numeric(coefs_lasso) != 0)]
)

# Predict with LASSO
pred_lasso <- as.vector(predict(cvfit, newx = X_full, s = "lambda.min", type = "response"))
roc_lasso <- roc(response = y_full, predictor = pred_lasso, levels = c(0,1), direction = "<")

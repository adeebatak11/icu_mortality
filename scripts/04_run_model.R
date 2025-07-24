# -------------------------------------------------------------------------
# 01. Feature Selection & Modeling: GLM vs LASSO
# -------------------------------------------------------------------------

source("scripts/03_feature_selection.R")

# -------------------------------------------------------------------------
# 02. Standard Logistic Regression (Full Model)
# -------------------------------------------------------------------------

formula_glm <- icumortality ~ .
model_glm <- glm(formula_glm, data = df_model, family = binomial)
# summary(model_glm)   # Too many non-significant, high SE predictors. See model output.

# Predict risk for each patient
pred_glm <- predict(model_glm, type = "response")

# Calculate ROC and AUC for standard GLM
roc_glm <- roc(df_model$icumortality, pred_glm)
auc_glm <- auc(roc_glm)

# -------------------------------------------------------------------------
# 03. Penalized Logistic Regression (LASSO)
# -------------------------------------------------------------------------

set.seed(42)  # Reproducibility for CV splits

# Prepare design matrix (removes intercept)
X <- model.matrix(icumortality ~ ., data = df_model)[, -1]
y <- as.numeric(as.character(df_model$icumortality)) # Should be 0/1

# Fit LASSO with cross-validation & weights
lasso_model<- cv.glmnet(X, y, family = "binomial", type.measure="deviance", alpha = 0.5)
plot(lasso_model) # Binomial deviance vs lambda

# Extract nonzero coefficients (selected variables)
coefs <- coef(lasso_model, s = "lambda.min")
coefs_lasso <- data.frame(
  Variable    = rownames(coefs)[which(as.numeric(coefs) != 0)],
  Coefficient = as.numeric(coefs)[which(as.numeric(coefs) != 0)]
)

print(coefs_lasso)

# Predict probability of ICU mortality
pred_lasso <- as.vector(predict(lasso_model, newx = X, s = "lambda.min", type = "response"))

# -------------------------------------------------------------------------
# 04. Risk Stratification: Low, Moderate, High
# -------------------------------------------------------------------------

# Risk group cutoffs (<5%, 5–20%, ≥20%)
risk_group <- cut(
  pred_lasso,
  breaks = c(0, 0.05, 0.20, 1),
  labels = c("Low", "Moderate", "High"),
  right = FALSE
)


# -------------------------------------------------------------------------
# 05. LASSO Model Performance 
# -------------------------------------------------------------------------

## 5.1: Internal Validity: Bootstrap Resampling for Full Internal Validation

## 5.2: Discrimination: AUC-ROC, AUC-PR
roc_lasso <- roc(y, pred_lasso)
plot(roc_lasso, main = "LASSO ROC Curve")
auc_lasso <- auc(roc_lasso)
print(auc_lasso)

# 5.3: Calibration: Brier Score, Calibration Curve

# 5.4: Clinical Utility (Risk Tier Validation)
risk_summary <- tibble(
  risk_group = risk_group,
  icumortality = df_model$icumortality
) %>%
  group_by(risk_group) %>%
  summarise(
    n = n(),
    deaths = sum(as.numeric(as.character(icumortality))),
    obs_mortality_perc = 100 * mean(as.numeric(as.character(icumortality)))
  )


print(risk_summary)
# -------------------------------------------------------------------------
# 06. Compare "simple" logistic Model vs LASSO
# -------------------------------------------------------------------------
# If you want to compare ROC curves:
 plot(roc_glm, col = "blue", main = "GLM (blue) vs LASSO (red)")
 plot(roc_lasso, col = "red", add = TRUE)
 legend("bottomright", legend = c("GLM", "LASSO"), col = c("blue", "red"), lwd = 2)
 
 
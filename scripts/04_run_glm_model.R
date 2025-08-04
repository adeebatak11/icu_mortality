
# -------------------------------------------------------------------------
# 01. Logistic Regression Modeling Script
# -------------------------------------------------------------------------
# This script:
#   - Loads selected features and modeling dataset
#   - Fits a standard logistic regression (GLM) for ICU mortality
#   - Predicts probabilities and assigns risk groups
#   - Summarizes mortality by risk group
#   - Evaluates model performance with ROC curve
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# 02. Load Data and Feature Engineering
# -------------------------------------------------------------------------

source("scripts/03_feature_selection.R")

# -------------------------------------------------------------------------
# 03. Fit Standard Logistic Regression Model
# -------------------------------------------------------------------------

formula_glm <- icumortality ~ .
model_glm   <- glm(formula_glm, data = df_model, family = binomial)

# -------------------------------------------------------------------------
# 04. Predict Probabilities and Assign Risk Groups
# -------------------------------------------------------------------------

pred_glm <- predict(model_glm, type = "response")

risk_group_glm <- cut(
  pred_glm,
  breaks = c(0, 0.05, 0.20, 1),
  labels = c("Low", "Moderate", "High"),
  right = FALSE
)

# -------------------------------------------------------------------------
# 05. Summarize Mortality by Risk Group
# -------------------------------------------------------------------------

group_summary_glm <- df_model %>%
  mutate(
    risk_group = risk_group_glm,
    y = as.numeric(as.character(icumortality))
  ) %>%
  group_by(risk_group) %>%
  summarise(
    N = n(),
    Deaths = sum(y),
    ObservedMortality = round(100 * mean(y), 1)
  )

# -------------------------------------------------------------------------
# 06. ROC Curve for Model Performance
# -------------------------------------------------------------------------

roc_glm <- roc(
  response = df_model$icumortality,
  predictor = pred_glm,
  levels = c(0, 1),
  direction = "<"
)

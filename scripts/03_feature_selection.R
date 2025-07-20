source("scripts/02_clean_data.R")

str(df_model)
formula1 <- icumortality ~ .-patientunitstayid

## The dataset to be modelled has 18 predictors and 43 predictor parameters
model1 <- glm(formula1, data = df_model, family = binomial)
summary(model1)

df_lasso <- df_model %>% select(-patientunitstayid)
df_lasso$icumortality <- as.numeric(as.character(df_lasso$icumortality))
x <- model.matrix(icumortality ~ . , data = df_lasso)[, -1]
y <- df_lasso$icumortality
set.seed(123)  # For reproducibility

cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1) # alpha = 1 is LASSO

# Plot cross-validation results
plot(cvfit)
pred_lasso <- predict(cvfit, newx = x, s = "lambda.min", type = "response")

# AUC (use pROC or ROCR as before)
library(pROC)
roc_lasso <- roc(y, as.numeric(pred_lasso))
auc(roc_lasso)

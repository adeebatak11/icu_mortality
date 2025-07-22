source("scripts/03_feature_selection.R")

# Model 1: the most simple logistic regression model
formula1 <- icumortality ~ .
model1 <- glm(formula1, data = df_model, family = binomial)
summary(model1) # too many variables with non-significant p-values, high standard errors, and/or sparse predictors.

# Model 2: Penalized logistic regression with LASSO
set.seed(42) # For reproducibility
X <- model.matrix(icumortality ~ ., data = df_model)[, -1] # Remove intercept
y <- as.numeric(as.character(df_model$icumortality)) # Should be 0/1

cvfit <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 10)  # alpha=1 is LASSO
plot(cvfit)

#Get the non-zero ceoffs from lasso reg
coefs_lasso <- coef(cvfit, s = "lambda.min") 
coefs_lasso <- data.frame(
  Variable = rownames(coefs)[which(as.numeric(coefs) != 0)],
  Coefficient = as.numeric(coefs)[which(as.numeric(coefs) != 0)]
)

pred_lasso <- as.vector(predict(cvfit, newx = X, s = "lambda.min", type = "response"))
roc_obj_lasso <- roc(y, pred_lasso)
plot(roc_obj_lasso, print.auc = TRUE, main = "LASSO ROC Curve")

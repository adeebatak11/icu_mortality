source("scripts/02_clean_data.R")

# the most simple logistic regression model
formula1 <- icumortality ~ .
model1 <- glm(formula1, data = df_model, family = binomial)
summary(model1)
pred1 <- predict(model1, type = "response")
roc_obj <- roc(df_model$icumortality, pred1)

# Get best threshold by Youden's J
best_coords <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE)
best_cutoff <- best_coords["threshold"]

# Plot ROC with extras
plot(
  roc_obj,
  print.auc = TRUE,      
  col = "#0072B2",       
  lwd = 3,               
  main = "ROC Curve for ICU Mortality Model",
  xlab = "1 - Specificity (False Positive Rate)",
  ylab = "Sensitivity (True Positive Rate)",
  legacy.axes = FALSE
)
abline(a = 0, b = 1, lty = 2, col = "grey70") # Add diagonal (random classifier)

# Add the best threshold as a point
points(1 - best_coords["specificity"], best_coords["sensitivity"], 
       col = "red", pch = 19, cex = 1.3)
text(1 - best_coords["specificity"], best_coords["sensitivity"],
     labels = sprintf("Best cutoff: %.2f", best_coords["threshold"]),
     pos = 4, col = "red")

# using best cutoff for identifying high-risk patients
# Classify as 1 (predicted death) if probability > best_cutoff
pred_class <- ifelse(pred1 > best_cutoff, 1, 0)

source("scripts/02_clean_data.R")

str(df_model)

## The dataset to be modelled has 18 predictors and 43 predictor parameters
model1 <- glm(true_mortality ~ . - patientunitstayid - admitdiagnosis - apacheadmissiondx , data = df_model, family = binomial)
summary(model1)

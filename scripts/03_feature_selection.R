source("scripts/02_clean_data.R")


model1 <- glm(true_mortality ~ . - patientunitstayid - admitdiagnosis - apacheadmissiondx , data = df_model, family = binomial)
summary(model1)

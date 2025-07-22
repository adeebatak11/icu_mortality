# -------------------------------------------------------------------------
# 01. Minimum sample size calculation for developing a multivariable prediction
# model. Based on: 1. Methods from Riley et al. (2019, Stat Med 38:1276-1296) 2.
# Model parameters (R^2, event rate) taken from APACHE IV validation set
# (Zimmerman et al., 2006)
# -------------------------------------------------------------------------


## ALos using dtaa from Zimmerman, APACHE IV paper.

n_val <- 44288
true_outcome_prop <- 0.135
E_val <- 44288*true_outcome_prop
R_sq_adj <- 0.44
S_vh <- 0.9

n_df <- 1718
E_df <- 79
## p1 = 44, n_min = 3222, p2 = 2270
p <- 23
MOE <- 0.05

## Eqn. 12
log_lik_L0_val <- E_val*log(E_val/n_val) + (n_val-E_val)*log(1-(E_val/n_val))

## Eqn. 15
R_sq_cs_app <- R_sq_adj*(1-exp(2*log_lik_L0_val/n_val))

## Eqn. 8
R_sq_cs_adj <- S_vh*R_sq_cs_app

## Eqn 11
n_1 <- p/((S_vh-1)*log(1-(R_sq_cs_adj/S_vh)))

## Eqn 12
log_lik_L0_df <- E_df*log(E_df/n_df) + (n_df-E_df)*log(1-(E_df/n_df))

## Eqn 23
max_R_sq_cs_app_df <- 1-exp(2*log_lik_L0_df/n_df)

## Eqn 26
S_vh_min <- R_sq_cs_adj/(R_sq_cs_adj + MOE*R_sq_cs_app)
S_vh_min >= S_vh

## Redo Eqn 11
n_2 <- p/((S_vh_min-1)*log(1-(R_sq_cs_adj/S_vh_min)))

## Eqn 27
n_3 <- ((1.96/MOE)^2)*true_outcome_prop*(1-true_outcome_prop)
print(max(n_1, n_2, n_3))

# -------------------------------------------------------------------------
# 02. Plotting key figures
# -------------------------------------------------------------------------
source("scripts/04_run_model.R")

# Correlation heatmap
df_num <- dummy_cols(df_model, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
df_num <- df_num %>% select(where(is.numeric))
cor_matrix_full <- cor(df_num, use = "pairwise.complete.obs")
corrplot(cor_matrix_full, method = "color", tl.cex=0.6)


# prediction cutoff tradeoff table 
cutoffs <- c(0.01, 0.025, youden_cutoff, 0.1, 0.2)
results <- data.frame(
  Cutoff = cutoffs,
  Predicted_High_Risk = NA,
  True_Deaths_Captured = NA,
  Sensitivity = NA,
  Specificity = NA
)

for (i in seq_along(cutoffs)) {
  threshold <- cutoffs[i]
  predicted <- ifelse(pred_lasso > threshold, 1, 0)
  cm <- table(Predicted = predicted, Actual = df_model$icumortality)
  # Extract confusion matrix elements
  TP <- cm["1", "1"]
  TN <- cm["0", "0"]
  FP <- cm["1", "0"]
  FN <- cm["0", "1"]
  # Fill in the results
  results$Predicted_High_Risk[i] <- sum(predicted)
  results$True_Deaths_Captured[i] <- TP
  results$Sensitivity[i] <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  results$Specificity[i] <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
}
print(results)

# -----------------------------------------------------------------------------
# 01. Minimum Sample Size Calculation for Multivariable Prediction Model
# -----------------------------------------------------------------------------
# Based on:
#   1. Riley et al. (2019, Stat Med 38:1276-1296)
#   2. APACHE IV validation set (Zimmerman et al., 2006)
# -----------------------------------------------------------------------------


# --- Parameters from APACHE IV paper ---


# Validation cohort
n_val <- 44288
true_outcome_prop <- 0.135
E_val <- n_val * true_outcome_prop
R_sq_adj <- 0.44
S_vh <- 0.9


# Development cohort
n_df <- 1718
E_df <- 79
# p1 = 44, n_min = 3222, p2 = 2270
p <- 23
MOE <- 0.05


# --- Sample size equations ---
# Eqn 12: Log-likelihood null model (validation)
log_lik_L0_val <- E_val * log(E_val / n_val) + (n_val - E_val) * log(1 - (E_val / n_val))

## Eqn. 15
R_sq_cs_app <- R_sq_adj*(1-exp(2*log_lik_L0_val/n_val))

# Eqn 8: Adjusted Cox-Snell R^2
R_sq_cs_adj <- S_vh * R_sq_cs_app

# Eqn 11: Minimum sample size (validation)
n_1 <- p / ((S_vh - 1) * log(1 - (R_sq_cs_adj / S_vh)))

# Eqn 12: Log-likelihood null model (development)
log_lik_L0_df <- E_df * log(E_df / n_df) + (n_df - E_df) * log(1 - (E_df / n_df))

# Eqn 23: Max apparent Cox-Snell R^2 (development)
max_R_sq_cs_app_df <- 1 - exp(2 * log_lik_L0_df / n_df)

# Eqn 26: Minimum shrinkage
S_vh_min <- R_sq_cs_adj / (R_sq_cs_adj + MOE * R_sq_cs_app)
S_vh_min >= S_vh

# Eqn 11 (recalculated with S_vh_min)
n_2 <- p / ((S_vh_min - 1) * log(1 - (R_sq_cs_adj / S_vh_min)))

# Eqn 27: Precision for outcome proportion
n_3 <- ((1.96 / MOE)^2) * true_outcome_prop * (1 - true_outcome_prop)

# Print required sample size
cat(sprintf("Required sample size: %.0f\n", max(n_1, n_2, n_3)))

# -----------------------------------------------------------------------------
# 02. Plotting Key Figures
# -----------------------------------------------------------------------------
source("scripts/03_feature_selection.R")  # Loads df_model and features

# Fit regression tree (max depth = 3)
tree_fit <- rpart(
  icumortality ~ ., 
  data = df_model, 
  method = "class", 
  control = rpart.control(
    cp = 0.001,        # Easier tree growth
    minsplit = 10,     # Minimum obs to consider a split
    minbucket = 5,     # Minimum obs in a terminal leaf
    maxdepth = 3       # Keep tree shallow
  )
)

# Plot regression tree
# png("docs/images/regression_tree_plot.png", width = 6, height = 8.5, units = "in", res = 300)
par(mar = c(1, 1, 2, 1))
rpart.plot(
  tree_fit,
  main = "Regression Tree for ICU Mortality Risk Prediction",
  box.palette = "Reds",
  type = 3,
  extra = 101,
  under = TRUE,
  cex = 0.8,         # Node label size
  tweak = 1.2        # Split text size
)
box(lwd = 2)
# dev.off()
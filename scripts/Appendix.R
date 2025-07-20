# -------------------------------------------------------------------------
# Minimum sample size calculation for developing a multivariable prediction
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

      
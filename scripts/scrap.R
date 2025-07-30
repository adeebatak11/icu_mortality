library(dcurves)
library(rmda)

dca_df <- df_model %>%
  mutate(
    risk = pred_lasso,
    outcome = as.numeric(as.character(icumortality))
  )
stopifnot(all(!is.na(dca_df$outcome)),
          all(!is.na(dca_df$risk)),
          all(dca_df$risk >= 0 & dca_df$risk <= 1))

pop_prev <- mean(dca_df$outcome)  # replace with true population prevalence if known


dca_rmda <- rmda::decision_curve(
  outcome ~ risk,
  data        = dca_df,
  thresholds  = seq(0.01, 0.50, by = 0.01),
  bootstraps  = 200,
  study.design= "cohort",
  policy      = "opt-in"
)

p <- plot_decision_curve(
  dca_rmda,
  curve.names = "LASSO",
  xlab = "Threshold Probability",
  ylab = "Net Benefit",
  legend.position = "bottomright",
  confidence.intervals = TRUE,   # shown if bootstraps were computed
  standardize = FALSE
)
p #figure out a way to add lines at 5% and 20%

library(purrr)
library(ggplot2)

# Cohort NB formulas
net_benefit_tbl <- function(y, p, thresholds) {
  stopifnot(length(y) == length(p), all(p >= 0 & p <= 1), all(y %in% c(0,1)))
  n <- length(y)
  prev <- mean(y)
  
  map_dfr(thresholds, function(t) {
    pred <- p >= t
    tp <- sum(pred & y == 1)
    fp <- sum(pred & y == 0)
    nb_model <- (tp/n) - (fp/n) * (t/(1 - t))
    nb_all   <- prev - (1 - prev) * (t/(1 - t))
    tibble(
      threshold   = t,
      net_benefit = nb_model,
      all         = nb_all,
      none        = 0,
      ia_per_100  = (nb_model - nb_all) / (t/(1 - t)) * 100  # interventions avoided per 100
    )
  })
}

y <- dca_df$outcome
p <- dca_df$risk
ths <- seq(0.01, 0.50, by = 0.01)

dca_tbl <- net_benefit_tbl(y, p, ths)

B <- 100  # use 500–1000 for final results
n <- length(y)

nb_mat <- sapply(1:B, function(b) {
  idx <- sample.int(n, replace = TRUE)
  net_benefit_tbl(y[idx], p[idx], ths)$net_benefit
})
ci_lo <- apply(nb_mat, 1, quantile, 0.025)
ci_hi <- apply(nb_mat, 1, quantile, 0.975)

dca_tbl <- dca_tbl %>%
  mutate(nb_lo = ci_lo, nb_hi = ci_hi)

ggplot(dca_tbl, aes(threshold, net_benefit)) +
  geom_ribbon(aes(ymin = nb_lo, ymax = nb_hi), alpha = 0.15) +
  geom_line(linewidth = 1.1) +
  geom_line(aes(y = all), linetype = 2) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(title = "Decision Curve Analysis (LASSO, cohort)",
       x = "Threshold Probability", y = "Net Benefit") +
  theme_minimal(base_size = 14)

useful <- dca_tbl %>%
  filter(net_benefit > 0, net_benefit > all, net_benefit > none)

useful_range <- range(useful$threshold)   # where model beats treat-all/none
peak <- useful %>% slice_max(net_benefit, n = 1) %>% select(threshold, net_benefit)

useful_range
peak
dca_tbl %>%
  +     slice(which.min(abs(threshold - 0.10))) %>%
  +     select(threshold, net_benefit, all, none, ia_per_100, nb_lo, nb_hi)

#Decision curve analysis indicated that the LASSO model provided higher net
#benefit than treating all or treating none across thresholds from 1% to 50%. At
#a clinically relevant threshold of 10%, the model’s net benefit was 0.016 (95%
#CI 0.010–0.023), equivalent to 1.6 net true-positives per 100 patients, and
#would avoid approximately 69 interventions per 100 patients compared with a
#treat-all strategy.

report_thresholds <- function(tbl, t_vec = c(0.05, 0.20)) {
  purrr::map_dfr(t_vec, function(tstar) {
    row <- tbl[which.min(abs(tbl$threshold - tstar)), ]
    tibble(
      threshold = tstar,
      nb_model  = row$net_benefit,
      nb_lo     = row$nb_lo %||% NA_real_,
      nb_hi     = row$nb_hi %||% NA_real_,
      nb_all    = row$all,
      nb_none   = row$none,
      ia_per_100= row$ia_per_100,
      flagged_pct = mean(dca_df$risk >= tstar) * 100
    )
  })
}

res_nb <- report_thresholds(dca_tbl, c(0.05, 0.20))
res_nb


group_summary

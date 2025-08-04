# -------------------------------------------------------------------------
# 01. Feature Selection Script
# -------------------------------------------------------------------------
# This script:
#   - Loads cleaned and engineered data
#   - Selects final features for modeling
# -------------------------------------------------------------------------

source("scripts/02_clean_data.R")

# Pull patientunitstayid before final variable selection for modeling
patientid <- pull(df_model, patientunitstayid)

# Select modeling features
df_model <- df_model %>%
  select(
    icumortality,
    aps,
    age,
    gender,
    admitdx_grouped,
    hepaticfailure,
    metastaticcancer,
    leukemia,
    immunosuppression,
    cirrhosis,
    diabetes,
    thrombolytics,
    ima,
    readmit,
    log_icu_hours
  )

cat("Feature selection complete. Rows:", nrow(df_model), "Cols:", ncol(df_model), "\n")

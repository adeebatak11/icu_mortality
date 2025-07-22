source("scripts/02_clean_data.R")

# -------------------------------------------------------------------------
# Pull patientunitstayid before final select()
# -------------------------------------------------------------------------
patientid <- pull(df_model, patientunitstayid)

# features selection
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

# -------------------------------------------------------------------------
# 01. Load Data & Dependencies
# -------------------------------------------------------------------------

source("scripts/01_load_data.R")

# -------------------------------------------------------------------------
# 02. Restrict to APACHE IVa Patients Only (standardizes cohort)
# -------------------------------------------------------------------------

p_results_IVa <- p_results %>% 
  filter(apacheversion == "IVa")

# -------------------------------------------------------------------------
# 03. Filter Demographics Table to Analysis Cohort
# -------------------------------------------------------------------------

p_demographics <- p_demographics %>% 
  filter(
    patientunitstayid %in% p_results_IVa$patientunitstayid,
    age >= 18,                        # Exclude peds
    icu_los_hours >= 4,               # Exclude ultra-short ICU stays
    hosp_to_icu_admit_hours >= 0      # Data sanity check
  ) %>%
  select(patientunitstayid, hosp_to_icu_admit_hours)

# -------------------------------------------------------------------------
# 04. Restrict All Data Tables to Analysis Cohort
# -------------------------------------------------------------------------

p_results_IVa <- p_results_IVa %>% filter(patientunitstayid %in% pull(p_demographics, patientunitstayid))
apache_var <- apache_var %>% filter(patientunitstayid %in% pull(p_demographics, patientunitstayid))

# -------------------------------------------------------------------------
# 05. Build Initial Modeling Dataset (merge all sources)
# -------------------------------------------------------------------------

df_model <- apache_var %>%
  mutate(
    aps = p_results_IVa$acutephysiologyscore, # Add APS score
    icumortality = p_results_IVa$actualicumortality      # Add ICU mortality
  ) %>%
  left_join(p_demographics, by = "patientunitstayid")

# -------------------------------------------------------------------------
# 06. Clean Diagnosis (missing to NA, prep for grouping)
# -------------------------------------------------------------------------

df_model$admitdiagnosis[trimws(df_model$admitdiagnosis) == ""] <- NA

# -------------------------------------------------------------------------
# 07. Finalize Feature Engineering & Variable Formatting
# -------------------------------------------------------------------------

df_model <- df_model %>%
  filter(aps >= 0) %>%
  mutate(
    icumortality = case_when(
      icumortality == "ALIVE"   ~ 0,
      icumortality == "EXPIRED" ~ 1
    ),
    # MI subgroup: only define amilocation if recent MI
    amilocation = case_when(
      midur == 0 ~ NA_integer_,
      midur == 1 ~ amilocation
    ),
    # Data-driven diagnosis grouping, then custom clinical collapsing
    admitdx_grouped = fct_lump_min(as.factor(admitdiagnosis), min = 11) %>%
      fct_explicit_na(na_level = "Other") %>%
      fct_collapse(
        "Drug/Alcohol Overdose/Withdrawal" = c("ODALCOH", "ODOTHER", "ODSEDHYP", "ODSTREET", "DRUGWITHD"),
        "GI Bleeding" = c("LOWGIBLEED", "UGIBLEED", "UNKGIBLEED"),
        "Sepsis" = c("SEPSISCUT", "SEPSISGI", "SEPSISPULM", "SEPSISUNK", "SEPSISUTI"),
        "Cardiac Arrhythmia" = c("RHYTHATR", "RHYTHCON", "RHYTHVEN"),
        "Pneumonia" = c("PNEUMASPIR", "PNEUMBACT"),
        "Acute Cardiac Event" = c("CARDARREST", "AMI", "UNSTANGINA"),
        "GI Surg Obstruct/Perf" = c("S-GIOBSTRX", "S-GIPERFOR")
      ),
    # Ensure all binary and categorical predictors are factors
    gender             = as.factor(gender),
    hepaticfailure     = as.factor(hepaticfailure),
    lymphoma           = as.factor(lymphoma),
    metastaticcancer   = as.factor(metastaticcancer),
    leukemia           = as.factor(leukemia),
    immunosuppression  = as.factor(immunosuppression),
    cirrhosis          = as.factor(cirrhosis),
    diabetes           = as.factor(diabetes),
    thrombolytics      = as.factor(thrombolytics),
    ima                = as.factor(ima),
    amilocation        = as.factor(amilocation),
    midur              = as.factor(midur),
    readmit            = as.factor(readmit),
    icumortality     = as.factor(icumortality)
  ) %>%
  select(
    patientunitstayid,
    icumortality,
    aps,
    age,
    gender,
    admitdx_grouped,
    hepaticfailure,
    lymphoma,
    metastaticcancer,
    leukemia,
    immunosuppression,
    cirrhosis,
    diabetes,
    thrombolytics,
    ima,
    midur,
    readmit,
    hosp_to_icu_admit_hours
  )
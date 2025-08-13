# -------------------------------------------------------------------------
# 01. Data Cleaning and Cohort Restriction Script
# -------------------------------------------------------------------------
# This script:
#   - Loads raw data tables and dependencies
#   - Restricts cohort to APACHE IVa adult ICU stays
#   - Cleans and merges demographic and clinical data
#   - Finalizes feature engineering and variable formatting
# -------------------------------------------------------------------------

source("scripts/01_data_load_and_connect.R")

# --- Restrict to APACHE IVa Patients Only (standardizes cohort) ---
p_results <- p_results %>% 
  filter(apacheversion == "IVa")

# --- Filter Demographics Table to Analysis Cohort ---
p_demographics <- p_demographics %>%
  mutate(
    age = trimws(age),
    age = case_when(
      age == "> 89" ~ 90,
      age == ""     ~ NA_real_,
      str_detect(age, "^\\d+$") ~ as.numeric(age),
      TRUE ~ NA_real_
    )
  )  %>% 
  filter(
    patientunitstayid %in% p_results$patientunitstayid,
    age >= 18,                        # Exclude pediatric patients
    icu_los_hours >= 4,               # Exclude ultra-short ICU stays
    hosp_to_icu_admit_hours >= 0      # Data sanity check
  ) %>%
  select(patientunitstayid, hosp_to_icu_admit_hours)

# --- Restrict All Data Tables to Analysis Cohort ---
p_results <- p_results %>% filter(patientunitstayid %in% pull(p_demographics, patientunitstayid))
apache_var <- apache_var %>% filter(patientunitstayid %in% pull(p_demographics, patientunitstayid))

# --- Build Initial Modeling Dataset (merge all columns) ---
df_model <- apache_var %>%
  mutate(
    aps = p_results$acutephysiologyscore, # Add APS score
    icumortality = p_results$actualicumortality      # Add ICU mortality
  ) %>%
  left_join(p_demographics, by = "patientunitstayid")

# --- Clean Diagnosis (missing to NA, prep for grouping) ---
df_model$admitdiagnosis[trimws(df_model$admitdiagnosis) == ""] <- NA

# --- Finalize Feature Engineering & Variable Formatting ---
df_model <- df_model %>%
  filter(aps >= 0,
         age >= 18) %>%
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
    admitdx_grouped = fct_collapse(
      as.factor(admitdiagnosis),
      "Drug/Alcohol Overdose/Withdrawal" = c("ODALCOH", "ODOTHER", "ODSEDHYP", "ODSTREET", "DRUGWITHD"),
      "GI Bleeding" = c("LOWGIBLEED", "UGIBLEED", "UNKGIBLEED"),
      "Sepsis" = c("SEPSISCUT", "SEPSISGI", "SEPSISPULM", "SEPSISUNK", "SEPSISUTI"),
      "Cardiac Arrhythmia" = c("RHYTHATR", "RHYTHCON", "RHYTHVEN"),
      "Pneumonia" = c("PNEUMASPIR", "PNEUMBACT"),
      "Acute Cardiac Event" = c("CARDARREST", "AMI", "UNSTANGINA"),
      "GI Surg Obstruct/Perf" = c("S-GIOBSTRX", "S-GIPERFOR")
    ) %>%
    # lump all remaining rare categories
    fct_lump_min(min = 41, other_level = "Other") %>%
    fct_na_value_to_level(level = "Other"),    
    # continuous variable hosp_to_icu_admit_hours is log transformed for right skewness.
    # log1p = log(1+x), to handle zeroes. 
    log_icu_hours = log1p(hosp_to_icu_admit_hours),
    
    # Ensure all binary and categorical predictors are factors
    gender             = as.factor(gender),
    hepaticfailure     = as.factor(hepaticfailure),
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
    icumortality       = as.factor(icumortality)
  )

# --- Print summary of final cohort ---
cat("Final cohort size:", nrow(df_model), "\n")

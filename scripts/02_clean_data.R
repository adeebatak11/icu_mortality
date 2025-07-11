# Load DB connection and libraries
source("scripts/01_load_data.R")

p_results_IVa <- p_results %>% filter(apacheversion == "IVa")

p_demographics <- p_demographics %>% 
  filter(patientunitstayid %in% p_results_IVa$patientunitstayid,
         age >= 18,
         icu_los_hours >= 4,
         hosp_to_icu_admit_hours >= 0,
         ) %>% 
  select(patientunitstayid, apacheadmissiondx, hosp_to_icu_admit_hours) 

vec_patientunitstayid <- p_demographics %>% 
  pull(patientunitstayid)

p_results_IVa <- p_results_IVa %>% filter(patientunitstayid %in% vec_patientunitstayid)

## Outcome Variable (trueicumortality)
true_icu_mortality <- p_results_IVa %>% 
  mutate(icumortality = case_when(
    actualicumortality == "ALIVE" ~ 0,
    actualicumortality == "EXPIRED" ~ 1,
  )) %>% 
  select(patientunitstayid, icumortality)

## restrict apache var dataset to only those patients for whom we have a (true) outcome variable
apache_var <- apache_var %>% filter(patientunitstayid %in% vec_patientunitstayid)

df_model <- apache_var %>% 
  mutate(aps = p_results_IVa$acutephysiologyscore,
         true_mortality = true_icu_mortality$icumortality) %>% 
  left_join(p_demographics, by = "patientunitstayid") 

#aps can't be negative
#gender: Female =1, Male = 0 
df_model <- df_model %>%
  filter(aps >= 0) %>% 
  mutate(amilocation = case_when(
    midur == 0 ~ NA_integer_, #NA when no MI in the past 6 months
    midur == 1 ~ amilocation    
  )) %>% 	#Binary? → Factor, Multi-category? → Factor, Continuous/Score/Measurement? → Numeric.
  mutate(
    gender = as.factor(gender),
    hepaticfailure = as.factor(hepaticfailure),
    lymphoma = as.factor(lymphoma),
    metastaticcancer = as.factor(metastaticcancer),
    leukemia = as.factor(leukemia),
    immunosuppression = as.factor(immunosuppression),
    cirrhosis = as.factor(cirrhosis),
    diabetes = as.factor(diabetes),
    graftcount = as.numeric(graftcount),  
    thrombolytics = as.factor(thrombolytics),
    ima = as.factor(ima),
    amilocation = as.factor(amilocation),
    midur = as.factor(midur),
    readmit = as.factor(readmit),
    true_mortality = as.factor(true_mortality)
  ) %>% 
  select(
    patientunitstayid,
    aps,
    age,
    gender,
    admitdiagnosis,
    apacheadmissiondx,
    hepaticfailure,
    lymphoma,
    metastaticcancer,
    leukemia,
    immunosuppression,
    cirrhosis,
    diabetes,
    graftcount,
    thrombolytics,
    #electivesurgery, (84% missing, no value)
    ima,
    # amilocation, only use amilocation if you’re modeling the MI subgroup (1705 NA's)
    midur,
    readmit,
    hosp_to_icu_admit_hours,
    true_mortality
  )

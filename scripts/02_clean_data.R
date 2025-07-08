# Load DB connection and libraries
source("scripts/01_load_data.R")

## Table 1a and 1b
p_results_IV <- p_results %>% filter(apacheversion == "IV")
p_results_IVa <- p_results %>% filter(apacheversion == "IVa")

## Outcome Variable (trueicumortality)
true_icu_mortality <- p_results_IV %>% 
  mutate(icumortality = case_when(
    actualicumortality == "ALIVE" ~ 0,
    actualicumortality == "EXPIRED" ~ 1,
  )) %>% 
  select(patientunitstayid, icumortality)

## restrict to only those patients for whom we have an outcome variable
apache_var <- apache_var %>% filter(patientunitstayid %in% true_icu_mortality$patientunitstayid)

confounders <- apache_var %>% 
  select(patientunitstayid, gender, age, graftcount, admitdiagnosis, thrombolytics,
         aids, hepaticfailure, lymphoma, metastaticcancer, leukemia, immunosuppression, cirrhosis, 
         electivesurgery, readmit, ima, amilocation,
         midur, diabetes)
## TABLE 1: Basic demographics.
# Apply the exclusion criteria on patient demographics table
#1. Under 18 years old.
# p_demographics <- p_demographics %>%
#   mutate(age = trimws(age),
#          age = ifelse(age == "> 89", "90", age),
#          age = ifelse(age == "", NA, age),
#          age = as.numeric(age)) %>%
#   filter(age >= 18)

#2. Missing target variable (hosp_mortality)


#3. Repeated ICU admissions: Already done in the SQL script.

#4. More than 80% of personal data was missing: Needs to be done when the whole feature set is established.

# 5. ICU stays shorter than 4 hours.
p_demographics <- p_demographics %>%
  filter(icu_los_hours >= 4)

## TABLE 2: APACHE score prediction variables
p_apache_vars <- p_demographics %>% left_join(apache_var, by = "patientunitstayid") %>% 
  select(-age.y, -gender.y, -diedinhospital, -uniquepid) %>% 
  rename(age = age.x,
         gender = gender.x)

results_joined <- p_demographics %>% left_join(p_results, by = "patientunitstayid") %>% 
  select(-actualhospitalmortality)

## TABLE 3: APACHE results
p_results_IV <- p_results %>% filter(apacheversion == "IV")
p_results_IVa <- p_results %>% filter(apacheversion == "IVa")

#sanity check
deaths <- tibble(results_joined$hosp_mortality, results_joined$actualHospitalMortality)




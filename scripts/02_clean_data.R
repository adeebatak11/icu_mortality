# Load DB connection and libraries
source("scripts/01_load_data.R")

## TABLE 1: Basic demographics.
# Apply the exclusion criteria on patient demographics table
#1. Under 18 years old.
p_demographics <- p_demographics %>%
  mutate(age = trimws(age),
         age = ifelse(age == "> 89", "90", age),
         age = ifelse(age == "", NA, age),
         age = as.numeric(age)) %>%
  filter(age >= 18)

#2. Missing target variable (hosp_mortality)
p_demographics <- p_demographics %>%
  filter(!is.na(hosp_mortality))

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

results_joined <-p_demographics %>% left_join(p_results, by = "patientunitstayid") %>% 
  select(-actualhospitalmortality)
  
#sanity check
deaths <- tibble(results_joined$hosp_mortality, results_joined$actualHospitalMortality)




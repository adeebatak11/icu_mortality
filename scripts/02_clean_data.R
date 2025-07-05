# Load DB connection and libraries
source("scripts/01_load_data.R")

# Apply the exclusion criteria on patient demographics table

#1. Under 18 years old.
p_demographics <- p_demographics %>%
  mutate(age = case_when(
    age == "> 89" ~ 90,
    age == ""     ~ NA_real_,
    TRUE          ~ as.numeric(age)
  )) %>% 
  filter(age >= 18)

#2. Missing target variable (hosp_mortality)
p_demographics <- p_demographics %>%
  filter(!is.na(hosp_mortality))

#3. Repeated ICU admissions
# -------------------------------------------------------------------------
# 01. Connect to Database
# -------------------------------------------------------------------------

source("scripts/00_db_connect.R")

# -------------------------------------------------------------------------
# 02. Load Core Data Tables
# -------------------------------------------------------------------------

# Load basic patient demographics
p_demographics <- dbGetQuery(
  con,
  paste(readLines("sql/basic_demographics.sql"), collapse = "\n")
)

# Load APACHE IVa prediction variables
apache_var <- dbGetQuery(
  con,
  paste(readLines("sql/apache_pred_var.sql"), collapse = "\n")
)

# Load APACHE predictions and actual patient outcomes
p_results <- dbGetQuery(
  con,
  paste(readLines("sql/apache_patient_results.sql"), collapse = "\n") 
)
# -------------------------------------------------------------------------
# 03. Disconnect from Database
# -------------------------------------------------------------------------

dbDisconnect(con)
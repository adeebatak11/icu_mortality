# -------------------------------------------------------------------------
# 01. Data Loading and Database Connection Script
# -------------------------------------------------------------------------
# This script:
#   - Loads required R packages (auto-installs if missing)
#   - Connects to the eICU SQLite database
#   - Loads core data tables needed for analysis
#   - Disconnects from the database
# -------------------------------------------------------------------------

# --- Load Required Libraries (auto-install if needed) ---
install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

pkgs_needed <- c(
  "DBI", "RSQLite", "tidyverse", "ggplot2", "pROC", "glmnet",
  "corrplot", "fastDummies", "caret", "boot", "DescTools",
  "knitr", "kableExtra", "gt", "webshot2", "here", "binom", 
  "rmda", "purrr"
)
invisible(lapply(pkgs_needed, install_if_needed))

# --- Connect to SQLite Database ---
db_path <- here::here("data", "eicu_v2_0_1.sqlite3")
stopifnot(file.exists(db_path))
con <- dbConnect(RSQLite::SQLite(), db_path)


# --- Load Core Data Tables ---
# Load basic patient demographics
p_demographics <- dbGetQuery(
  con,
  paste(readLines(here::here("sql", "basic_demographics.sql")), collapse = "\n")
)

# Load APACHE IVa prediction variables
apache_var <- dbGetQuery(
  con,
  paste(readLines(here::here("sql", "apache_pred_var.sql")), collapse = "\n")
)

# Load APACHE predictions and actual patient outcomes
p_results <- dbGetQuery(
  con,
  paste(readLines(here::here("sql", "apache_patient_results.sql")), collapse = "\n")
)

# --- Disconnect from Database ---
dbDisconnect(con)

# -------------------------------------------------------------------------
# End of Script: 01_data_load_and_connect.R
# Load DB connection and libraries
source("scripts/00_db_connect.R")

#loading the basic demographics table
p_demographics <- dbGetQuery(con, paste(readLines("sql/basic_demographics.sql"), collapse = "\n"))

#load the APS variables table
aps_var <- dbGetQuery(con, paste(readLines("sql/apache_APS_var.sql"), collapse = "\n"))

#load the APACHE prediction variables
apache_var <- dbGetQuery(con, paste(readLines("sql/apache_pred_var.sql"), collapse = "\n"))





# used for testing
test_table <- dbGetQuery(con, paste(readLines("sql/testing.sql"), collapse = "\n"))

dbDisconnect(con)
# Load DB connection and libraries
source("scripts/00_db_connect.R")

# Tables in the SQLite Database
tables <- dbListTables(con)

#loading the basic demographics table
sql_demographics_query <- paste(readLines("sql/basic_demographics.sql"), collapse = "\n")
p_demographics <- dbGetQuery(con, sql_demographics_query)


#load the labs table
sql_labs_query <- paste(readLines("sql/labs_first_day.sql"), collapse = "\n")
p_labs <- dbGetQuery(con, sql_labs_query)










# used for testing
test_query <- paste(readLines("sql/testing.sql"), collapse = "\n")
test_table <- dbGetQuery(con, test_query)

dbDisconnect(con)
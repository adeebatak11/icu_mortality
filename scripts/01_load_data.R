# Load DB connection and libraries
source("scripts/00_db_connect.R")

# Tables in the SQLite Database
tables <- dbListTables(con)

#loading the basic demographics table
sql_demographics_query <- paste(readLines("sql/basic_demographics.sql"), collapse = "\n")
p_demographics <- dbGetQuery(con, sql_demographics_query)


















dbDisconnect(con)
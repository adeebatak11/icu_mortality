# Install the necessary libraries.
install_if_needed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE, )
  }
}

invisible(lapply(c("DBI", "RSQLite", "tidyverse", "ggplot2"), install_if_needed
                 ))

# Connect to the SQLite Database.
con <- dbConnect(RSQLite::SQLite(), "/Users/adeebatak/Desktop/Projects/icu_mortality/data/eicu_v2_0_1.sqlite3")

# Tables in the SQLite Database
tables <- dbListTables(con)
print(tables)

#loading the SQL's
sql_text <- paste(readLines("sql/basic_demographics.sql"), collapse = "\n")
basic_demo <- dbGetQuery(con, sql_text)
table(basic_demo$hosp_mortality)

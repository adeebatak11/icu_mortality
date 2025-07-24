# -------------------------------------------------------------------------
# 01. Load Required Libraries (with auto-install helper)
# -------------------------------------------------------------------------

install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

pkgs_needed <- c("DBI", "RSQLite", "tidyverse", "ggplot2", "pROC", "glmnet", "corrplot", "fastDummies", "caret", "boot", "DescTools")
invisible(lapply(pkgs_needed, install_if_needed))

# -------------------------------------------------------------------------
# 02. Connect to SQLite Database
# -------------------------------------------------------------------------

db_path <- "/Users/adeebatak/Desktop/Projects/icu_mortality/data/eicu_v2_0_1.sqlite3"
stopifnot(file.exists(db_path))
con <- dbConnect(RSQLite::SQLite(), db_path)
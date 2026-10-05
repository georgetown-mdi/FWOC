############################################################
# 02_open_subset.R
# Author: Mark Richardson and Claude Code
#
# Opens the Parquet files saved by 01_download_subset.R, shows how to look at
# them without loading them into memory, and shows how to save a piece as CSV
# or Stata.
#
# The same code opens a complete dataset downloaded as a zip file from
# parquet/zip/ on the Download page: unzip it and point SUBSET_DIR at the
# unzipped folder (for example "emp_hive").
#
# This script continues Example 1 of 01_download_subset.R: employment at the
# Department of Defense (DD) and Veterans Affairs (VA) in 2024.
#
# The data are stored "hive-style": one Parquet file per month, in folders
# named year=YYYY/month=M. Arrow reads such a folder as a single dataset and
# takes each file's year and month from the folder names. For a fuller
# introduction to working with data stored this way, see the Arrow article
# "Working with multi-file data sets":
#   https://arrow.apache.org/docs/r/articles/dataset.html
#
# Requires: arrow, dplyr (and haven to save a Stata file).
############################################################

library(arrow)
library(dplyr)


############################################################
# SETTINGS -- edit these
############################################################

# The folder that holds the year=/month= folders.
SUBSET_DIR <- "fwoc_subset/emp"


############################################################
# OPEN THE DATA WITHOUT LOADING IT
############################################################

# open_dataset() reads only the description of each file (variable names and
# types), not the rows. `emp` is therefore a pointer to the files on disk and
# uses almost no memory, however large the data are.
#
# unify_schemas = TRUE makes Arrow check the variables in every file, not only
# the first. Every file in a release has the same variables, so this matters
# only if your local copy mixes months downloaded at different times.
emp <- open_dataset(SUBSET_DIR, unify_schemas = TRUE)

# Number of rows and the variable names. Neither reads the data itself.
nrow(emp)
names(emp)


############################################################
# SUMMARIZE BEFORE YOU COLLECT
############################################################

# Nothing is read until collect(). Arrow does the filtering and counting on
# the files and returns only the result, here one row per agency per date.
# This works even when the data are far larger than your computer's memory.
headcount <- emp |>
  count(agency_code, date, name = "employees") |>
  collect() |>
  arrange(agency_code, date)

headcount


############################################################
# PULL A SLICE INTO MEMORY
############################################################

# To work with individual records, narrow the data first: filter() the rows
# and select() the variables you need, then collect(). The result is an
# ordinary data frame.
#
# A word of warning: collect() without a filter loads everything. For the full
# employment dataset that needs hundreds of gigabytes of memory, and a computer
# that runs out of memory may freeze instead of reporting an error. If you are
# unsure how big a slice is, count its rows first:
#   emp |> filter(year == 2024, month == 3, agency_code == "VA") |> nrow()
va_march_2024 <- emp |>
  filter(year == 2024, month == 3, agency_code == "VA") |>
  select(date, year, month, agency_code, agency) |>
  collect()

head(va_march_2024)


############################################################
# SAVE A SLICE AS CSV OR STATA
############################################################

# Once a slice is a data frame it can be saved in any format.

# CSV. na = "" writes missing values as empty cells. This matters because the
# text "NA" is a real code in these data (an agency code and a pay plan code).
# When you read the file back, treat only empty cells as missing, for example
# readr::read_csv("va_march_2024.csv", na = "").
write_csv_arrow(va_march_2024, "va_march_2024.csv", na = "")

# Stata. Stata limits variable names to 32 characters, and a few variables in
# these data have longer names (for example annualized_adjusted_basic_pay_num).
# write_dta() stops with an error if one is included, so rename() or drop any
# such variable first.
haven::write_dta(va_march_2024, "va_march_2024.dta")

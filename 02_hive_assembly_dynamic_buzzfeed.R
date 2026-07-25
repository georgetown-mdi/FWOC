# Author: Mark Richardson and Claude Code
# Purpose: Assemble the quarterly dynamic data from buzzfeed into a
#          hive-style data structure

# load packages
library(arrow)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

#### First release - dynamic MAR1982 to JUN2014 ####

# specify file paths and create a vector of file names

path_in_non <- "buzz_feed/data/1973-09-to-2014-06/non-dod/dynamic/"
path_in_dod <- "buzz_feed/data/1973-09-to-2014-06/dod/dynamic/"


# create vector of quarters for complete years
qtrs_c <- paste0(rep(c("MAR", "JUN", "SEP", "DEC"), times = 32),
                 sort(rep(as.character(1982:2013), times = 4)))

# add quarters MAR and JUN of 2014
qtrs <- c(qtrs_c, "MAR2014", "JUN2014")

rm(qtrs_c)

# specify column widths, names, and type (make all character type for robustness across years)
widths <- fwf_widths(c(9, 23, 4, 2, 8, 6, 2, 2, 6, 9, 4, 1, 6, 2, 1),
                     col_names = c("pseudo_id", "name", "subagency_code", "acc_sep", "eff_date", "age_75", "pay_plan_code",
                                   "grade", "los_level", "duty_stat", "occ_code", "occ_cat_code", "adj_basic_pay",
                                   "appt_type_code", "work_sched_code"))

types <- cols(pseudo_id = "c", name = "c", subagency_code = "c", acc_sep = "c", eff_date = "c", age_75 = "c",
              pay_plan_code = "c", grade = "c", los_level = "c", duty_stat = "c", occ_code = "c", occ_cat_code = "c",
              adj_basic_pay = "c", appt_type_code = "c", work_sched_code = "c")


#### Dynamic - Non_DoD MAR1982 to JUN2014 ####

# list for storing each quarter
lst_non <- list()

# read each quarter
for (i in seq_along(qtrs)) {

  lst_non[[i]] <- read_fwf(file = paste0(path_in_non, qtrs[i], ".NONDOD.FO05M3.TXT"),
                           col_positions = widths, col_types = types, na = "")

}

#### Dynamic - DoD MAR1982 to JUN2014 ####

# list for storing each quarter
lst_dod <- list()

# read each quarter
for (i in seq_along(qtrs)) {

  lst_dod[[i]] <- read_fwf(file = paste0(path_in_dod, qtrs[i], ".DOD.FO05M4.TXT"),
                           col_positions = widths, col_types = types, na = "")

}

#### Merge the lists and append the files #####

dyn <- bind_rows(bind_rows(lst_non),
                 bind_rows(lst_dod))

rm(lst_non, lst_dod, types, widths, i, path_in_dod, path_in_non)

#### Create year, month, and quarter variables ####

dyn <- dyn |>
  mutate(eff_date = ymd(eff_date),
         year     = year(eff_date),
         month    = month(eff_date),
         quarter  = quarter(eff_date))

# Null stray numeric work_sched_code: BZ work schedule uses letter codes (F/P/I/...)
# plus * for unspecified; "1"/"9" are micro data-entry anomalies, not real schedules.
dyn <- dyn |>
  mutate(work_sched_code = if_else(work_sched_code %in% c("1", "9"),
                                   NA_character_, work_sched_code))

#### Write the data to a hive style structure ####

# Subset data to accessions and separations

acc <- dyn |>
  filter(str_sub(acc_sep, start = 1, end = 1) == "A") |>
  rename(acc_code = acc_sep)

sep <- dyn |>
  filter(str_sub(acc_sep, start = 1, end = 1) == "S") |>
  rename(sep_code = acc_sep)

# No observations lost: every acc_sep starts with A or S
stopifnot(nrow(dyn) == nrow(acc) + nrow(sep))

# Partitions written by the first release, per hive. The second release appends
# to the same hives and would silently delete and replace any matching partition,
# so neither hive may receive the same year/quarter from both releases.
rel1_acc_keys <- distinct(acc, year, quarter)
rel1_sep_keys <- distinct(sep, year, quarter)

write_dataset(acc,
              path = "buzz_feed/bz_acc_hive_raw",
              format = "parquet",
              partitioning = c("year", "quarter"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

write_dataset(sep,
              path = "buzz_feed/bz_sep_hive_raw",
              format = "parquet",
              partitioning = c("year", "quarter"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(qtrs, dyn, acc, sep)
gc()

#### Second release ####

#### Accessions ####

path_nd <- "buzz_feed/data/2014-09-to-2016-09/non-dod/accessions/"

path_dd <- "buzz_feed/data/2014-09-to-2016-09/dod/accessions/"

dates <- c("2014Q4",
           paste0(sort(rep(c(2015, 2016), times = 4)), c("Q1", "Q2", "Q3", "Q4")))

var_names <- c("last_name",
               "first_middle_name",
               "eff_date",
               "agency",
               "subagency",
               "acc_code",
               "state_code",
               "age_65",
               "ysd_range",
               "education_code",
               "pay_plan_code",
               "grade",
               "los_level",
               "occupation",
               "occ_cat_code",
               "adj_basic_pay",
               "appt_type_code",
               "work_sched_code",
               "nsftp_code")

acc_list <- list()

for (i in seq_along(dates)) {

  print(paste("Starting:", dates[i]))

  nd <- read_delim(file = paste0(path_nd, "Accessions Non_DoD FY", dates[i], ".txt"),
                   delim = "|", col_types = cols(.default = "c"),
                   col_names = var_names, na = "")

  dd <- read_delim(file = paste0(path_dd, "Accessions DoD FY", dates[i], ".txt"),
                   delim = "|", col_types = cols(.default = "c"),
                   col_names = var_names, na = "")

  ad <- bind_rows(nd, dd)

  rm(nd, dd)

  # create year, month, and quarter variables
  ad <- ad |>
    mutate(eff_date = ymd(eff_date),
           year     = year(eff_date),
           month    = month(eff_date),
           quarter  = quarter(eff_date))

  acc_list[[i]] <- ad

}

acc <- bind_rows(acc_list)

rm(acc_list, ad)

stopifnot(nrow(inner_join(rel1_acc_keys, distinct(acc, year, quarter),
                          by = c("year", "quarter"))) == 0)

## Agency and subagency codes; occupation labels;

acc <- acc |>
  mutate(agency_code = str_sub(agency, start = 1, end = 2),
         agency_name = str_sub(agency, start = 4, end = -1),
         subagency_code = str_sub(subagency, start = 1, end = 4),
         subagency_name = str_sub(subagency, start = 6, end = -1),
         occ_code = str_sub(occupation, start = 1, end = 4),
         occ_label = str_sub(occupation, start = 6, end = -1))

# Drop variables that combine codes and labels

acc <- acc |>
  select(!c(agency, subagency, occupation))

# Drop labels so variables are all labelled by the same script

acc <- acc |>
  select(!c(agency_name, subagency_name, occ_label))

# Write the hive-style partitions
write_dataset(acc, path = "buzz_feed/bz_acc_hive_raw",
              format = "parquet",
              partitioning = c("year", "quarter"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(acc)

#### Separations ####

path_nd <- "buzz_feed/data/2014-09-to-2016-09/non-dod/separations/"

path_dd <- "buzz_feed/data/2014-09-to-2016-09/dod/separations/"

var_names_sep <- replace(var_names, var_names == "acc_code", "sep_code")

sep_list <- list()

for (i in seq_along(dates)) {

  print(paste("Starting:", dates[i]))

  nd <- read_delim(file = paste0(path_nd, "Separations Non_DoD FY", dates[i], ".txt"),
                   delim = "|", col_types = cols(.default = "c"),
                   col_names = var_names_sep, na = "")

  dd <- read_delim(file = paste0(path_dd, "Separations DoD FY", dates[i], ".txt"),
                   delim = "|", col_types = cols(.default = "c"),
                   col_names = var_names_sep, na = "")

  ad <- bind_rows(nd, dd)

  rm(nd, dd)

  # create year, month, and quarter variables
  ad <- ad |>
    mutate(eff_date = ymd(eff_date),
           year     = year(eff_date),
           month    = month(eff_date),
           quarter  = quarter(eff_date))

  sep_list[[i]] <- ad

}

sep <- bind_rows(sep_list)

rm(sep_list, ad)

stopifnot(nrow(inner_join(rel1_sep_keys, distinct(sep, year, quarter),
                          by = c("year", "quarter"))) == 0)

## Agency and subagency codes; occupation labels;

sep <- sep |>
  mutate(agency_code = str_sub(agency, start = 1, end = 2),
         agency_name = str_sub(agency, start = 4, end = -1),
         subagency_code = str_sub(subagency, start = 1, end = 4),
         subagency_name = str_sub(subagency, start = 6, end = -1),
         occ_code = str_sub(occupation, start = 1, end = 4),
         occ_label = str_sub(occupation, start = 6, end = -1))

# Drop variables that combine codes and labels

sep <- sep |>
  select(!c(agency, subagency, occupation))

# Drop labels so variables are all labelled by the same script

sep <- sep |>
  select(!c(agency_name, subagency_name, occ_label))

# Write the hive-style partitions
write_dataset(sep, path = "buzz_feed/bz_sep_hive_raw",
              format = "parquet",
              partitioning = c("year", "quarter"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

#### Third release ####

# The third FOIA release (2016-12-to-2017-03) contains status data only; there
# are no accession or separation files to assemble.

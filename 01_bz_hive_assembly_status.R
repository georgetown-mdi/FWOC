# Author: Mark Richardson and Claude Code
# Purpose: Assemble the quarterly status data from buzzfeed into a
#          hive-style data structure


# load packages
library(arrow)
library(data.table)
setDTthreads(threads = (parallel::detectCores() - 2)) # set to n-2 cores
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

#### First release - status 1973-09-to-2014-06 ####

# specify file paths and create a vector of file names

path_in_non <- "buzz_feed/data/1973-09-to-2014-06/non-dod/status/Status_Non_DoD_"
path_in_dod <- "buzz_feed/data/1973-09-to-2014-06/dod/status/Status_DoD_"


# create vector of quarters for complete years
qtrs_c <- paste(sort(rep(as.character(1974:2013), times = 4)),
                rep(c("_03", "_06", "_09", "_12"), times = 40),
                sep = "")

# add quarters from 1973 and 2014
qtrs <- c("1973_09", "1973_12", qtrs_c, "2014_03", "2014_06")

rm(qtrs_c)

# specify column widths, names, and type (make all character type for robustness across years)
# note: Years Since Degree Range on FOIA letter is not in data
widths <- fwf_widths(c(9, 23, 8, 4, 9, 6, 2, 2, 2, 6, 4, 1, 6, 1, 2, 1, 1),
                     col_names = c("pseudo_id", "name", "date", "subagency_code", "duty_stat", "age_75", "education_code",
                                   "pay_plan_code", "grade", "los_level", "occ_code", "occ_cat_code", "adj_basic_pay",
                                   "super_stat_code", "appt_type_code", "work_sched_code", "nsftp_code"))

types <- cols(pseudo_id = "c", name = "c", date = "c", subagency_code = "c", duty_stat = "c", age_75 = "c", education_code = "c",
              pay_plan_code = "c", grade = "c", los_level = "c", occ_code = "c", occ_cat_code = "c", adj_basic_pay = "c",
              super_stat_code = "c", appt_type_code = "c", work_sched_code = "c", nsftp_code = "c")

my_schema <- arrow::schema(
  pseudo_id       = utf8(),
  name            = utf8(),
  date            = date32(),
  subagency_code  = utf8(),
  duty_stat       = utf8(),
  age_75          = utf8(),
  education_code  = utf8(),
  pay_plan_code   = utf8(),
  grade           = utf8(),
  los_level       = utf8(),
  occ_code        = utf8(),
  occ_cat_code    = utf8(),
  adj_basic_pay   = utf8(),
  super_stat_code = utf8(),
  appt_type_code  = utf8(),
  work_sched_code = utf8(),
  nsftp_code      = int8(),
  year            = int16(),
  month           = int8(),
  quarter         = int8()
)

for (i in seq_along(qtrs)) {

  # na = "" so the literal agency code "NA" (National Aeronautics and
  # Space Council) is preserved as a string instead of being read as missing.
  non <- read_fwf(file = paste0(path_in_non, qtrs[i], ".txt"),
                  col_positions = widths, col_types = types, na = "")
  dod <- read_fwf(file = paste0(path_in_dod, qtrs[i], ".txt"),
                  col_positions = widths, col_types = types, na = "")

  chunk <- rbindlist(list(non, dod), use.names = TRUE)
  rm(non, dod)

  # Note: := modifies the data.table in place and returns invisibly; no assignment necessary
  chunk[, date := ymd(date)]
  chunk[, `:=`(year       = as.integer(year(date)),
               month      = as.integer(month(date)),
               quarter    = as.integer(quarter(date)),
               nsftp_code = as.integer(nsftp_code))]

  # Null the corrupt 0xFF fill byte (invalid UTF-8) in the 1988-89 fixed-width
  # work_sched_code, so published codes stay clean UTF-8 (1,408 rows, no label).
  chunk[!validUTF8(work_sched_code), work_sched_code := NA_character_]

  # Null a stray "||" (two 0x7c bytes) in the 1975-76 fixed-width pay_plan field
  # (5 rows; valid ASCII but not a pay-plan code) so the published column stays clean.
  chunk[pay_plan_code == "||", pay_plan_code := NA_character_]

  # Null numeric pay_plan_code: OPM pay plans are alpha (GS/ES/AD/...); "00"/"01"/"04"/
  # "06"/"0T" are BZ-era values absent from the OPM master and FedScope -> NA. (Code 99,
  # the FedScope-documented "All Other Pay Plans" catch-all, is kept, not nulled.)
  chunk[pay_plan_code %in% c("00", "01", "04", "06", "0T"), pay_plan_code := NA_character_]

  # Null stray numeric work_sched_code: BZ work schedule uses letter codes (F/P/I/...)
  # plus * for unspecified; "1"/"9" are micro data-entry anomalies, not real schedules.
  chunk[work_sched_code %in% c("1", "9"), work_sched_code := NA_character_]

  arrow_chunk <- arrow_table(chunk, schema = my_schema)
  rm(chunk)

  # arrow_table() copies the data.table's R attributes into the table's
  # metadata under the key "r": its class, and the indexes data.table builds
  # automatically for the == and %in% filters above (one integer per row, per
  # indexed column). Written to Parquet, that is ~45 MB in every file's footer,
  # and later steps copy it onto every emp partition they rewrite. Drop it.
  arrow_chunk$metadata$r <- NULL

  write_dataset(
    arrow_chunk,
    path                   = "buzz_feed/bz_status_hive_raw/",
    format                 = "parquet",
    partitioning           = c("year", "quarter"),
    hive_style             = TRUE,
    compression            = "zstd",
    existing_data_behavior = "delete_matching",
    use_dictionary         = FALSE
  )

  rm(arrow_chunk)
  gc()
}

rm(types, widths, path_in_non, path_in_dod, qtrs, my_schema, i)

#### Second release ####

path_nd <- "buzz_feed/data/2014-09-to-2016-09/non-dod/status/"

path_dd <- "buzz_feed/data/2014-09-to-2016-09/dod/status/"

dates <- c("201409", "201412",
           paste0(sort(rep(2015, times = 4)), c("03", "06", "09", "12")),
           "201603", "201606", "201609")

var_names <- c("last_name",
               "first_middle_name",
               "file_date",
               "agency",
               "subagency",
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
               "super_stat_code",
               "appt_type_code",
               "work_sched_code",
               "nsftp_code")

st_list <- list()

for (i in seq_along(dates)) {

  print(paste("Starting:", dates[i]))

  nd <- read_delim(file = paste0(path_nd, "Non_DoD_", dates[i], ".txt"),
                   delim = "|", col_types = cols(.default = "c"),
                   col_names = var_names, na = "")

  dd <- read_delim(file = paste0(path_dd, "DoD_", dates[i], ".txt"),
                   delim = "|", col_types = cols(.default = "c"),
                   col_names = var_names, na = "")

  ad <- bind_rows(nd, dd)

  rm(nd, dd)

  # create year, month, and quarter variables
  ad <- ad |>
    mutate(year  = as.numeric(str_sub(file_date, start = 1, end = 4)),
           month = as.numeric(str_sub(file_date, start = 5, end = 6)),
           quarter = quarter(ymd(paste0(file_date, "01"))))

  st_list[[i]] <- ad

}

rm(ad)

st <- bind_rows(st_list)

rm(st_list)

# Split variables and codes

st <- st |>
  mutate(agency_code = str_sub(agency, start = 1, end =  2),
         agency_name = str_sub(agency, start = 4, end = -1),
         subagency_code = str_sub(subagency, start = 1, end =  4),
         subagency_name = str_sub(subagency, start = 6, end = -1),
         occ_code  = str_sub(occupation, start = 1, end =  4),
         occ_label = str_sub(occupation, start = 6, end = -1))

# Drop variables that combine codes and labels

st <- st |>
  select(!c(agency, subagency, occupation))

# Drop labels so variables are all labelled by the same script

st <- st |>
  select(!c(agency_name, subagency_name, occ_label))

st <- st |>
  mutate(nsftp_code = as.integer(nsftp_code),
         year       = as.integer(year),
         month      = as.integer(month))

st_arw <- arrow_table(st, schema = arrow::schema(
  last_name       = utf8(),
  first_middle_name = utf8(),
  file_date       = utf8(),
  state_code      = utf8(),
  age_65          = utf8(),
  ysd_range       = utf8(),
  education_code  = utf8(),
  pay_plan_code   = utf8(),
  grade           = utf8(),
  los_level       = utf8(),
  occ_cat_code    = utf8(),
  adj_basic_pay   = utf8(),
  super_stat_code = utf8(),
  appt_type_code  = utf8(),
  work_sched_code = utf8(),
  nsftp_code      = int8(),
  year            = int16(),
  month           = int8(),
  quarter         = int8(),
  agency_code     = utf8(),
  subagency_code  = utf8(),
  occ_code        = utf8()
))

# Write the hive-style partitions
write_dataset(st_arw,
              path = "buzz_feed/bz_status_hive_raw/",
              format = "parquet",
              partitioning = c("year", "quarter"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(st, st_arw)

#### Third release ####

path_nd <- "buzz_feed/data/2016-12-to-2017-03/non-dod/status/"

path_dd <- "buzz_feed/data/2016-12-to-2017-03/dod/status/"

nd1 <- read_delim(file = paste0(path_nd, "Non-DoD FOIA 2017-04762 201612.txt"),
                  delim = ";", col_types = cols(.default = "c"),
                  col_names = TRUE, na = "")

nd2 <- read_delim(file = paste0(path_nd, "Non-DoD FOIA 2017-04762 201703.txt"),
                  delim = ";", col_types = cols(.default = "c"),
                  col_names = TRUE, na = "")

dd1 <- read_delim(file = paste0(path_dd, "17-F-1232-Doc1.txt"),
                  delim = ";", col_types = cols(.default = "c"),
                  col_names = TRUE, na = "")

dd2 <- read_delim(file = paste0(path_dd, "17-F-1232-Doc2.txt"),
                  delim = ";", col_types = cols(.default = "c"),
                  col_names = TRUE, na = "")

st <- bind_rows(nd1, nd2, dd1, dd2)

rm(nd1, nd2, dd1, dd2)

# Replace names - standardize and properly format
st <- st |>
  rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE)))

# Create year, month, and quarter variables

st <- st |>
  mutate(year  = as.numeric(str_sub(file_date, start = 1, end = 4)),
         month = as.numeric(str_sub(file_date, start = 5, end = 6))) |>
  mutate(quarter = quarter(ymd(paste0(file_date, "01"))))

# Split codes and labels

st <- st |>
  rename(first_middle_name = first_name,
         age_65 = age_range,
         occ_code = occupation,
         occ_cat_code = patco,
         adj_basic_pay = adjusted_basic_pay) |>
  mutate(agency_code = str_split_fixed(agency, pattern = "-", n = 2)[, 1],
         agency_name = str_split_fixed(agency, pattern = "-", n = 2)[, 2],
         subagency_code = str_split_fixed(subagency, pattern = "-", n = 2)[, 1],
         subagency_name = str_split_fixed(subagency, pattern = "-", n = 2)[, 2],
         state_code = str_split_fixed(state, pattern = "-", n = 2)[, 1],
         state_name = str_split_fixed(state, pattern = "-", n = 2)[, 2],
         education_code =  str_split_fixed(education_level, pattern = "-", n = 2)[, 1],
         education_label = str_split_fixed(education_level, pattern = "-", n = 2)[, 2],
         pay_plan_code  = str_split_fixed(pay_plan, pattern = "-", n = 2)[, 1],
         pay_plan_label = str_split_fixed(pay_plan, pattern = "-", n = 2)[, 2],
         super_stat_code  = str_split_fixed(supervisory_status, pattern = "-", n = 2)[, 1],
         super_stat_label = str_split_fixed(supervisory_status, pattern = "-", n = 2)[, 2],
         appt_type_code  = str_split_fixed(toa, pattern = "-", n = 2)[, 1],
         appt_type_label = str_split_fixed(toa, pattern = "-", n = 2)[, 2],
         work_sched_code  = str_split_fixed(work_schedule, pattern = "-", n = 2)[, 1],
         work_sched_label = str_split_fixed(work_schedule, pattern = "-", n = 2)[, 2],
         nsftp_code  = str_split_fixed(nsftp_indicator, pattern = "-", n = 2)[, 1],
         nsftp_label = str_split_fixed(nsftp_indicator, pattern = "-", n = 2)[, 2])

# Drop variables that contain codes and labels
st <- st |>
  select(!c(agency, subagency, state, education_level, pay_plan,
            supervisory_status, toa, work_schedule, nsftp_indicator))

# Drop labels so variables are all labelled by the same script
st <- st |>
  select(!contains("_label")) |>
  select(!c(agency_name, subagency_name, state_name))

st <- st |>
  mutate(nsftp_code = as.integer(nsftp_code),
         year       = as.integer(year),
         month      = as.integer(month))

st_arw <- arrow_table(st, schema = arrow::schema(
  last_name       = utf8(),
  first_middle_name = utf8(),
  file_date       = utf8(),
  age_65          = utf8(),
  ysd_range       = utf8(),
  grade           = utf8(),
  los_level       = utf8(),
  occ_code        = utf8(),
  occ_cat_code    = utf8(),
  adj_basic_pay   = utf8(),
  year            = int16(),
  month           = int8(),
  quarter         = int8(),
  agency_code     = utf8(),
  subagency_code  = utf8(),
  state_code      = utf8(),
  education_code  = utf8(),
  pay_plan_code   = utf8(),
  super_stat_code = utf8(),
  appt_type_code  = utf8(),
  work_sched_code = utf8(),
  nsftp_code      = int8()
))

# Write the hive-style partitions
write_dataset(st_arw, path = "buzz_feed/bz_status_hive_raw/",
              format = "parquet",
              partitioning = c("year", "quarter"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(st, st_arw)


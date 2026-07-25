# Author: Mark Richardson and Claude Code
# Purpose: Make buzzfeed data compatible with data.opm.gov, including:
#          1. Standardize age variable
#          2. Create a numeric salary variable
#          3. Create an agency_code variable
#          4. Rename all BZ variables to FWD naming conventions
#          5. Create nsftp_indicator (1/0 from BZ nsftp_code)
#          6. Explicitly define the schema

# Load packages
library(arrow)
library(dplyr)
library(lubridate)
library(stringr)

# Open the status hive data
sts_org <- open_dataset(sources = "buzz_feed/bz_status_hive_raw/",
                        partitioning = c("year", "quarter"),
                        unify_schemas = TRUE)

#### Make buzzfeed data compatible with data.gov ####

#### Employment ####

# Filter to years before 2005
sts <- sts_org |>
  filter(year <= 2004)

# BZ source-data fix: zero-pad single-digit education_code ("1"/"2") to
# two-digit form ("01"/"02"). Affects ~100 rows in 1974-1977 BZ data;
# without the fix these rows would join as invalid in script 12. The valid
# OPM codes are zero-padded; we assume the leading-zero is missing in error;
# Adds leading 0 to 101 observations from 1974-03 to 1977-03
# Match values that are length 1 and an integer.
sts <- sts |>
  mutate(
    education_code = if_else(str_detect(education_code, "^[0-9]$"),
                             paste0("0", education_code),
                             education_code)
  )

# Note: age_75 is the valid age variable for 2004 and earlier

# Create an age variable that maps to data.gov
sts <- sts |>
  mutate(age_bracket = case_when(age_75 == "15-19" ~ "Less than 20",
                                 age_75 == "65-69" ~ "65 or more",
                                 age_75 == "70-74" ~ "65 or more",
                                 age_75 == "75+" ~ "65 or more",
                                 age_75 == "UNSP" ~ "Unspecified",
                                 TRUE ~ age_75))

# Create a numeric version of adjusted_basic_pay. The BZ status data has three
# non-numeric forms, all of which become NA: absent, the "******" sentinel, and
# "033  0" (4 rows). na_salary counts them up front so the recode can be checked
# against it.
na_salary <- sts |>
  select(adj_basic_pay) |>
  mutate(is_na = if_else(adj_basic_pay == "******" | is.na(adj_basic_pay) |
                           adj_basic_pay ==  "033  0", TRUE, FALSE)) |>
  count(is_na) |>
  collect()

# Recode any salary that is non-numeric to NA and cast to a number
sts <- sts |>
  mutate(
    adj_basic_pay_num = cast(
      if_else(grepl("^[0-9]+$", adj_basic_pay), adj_basic_pay, NA_character_),
      float64()
    )
  )

# Check recoding
na_salary_rc <- sts |>
  select(adj_basic_pay_num) |>
  mutate(is_na = is.na(adj_basic_pay_num)) |>
  count(is_na) |>
  collect()

stopifnot(identical(na_salary, na_salary_rc)) # No unexpected NAs

# clean up
rm(na_salary, na_salary_rc)

# Create 1/0 coded nsftp indicator variable
# Note: must first convert to integer to avoid arrow error

sts <- sts |>
  mutate(
    nsftp_indicator = as.integer(nsftp_code),
    nsftp_indicator = if_else(nsftp_indicator == 1, 1, 0)
  )


#### Set the schema and write the new hive-style partition

sts |>
  mutate(pseudo_id                         = cast(pseudo_id, utf8()),
         name                              = cast(name, utf8()),
         date                              = cast(date, date32()),
         agency_code                       = cast(str_sub(subagency_code, start = 1, end = 2), utf8()),
         agency_subelement_code            = cast(subagency_code, utf8()),
         duty_station_code                 = cast(duty_stat, utf8()),
         age_75                            = cast(age_75, utf8()),
         education_level_code              = cast(education_code, utf8()),
         pay_plan_code                     = cast(pay_plan_code, utf8()),
         grade                             = cast(grade, utf8()),
         length_of_service_ord             = cast(los_level, utf8()),
         occupational_series_code          = cast(occ_code, utf8()),
         occupational_category_code        = cast(occ_cat_code, utf8()),
         annualized_adjusted_basic_pay     = cast(adj_basic_pay, utf8()),
         supervisory_status_code           = cast(super_stat_code, utf8()),
         appointment_type_code             = cast(appt_type_code, utf8()),
         work_schedule_code                = cast(work_sched_code, utf8()),
         nsftp_indicator                   = cast(nsftp_indicator, int8()),
         month                             = cast(month, int8()),
         year                              = cast(year, int16()),
         quarter                           = cast(quarter, int8()),
         age_bracket                       = cast(age_bracket, utf8()),
         annualized_adjusted_basic_pay_num = adj_basic_pay_num
         ) |>
  select(pseudo_id, name, date, agency_code, agency_subelement_code, duty_station_code,
         age_75, education_level_code, pay_plan_code, grade, length_of_service_ord,
         occupational_series_code, occupational_category_code,
         annualized_adjusted_basic_pay, annualized_adjusted_basic_pay_num,
         supervisory_status_code, appointment_type_code, work_schedule_code,
         nsftp_indicator, age_bracket, year, month, quarter) |>
  write_dataset("buzz_feed/bz_emp_hive_staging/",
                format = "parquet",
                partitioning = c("year", "month"),
                hive_style = TRUE,
                compression = "zstd",
                existing_data_behavior = "delete_matching",
                use_dictionary = FALSE)


# Clean up
rm(sts, sts_org)

#### Accessions ####

# Pull the accessions data from 2004 and earlier into memory
acc <- open_dataset(sources = "buzz_feed/bz_acc_hive_raw/",
                    partitioning = c("year", "quarter")) |>
  filter(year <= 2004) |>
  collect()

# Count the non-numeric pay forms up front so the recode below can be checked
# against them. Only two occur here -- the status data also has "033  0", which
# does not appear in the dynamic files.
na_salary <- acc |>
  select(adj_basic_pay) |>
  mutate(is_na = if_else(adj_basic_pay == "******" | is.na(adj_basic_pay),
                         TRUE, FALSE)) |>
  count(is_na) |>
  collect()

acc <- acc |>
  mutate(age_bracket = case_when(age_75 == "15-19" ~ "Less than 20",
                                 age_75 == "65-69" ~ "65 or more",
                                 age_75 == "70-74" ~ "65 or more",
                                 age_75 == "75+" ~ "65 or more",
                                 age_75 == "UNSP" ~ "Unspecified",
                                 TRUE ~ age_75),
         adj_basic_pay_num = if_else(grepl("^[0-9]+$", adj_basic_pay), adj_basic_pay, NA_character_))

# Check recoding
na_salary_rc <- acc |>
  select(adj_basic_pay_num) |>
  mutate(is_na = is.na(adj_basic_pay_num)) |>
  count(is_na) |>
  collect()

stopifnot(identical(na_salary, na_salary_rc)) # No unexpected NAs

# clean up
rm(na_salary, na_salary_rc)

# Cast the variables to define the schema and write the new partition

acc |>
  as_arrow_table() |>
  mutate(pseudo_id                         = cast(pseudo_id, utf8()),
         name                              = cast(name, utf8()),
         eff_date                          = cast(eff_date, date32()),
         agency_code                       = cast(str_sub(subagency_code, start = 1, end = 2), utf8()),
         agency_subelement_code            = cast(subagency_code, utf8()),
         accession_category_code           = cast(acc_code, utf8()),
         duty_station_code                 = cast(duty_stat, utf8()),
         age_75                            = cast(age_75, utf8()),
         pay_plan_code                     = cast(pay_plan_code, utf8()),
         grade                             = cast(grade, utf8()),
         length_of_service_ord             = cast(los_level, utf8()),
         occupational_series_code          = cast(occ_code, utf8()),
         occupational_category_code        = cast(occ_cat_code, utf8()),
         annualized_adjusted_basic_pay     = cast(adj_basic_pay, utf8()),
         appointment_type_code             = cast(appt_type_code, utf8()),
         work_schedule_code                = cast(work_sched_code, utf8()),
         month                             = cast(month, int8()),
         year                              = cast(year, int16()),
         quarter                           = cast(quarter, int8()),
         age_bracket                       = cast(age_bracket, utf8()),
         annualized_adjusted_basic_pay_num = cast(adj_basic_pay_num, float64())
  ) |>
  select(pseudo_id, name, eff_date, agency_code, agency_subelement_code,
         accession_category_code, duty_station_code, age_75, pay_plan_code,
         grade, length_of_service_ord, occupational_series_code, occupational_category_code,
         annualized_adjusted_basic_pay, annualized_adjusted_basic_pay_num,
         appointment_type_code, work_schedule_code, age_bracket, year, month, quarter) |>
  write_dataset("buzz_feed/bz_acc_hive_staging",
                format = "parquet",
                partitioning = c("year", "month"),
                hive_style = TRUE,
                compression = "zstd",
                existing_data_behavior = "delete_matching",
                use_dictionary = FALSE)

# Clean up
rm(acc)

#### Separations ####

# Pull the separations data from 2004 and earlier into memory
sep <- open_dataset(sources = "buzz_feed/bz_sep_hive_raw/",
                    partitioning = c("year", "quarter")) |>
  filter(year <= 2004) |>
  collect()

# Count the non-numeric pay forms up front so the recode below can be checked
# against them. Only two occur here -- the status data also has "033  0", which
# does not appear in the dynamic files.
na_salary <- sep |>
  select(adj_basic_pay) |>
  mutate(is_na = if_else(adj_basic_pay == "******" | is.na(adj_basic_pay),
                         TRUE, FALSE)) |>
  count(is_na) |>
  collect()

sep <- sep |>
  mutate(age_bracket = case_when(age_75 == "15-19" ~ "Less than 20",
                                 age_75 == "65-69" ~ "65 or more",
                                 age_75 == "70-74" ~ "65 or more",
                                 age_75 == "75+" ~ "65 or more",
                                 age_75 == "UNSP" ~ "Unspecified",
                                 TRUE ~ age_75),
         adj_basic_pay_num = if_else(grepl("^[0-9]+$", adj_basic_pay), adj_basic_pay, NA_character_))

# Check recoding
na_salary_rc <- sep |>
  select(adj_basic_pay_num) |>
  mutate(is_na = is.na(adj_basic_pay_num)) |>
  count(is_na) |>
  collect()

stopifnot(identical(na_salary, na_salary_rc)) # No unexpected NAs

# clean up
rm(na_salary, na_salary_rc)

# Cast the variables to define the schema and write the new partition

sep |>
  as_arrow_table() |>
  mutate(pseudo_id                         = cast(pseudo_id, utf8()),
         name                              = cast(name, utf8()),
         eff_date                          = cast(eff_date, date32()),
         agency_code                       = cast(str_sub(subagency_code, start = 1, end = 2), utf8()),
         agency_subelement_code            = cast(subagency_code, utf8()),
         separation_category_code          = cast(sep_code, utf8()),
         duty_station_code                 = cast(duty_stat, utf8()),
         age_75                            = cast(age_75, utf8()),
         pay_plan_code                     = cast(pay_plan_code, utf8()),
         grade                             = cast(grade, utf8()),
         length_of_service_ord             = cast(los_level, utf8()),
         occupational_series_code          = cast(occ_code, utf8()),
         occupational_category_code        = cast(occ_cat_code, utf8()),
         annualized_adjusted_basic_pay     = cast(adj_basic_pay, utf8()),
         appointment_type_code             = cast(appt_type_code, utf8()),
         work_schedule_code                = cast(work_sched_code, utf8()),
         month                             = cast(month, int8()),
         year                              = cast(year, int16()),
         quarter                           = cast(quarter, int8()),
         age_bracket                       = cast(age_bracket, utf8()),
         annualized_adjusted_basic_pay_num = cast(adj_basic_pay_num, float64())
  ) |>
  select(pseudo_id, name, eff_date, agency_code, agency_subelement_code,
         separation_category_code, duty_station_code, age_75, pay_plan_code,
         grade, length_of_service_ord, occupational_series_code, occupational_category_code,
         annualized_adjusted_basic_pay, annualized_adjusted_basic_pay_num,
         appointment_type_code, work_schedule_code, age_bracket, year, month, quarter) |>
  write_dataset("buzz_feed/bz_sep_hive_staging",
                format = "parquet",
                partitioning = c("year", "month"),
                hive_style = TRUE,
                compression = "zstd",
                existing_data_behavior = "delete_matching",
                use_dictionary = FALSE)

# Clean up
rm(sep)

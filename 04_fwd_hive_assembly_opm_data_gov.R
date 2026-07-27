# Author: Mark Richardson and Claude Code
# Purpose: Assemble the fwd_{emp,acc,sep} staging hives from the Parquet files
#          downloaded from the data.opm.gov API by script 03.

# Load packages
library(arrow)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

# Which file to read for each period. OPM republishes a period as a new numbered
# version rather than replacing it, and flags the authoritative one as `current`.
# Script 03 records that flag, so the choice comes from the API's own field.
manifest <- read_csv("metadata/fwd/file_manifest.csv",
                     col_types = cols(.default         = "c",
                                      version          = "i",
                                      publish_date     = "T",
                                      complete_on_disk = "l",
                                      bytes            = "i",
                                      current          = "l"),
                     na = "")

#### Employment files ####

path_pre <- "data_opm_gov/employment/"

# Get list of employment files
emp_files <- manifest |>
  filter(dataset == "employment" & current) |>
  arrange(year, month)

# OPM must flag exactly one current version per period; two would silently
# double that month's rows in the hive.
stopifnot(nrow(emp_files) == nrow(distinct(emp_files, year, month)))

# Master type map covering all known schema versions
emp_type_map <- list(
  age_bracket                               = utf8(),
  agency                                    = utf8(),
  agency_code                               = utf8(),
  agency_subelement                         = utf8(),
  agency_subelement_code                    = utf8(),
  annualized_adjusted_basic_pay             = utf8(),
  appointment_type                          = utf8(),
  appointment_type_code                     = utf8(),
  bargaining_unit                           = utf8(),
  bargaining_unit_code                      = utf8(),
  bargaining_unit_status                    = utf8(),
  cfo_act_agency_indicator                  = utf8(),
  consolidated_statistical_area             = utf8(),
  consolidated_statistical_area_code        = utf8(),
  core_based_statistical_area               = utf8(),
  core_based_statistical_area_code          = utf8(),
  count                                     = int8(),
  department                                = utf8(),
  department_code                           = utf8(),
  duty_station_city                         = utf8(),
  duty_station_code                         = utf8(),
  duty_station_country                      = utf8(),
  duty_station_country_code                 = utf8(),
  duty_station_county                       = utf8(),
  duty_station_county_code                  = utf8(),
  duty_station_state                        = utf8(),
  duty_station_state_abbreviation           = utf8(),
  duty_station_state_code                   = utf8(),
  duty_station_state_country_territory_code = utf8(),
  education_level                           = utf8(),
  education_level_bracket                   = utf8(),
  education_level_code                      = utf8(),
  flsa_category                             = utf8(),
  flsa_category_code                        = utf8(),
  grade                                     = utf8(),
  length_of_service_years                   = int16(),
  locality_pay_area                         = utf8(),
  locality_pay_area_code                    = utf8(),
  nsftp_indicator                           = utf8(),
  occupational_category                     = utf8(),
  occupational_category_code                = utf8(),
  occupational_group                        = utf8(),
  occupational_group_code                   = utf8(),
  occupational_series                       = utf8(),
  occupational_series_code                  = utf8(),
  pay_basis                                 = utf8(),
  pay_basis_code                            = utf8(),
  pay_plan                                  = utf8(),
  pay_plan_code                             = utf8(),
  personnel_office_identifier_code          = utf8(),
  position_occupied                         = utf8(),
  position_occupied_code                    = utf8(),
  service_computation_date_leave            = date32(),
  snapshot_yyyymm                           = utf8(),
  stem_occupation                           = utf8(),
  stem_occupation_type                      = utf8(),
  step_or_rate_type                         = utf8(),
  step_or_rate_type_code                    = utf8(),
  supervisory_status                        = utf8(),
  supervisory_status_code                   = utf8(),
  tenure                                    = utf8(),
  tenure_code                               = utf8(),
  veteran_indicator                         = utf8(),
  work_schedule                             = utf8(),
  work_schedule_code                        = utf8(),
  year                                      = int16(),
  month                                     = int8(),
  # float64, not int32: OPM publishes cents on accession pay, which an integer
  # cast truncates silently. All three hives share the type so they unify.
  annualized_adjusted_basic_pay_num         = float64()
)

# Loop to read in files and save in hive format with year and month keys
for (i in seq_along(emp_files$filename)) {

  # Read the Parquet file (all columns are stored as character)
  emp <- read_parquet(paste0(path_pre, emp_files$filename[i], ".parquet")) |>
    mutate(count = as.integer(count),
           length_of_service_years = as.integer(length_of_service_years),
           year = str_sub(snapshot_yyyymm, start = 1, end = 4),
           year = as.integer(year),
           month = str_sub(snapshot_yyyymm, start = 5, end = 6),
           month = as.integer(month))

  # Create a numeric salary variable
  n_salary_string <- sum(emp$annualized_adjusted_basic_pay == "REDACTED", na.rm = TRUE) +
    sum(is.na(emp$annualized_adjusted_basic_pay))

  emp <- emp |>
    mutate(annualized_adjusted_basic_pay_num = as.numeric(annualized_adjusted_basic_pay))

  # as.numeric() may only produce NA where the source was already NA or
  # "REDACTED"; any other NA means a pay value failed to parse.
  stopifnot(n_salary_string ==
              sum(is.na(emp$annualized_adjusted_basic_pay_num)))

  # Parse date column when present
  if ("service_computation_date_leave" %in% names(emp)) {
    emp <- emp |>
      mutate(service_computation_date_leave = ymd(service_computation_date_leave))
  }

  # Build schema from actual columns; errors if a new unknown column appears
  file_schema <- do.call(schema, emp_type_map[names(emp)])
  emp <- arrow_table(emp, schema = file_schema)

  write_dataset(emp,
                path = "data_opm_gov/fwd_emp_hive",
                hive_style = TRUE,
                partitioning = c("year", "month"),
                compression = "zstd",
                use_dictionary = FALSE)

}

# Clean up
rm(list = setdiff(ls(), "manifest"))

#### Accessions ####

path_pre <- "data_opm_gov/accessions/"

# Get list of accession files
acc_files <- manifest |>
  filter(dataset == "accessions" & current) |>
  arrange(year, month)

# OPM must flag exactly one current version per period; two would silently
# double that month's rows in the hive.
stopifnot(nrow(acc_files) == nrow(distinct(acc_files, year, month)))

acc_list <- list()

for (i in seq_along(acc_files$filename)) {

  acc_list[[i]] <- read_parquet(paste0(path_pre, acc_files$filename[i], ".parquet")) |>
    mutate(count = as.integer(count),
           length_of_service_years = as.integer(length_of_service_years),
           year = str_sub(personnel_action_effective_date_yyyymm, start = 1, end = 4),
           year = as.integer(year),
           month = str_sub(personnel_action_effective_date_yyyymm, start = 5, end = 6),
           month = as.integer(month))

}

acc <- bind_rows(acc_list)

# Create a numeric salary variable
n_salary_string <- sum(acc$annualized_adjusted_basic_pay == "REDACTED", na.rm = TRUE) +
    sum(is.na(acc$annualized_adjusted_basic_pay))

acc <- acc |>
    mutate(annualized_adjusted_basic_pay_num = as.numeric(annualized_adjusted_basic_pay))

# as.numeric() may only produce NA where the source was already NA or
# "REDACTED"; any other NA means a pay value failed to parse.
stopifnot(n_salary_string == sum(is.na(acc$annualized_adjusted_basic_pay_num)))

# Parse date variables
acc <- acc |>
  mutate(appointment_not_to_exceed_date  = ymd(appointment_not_to_exceed_date),
         service_computation_date_leave  = ymd(service_computation_date_leave))

# Master type map covering all known acc columns; subset by the columns
# actually present so the schema follows the (alphabetical) parquet order.
acc_type_map <- list(
  accession_category = utf8(),
  accession_category_code = utf8(),
  age_bracket = utf8(),
  agency = utf8(),
  agency_code = utf8(),
  agency_subelement = utf8(),
  agency_subelement_code = utf8(),
  annualized_adjusted_basic_pay = utf8(),
  appointment_type = utf8(),
  appointment_type_code = utf8(),
  count = int8(),
  duty_station_country = utf8(),
  duty_station_country_code = utf8(),
  duty_station_state = utf8(),
  duty_station_state_abbreviation = utf8(),
  duty_station_state_code = utf8(),
  education_level = utf8(),
  education_level_code = utf8(),
  grade = utf8(),
  length_of_service_years = int16(),
  occupational_group = utf8(),
  occupational_group_code = utf8(),
  occupational_series = utf8(),
  occupational_series_code = utf8(),
  pay_plan = utf8(),
  pay_plan_code = utf8(),
  personnel_action_effective_date_yyyymm = utf8(),
  stem_occupation = utf8(),
  stem_occupation_type = utf8(),
  supervisory_status = utf8(),
  supervisory_status_code = utf8(),
  work_schedule = utf8(),
  work_schedule_code = utf8(),
  year = int16(),
  month = int8(),
  appointment_not_to_exceed_date = date32(),
  bargaining_unit = utf8(),
  bargaining_unit_code = utf8(),
  bargaining_unit_status = utf8(),
  cfo_act_agency_indicator = utf8(),
  consolidated_statistical_area = utf8(),
  consolidated_statistical_area_code = utf8(),
  core_based_statistical_area = utf8(),
  core_based_statistical_area_code = utf8(),
  duty_station_code = utf8(),
  duty_station_county = utf8(),
  duty_station_county_code = utf8(),
  duty_station_state_country_territory_code = utf8(),
  education_level_bracket = utf8(),
  flsa_category = utf8(),
  flsa_category_code = utf8(),
  locality_pay_area = utf8(),
  locality_pay_area_code = utf8(),
  nsftp_indicator = utf8(),
  occupational_category = utf8(),
  occupational_category_code = utf8(),
  pay_basis = utf8(),
  pay_basis_code = utf8(),
  personnel_office_identifier_code = utf8(),
  position_occupied = utf8(),
  position_occupied_code = utf8(),
  service_computation_date_leave = date32(),
  step_or_rate_type = utf8(),
  step_or_rate_type_code = utf8(),
  tenure = utf8(),
  tenure_code = utf8(),
  veteran_indicator = utf8(),
  department = utf8(),
  department_code = utf8(),
  duty_station_city = utf8(),
  pathways_group = utf8(),
  # float64, not int32: OPM publishes cents on accession pay (see 04 emp map).
  annualized_adjusted_basic_pay_num = float64()
)

acc <- arrow_table(acc, schema = do.call(schema, acc_type_map[names(acc)]))

write_dataset(acc,
              path = "data_opm_gov/fwd_acc_hive",
              hive_style = TRUE,
              partitioning = c("year", "month"),
              compression = "zstd",
              use_dictionary = FALSE)

# Clean up
rm(list = setdiff(ls(), "manifest"))

#### Separations ####

path_pre <- "data_opm_gov/separations/"

# Get list of separation files
sep_files <- manifest |>
  filter(dataset == "separations" & current) |>
  arrange(year, month)

# OPM must flag exactly one current version per period; two would silently
# double that month's rows in the hive.
stopifnot(nrow(sep_files) == nrow(distinct(sep_files, year, month)))

sep_list <- list()

for (i in seq_along(sep_files$filename)) {

  sep_list[[i]] <- read_parquet(paste0(path_pre, sep_files$filename[i], ".parquet")) |>
    mutate(count = as.integer(count),
           length_of_service_years = as.integer(length_of_service_years),
           year = str_sub(personnel_action_effective_date_yyyymm, start = 1, end = 4),
           year = as.integer(year),
           month = str_sub(personnel_action_effective_date_yyyymm, start = 5, end = 6),
           month = as.integer(month))

}

sep <- bind_rows(sep_list)

  # Create a numeric salary variable
n_salary_string <- sum(sep$annualized_adjusted_basic_pay == "REDACTED", na.rm = TRUE) +
    sum(is.na(sep$annualized_adjusted_basic_pay))

sep <- sep |>
    mutate(annualized_adjusted_basic_pay_num = as.numeric(annualized_adjusted_basic_pay))

# as.numeric() may only produce NA where the source was already NA or
# "REDACTED"; any other NA means a pay value failed to parse.
stopifnot(n_salary_string == sum(is.na(sep$annualized_adjusted_basic_pay_num)))

# Parse date variables
sep <- sep |>
  mutate(appointment_not_to_exceed_date = ymd(appointment_not_to_exceed_date),
         service_computation_date_leave = ymd(service_computation_date_leave))

# Master type map covering all known sep columns; subset by the columns
# actually present so the schema follows the (alphabetical) parquet order.
sep_type_map <- list(
  age_bracket = utf8(),
  agency = utf8(),
  agency_code = utf8(),
  agency_subelement = utf8(),
  agency_subelement_code = utf8(),
  annualized_adjusted_basic_pay = utf8(),
  appointment_type = utf8(),
  appointment_type_code = utf8(),
  count = int8(),
  duty_station_country = utf8(),
  duty_station_country_code = utf8(),
  duty_station_state = utf8(),
  duty_station_state_abbreviation = utf8(),
  duty_station_state_code = utf8(),
  education_level = utf8(),
  education_level_code = utf8(),
  grade = utf8(),
  length_of_service_years = int16(),
  occupational_group = utf8(),
  occupational_group_code = utf8(),
  occupational_series = utf8(),
  occupational_series_code = utf8(),
  pay_plan = utf8(),
  pay_plan_code = utf8(),
  personnel_action_effective_date_yyyymm = utf8(),
  separation_category = utf8(),
  separation_category_code = utf8(),
  stem_occupation = utf8(),
  stem_occupation_type = utf8(),
  supervisory_status = utf8(),
  supervisory_status_code = utf8(),
  work_schedule = utf8(),
  work_schedule_code = utf8(),
  year = int16(),
  month = int8(),
  appointment_not_to_exceed_date = date32(),
  bargaining_unit = utf8(),
  bargaining_unit_code = utf8(),
  bargaining_unit_status = utf8(),
  cfo_act_agency_indicator = utf8(),
  consolidated_statistical_area = utf8(),
  consolidated_statistical_area_code = utf8(),
  core_based_statistical_area = utf8(),
  core_based_statistical_area_code = utf8(),
  drp_indicator = utf8(),
  duty_station_code = utf8(),
  duty_station_county = utf8(),
  duty_station_county_code = utf8(),
  duty_station_state_country_territory_code = utf8(),
  education_level_bracket = utf8(),
  flsa_category = utf8(),
  flsa_category_code = utf8(),
  locality_pay_area = utf8(),
  locality_pay_area_code = utf8(),
  nsftp_indicator = utf8(),
  occupational_category = utf8(),
  occupational_category_code = utf8(),
  pay_basis = utf8(),
  pay_basis_code = utf8(),
  personnel_office_identifier_code = utf8(),
  position_occupied = utf8(),
  position_occupied_code = utf8(),
  service_computation_date_leave = date32(),
  step_or_rate_type = utf8(),
  step_or_rate_type_code = utf8(),
  tenure = utf8(),
  tenure_code = utf8(),
  veteran_indicator = utf8(),
  department = utf8(),
  department_code = utf8(),
  duty_station_city = utf8(),
  pathways_group = utf8(),
  # float64, not int32: OPM publishes cents on accession pay (see 04 emp map).
  annualized_adjusted_basic_pay_num = float64()
)

sep <- arrow_table(sep, schema = do.call(schema, sep_type_map[names(sep)]))

write_dataset(sep,
              path = "data_opm_gov/fwd_sep_hive",
              hive_style = TRUE,
              partitioning = c("year", "month"),
              compression = "zstd",
              use_dictionary = FALSE)

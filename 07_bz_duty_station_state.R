# Author: Mark Richardson and Claude Code
# Purpose: Enrich BZ duty station with state abbreviations, rename county
#          columns to FWD conventions, and write BZ data to the final hives
#          (emp_hive, acc_hive, sep_hive).
#          Runs after 06, which adds the country/county/city labels renamed
#          below.

library(arrow)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# find_code_table() resolves the current DutyStations_<date>.csv, so a new
# monthly download needs no edit here (it errors if 0 or >1 match). It parses the
# date with ymd(), which is why lubridate is attached above.
source("utils/code_table_functions.R")

#### Build duty-station reference tables ####

# Load DutyStations reference for US state/territory identification
ds_file <- find_code_table("metadata/code_tables/", "DutyStations", "csv")$file
ds_ref  <- read_csv(ds_file, col_types = cols(.default = "c"), na = "")

# Two-character country codes that represent US states and territories
st_terr <- ds_ref |>
  mutate(country_code = str_sub(Code, start = 1, end = 2)) |>
  filter(Country == "UNITED STATES") |>
  distinct(country_code)

# State name -> abbreviation -> FIPS lookup (50 states + DC). The FIPS code and
# the name come from the DutyStations table: the first two characters of a US
# station code are the state FIPS (territories carry an alphabetic pair and drop
# out of the digit filter). The USPS abbreviation is not in that table, so it
# comes from base R's state.name / state.abb constants plus a row for DC. Both
# sources are frozen, so this needs no data scan.
usps <- tibble(
  duty_station_state              = str_to_upper(c(state.name, "District of Columbia")),
  duty_station_state_abbreviation = c(state.abb, "DC")
)

state_lookup <- ds_ref |>
  mutate(country_code = str_sub(Code, start = 1, end = 2)) |>
  filter(Country == "UNITED STATES",
         str_detect(country_code, "^[:digit:][:digit:]$")) |>
  distinct(country_code, duty_station_state = State) |>
  inner_join(usps, by = "duty_station_state") |>
  mutate(duty_station_state_code = country_code) |>
  select(duty_station_state,
         duty_station_state_abbreviation,
         duty_station_state_code,
         country_code)

rm(ds_ref, usps)

# Assert the lookup still covers 50 states + DC -- by count and
# by identity, because neither alone is enough. The count is fixed at 50 states plus
# DC. The identity check is against DutyStations' numeric codes; its alphabetic codes
# are territories, which carry no state name in FWD and are legitimately absent.
state_codes <- st_terr |>
  filter(str_detect(country_code, "[:digit:][:digit:]")) |>
  pull(country_code)

stopifnot(nrow(state_lookup) == 51,
          setequal(state_lookup$country_code, state_codes))

# Arrow accepts relationship = "many-to-one" but silently ignores it, so the three
# joins below omit the argument rather than implying a guarantee it does not give.
# Assert the join key is unique instead: a duplicate would multiply hive rows.
stopifnot(nrow(state_lookup) == nrow(distinct(state_lookup, country_code)))

#### Employment ####

emp <- open_dataset("buzz_feed/bz_emp_hive_staging",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

emp <- emp |>
  mutate(duty_station_country_code = if_else(country_code %in% st_terr$country_code,
                                             "US", country_code),
         duty_station_country      = if_else(country_code %in% st_terr$country_code,
                                             "UNITED STATES", country_name)) |>
  rename(duty_station_county      = county_name,
         duty_station_county_code = county_code,
         duty_station_city        = city_name)

emp <- left_join(emp, state_lookup,
                 by = "country_code",
                 na_matches = "never") |>
  select(!c(country_code, country_name))

write_dataset(emp,
              path = "emp_hive",
              format = "parquet",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(emp)
gc()

#### Accessions ####

acc <- open_dataset("buzz_feed/bz_acc_hive_staging",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

acc <- acc |>
  mutate(duty_station_country_code = if_else(country_code %in% st_terr$country_code,
                                             "US", country_code),
         duty_station_country      = if_else(country_code %in% st_terr$country_code,
                                             "UNITED STATES", country_name),
         personnel_action_effective_date_yyyymm = paste0(as.character(year),
                                                         str_pad(as.character(month),
                                                                 2, pad = "0"))) |>
  rename(duty_station_county      = county_name,
         duty_station_county_code = county_code,
         duty_station_city        = city_name)

acc <- left_join(acc, state_lookup,
                 by = "country_code",
                 na_matches = "never") |>
  select(!c(country_code, country_name))

write_dataset(acc,
              path = "acc_hive",
              format = "parquet",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(acc)
gc()

#### Separations ####

sep <- open_dataset("buzz_feed/bz_sep_hive_staging",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

sep <- sep |>
  mutate(duty_station_country_code = if_else(country_code %in% st_terr$country_code,
                                             "US", country_code),
         duty_station_country      = if_else(country_code %in% st_terr$country_code,
                                             "UNITED STATES", country_name),
         personnel_action_effective_date_yyyymm = paste0(as.character(year),
                                                         str_pad(as.character(month),
                                                                 2, pad = "0"))) |>
  rename(duty_station_county      = county_name,
         duty_station_county_code = county_code,
         duty_station_city        = city_name)

sep <- left_join(sep, state_lookup,
                 by = "country_code",
                 na_matches = "never") |>
  select(!c(country_code, country_name))

write_dataset(sep,
              path = "sep_hive",
              format = "parquet",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(sep)
gc()

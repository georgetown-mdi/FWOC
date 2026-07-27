# Author: Mark Richardson and Claude Code
# Purpose: Label duty station in the emp/status, accessions, and separations
#          data for BZ era and write it to the staging hives.
#          Runs after 05, which rebuilds those hives unlabelled. BZ is frozen,
#          so 05 and 07 are first-run-only -- but skip or run them as a PAIR:
#          05 alone leaves the BZ era with no duty station labels; 07 alone
#          re-joins the label columns onto themselves, and Arrow suffixes them
#          .x/.y rather than erroring. The guards below halt before that join.

# Load packages
library(arrow)
library(dplyr)
library(data.table)
library(lubridate)
library(readr)
library(stringr)

#### Get the mapping ####

load("metadata/labels/duty_station/ds_labels.RData")

#### Employment ####

# Open the emp data
emp <- open_dataset("buzz_feed/bz_emp_hive_staging",
                    partitioning = c("year", "month"))

# Not re-runnable in place: halt if 07 has already labelled this hive
stopifnot(!any(grepl("^(country|county|city)_name", names(emp))))

# Get the observed duty station codes
ds_obs <- emp |>
  select(date, year, month, duty_station_code) |>
  distinct() |>
  collect()

# Extract the country and county codes
ds_obs <- ds_obs |>
  mutate(country_code  = str_sub(duty_station_code, start = 1, end = 2),
         county_code   = str_sub(duty_station_code, start = 7, end = 9))

# Drop missing duty station codes
ds_obs <- ds_obs |>
  filter(!(is.na(duty_station_code) |
             duty_station_code == "#########" |
             duty_station_code == "*********"))

# Join with the observed codes
n <- nrow(ds_obs)

# Country
ds_obs <- left_join(ds_obs,
                    country_cw,
                    by = join_by(year, month, country_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # Assert that the join added no rows

# County
ds_obs <- left_join(ds_obs,
                    county_cw,
                    by = join_by(year, month, country_code, county_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # Assert that the join added no rows

# City
ds_obs <- left_join(ds_obs,
                    city_cw,
                    by = join_by(year, month, duty_station_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # Assert that the join added no rows


# Drop observations with no label
ds_obs <- ds_obs |>
  filter(!(is.na(country_name) &
            is.na(county_name) &
            is.na(city_name)))

# Arrow accepts relationship = "many-to-one" but silently ignores it, so the join
# below omits the argument rather than implying a guarantee it does not give.
# Assert the join key is unique instead: a duplicate would multiply hive rows.
stopifnot(nrow(ds_obs) == nrow(distinct(ds_obs, date, year, month,
                                        duty_station_code, country_code, county_code)))

# Make ds_obs an arrow table for join compatibility
# Note: While month was cast to int8() arrow defaults to int32()
ds_obs <- arrow_table(ds_obs,
                      schema = schema(date = date32(),
                                      year = int32(),
                                      month = int32(),
                                      duty_station_code = utf8(),
                                      country_code = utf8(),
                                      county_code = utf8(),
                                      country_name = utf8(),
                                      county_name = utf8(),
                                      city_name = utf8()))


# Parse duty station codes
emp <- emp |>
  mutate(country_code = str_sub(duty_station_code, start = 1, end = 2),
         county_code  = str_sub(duty_station_code, start = 7, end = 9))

# Join with the labels
emp <- left_join(emp, ds_obs,
                 na_matches = "never",
                 by = c("date", "year", "month",
                        "duty_station_code", "country_code", "county_code"))

# Write the revised data set

write_dataset(emp,
              path = "buzz_feed/bz_emp_hive_staging",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(ds_obs, emp)

#### Accessions ####

acc <- open_dataset("buzz_feed/bz_acc_hive_staging",
                    partitioning = c("year", "month"))

# Not re-runnable in place: halt if 07 has already labelled this hive
stopifnot(!any(grepl("^(country|county|city)_name", names(acc))))

ds_obs <- acc |>
  select(eff_date, year, month, duty_station_code) |>
  distinct() |>
  collect()

ds_obs <- ds_obs |>
  mutate(country_code  = str_sub(duty_station_code, start = 1, end = 2),
         county_code   = str_sub(duty_station_code, start = 7, end = 9))

ds_obs <- ds_obs |>
  filter(!(is.na(duty_station_code) |
             duty_station_code == "#########" |
             duty_station_code == "*********"))

n <- nrow(ds_obs)

# Country
ds_obs <- left_join(ds_obs,
                    country_cw,
                    by = join_by(year, month, country_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # join added no rows

# County
ds_obs <- left_join(ds_obs,
                    county_cw,
                    by = join_by(year, month, country_code, county_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # join added no rows

# City
ds_obs <- left_join(ds_obs,
                    city_cw,
                    by = join_by(year, month, duty_station_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # join added no rows


ds_obs <- ds_obs |>
  filter(!(is.na(country_name) &
             is.na(county_name) &
             is.na(city_name)))

# Arrow accepts relationship = "many-to-one" but silently ignores it, so the join
# below omits the argument rather than implying a guarantee it does not give.
# Assert the join key is unique instead: a duplicate would multiply hive rows.
stopifnot(nrow(ds_obs) == nrow(distinct(ds_obs, eff_date, year, month,
                                        duty_station_code, country_code, county_code)))

ds_obs <- arrow_table(ds_obs,
                      schema = schema(eff_date = date32(),
                                      year = int32(),
                                      month = int32(),
                                      duty_station_code = utf8(),
                                      country_code = utf8(),
                                      county_code = utf8(),
                                      country_name = utf8(),
                                      county_name = utf8(),
                                      city_name = utf8()))


acc <- acc |>
  mutate(country_code = str_sub(duty_station_code, start = 1, end = 2),
         county_code  = str_sub(duty_station_code, start = 7, end = 9))

acc <- left_join(acc, ds_obs,
                 na_matches = "never",
                 by = c("eff_date", "year", "month",
                        "duty_station_code", "country_code", "county_code"))

write_dataset(acc,
              path = "buzz_feed/bz_acc_hive_staging",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(ds_obs, acc)

#### Separations ####

sep <- open_dataset("buzz_feed/bz_sep_hive_staging",
                    partitioning = c("year", "month"))

# Not re-runnable in place: halt if 07 has already labelled this hive
stopifnot(!any(grepl("^(country|county|city)_name", names(sep))))

ds_obs <- sep |>
  select(eff_date, year, month, duty_station_code) |>
  distinct() |>
  collect()

ds_obs <- ds_obs |>
  mutate(country_code  = str_sub(duty_station_code, start = 1, end = 2),
         county_code   = str_sub(duty_station_code, start = 7, end = 9))

ds_obs <- ds_obs |>
  filter(!(is.na(duty_station_code) |
             duty_station_code == "#########" |
             duty_station_code == "*********"))

n <- nrow(ds_obs)

# Country
ds_obs <- left_join(ds_obs,
                    country_cw,
                    by = join_by(year, month, country_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # join added no rows

# County
ds_obs <- left_join(ds_obs,
                    county_cw,
                    by = join_by(year, month, country_code, county_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # join added no rows

# City
ds_obs <- left_join(ds_obs,
                    city_cw,
                    by = join_by(year, month, duty_station_code),
                    na_matches = "never",
                    relationship = "many-to-one")

stopifnot(n == nrow(ds_obs)) # join added no rows


ds_obs <- ds_obs |>
  filter(!(is.na(country_name) &
             is.na(county_name) &
             is.na(city_name)))

# Arrow accepts relationship = "many-to-one" but silently ignores it, so the join
# below omits the argument rather than implying a guarantee it does not give.
# Assert the join key is unique instead: a duplicate would multiply hive rows.
stopifnot(nrow(ds_obs) == nrow(distinct(ds_obs, eff_date, year, month,
                                        duty_station_code, country_code, county_code)))

ds_obs <- arrow_table(ds_obs,
                      schema = schema(eff_date = date32(),
                                      year = int32(),
                                      month = int32(),
                                      duty_station_code = utf8(),
                                      country_code = utf8(),
                                      county_code = utf8(),
                                      country_name = utf8(),
                                      county_name = utf8(),
                                      city_name = utf8()))


sep <- sep |>
  mutate(country_code = str_sub(duty_station_code, start = 1, end = 2),
         county_code  = str_sub(duty_station_code, start = 7, end = 9))

sep <- left_join(sep, ds_obs,
                 na_matches = "never",
                 by = c("eff_date", "year", "month",
                        "duty_station_code", "country_code", "county_code"))

write_dataset(sep,
              path = "buzz_feed/bz_sep_hive_staging",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(ds_obs, sep)

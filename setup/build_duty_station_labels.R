# Author: Mark Richardson, Benjamin Reese, and Claude Code
# Purpose: Parse the OPM duty-station codes from the fixed-width SCTFILE.TXT
#          (shipped with the BuzzFeed release) and build the country/county/city
#          label crosswalks. Re-run only if SCTFILE.TXT changes.

library(dplyr)
library(data.table)
library(readr)
library(stringr)

source("utils/label_functions.R")

#### Parse the OPM code definitions (SCTFILE.TXT) ####

# SCTFILE.TXT layout: 2-char table_id, 9-char data_code, two (from, until) pairs
# of data-code validity dates, then six (label, from, until) translation triplets.
widths <- fwf_widths(c(2, 9, rep(6, times = 4), rep(c(40, 6, 6), times = 6)),
  col_names = c(
    "table_id", "data_code", "data_code_f1",
    "data_code_u1", "data_code_f2", "data_code_u2",
    "trans_1", "trans_f1", "trans_u1",
    "trans_2", "trans_f2", "trans_u2",
    "trans_3", "trans_f3", "trans_u3",
    "trans_4", "trans_f4", "trans_u4",
    "trans_5", "trans_f5", "trans_u5",
    "trans_6", "trans_f6", "trans_u6"
  )
)

# Read all columns as character; drop empty / unparseable rows (a real code row
# always has table_id + data_code) generically rather than by row number.
opm_codes <- read_fwf("buzz_feed/data/1973-09-to-2014-06/SCTFILE.TXT",
  col_positions = widths,
  col_types = cols(.default = "c")
) |>
  filter(!is.na(table_id), !is.na(data_code))

# Stack the 6 translation triplets into one long table, dropping empty translations.
tr <- list()
tr_main <- paste0("trans_", 1:6)
tr_f <- paste0("trans_f", 1:6)
tr_u <- paste0("trans_u", 1:6)

for (i in seq_along(tr_main)) {
  tr[[i]] <- opm_codes |>
    filter(!is.na(.data[[tr_main[i]]])) |>
    select(!(data_code_f1:data_code_u2)) |>
    select(table_id, data_code, contains(tr_main[i]):contains(tr_u[i])) |>
    rename(
      trans = contains(tr_main[i]),
      trans_f = contains(tr_f[i]),
      trans_u = contains(tr_u[i])
    )
}

opm_codes_stk <- rbindlist(tr, use.names = TRUE) |>
  tibble::as_tibble() |>
  mutate(
    year_f = as.integer(str_sub(trans_f, 1, 4)),
    year_u = as.integer(str_sub(trans_u, 1, 4)),
    month_f = as.integer(str_sub(trans_f, 5, 6)),
    month_u = as.integer(str_sub(trans_u, 5, 6))
  )

# Extract the duty station codes and replace sentinel values with period end
ds_map <- opm_codes_stk |>
  filter(table_id == "VX") |>
  mutate(
    year_u = if_else(year_u == 9999,
      2014, year_u
    ),
    month_u = if_else(month_u == 99,
      6, month_u
    ),
    trans_f = as.integer(trans_f),
    trans_u = as.integer(trans_u)
  )

# Note: US states and territories are same as country code (i.e., technically, there is no US country code)
#       City codes use the full duty station code

#### Country codes map ####
country <- ds_map |>
  filter(str_sub(data_code, start = 3, end = 9) == "0000000")

country_ov <- opmOverLap(as.data.table(country))

table(country_ov$ov) # 16 overlaps of some sort

table(country_ov$overlap_transition_single) # All single period overlaps

country_ov <- opmCleanSingle(country_ov)

country_cw <- opmFull(country_ov)

country_cw <- country_cw |>
  mutate(country_code = str_sub(data_code, start = 1, end = 2)) |>
  select(!data_code) |>
  rename(country_name = label)

#### County codes map ####

county <- ds_map |>
  filter(str_sub(data_code, start = 3, end = 6) == "0000" &
    str_sub(data_code, start = 7, end = 9) != "000") |>
  mutate(
    country_code = str_sub(data_code, start = 1, end = 2),
    county_code = str_sub(data_code, start = 7, end = 9)
  )

table(str_sub(county$data_code, start = 1, end = 2)) # All US states

county_ov <- opmOverLap(as.data.table(county))

table(county_ov$ov) # 1 overlaps of some sort

table(county_ov$overlap_transition_single) # All single period overlaps

county_ov <- opmCleanSingle(county_ov)

county_cw <- opmFull(county_ov)

county_cw <- county_cw |>
  mutate(
    country_code = str_sub(data_code, start = 1, end = 2),
    county_code = str_sub(data_code, start = 7, end = 9)
  ) |>
  select(!data_code) |>
  rename(county_name = label)

#### City codes map ####

city <- ds_map |>
  filter(str_sub(data_code, start = 3, end = 6) != "0000") |>
  mutate(
    country_code = str_sub(data_code, start = 1, end = 2),
    county_code = str_sub(data_code, start = 7, end = 9)
  )

city_ov <- opmOverLap(as.data.table(city)) # Note: Use the full data code for cities, otherwise, duplications are over counted

table(city_ov$ov) # 125 overlaps

table(city_ov$overlap_transition_single) # All are single transitions

city_ov <- opmCleanSingle(city_ov)

city_cw <- opmFull(city_ov)

city_cw <- city_cw |>
  rename(
    city_name = label,
    duty_station_code = data_code
  )

# Save the three crosswalks, compressed. ds_map is an intermediate that is not
# used downstream, so it is dropped. xz keeps the period-expanded city_cw
# (~22M rows) under ~1 MB on disk, so the artifact ships in the repo.
dir.create("metadata/labels/duty_station", recursive = TRUE, showWarnings = FALSE)

save(country_cw,
  county_cw,
  city_cw,
  file = "metadata/labels/duty_station/ds_labels.RData",
  compress = "xz"
)

# Author: Mark Richardson and Claude Code
# Purpose: Harmonize the duty station state variables across both eras and add
#          state+territory label variables. Overwrites OPM inconsistencies; the
#          changes are documented in the codebook.
#
# Overwrites:
#   duty_station_state_code         - rebuild from the state name: FIPS for US
#                                     states, NA for non-US and territories.
#                                     This removes the country code leak (OPM
#                                     puts territory and country codes here) and
#                                     repairs 2015+, where the column is absent
#                                     from the current file version and so
#                                     arrives here entirely NULL
#   duty_station_state              - NA for non-US and territories (FWD wrote
#                                     "NO DATA REPORTED", BZ wrote NA; this is
#                                     where the two eras are reconciled)
#   duty_station_state_abbreviation - NA for non-US and territories (was "NDR")
#   duty_station_city               - backfill NAs from the DutyStations lookup
#                                     where duty_station_code is a valid 9-digit
#                                     code
#
# New variables:
#   duty_station_state_territory              - US state or territory name
#   duty_station_state_territory_abbreviation - USPS code for states and
#                                               territories
#
# REDACTED is preserved as a literal value wherever OPM published it. It is a
# real value OPM publishes in these fields, not a missing marker, so it must
# survive the backfill rather than being nulled with the non-US rows.
#
# duty_station_city is the exception, and not because anything is discarded:
# OPM did not publish the column at all before 2015, so those years arrive
# entirely NULL and hold no REDACTED token to preserve. The backfill only adds
# city values (from the DutyStations lookup, where duty_station_code matches);
# it never nulls one. Rows that are redacted in unpublished OPM data therefore
# end up NA rather than REDACTED before 2015 -- an absence in the source,
# not a choice made here. Documented in the codebook.

library(arrow)
library(dplyr)
library(readr)
library(stringr)
# lubridate is attached for find_code_table(), which calls ymd() and assumes the
# caller attached it.
library(lubridate)

# find_code_table() resolves the current DutyStations_<date>.csv, so a new
# monthly download needs no edit here (it errors if 0 or >1 match).
source("utils/code_table_functions.R")

# Arrow silently ignores relationship = "many-to-one" -- it accepts any string
# without checking -- so a duplicated key on the right side of a join inflates
# hive rows with no error, warning or message. Every join below is onto an Arrow
# dataset, so each right-side key is checked here instead.
stop_if_not_unique <- function(tbl, key, name) {
  if (nrow(tbl) != nrow(distinct(tbl, across(all_of(key))))) {
    stop(name, " is not unique on ", paste(key, collapse = " + "),
         " -- joining it would inflate hive rows.")
  }
  invisible(tbl)
}


# ============================================================
#### Build crosswalks ####
# ============================================================

# na = "" so a literal "NA"-style value is kept as a string rather than dropped
# to missing.
ds_file <- find_code_table("metadata/code_tables/", "DutyStations", "csv")$file
ds_ref  <- read_csv(ds_file, col_types = cols(.default = "c"), na = "")

# --- State name -> FIPS crosswalk (51 rows: 50 states + DC) ---
# The first two characters of a duty station Code are the state FIPS for US
# rows; territories carry an alphabetic pair instead and are handled below.
state_cw <- ds_ref |>
  mutate(fips_code = substr(Code, 1, 2)) |>
  filter(Country == "UNITED STATES" & str_detect(fips_code, "^[0-9]{2}$")) |>
  distinct(fips_code, state_name = State)

# --- Duty station code -> city crosswalk ---
city_cw <- ds_ref |>
  select(duty_station_code = Code, ref_city = City) |>
  distinct()

rm(ds_ref)

# USPS abbreviations are not in the DutyStations table, so take them from the
# hive wherever both the state name and its abbreviation are populated with real
# values. NDR / NO DATA REPORTED / REDACTED / INVALID are OPM sentinels, not
# state names, and would otherwise become crosswalk rows.
emp_meta <- open_dataset("emp_hive/",
                         partitioning  = c("year", "month"),
                         unify_schemas = TRUE)

abbr <- emp_meta |>
  filter(!is.na(duty_station_state_abbreviation) &
           duty_station_state_abbreviation != "REDACTED" &
           duty_station_state_abbreviation != "NDR" &
           !is.na(duty_station_state) &
           duty_station_state != "REDACTED" &
           duty_station_state != "NO DATA REPORTED" &
           duty_station_state != "INVALID") |>
  select(duty_station_state, duty_station_state_abbreviation) |>
  distinct() |>
  collect()

rm(emp_meta)

# relationship = "one-to-one" is kept here and DOES enforce: both sides are
# plain data frames, so this is dplyr's join, not Arrow's. It fails loudly if
# the hive ever supplies two abbreviations for one state name.
state_cw <- left_join(state_cw, abbr,
                      by = c("state_name" = "duty_station_state"),
                      relationship = "one-to-one") |>
  rename(state_usps = duty_station_state_abbreviation)

rm(abbr)

if (nrow(state_cw) != 51) {
  stop("state_cw has ", nrow(state_cw), " rows, expected 51 (50 states + DC). ",
       "Check the DutyStations table for added or dropped US rows.")
}
if (any(is.na(state_cw$state_usps))) {
  stop("state_cw is missing a USPS abbreviation for: ",
       paste(state_cw$state_name[is.na(state_cw$state_usps)], collapse = ", "),
       ". The hive supplied no abbreviation for these state names.")
}

# --- Territory Q-code crosswalk (14 rows) ---
# Territories carry an alphabetic pair in the first two characters of the duty
# station code rather than a numeric FIPS, so they are absent from state_cw and
# are mapped here instead. UM is the USPS code for the US Minor Outlying Islands,
# which is why nine of these share it.
terr_cw <- tribble(
  ~q_code, ~territory_name,            ~territory_usps,
  "AQ",    "AMERICAN SAMOA",           "AS",
  "BQ",    "NAVASSA ISLAND",           "UM",
  "CQ",    "NORTHERN MARIANA ISLANDS", "MP",
  "DQ",    "JARVIS ISLAND",            "UM",
  "FQ",    "BAKER ISLAND",             "UM",
  "GQ",    "GUAM",                     "GU",
  "HQ",    "HOWLAND ISLAND",           "UM",
  "JQ",    "JOHNSTON ATOLL",           "UM",
  "KQ",    "KINGMAN REEF",             "UM",
  "LQ",    "PALMYRA ATOLL",            "UM",
  "MQ",    "MIDWAY ISLANDS",           "UM",
  "RQ",    "PUERTO RICO",              "PR",
  "VQ",    "VIRGIN ISLANDS",           "VI",
  "WQ",    "WAKE ISLAND",              "UM"
)

stop_if_not_unique(state_cw, "state_name",        "state_cw")
stop_if_not_unique(terr_cw,  "q_code",            "terr_cw")
stop_if_not_unique(city_cw,  "duty_station_code", "city_cw")

# Written for provenance only -- nothing in the pipeline reads them back. They
# record what the build actually used, so a published crosswalk can be diffed
# against a later monthly download. state_cw is sorted first: it is built from an
# Arrow distinct(), whose row order is not stable between runs, and an unsorted
# write could show reorder diffs every build whether or not any content changed.
# terr_cw is a tribble, so its order is already fixed.
write_csv(arrange(state_cw, fips_code), "metadata/crosswalks/state_fips_crosswalk.csv", na = "")
write_csv(terr_cw,  "metadata/crosswalks/territory_crosswalk.csv",  na = "")


# ============================================================
#### Duty station harmonization ####
# ============================================================

# The two columns this script creates. Dropped before the joins so a re-run
# rebuilds them rather than colliding with its own output.
new_cols <- c("duty_station_state_territory",
              "duty_station_state_territory_abbreviation")

# Columns brought in by the three joins purely to drive the case_when logic.
# They are removed again before the write so they never reach the hive.
join_cols <- c("ds_code_prefix", "fips_code",
               "territory_name", "territory_usps", "ref_city")

# The whole harmonization, written once and applied identically to emp, acc and
# sep. fips_code is the US/non-US discriminator throughout: it is non-NA only
# when the row's state name matched a real US state, so `!is.na(fips_code)`
# means "this is a US state row" in every branch below.
harmonize_duty_station <- function(data) {
  data |>
    select(-any_of(new_cols)) |>
    mutate(ds_code_prefix = str_sub(duty_station_code, 1, 2)) |>
    # state_usps is dropped rather than joined: nothing below reads it.
    left_join(select(state_cw, -state_usps),
              by = c("duty_station_state" = "state_name"),
              na_matches = "never") |>
    left_join(terr_cw, by = c("ds_code_prefix" = "q_code"),
              na_matches = "never") |>
    mutate(
      # duty_station_state_code is 100% null for 2015+ in FWD while
      # duty_station_state is populated for 2015+. The column is not missing
      # from all versions of OPM's data: OPM publishes several file versions
      # per month, script 04 reads the current (highest) one, and from
      # 2015-01 that version dropped the column while superseded versions
      # still carry it. Therefore, we populate the code with REDACTED when
      # the name is REDACTED to populate periods when the code is
      # structurally missing. The branch is not year-scoped: pre-2015 it
      # reproduces the value the source already carried, so it is a no-op
      # there.
      duty_station_state_code = case_when(
        duty_station_state == "REDACTED" ~ "REDACTED",
        duty_station_state_code == "**"  ~ "**",
        !is.na(fips_code)                ~ fips_code,
        TRUE                             ~ NA_character_
      ),
      duty_station_state = case_when(
        duty_station_state == "REDACTED" ~ "REDACTED",
        !is.na(fips_code)                ~ duty_station_state,
        TRUE                             ~ NA_character_
      ),
      duty_station_state_abbreviation = case_when(
        duty_station_state_abbreviation == "REDACTED" ~ "REDACTED",
        !is.na(fips_code)                             ~ duty_station_state_abbreviation,
        TRUE                                          ~ NA_character_
      ),
      duty_station_state_territory = case_when(
        duty_station_state == "REDACTED" ~ "REDACTED",
        !is.na(fips_code)                ~ duty_station_state,
        !is.na(territory_name)           ~ territory_name,
        TRUE                             ~ NA_character_
      ),
      duty_station_state_territory_abbreviation = case_when(
        duty_station_state_abbreviation == "REDACTED" ~ "REDACTED",
        !is.na(fips_code)                             ~ duty_station_state_abbreviation,
        !is.na(territory_usps)                        ~ territory_usps,
        TRUE                                          ~ NA_character_
      )
    ) |>
    left_join(city_cw, by = "duty_station_code", na_matches = "never") |>
    mutate(
      duty_station_city = case_when(
        duty_station_city == "REDACTED" ~ "REDACTED",
        !is.na(duty_station_city)       ~ duty_station_city,
        !is.na(ref_city)                ~ ref_city,
        TRUE                            ~ NA_character_
      )
    ) |>
    select(-any_of(join_cols))
}


# ============================================================
#### Employment ####
# ============================================================

# Process emp_hive one partition file at a time to bound peak RAM.
parts <- list.files("emp_hive", pattern = "\\.parquet$",
                    recursive = TRUE, full.names = TRUE)

for (i in seq_along(parts)) {
  f  <- parts[i]
  ym <- sub(".*/year=([0-9]+)/month=([0-9]+)/.*", "\\1-\\2", f)
  message(sprintf("[%d/%d] %s", i, length(parts), ym))

  # Write to a temporary file in the SAME directory and rename over the target,
  # so a process killed mid-write leaves the old partition intact rather than a
  # truncated one with no source to rebuild from. Same directory keeps the
  # rename on one filesystem, where it is atomic; the leading dot keeps a
  # leftover temp file from being read as data by open_dataset(); and the
  # rename is checked because file.rename() returns FALSE rather than erroring.
  tmp <- file.path(dirname(f), paste0(".", basename(f), ".tmp"))

  read_parquet(f, as_data_frame = FALSE) |>
    harmonize_duty_station() |>
    compute() |>
    write_parquet(tmp, compression = "zstd", use_dictionary = FALSE)

  # Evaluating file.rename() inside if() executes the rename.
  if (!file.rename(tmp, f)) stop("failed to replace partition file: ", f)
  gc()
}

rm(parts)
gc()


# ============================================================
#### Accessions ####
# ============================================================

acc <- open_dataset("acc_hive/",
                    partitioning  = c("year", "month"),
                    unify_schemas = TRUE) |>
  harmonize_duty_station()

write_dataset(acc,
              path                   = "acc_hive",
              format                 = "parquet",
              partitioning           = c("year", "month"),
              hive_style             = TRUE,
              compression            = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary         = FALSE)

rm(acc)
gc()


# ============================================================
#### Separations ####
# ============================================================

sep <- open_dataset("sep_hive/",
                    partitioning  = c("year", "month"),
                    unify_schemas = TRUE) |>
  harmonize_duty_station()

write_dataset(sep,
              path                   = "sep_hive",
              format                 = "parquet",
              partitioning           = c("year", "month"),
              hive_style             = TRUE,
              compression            = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary         = FALSE)

rm(sep)
gc()

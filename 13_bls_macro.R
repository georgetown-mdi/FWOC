# Author: Mark Richardson, Benjamin Reese, and Claude Code
# Purpose: Join BLS macro covariates (CPI-U, national unemployment, state
#          unemployment) to the emp, acc and sep hives and add a real
#          (inflation-adjusted) salary variable. CPI is rebased to the 2024
#          annual average so annualized_adjusted_basic_pay_real_2024 is
#          expressed in 2024 dollars. Joins are by (year, month) -- the natural
#          grain of monthly BLS data -- and, for state unemployment, also by the
#          2-digit state FIPS that matches duty_station_state_code.
#
# Coverage: cpi_u starts in 1913 and unrate_us in 1948, so both span the whole
#          FWOC range. unrate_state starts in 1976, so 1973-1975 rows carry NA
#          there by construction, not by error. Rows whose
#          duty_station_state_code is not a FIPS (REDACTED, "**", NA -- see 12)
#          match no state and carry NA unrate_state, also by construction.
#
# ORDER:   this script MUST run after 12. The state-unemployment join keys on
#          duty_station_state_code, and 12 is what populates that column for
#          2015+, where OPM's current file version omits it entirely. Measured
#          before 13 runs: 2015-03 and 2024-09 come back 100% NA on
#          unrate_state. The failure is silent -- the join simply matches
#          nothing -- so it would ship as a wholly empty column for the FWD era
#          rather than raising anything.

library(arrow)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)

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

bls_path <- "metadata/bls/formatted/"


# ============================================================
#### Read BLS series and compute CPI base ####
# ============================================================

# na = "" so a literal "NA"-style value is kept as a string rather than dropped
# to missing. col_types has no date field: setup/get_bls_data.R no longer writes
# a `date` column, since every join here is by (year, month) to avoid
# month-end-vs-first-of-month arithmetic. A col_types/header mismatch only WARNS
# in readr -- it returns a mangled frame and carries on -- so these strings must
# track the CSVs exactly.
cpi       <- read_csv(paste0(bls_path, "cpi.csv"),
                      col_types = "iid",  na = "")
unrate_us <- read_csv(paste0(bls_path, "unrate_national.csv"),
                      col_types = "iid",  na = "")
unrate_st <- read_csv(paste0(bls_path, "unrate_state.csv"),
                      col_types = "ciid", na = "")

# The 2024 annual average is the deflator base for every real-pay value in all
# three hives. mean(numeric(0)) is NaN, so a missing or partial 2024 would turn
# every annualized_adjusted_basic_pay_real_2024 into NaN with no error anywhere
# -- and a partial year would produce a base that is quietly wrong rather than
# obviously broken. Both cases must halt.
missing_2024 <- setdiff(1:12, cpi$month[cpi$year == 2024])
if (length(missing_2024) > 0) {
  stop("cpi.csv is missing 2024 month(s): ",
       paste(missing_2024, collapse = ", "),
       " -- the 2024 annual average is the deflator base for every real-pay ",
       "value in all three hives, so a partial base must not be used.")
}

# 2024 annual-average CPI-U: locked deflator base. Real dollars are stable
# across re-runs of this script regardless of which months are available.
cpi_2024 <- cpi |>
  filter(year == 2024) |>
  pull(cpi_u) |>
  mean(na.rm = TRUE)

cpi <- cpi |>
  mutate(cpi_index = cpi_u / cpi_2024)

stop_if_not_unique(cpi,       c("year", "month"),               "cpi")
stop_if_not_unique(unrate_us, c("year", "month"),               "unrate_us")
stop_if_not_unique(unrate_st, c("state_fips", "year", "month"), "unrate_st")

# Wrap as arrow_tables so partition-loop joins stay in arrow.
cpi_arr       <- arrow_table(cpi,
                             schema = schema(year      = int32(),
                                             month     = int32(),
                                             cpi_u     = float64(),
                                             cpi_index = float64()))
unrate_us_arr <- arrow_table(unrate_us,
                             schema = schema(year      = int32(),
                                             month     = int32(),
                                             unrate_us = float64()))
unrate_st_arr <- arrow_table(unrate_st,
                             schema = schema(state_fips   = utf8(),
                                             year         = int32(),
                                             month        = int32(),
                                             unrate_state = float64()))

rm(cpi, unrate_us, unrate_st)
gc()


# ============================================================
#### Employment ####
# ============================================================

# Process emp_hive one partition file at a time to bound peak RAM.
# Each iteration is idempotent: existing BLS / real-pay columns are dropped
# before being recomputed, so a partial run can be safely re-executed. That
# drop is required, not defensive -- without it a second run would re-join
# columns the partition already carries and Arrow would suffix them .x/.y
# silently.
parts <- list.files("emp_hive", pattern = "\\.parquet$",
                    recursive = TRUE, full.names = TRUE)

bls_cols <- c("cpi_u", "cpi_index", "unrate_us", "unrate_state",
              "annualized_adjusted_basic_pay_real_2024")

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

  # year and month are partition keys: they live in the path, not in the parquet
  # file, so they cannot be joined on directly here. The join grain is rebuilt
  # from `date` instead. emp dates are end-of-period (BZ Q4 is 12-13, FWD is
  # month-end) but only the year and month are taken, so the day-of-month
  # convention never matters.
  read_parquet(f, as_data_frame = FALSE) |>
    select(-any_of(bls_cols)) |>
    mutate(bls_year  = cast(year(date),  int32()),
           bls_month = cast(month(date), int32())) |>
    left_join(cpi_arr,       by = c("bls_year" = "year", "bls_month" = "month"),
              na_matches = "never") |>
    left_join(unrate_us_arr, by = c("bls_year" = "year", "bls_month" = "month"),
              na_matches = "never") |>
    left_join(unrate_st_arr, by = c("bls_year" = "year", "bls_month" = "month",
                                    "duty_station_state_code" = "state_fips"),
              na_matches = "never") |>
    mutate(annualized_adjusted_basic_pay_real_2024 =
             annualized_adjusted_basic_pay_num / cpi_index) |>
    select(-bls_year, -bls_month) |>
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

# acc and sep are read as whole datasets, where year and month ARE exposed as
# partition columns, so they join directly with no date arithmetic.
acc <- open_dataset("acc_hive/",
                    partitioning  = c("year", "month"),
                    unify_schemas = TRUE) |>
  select(-any_of(bls_cols)) |>
  left_join(cpi_arr,       by = c("year", "month"), na_matches = "never") |>
  left_join(unrate_us_arr, by = c("year", "month"), na_matches = "never") |>
  left_join(unrate_st_arr, by = c("year"  = "year",
                                  "month" = "month",
                                  "duty_station_state_code" = "state_fips"),
            na_matches = "never") |>
  mutate(annualized_adjusted_basic_pay_real_2024 =
           annualized_adjusted_basic_pay_num / cpi_index)

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
  select(-any_of(bls_cols)) |>
  left_join(cpi_arr,       by = c("year", "month"), na_matches = "never") |>
  left_join(unrate_us_arr, by = c("year", "month"), na_matches = "never") |>
  left_join(unrate_st_arr, by = c("year"  = "year",
                                  "month" = "month",
                                  "duty_station_state_code" = "state_fips"),
            na_matches = "never") |>
  mutate(annualized_adjusted_basic_pay_real_2024 =
           annualized_adjusted_basic_pay_num / cpi_index)

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

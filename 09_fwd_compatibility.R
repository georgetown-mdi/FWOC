# Author: Mark Richardson and Claude Code
# Purpose: Harmonize data.opm.gov (FWD) with the BZ conventions -- ordinal length
#          of service, 1/0 indicators, and an end-of-month date and quarter -- and
#          write the 2005-onward data to the final hives (emp_hive, acc_hive,
#          sep_hive). Runs alongside 08, which writes the pre-2005 BZ side.

# Load packages
library(arrow)
library(dplyr)
library(lubridate)
source("utils/derive_functions.R")   # add_los_ord(), shared with validation/01_overlap.R

# OPM ships these indicators as two-valued strings. The recodes below map anything
# that is not the positive value to 0, so a value OPM added would land as 0 with no
# error. Each block asserts its source values first; the checks read one column only.
yn_levels  <- c("Y", "N", NA)
cfo_levels <- c("CFO ACT AGENCY", "OTHER AGENCY", NA)

#### Employment ####

# Load data
emp <- open_dataset(sources = "data_opm_gov/fwd_emp_hive",
                    hive_style = TRUE,
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

# Create ordinal los
emp <- add_los_ord(emp)

# Recode OPM indicators to 1/0 in place (cfo_act source is "CFO ACT AGENCY"/"OTHER AGENCY", not Y/N) (scholars want numeric).
# Base name = OPM-pure value; the gap-fill we derive lives in
# nsftp_indicator_fwoc (created in 12_harmonize_labels.R).
# Note: all three indicators are missing in fwd data for 2005 through 2009
# (verified 2026-07-27; OPM does backfill variables retroactively, so re-check).

stopifnot(
  emp |> select(nsftp_indicator)          |> distinct() |> pull(as_vector = TRUE) %in% yn_levels,
  emp |> select(veteran_indicator)        |> distinct() |> pull(as_vector = TRUE) %in% yn_levels,
  emp |> select(cfo_act_agency_indicator) |> distinct() |> pull(as_vector = TRUE) %in% cfo_levels
)

emp <- emp |>
  mutate(nsftp_indicator          = cast(nsftp_indicator          == "Y", int8()),
         veteran_indicator        = cast(veteran_indicator        == "Y", int8()),
         cfo_act_agency_indicator = cast(cfo_act_agency_indicator == "CFO ACT AGENCY", int8()))

# Make a complete date variable to match bz data
# OPM reports as of February 29th for leap years (e.g., 02/29/2012)
# Leap year rule: Every year exactly divisible by 4, except years that are
# exactly divisible by 100, but centennial years are leap years if they are
# exactly divisible by 400

emp <- emp |>
  mutate(date = make_date(year, month, case_when(
    month %in% c(1L, 3L, 5L, 7L, 8L, 10L, 12L)         ~ 31L,
    month %in% c(4L, 6L, 9L, 11L)                      ~ 30L,
    month == 2L & year %% 4L == 0L &
      (year %% 100L != 0L | year %% 400L == 0L)         ~ 29L,
    month == 2L                                         ~ 28L))) |>
  mutate(quarter = cast(quarter(date), int8()))


# Write fwd to combined hive
write_dataset(emp, path = "emp_hive",
              format = "parquet",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(emp)
gc()

#### Accessions ####

acc <- open_dataset(sources = "data_opm_gov/fwd_acc_hive",
                    hive_style = TRUE,
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

# Create ordinal los
acc <- add_los_ord(acc)

# Derive calendar quarter from the effective-date YYYYMM (FWD acc/sep lack the emp `date`
# field; BZ acc/sep get quarter in 02/05). Codebook documents quarter for all three datasets.
acc <- acc |>
  mutate(quarter = cast(quarter(ymd(paste0(personnel_action_effective_date_yyyymm, "01"))), int8()))

# Recode OPM indicators to 1/0 in place (cfo_act source is "CFO ACT AGENCY"/"OTHER AGENCY", not Y/N)
stopifnot(
  acc |> select(nsftp_indicator)          |> distinct() |> pull(as_vector = TRUE) %in% yn_levels,
  acc |> select(veteran_indicator)        |> distinct() |> pull(as_vector = TRUE) %in% yn_levels,
  acc |> select(cfo_act_agency_indicator) |> distinct() |> pull(as_vector = TRUE) %in% cfo_levels
)

acc <- acc |>
  mutate(nsftp_indicator          = cast(nsftp_indicator          == "Y", int8()),
         veteran_indicator        = cast(veteran_indicator        == "Y", int8()),
         cfo_act_agency_indicator = cast(cfo_act_agency_indicator == "CFO ACT AGENCY", int8()))

write_dataset(acc, path = "acc_hive",
              format = "parquet",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(acc)
gc()

#### Separations ####

sep <- open_dataset(sources = "data_opm_gov/fwd_sep_hive",
                    hive_style = TRUE,
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

# Create ordinal los
sep <- add_los_ord(sep)

# Derive calendar quarter from the effective-date YYYYMM (see acc block above).
sep <- sep |>
  mutate(quarter = cast(quarter(ymd(paste0(personnel_action_effective_date_yyyymm, "01"))), int8()))

# Recode OPM indicators to 1/0 in place (cfo_act source is "CFO ACT AGENCY"/"OTHER AGENCY", not Y/N)
stopifnot(
  sep |> select(nsftp_indicator)          |> distinct() |> pull(as_vector = TRUE) %in% yn_levels,
  sep |> select(veteran_indicator)        |> distinct() |> pull(as_vector = TRUE) %in% yn_levels,
  sep |> select(cfo_act_agency_indicator) |> distinct() |> pull(as_vector = TRUE) %in% cfo_levels,
  sep |> select(drp_indicator)            |> distinct() |> pull(as_vector = TRUE) %in% yn_levels
)

sep <- sep |>
  mutate(nsftp_indicator          = cast(nsftp_indicator          == "Y", int8()),
         veteran_indicator        = cast(veteran_indicator        == "Y", int8()),
         cfo_act_agency_indicator = cast(cfo_act_agency_indicator == "CFO ACT AGENCY", int8()),
         drp_indicator            = cast(drp_indicator            == "Y", int8()))

write_dataset(sep, path = "sep_hive",
              format = "parquet",
              partitioning = c("year", "month"),
              hive_style = TRUE,
              compression = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary = FALSE)

rm(sep)
gc()

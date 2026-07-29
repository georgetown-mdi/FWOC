# Author: Mark Richardson and Claude Code
# Purpose: Join agency and subagency labels to emp, acc, and sep hives from the
#          OPM lookup tables. No edits or corrections are applied to `agency`
#          or `agency_subelement` here so they remain the official OPM version.
#          Also derives the FWOC variables:
#            - agency_code_fwoc / agency_fwoc (AF/AR/NV collapsed to DD)
#            - agency_subelement_code_fwoc / agency_subelement_fwoc
#              (AF*/AR*/NV* sub-subagency codes rolled up to DDAF/DDAR/DDNV;
#              other codes pass through unchanged, but labels come from
#              subagency_labels_fwoc.csv which carries minor OPM corrections
#              for the 15 executive departments).
#          The fwoc lookups also carry four correction-tracking columns, joined
#          at both the agency level (_agency suffix) and subagency level
#          (_subagency suffix): case, opm_label_misleading (int8 1/0), func_id
#          and reorg_id.
#          Runs after 08 and 09, which write the BZ and FWD rows of the final
#          hives, and before new variables and the remaining label joins so that
#          downstream scripts operate on a narrower hive.

library(arrow)
library(dplyr)
library(lubridate)
library(readr)
# data.table is attached for label_codes(), which is defined in
# utils/label_functions.R and calls data.table verbs without attaching it.
library(data.table)

source("utils/label_functions.R")

lbl_path <- "metadata/labels/"

# na = "" on label reads so future "NA"-style codes are preserved as
# strings rather than silently dropped to missing.
agy_map      <- read_csv(paste0(lbl_path, "agency_labels.csv"),    col_types = "ccDD", na = "")
subagy_map   <- read_csv(paste0(lbl_path, "subagency_labels.csv"), col_types = "ccDD", na = "")

# fwoc lookups now carry, alongside (code, label, date_from, date_to), four
# correction-tracking columns: case, opm_label_misleading, func_id, reorg_id.
# cols_only() reads exactly those eight and drops the unwanted opm_label_note
# and source_document columns. opm_label_misleading (TRUE/FALSE, with the odd
# stray "F" parsed as FALSE) is recoded to an int8 1/0 indicator downstream;
# case, func_id and reorg_id stay character. na = "" keeps any literal
# "NA"-style code as a string rather than dropping it to missing.
fwoc_cols <- cols_only(
  code                 = col_character(),
  label                = col_character(),
  date_from            = col_date(),
  date_to              = col_date(),
  case                 = col_character(),
  opm_label_misleading = col_logical(),
  func_id              = col_character(),
  reorg_id             = col_character()
)

# Drop AF/AR/NV from the agency map since those codes are collapsed to DD before the join.
agy_fwoc_map <- read_csv(paste0(lbl_path, "agency_labels_fwoc.csv"),
                         col_types = fwoc_cols, na = "") |>
  filter(!code %in% c("AF", "AR", "NV")) |>
  mutate(opm_label_misleading = as.integer(opm_label_misleading))

subagy_fwoc_map <- read_csv(paste0(lbl_path, "subagency_labels_fwoc.csv"),
                            col_types = fwoc_cols, na = "") |>
  mutate(opm_label_misleading = as.integer(opm_label_misleading))

# Pass-through specs for the four correction columns. Agency-level columns get
# the _agency suffix, subagency-level the _subagency suffix; opm_label_misleading
# is typed int8 to match the project's binary-indicator convention, the others
# stay utf8.
agy_extra <- list(
  case_agency                 = list(col = "case",                 type = utf8()),
  opm_label_misleading_agency = list(col = "opm_label_misleading", type = int8()),
  func_id_agency              = list(col = "func_id",              type = utf8()),
  reorg_id_agency             = list(col = "reorg_id",             type = utf8())
)
subag_extra <- list(
  case_subagency                 = list(col = "case",                 type = utf8()),
  opm_label_misleading_subagency = list(col = "opm_label_misleading", type = int8()),
  func_id_subagency              = list(col = "func_id",              type = utf8()),
  reorg_id_subagency             = list(col = "reorg_id",             type = utf8())
)

# The label columns this script adds. Listed once and dropped before the joins
# in each block so a re-run relabels rather than colliding with its own output.
lbl_cols <- c(
  "agency", "agency_subelement",
  "agency_code_fwoc", "agency_fwoc",
  "agency_subelement_code_fwoc", "agency_subelement_fwoc",
  "case_agency", "opm_label_misleading_agency",
  "func_id_agency", "reorg_id_agency",
  "case_subagency", "opm_label_misleading_subagency",
  "func_id_subagency", "reorg_id_subagency"
)

# The FWOC code derivations are each needed twice per hive -- once to build the
# label lookup and once on the hive itself -- so they are defined once here
# rather than written out six times. Both take and return the data object, which
# keeps them usable on an Arrow dataset and on a collected data frame alike.
# AF/AR/NV are the military departments, folded into DD. The codes mapped to NA
# are absent from the OPM master and so have no valid fwoc label; the OPM-pure
# agency_code / agency_subelement_code columns keep them untouched.
add_agency_code_fwoc <- function(data) {
  mutate(data, agency_code_fwoc = case_when(
    agency_code %in% c("AF", "AR", "NV") ~ "DD",
    agency_code %in% c("00", "04") ~ NA_character_,
    TRUE ~ agency_code
  ))
}

add_subelement_code_fwoc <- function(data) {
  mutate(data, agency_subelement_code_fwoc = case_when(
    agency_code == "AF" ~ "DDAF",
    agency_code == "AR" ~ "DDAR",
    agency_code == "NV" ~ "DDNV",
    agency_subelement_code %in% c("00AD", "04GS") ~ NA_character_,
    TRUE ~ agency_subelement_code
  ))
}

# ============================================================
#### Employment ####
# ============================================================

# Build agency / subagency label lookups once over the full hive.
# Output is small (distinct codes x dates), so a single lazy pass is cheap.
emp_meta  <- open_dataset("emp_hive/",
                          partitioning  = c("year", "month"),
                          unify_schemas = TRUE)

agy_obs   <- emp_meta |> select(agency_code,            date) |> distinct() |> collect()
subag_obs <- emp_meta |> select(agency_subelement_code, date) |> distinct() |> collect()

agy_fwoc_obs <- agy_obs |>
  add_agency_code_fwoc() |>
  distinct(agency_code_fwoc, date)

# Subagency fwoc code depends on agency_code, so pull both together.
subag_fwoc_obs <- emp_meta |>
  select(agency_code, agency_subelement_code, date) |>
  distinct() |>
  collect() |>
  add_subelement_code_fwoc() |>
  distinct(agency_subelement_code_fwoc, date)

agy_lbl        <- label_codes(agy_obs,        agy_map,         "agency_code",                 "date", "agency")
subag_lbl      <- label_codes(subag_obs,      subagy_map,      "agency_subelement_code",      "date", "agency_subelement")
agy_fwoc_lbl   <- label_codes(agy_fwoc_obs,   agy_fwoc_map,    "agency_code_fwoc",            "date", "agency_fwoc",            extra = agy_extra)
subag_fwoc_lbl <- label_codes(subag_fwoc_obs, subagy_fwoc_map, "agency_subelement_code_fwoc", "date", "agency_subelement_fwoc", extra = subag_extra)

rm(emp_meta, agy_obs, subag_obs, agy_fwoc_obs, subag_fwoc_obs)
gc()

# Process emp_hive one partition file at a time to bound peak RAM. Each
# completed iteration is idempotent: existing label columns are dropped before
# the joins, so re-running relabels rather than duplicating columns.
parts <- list.files("emp_hive", pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)

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
    select(-any_of(lbl_cols)) |> # Drop this script's own output; a stale join column would become .x/.y
    left_join(agy_lbl,   by = c("date", "agency_code"),            na_matches = "never") |>
    left_join(subag_lbl, by = c("date", "agency_subelement_code"), na_matches = "never") |>
    add_agency_code_fwoc() |>
    add_subelement_code_fwoc() |>
    left_join(agy_fwoc_lbl,   by = c("date", "agency_code_fwoc"),            na_matches = "never") |>
    left_join(subag_fwoc_lbl, by = c("date", "agency_subelement_code_fwoc"), na_matches = "never") |>
    compute() |>
    write_parquet(tmp, compression = "zstd", use_dictionary = FALSE)

  # Evaluating file.rename() inside if() executes the rename.
  if (!file.rename(tmp, f)) stop("failed to replace partition file: ", f)
  gc()
}

rm(agy_lbl, subag_lbl, agy_fwoc_lbl, subag_fwoc_lbl)
gc()

# ============================================================
#### Accessions ####
# ============================================================

acc <- open_dataset("acc_hive/",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE) |>
  mutate(join_date = make_date(year, month, 1L))

agy_obs   <- acc |> select(agency_code,            join_date) |> distinct() |> collect()
subag_obs <- acc |> select(agency_subelement_code, join_date) |> distinct() |> collect()

agy_fwoc_obs <- agy_obs |>
  add_agency_code_fwoc() |>
  distinct(agency_code_fwoc, join_date)

subag_fwoc_obs <- acc |>
  select(agency_code, agency_subelement_code, join_date) |>
  distinct() |>
  collect() |>
  add_subelement_code_fwoc() |>
  distinct(agency_subelement_code_fwoc, join_date)

agy_lbl        <- label_codes(agy_obs,        agy_map,         "agency_code",                 "join_date", "agency")
subag_lbl      <- label_codes(subag_obs,      subagy_map,      "agency_subelement_code",      "join_date", "agency_subelement")
agy_fwoc_lbl   <- label_codes(agy_fwoc_obs,   agy_fwoc_map,    "agency_code_fwoc",            "join_date", "agency_fwoc",            extra = agy_extra)
subag_fwoc_lbl <- label_codes(subag_fwoc_obs, subagy_fwoc_map, "agency_subelement_code_fwoc", "join_date", "agency_subelement_fwoc", extra = subag_extra)

rm(agy_obs, subag_obs, agy_fwoc_obs, subag_fwoc_obs)

acc <- acc |>
  select(-any_of(lbl_cols)) |>
  left_join(agy_lbl,   by = c("join_date", "agency_code"),            na_matches = "never") |>
  left_join(subag_lbl, by = c("join_date", "agency_subelement_code"), na_matches = "never") |>
  add_agency_code_fwoc() |>
  add_subelement_code_fwoc() |>
  left_join(agy_fwoc_lbl,   by = c("join_date", "agency_code_fwoc"),            na_matches = "never") |>
  left_join(subag_fwoc_lbl, by = c("join_date", "agency_subelement_code_fwoc"), na_matches = "never") |>
  select(-join_date)

rm(agy_lbl, subag_lbl, agy_fwoc_lbl, subag_fwoc_lbl)

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
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE) |>
  mutate(join_date = make_date(year, month, 1L))

agy_obs   <- sep |> select(agency_code,            join_date) |> distinct() |> collect()
subag_obs <- sep |> select(agency_subelement_code, join_date) |> distinct() |> collect()

agy_fwoc_obs <- agy_obs |>
  add_agency_code_fwoc() |>
  distinct(agency_code_fwoc, join_date)

subag_fwoc_obs <- sep |>
  select(agency_code, agency_subelement_code, join_date) |>
  distinct() |>
  collect() |>
  add_subelement_code_fwoc() |>
  distinct(agency_subelement_code_fwoc, join_date)

agy_lbl        <- label_codes(agy_obs,        agy_map,         "agency_code",                 "join_date", "agency")
subag_lbl      <- label_codes(subag_obs,      subagy_map,      "agency_subelement_code",      "join_date", "agency_subelement")
agy_fwoc_lbl   <- label_codes(agy_fwoc_obs,   agy_fwoc_map,    "agency_code_fwoc",            "join_date", "agency_fwoc",            extra = agy_extra)
subag_fwoc_lbl <- label_codes(subag_fwoc_obs, subagy_fwoc_map, "agency_subelement_code_fwoc", "join_date", "agency_subelement_fwoc", extra = subag_extra)

rm(agy_obs, subag_obs, agy_fwoc_obs, subag_fwoc_obs)

sep <- sep |>
  select(-any_of(lbl_cols)) |>
  left_join(agy_lbl,   by = c("join_date", "agency_code"),            na_matches = "never") |>
  left_join(subag_lbl, by = c("join_date", "agency_subelement_code"), na_matches = "never") |>
  add_agency_code_fwoc() |>
  add_subelement_code_fwoc() |>
  left_join(agy_fwoc_lbl,   by = c("join_date", "agency_code_fwoc"),            na_matches = "never") |>
  left_join(subag_fwoc_lbl, by = c("join_date", "agency_subelement_code_fwoc"), na_matches = "never") |>
  select(-join_date)

rm(agy_lbl, subag_lbl, agy_fwoc_lbl, subag_fwoc_lbl)

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

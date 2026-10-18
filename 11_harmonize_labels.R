# Author: Mark Richardson and Claude Code
# Purpose: Apply the standardized OPM code -> label maps to the assembled emp,
#          acc and sep hives. One label source covers both the BZ and FWD eras.
#          Also derives:
#            - nsftp_indicator_fwoc, which fills the OPM nsftp gap 2005-03
#              to 2009-12
#            - education_level_bracket_fwoc
#            - drp_indicator_fwoc (sep only)
#            - occupational_category_code_fwoc and its label (emp only), from the
#              per-(year, month) PATCO crosswalk: a uniform rule-based PATCO
#              classification applied to every period, which also fills the
#              2005-2009 FWD occupational_category gap.
#              See crosswalks/build_patco_crosswalk.R.
#          Runs after 09 (which writes the agency labels this script joins on)
#          and 10.

library(arrow)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
# data.table is attached for label_codes(), which is defined in
# utils/label_functions.R and calls data.table verbs without attaching it.
library(data.table)

source("utils/label_functions.R")

# Arrow silently ignores relationship = "many-to-one" -- it accepts any string
# without checking -- so a duplicated key on the right side of a join inflates
# hive rows with no error, warning or message. label_codes() guards its own
# output; this covers the joins in this script that do not go through it.
stop_if_not_unique <- function(tbl, key, name) {
  if (nrow(tbl) != nrow(distinct(tbl, across(all_of(key))))) {
    stop(name, " is not unique on ", paste(key, collapse = " + "),
         " -- joining it would inflate hive rows.")
  }
  invisible(tbl)
}

emp_meta <- open_dataset("emp_hive/",
                         partitioning  = c("year", "month"),
                         unify_schemas = TRUE)


# ============================================================
#### ed_bracket baseline ####
# ============================================================
# education_level_bracket is OPM-published but only fully populated from 2010+.
# Derive a code -> bracket map from observed data, lock it to a baseline,
# and join it as education_level_bracket_fwoc (which covers the BZ era too).
# On first run the baseline csv is written; on every subsequent run the observed
# mapping is compared to the committed baseline and the build halts on any drift.

bracket_csv <- "metadata/labels/ed_bracket_labels.csv"

obs_bracket <- emp_meta |>
  filter(!is.na(education_level_bracket)) |>
  select(education_level_code, education_level, education_level_bracket) |>
  distinct() |>
  collect() |>
  filter(str_detect(education_level_code, "^[0-9]{2}$")) |> # Keep two digit codes
  arrange(education_level_code)

if (anyDuplicated(obs_bracket$education_level_code) > 0) {
  dupes <- unique(obs_bracket$education_level_code[
    duplicated(obs_bracket$education_level_code)])
  stop("Observed data has multiple (label, bracket) pairs for code(s): ",
       paste(dupes, collapse = ", "))
}

if (!file.exists(bracket_csv)) {
  message("ed_bracket baseline missing -- writing ", bracket_csv)
  write_csv(obs_bracket, bracket_csv, na = "")
} else {
  bracket <- read_csv(bracket_csv, col_types = "ccc", na = "")

  obs_cb <- obs_bracket |> select(education_level_code, education_level_bracket) # Observed
  cb     <- bracket     |> select(education_level_code, education_level_bracket) # Prior bracket

  added   <- anti_join(obs_cb, cb, by = "education_level_code") # observed codes not in baseline
  removed <- anti_join(cb, obs_cb, by = "education_level_code") # baseline codes not observed
  changed <- inner_join(obs_cb, cb, by = "education_level_code",
                        suffix = c("_obs", "_baseline")) |>
             filter(education_level_bracket_obs != education_level_bracket_baseline) # Get codes in both with different brackets

  # Report every category that drifted, not just the first. A bare `if` returns
  # invisible NULL and c() drops NULL, so only the branches that fire contribute
  # a line. Reading them together matters: a code in both `added` and `removed`
  # is usually one recode rather than two independent drifts.
  drift <- c(
    if (nrow(added) > 0)
      paste0("new code(s) in data: ",
             paste(added$education_level_code, collapse = ", "),
             " -- update the baseline deliberately if intentional"),
    if (nrow(removed) > 0)
      paste0("code(s) in the baseline no longer in data: ",
             paste(removed$education_level_code, collapse = ", "),
             " -- investigate before proceeding"),
    if (nrow(changed) > 0)
      paste0("bracket changed for code(s): ",
             paste(changed$education_level_code, collapse = ", "),
             " -- OPM remapped, review before proceeding")
  )

  if (length(drift) > 0) stop("ed_bracket drift --\n  ", paste(drift, collapse = "\n  "))

  message("ed_bracket baseline check passed: ", nrow(bracket), " codes match.")
}

# na = "" so a future "NA"-style code is kept as a string rather than dropped
# to missing.
ed_bracket_map <- read_csv(bracket_csv, col_types = "ccc", na = "") |>
  select(education_level_code,
         education_level_bracket_fwoc = education_level_bracket) |>
  stop_if_not_unique("education_level_code", "ed_bracket_map")

# intersect() with ls() because the drift-check objects exist only on the else
# branch above -- rm() on a missing object warns.
rm(list = intersect(c("obs_bracket", "bracket_csv", "bracket", "obs_cb", "cb",
                      "added", "removed", "changed", "drift"), ls()))
gc()


# ============================================================
#### Label maps and shared derivations ####
# ============================================================

# na = "" on every label read so future "NA"-style codes are preserved as
# strings rather than silently dropped to missing.
lbl_path <- "metadata/labels/"

ed_map      <- read_csv(paste0(lbl_path, "ed_labels.csv"),         col_types = "ccDD",   na = "")
occ_map     <- read_csv(paste0(lbl_path, "occ_labels.csv"),        col_types = "ccDD",   na = "")
occ_cat_map <- read_csv(paste0(lbl_path, "occ_cat_labels.csv"),    col_types = "ccDD",   na = "")
pp_map      <- read_csv(paste0(lbl_path, "pay_plan_labels.csv"),   col_types = "ccDD",   na = "")
sp_map      <- read_csv(paste0(lbl_path, "super_stat_labels.csv"), col_types = "ccDD",   na = "")
ta_map      <- read_csv(paste0(lbl_path, "appt_type_labels.csv"),  col_types = "ccDD",   na = "")
wk_map      <- read_csv(paste0(lbl_path, "work_sched_labels.csv"), col_types = "ccDD",   na = "")
acc_map     <- read_csv(paste0(lbl_path, "acc_labels.csv"),        col_types = "cc",     na = "")
sep_map     <- read_csv(paste0(lbl_path, "sep_labels.csv"),        col_types = "cc",     na = "")
poi_map     <- read_csv(paste0(lbl_path, "poi_labels.csv"),        col_types = "cccccc", na = "")

stop_if_not_unique(acc_map, "accession_category_code",  "acc_map")
stop_if_not_unique(sep_map, "separation_category_code", "sep_map")
# The POI key includes agency_subelement -- the OPM label column, not just the
# code -- which is another reason this script must run after 09.
poi_key <- c("personnel_office_identifier_code", "agency_subelement_code",
             "agency_subelement")
stop_if_not_unique(poi_map, poi_key, "poi_map")

# The seven time-varying code -> label pairs, applied identically to emp, acc and
# sep. Written once here rather than three times per block; the only thing that
# differs between blocks is the date column the window join uses.
lbl_specs <- list(
  education_level_code       = list(map = ed_map,      label = "education_level"),
  occupational_series_code   = list(map = occ_map,     label = "occupational_series"),
  occupational_category_code = list(map = occ_cat_map, label = "occupational_category"),
  pay_plan_code              = list(map = pp_map,      label = "pay_plan"),
  supervisory_status_code    = list(map = sp_map,      label = "supervisory_status"),
  appointment_type_code      = list(map = ta_map,      label = "appointment_type"),
  work_schedule_code         = list(map = wk_map,      label = "work_schedule")
)

# Collect the observed (code, date) pairs from `ds` and resolve each through its
# label map. Output is small -- distinct codes x dates -- so a single lazy pass
# per code is cheap. label_codes() halts if any lookup is not unique on
# (code, date), which is the guard for all seven of these joins.

build_code_labels <- function(ds, date_var) {
  lbls <- lapply(names(lbl_specs), function(code_var) { # lapply() loops over the code column names; the map-label pair is looked up from each
    obs <- ds |>
      select(all_of(c(code_var, date_var))) |>
      distinct() |>
      collect()

    # Label the extracted codes
    label_codes(obs, lbl_specs[[code_var]]$map, code_var, date_var,
                lbl_specs[[code_var]]$label)
  }) # lapply() returns an UNNAMED list of labelled code-date pairs
  stats::setNames(lbls, names(lbl_specs)) # Name each arrow table by its code column, which join_code_labels() uses as the join key
}

# Join the lookups built above. `lbls` is named by code column, so the join key
# is (date, code) in every case. Note: arrow does not allow joins with mixed
# names in the by argument: c("a", "b") is okay,
# c("x_a" = "y_a", "x_b" = "y_b") is okay, but c("a", "x_b" = "y_b")
# errors loudly.
# by= all-or-none is satisfied here.
join_code_labels <- function(data, lbls, date_var) {
  for (code_var in names(lbls)) {
    data <- left_join(data, lbls[[code_var]], by = c(date_var, code_var),
                      na_matches = "never")
  }
  data
}

# Appointment types that count as non-seasonal full-time permanent when paired
# with a full-time work schedule.
nsftp_appt_codes <- c("10", "15", "30", "32", "34", "35", "36", "38", "50", "55", "67")

# OPM's "not specified" sentinels for appointment_type_code (both) and
# work_schedule_code (*). A row carrying one is unknown, not a known
# non-member, and is treated below exactly like a missing code.
not_specified_codes <- c("*", "**")

# The three era-harmonizations that apply to all three hives.
#   age_bracket:           BZ is title case, FWD upper case.
#   length_of_service_ord: BZ spells its missing value UNSP, expanded here.
#                          FWD leaves it NULL (see 08).
#   nsftp_indicator_fwoc:  derived for every period, which fills the gap where
#                          OPM published no nsftp_indicator (2005-03 to 2009-12).
#                          Two conditions must both hold: a full-time schedule
#                          (F) and a permanent appointment type. 1 when both
#                          hold, 0 when either fails (one failing condition is
#                          enough even if the other code is unknown), NULL when
#                          both codes are unknown or one condition holds and the
#                          other code is unknown. Unknown = missing or OPM's
#                          not-specified sentinel. %in% never returns NULL, so
#                          each test is wrapped to yield NULL for an unknown
#                          code, and & then propagates it (TRUE & NULL = NULL,
#                          FALSE & NULL = FALSE). Unwrapped, an unknown
#                          appointment type would score a definite 0, which
#                          OPM's ** sentinel makes common before 1986.
add_harmonized_cols <- function(data) {
  mutate(data,
    age_bracket           = str_to_upper(age_bracket),
    length_of_service_ord = if_else(length_of_service_ord == "UNSP",
                                    "UNSPECIFIED", length_of_service_ord),
    # NA if appointment_type_code is NA or a not-specified sentinel, TRUE if
    # appointment_type_code is a permanent code, FALSE if not a permanent code
    appt_permanent        = if_else(
      is.na(appointment_type_code) |
        appointment_type_code %in% not_specified_codes,
      NA,
      appointment_type_code %in% nsftp_appt_codes
    ),
    # NA if work_schedule_code is NA or a not-specified sentinel, TRUE if F,
    # FALSE if not F
    sched_full_time       = if_else(
      is.na(work_schedule_code) |
        work_schedule_code %in% not_specified_codes,
      NA,
      work_schedule_code == "F"
    ),
    nsftp_indicator_fwoc  = if_else(appt_permanent & sched_full_time, 1, 0),
    nsftp_indicator_fwoc  = cast(nsftp_indicator_fwoc, int8())
  ) |>
    select(-appt_permanent, -sched_full_time)
}

# The columns this script writes. Dropped before the joins in each block so a
# re-run relabels rather than colliding with its own output -- a surviving
# column would come back from the join as .x/.y. Two are block-specific and are
# appended below: accession_category on acc, separation_category on sep.
out_cols <- c(
  "education_level", "occupational_series", "occupational_category",
  "pay_plan", "supervisory_status", "appointment_type", "work_schedule",
  "education_level_bracket_fwoc", "nsftp_indicator_fwoc",
  "occupational_category_code_fwoc", "occupational_category_fwoc",
  "personnel_office_name", "poi_city", "poi_state_country"
)

# PATCO crosswalk -> occupational_category_code_fwoc. Per-(year, month) and
# layered: level 1 keys (year, month, series); level 2 keys
# (year, month, series, pay_plan_code, grade) for the ~140 series whose PATCO is
# not a function of the series alone. Applied uniformly to every period, so it
# also fills the 2005-2009 FWD gap. Irreducibly-ambiguous cells are absent from
# both tables, leaving _fwoc NA. See crosswalks/build_patco_crosswalk.R.
patco_l1 <- read_csv("metadata/crosswalks/occ_series_patco_crosswalk.csv",
                     col_types = cols(.default = "c", year = "i", month = "i"), na = "") |>
  select(year, month, occupational_series_code, occ_cat_fwoc_l1 = occupational_category_code)
patco_l2 <- read_csv("metadata/crosswalks/occ_series_grade_patco_crosswalk.csv",
                     col_types = cols(.default = "c", year = "i", month = "i"), na = "") |>
  select(year, month, occupational_series_code, pay_plan_code, grade,
         occ_cat_fwoc_l2 = occupational_category_code)

stop_if_not_unique(patco_l1, c("year", "month", "occupational_series_code"),
                   "patco_l1")
stop_if_not_unique(patco_l2, c("year", "month", "occupational_series_code",
                               "pay_plan_code", "grade"), "patco_l2")

# The two levels must also be disjoint WITHIN a (year, month). They currently
# are -- zero series appear in both tables in the same month -- and the
# coalesce() below depends on it: if a series were in both, L1 would win and
# L2's grade-specific answer would be silently discarded rather than erroring.
patco_overlap <- inner_join(
  distinct(patco_l1, year, month, occupational_series_code),
  distinct(patco_l2, year, month, occupational_series_code),
  by = c("year", "month", "occupational_series_code")
)
if (nrow(patco_overlap) > 0) {
  stop("PATCO levels overlap on ", nrow(patco_overlap),
       " (year, month, series) cell(s) -- level 1 would silently shadow level 2. First: ",
       paste(patco_overlap$year[1], patco_overlap$month[1],
             patco_overlap$occupational_series_code[1], sep = "-"))
}
rm(patco_overlap)

# Static code -> label for the _fwoc category. occ_cat labels are 1:1 over time
# and the observed-code lookup is blank across the FWD gap, so map from
# occ_cat_map rather than from the data.
occ_cat_fwoc_lbl <- occ_cat_map |>
  distinct(occupational_category_code_fwoc = code, occupational_category_fwoc = label) |>
  stop_if_not_unique("occupational_category_code_fwoc", "occ_cat_fwoc_lbl")


# ============================================================
#### Employment ####
# ============================================================

emp_lbls <- build_code_labels(emp_meta, "date")

rm(emp_meta)
gc()

# Process emp_hive one partition file at a time to bound peak RAM. Each
# completed iteration is idempotent: this script's own output is dropped before
# the joins, so re-running relabels rather than duplicating columns.
parts <- list.files("emp_hive", pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)

for (i in seq_along(parts)) {
  f <- parts[i]
  # year/month live in the partition path, not in the file's rows -- pull them
  # straight from f, as integers, to slice the PATCO crosswalk below.
  yr <- as.integer(str_extract(f, "(?<=/year=)[0-9]+"))
  mo <- as.integer(str_extract(f, "(?<=/month=)[0-9]+"))
  # A failed match yields NA, and filter(year == NA) returns zero rows without
  # erroring -- the PATCO slices would be empty and _fwoc NULL for the whole
  # partition, silently. Halt instead.
  if (is.na(yr) || is.na(mo)) stop("could not parse year/month from partition path: ", f)
  message(sprintf("[%d/%d] %d-%d", i, length(parts), yr, mo))

  l1y <- patco_l1 |> filter(year == yr, month == mo) |> select(-year, -month)
  l2y <- patco_l2 |> filter(year == yr, month == mo) |> select(-year, -month)

  tbl <- read_parquet(f, as_data_frame = FALSE) |>
    select(-any_of(out_cols)) |>
    add_harmonized_cols() |>
    left_join(ed_bracket_map, by = "education_level_code", na_matches = "never") |>
    join_code_labels(emp_lbls, "date") |>
    left_join(l1y, by = "occupational_series_code", na_matches = "never") |>
    left_join(l2y, by = c("occupational_series_code", "pay_plan_code", "grade"),
              na_matches = "never") |>
    mutate(occupational_category_code_fwoc = coalesce(occ_cat_fwoc_l1, occ_cat_fwoc_l2)) |>
    left_join(occ_cat_fwoc_lbl, by = "occupational_category_code_fwoc",
              na_matches = "never") |>
    select(-occ_cat_fwoc_l1, -occ_cat_fwoc_l2)

  # POI is FWD-era only, so the BZ partitions have no such column and the join
  # would error on them. acc and sep need no equivalent guard: they are
  # processed as whole datasets under unify_schemas = TRUE, which gives their BZ
  # rows a null-filled column.
  if ("personnel_office_identifier_code" %in% names(tbl)) {
    tbl <- left_join(tbl, poi_map, by = poi_key, na_matches = "never")
  }

  # Write to a temporary file in the SAME directory and rename over the target,
  # so a process killed mid-write leaves the old partition intact rather than a
  # truncated one with no source to rebuild from. Same directory keeps the
  # rename on one filesystem, where it is atomic; the leading dot keeps a
  # leftover temp file from being read as data by open_dataset(); and the
  # rename is checked because file.rename() returns FALSE rather than erroring.
  tmp <- file.path(dirname(f), paste0(".", basename(f), ".tmp"))

  tbl |>
    compute() |>
    write_parquet(tmp, compression = "zstd", use_dictionary = FALSE)

  # Evaluating file.rename() inside if() executes the rename.
  if (!file.rename(tmp, f)) stop("failed to replace partition file: ", f)
  gc()
}

rm(emp_lbls, patco_l1, patco_l2, occ_cat_fwoc_lbl, parts)
gc()


# ============================================================
#### Accessions ####
# ============================================================
# occupational_category_code_fwoc is emp-only (see
# crosswalks/build_patco_crosswalk.R); the drop below strips any stale _fwoc
# column from acc and sep to keep them clean.

acc <- open_dataset("acc_hive/",
                    partitioning  = c("year", "month"),
                    unify_schemas = TRUE) |>
  mutate(join_date = make_date(year, month, 1L))

acc_lbls <- build_code_labels(acc, "join_date")

acc <- acc |>
  select(-any_of(c(out_cols, "accession_category"))) |>
  add_harmonized_cols() |>
  left_join(ed_bracket_map, by = "education_level_code",  na_matches = "never") |>
  join_code_labels(acc_lbls, "join_date") |>
  left_join(acc_map, by = "accession_category_code",      na_matches = "never") |>
  left_join(poi_map, by = poi_key,                        na_matches = "never") |>
  select(-join_date)

rm(acc_lbls)

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
  mutate(join_date = make_date(year, month, 1L))

sep_lbls <- build_code_labels(sep, "join_date")

sep <- sep |>
  select(-any_of(c(out_cols, "separation_category", "drp_indicator_fwoc"))) |>
  add_harmonized_cols() |>
  # drp_indicator is FWD-era and sparse; the _fwoc version is 0 rather than NULL
  # wherever OPM published nothing, so it can be summed over the whole series.
  mutate(drp_indicator_fwoc = if_else(!is.na(drp_indicator) & drp_indicator == 1L, 1L, 0L),
         drp_indicator_fwoc = cast(drp_indicator_fwoc, int8())) |>
  left_join(ed_bracket_map, by = "education_level_code",  na_matches = "never") |>
  join_code_labels(sep_lbls, "join_date") |>
  left_join(sep_map, by = "separation_category_code",     na_matches = "never") |>
  left_join(poi_map, by = poi_key,                        na_matches = "never") |>
  select(-join_date)

rm(sep_lbls)

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

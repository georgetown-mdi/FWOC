# Author: Mark Richardson and Claude Code
# Purpose: Create derived indicator variables for emp, acc, and sep hives.
#          Variables added: leg_branch, jud_branch, a1_court, exec_branch,
#          eop, core_eop, pres_admin.
#          Runs after 10, and depends on it for more than ordering: it reads
#          agency_code_fwoc, which 10 nulls for the codes absent from the OPM
#          master, to null this script's indicators for those rows.
#          Also reads the hand-curated EOP membership table at
#          metadata/eop/eop_labels_fwoc.csv.

library(arrow)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
# data.table is attached for label_codes(), which is defined in
# utils/label_functions.R and calls data.table verbs without attaching it.
library(data.table)

source("utils/label_functions.R")

# ============================================================
# Constants
# ============================================================

# Legislative branch agency codes.
lb_codes <- c("ZU", "YF", "LP", "ZL", "YG", "ZS", "ZP", "LA", "LD", "LG", "LC", "LB", "ZG")

# The columns this script adds. Listed once and dropped before they are
# recomputed in each block, so a re-run derives rather than colliding.
derived_cols <- c(
  "leg_branch", "jud_branch", "a1_court",
  "exec_branch", "eop", "core_eop", "pres_admin"
)

# ============================================================
# EOP data
# ============================================================

# na = "" so the literal agency code "NA" (NA00, National Aeronautics and Space
# Council) survives as a string. An open-ended membership has an empty
# `through`, which reads as NA and is closed at 2026-12-31 so the date-window
# join has a bound to compare against. NB this fills ANY date that fails to
# parse, so a malformed entry would also be extended (read_csv warns) rather
# than stop the run.
#
# The WH rows overlap on purpose. The table carries the EOP itself (WH) and its
# subelements WH01-WH05, which all truncate to "WH" below, and WH01 is core
# while the others are not, which an agency-level join cannot express. No WH
# record has ever appeared in the data. If one does, label_codes() finds two
# rows for it and stops the run, so the coding gets decided rather than
# guessed. Do not tidy the overlap away.
eop <- read_csv(
  "metadata/eop/eop_labels_fwoc.csv",
  col_types = cols(
    eop_agency_code = "c", eop_agency_name = "c", eop = "i",
    core_eop = "i", from = "D", through = "D"
  ),
  na = ""
) |>
  mutate(through = if_else(is.na(through), ymd("2026-12-31"), through))

# EOP membership is recorded at the subelement level but joined at the agency
# level, so the code is truncated to its first two characters.
eop_codes <- str_sub(eop$eop_agency_code, start = 1, end = 2) |> unique()

# Both maps are label tables in label_codes() shape: code, label, date_from,
# date_to. `label` carries the indicator value, which is cast to int8 after the
# join. eop is membership itself, so its label is a constant 1.
eop_map <- eop |>
  rename(
    date_from = from,
    date_to   = through
  ) |>
  mutate(
    code  = str_sub(eop_agency_code, start = 1, end = 2),
    label = "1"
  ) |>
  select(code, label, date_from, date_to) |>
  mutate(
    date_from = ymd(date_from),
    date_to   = ymd(date_to)
  )

core_eop_map <- eop |>
  rename(
    date_from = from,
    date_to   = through
  ) |>
  mutate(
    code  = str_sub(eop_agency_code, start = 1, end = 2),
    label = as.character(core_eop)
  ) |>
  select(code, label, date_from, date_to) |>
  mutate(
    date_from = ymd(date_from),
    date_to   = ymd(date_to)
  )

rm(eop)

# ============================================================
# Helpers
# ============================================================

# Branch indicators. Takes and returns the data object so it serves an Arrow
# dataset and a collected data frame alike. exec_branch reads the other three,
# so it needs its own mutate() after them.
# DD08 (U.S. Court of Appeals for the Armed Forces) is deliberately BOTH
# a1_court and exec_branch: it is an Article I court placed inside the
# Department of Defense by statute, and the codebook documents it that way.
add_branch_indicators <- function(data) {
  data |>
    mutate(
      leg_branch = cast(agency_code %in% lb_codes, int8()),
      # coalesce() because == and != propagate NA where %in% does not: without
      # it a missing agency or subelement code would yield NA here, and again in
      # exec_branch, instead of 0.
      jud_branch = cast(coalesce(agency_code == "JL" &
        agency_subelement_code != "JL03", FALSE), int8()),
      a1_court   = cast(agency_code %in% c("LT", "JV", "ZD") |
        agency_subelement_code %in% c("DD08", "JL03"), int8())
    ) |>
    mutate(exec_branch = cast((leg_branch == 0 & jud_branch == 0 & a1_court == 0) |
      agency_subelement_code %in% "DD08", int8()))
}

# 10_ nulls agency_code_fwoc for the agency codes it finds absent from the OPM
# master. Those rows have no valid branch or EOP membership, so their indicators
# are nulled here. Without this exec_branch is 1 for them by elimination, since
# it is defined residually: anything not legislative, judicial or Article I is
# treated as executive, which silently absorbs unrecognized codes.
# Keyed on agency_code_fwoc so the decision about which codes are invalid stays
# in 10_ rather than being duplicated here. Note this is NOT the same as an
# agency whose label window does not cover the observation date -- many valid
# historical codes fall outside their windows and must keep their indicators.
null_invalid_indicators <- function(data) {
  mutate(
    data,
    leg_branch  = if_else(is.na(agency_code_fwoc), cast(NA, int8()), leg_branch),
    jud_branch  = if_else(is.na(agency_code_fwoc), cast(NA, int8()), jud_branch),
    a1_court    = if_else(is.na(agency_code_fwoc), cast(NA, int8()), a1_court),
    exec_branch = if_else(is.na(agency_code_fwoc), cast(NA, int8()), exec_branch),
    eop         = if_else(is.na(agency_code_fwoc), cast(NA, int8()), eop),
    core_eop    = if_else(is.na(agency_code_fwoc), cast(NA, int8()), core_eop)
  )
}

# Presidential administration for a vector of dates. No default branch: dates
# before the series begins return NA rather than being attributed to a
# president. Add a line here when the administration changes.
pres_admin_of <- function(d) {
  case_when(
    d >= as.Date("1973-09-30") & d < as.Date("1974-08-09") ~ "Nixon",
    d >= as.Date("1974-08-09") & d < as.Date("1977-01-20") ~ "Ford",
    d >= as.Date("1977-01-20") & d < as.Date("1981-01-20") ~ "Carter",
    d >= as.Date("1981-01-20") & d < as.Date("1989-01-20") ~ "Reagan",
    d >= as.Date("1989-01-20") & d < as.Date("1993-01-20") ~ "H.W. Bush",
    d >= as.Date("1993-01-20") & d < as.Date("2001-01-20") ~ "Clinton",
    d >= as.Date("2001-01-20") & d < as.Date("2009-01-20") ~ "W. Bush",
    d >= as.Date("2009-01-20") & d < as.Date("2017-01-20") ~ "Obama",
    d >= as.Date("2017-01-20") & d < as.Date("2021-01-20") ~ "Trump",
    d >= as.Date("2021-01-20") & d < as.Date("2025-01-20") ~ "Biden",
    d >= as.Date("2025-01-20") ~ "Trump"
  )
}

# Built as a small lookup over the distinct dates and joined, rather than
# evaluated over every row of the hive.
build_pres_admin_lut <- function(ds, date_var) {
  lut <- ds |>
    select(all_of(date_var)) |>
    distinct() |>
    collect()

  lut$pres_admin <- pres_admin_of(lut[[date_var]])

  arrow_table(lut, schema = do.call(
    schema,
    stats::setNames(list(date32(), utf8()), c(date_var, "pres_admin"))
  ))
}

# eop and core_eop share one set of observed code/date pairs, so they are built
# together. label_codes() asserts the lookup is unique on code/date, which is
# what catches an overlapping EOP membership window.
build_eop_lbl <- function(ds, date_var) {
  obs <- ds |>
    filter(agency_code %in% eop_codes) |>
    select(all_of(c("agency_code", date_var))) |>
    distinct() |>
    collect()

  list(
    eop = label_codes(obs, eop_map, "agency_code", date_var, "eop") |>
      mutate(eop = cast(eop, int8())) |>
      compute(),
    core_eop = label_codes(obs, core_eop_map, "agency_code", date_var, "core_eop") |>
      mutate(core_eop = cast(core_eop, int8())) |>
      compute()
  )
}

# ============================================================
#### Employment ####
# ============================================================

# Build lookups once over the full hive, then process partition-by-partition.
emp_meta <- open_dataset("emp_hive/",
                         partitioning  = c("year", "month"),
                         unify_schemas = TRUE)

eop_lbls       <- build_eop_lbl(emp_meta, "date")
pres_admin_lut <- build_pres_admin_lut(emp_meta, "date")

rm(emp_meta)
gc()

# Process emp_hive one partition file at a time. Each completed iteration is
# idempotent: pre-existing derived columns are dropped before being recomputed.
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
    select(-any_of(derived_cols)) |> # Drop this script's own output before recomputing
    add_branch_indicators() |>
    left_join(eop_lbls$eop,      by = c("date", "agency_code"), na_matches = "never") |>
    left_join(eop_lbls$core_eop, by = c("date", "agency_code"), na_matches = "never") |>
    # A code absent from the EOP table is not in the EOP, so its indicator is 0.
    mutate(
      eop      = coalesce(eop,      cast(0L, int8())),
      core_eop = coalesce(core_eop, cast(0L, int8()))
    ) |>
    null_invalid_indicators() |>
    left_join(pres_admin_lut, by = "date", na_matches = "never") |>
    compute() |>
    write_parquet(tmp, compression = "zstd", use_dictionary = FALSE)

  # Evaluating file.rename() inside if() executes the rename.
  if (!file.rename(tmp, f)) stop("failed to replace partition file: ", f)
  gc()
}

rm(eop_lbls, pres_admin_lut)
gc()

# ============================================================
#### Accessions ####
# ============================================================

# acc has no date variable; create join_date from year/month for EOP and pres_admin
acc <- open_dataset("acc_hive/",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE) |>
  mutate(join_date = make_date(year, month, 1L)) |>
  select(-any_of(derived_cols))

eop_lbls       <- build_eop_lbl(acc, "join_date")
pres_admin_lut <- build_pres_admin_lut(acc, "join_date")

acc <- acc |>
  add_branch_indicators() |>
  left_join(eop_lbls$eop,      by = c("join_date", "agency_code"), na_matches = "never") |>
  left_join(eop_lbls$core_eop, by = c("join_date", "agency_code"), na_matches = "never") |>
  mutate(
    eop      = coalesce(eop,      cast(0L, int8())),
    core_eop = coalesce(core_eop, cast(0L, int8()))
  ) |>
  null_invalid_indicators() |>
  left_join(pres_admin_lut, by = "join_date", na_matches = "never") |>
  select(-join_date)

rm(eop_lbls, pres_admin_lut)

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
  mutate(join_date = make_date(year, month, 1L)) |>
  select(-any_of(derived_cols))

eop_lbls       <- build_eop_lbl(sep, "join_date")
pres_admin_lut <- build_pres_admin_lut(sep, "join_date")

sep <- sep |>
  add_branch_indicators() |>
  left_join(eop_lbls$eop,      by = c("join_date", "agency_code"), na_matches = "never") |>
  left_join(eop_lbls$core_eop, by = c("join_date", "agency_code"), na_matches = "never") |>
  mutate(
    eop      = coalesce(eop,      cast(0L, int8())),
    core_eop = coalesce(core_eop, cast(0L, int8()))
  ) |>
  null_invalid_indicators() |>
  left_join(pres_admin_lut, by = "join_date", na_matches = "never") |>
  select(-join_date)

rm(eop_lbls, pres_admin_lut)

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

# Author: Mark Richardson and Claude Code
# Purpose: Per-period headcounts from each hive (emp, acc, sep) at three
#          levels (government, agency, subagency), published to the R2 bucket
#          as CSV under export/aggregates/. Each row carries two counts: every
#          record in the period (n_all) and the non-seasonal full-time
#          permanent subset (n_nsftp).
#
#          These are NOT the dashboard aggregates in aggregate/agg_*.R. Those
#          scope the universe through prep() and shift the emp denominators to
#          the following period so Superset can divide. Here every row is the
#          hive's own period, unfiltered: a user who wants a rate matches the
#          numerator and denominator periods themselves. The published
#          README.md beside the CSVs says so.
#
#          Agencies are the FWOC codes, so the military departments (AR/AF/NV)
#          roll up into DD and the services nest as subelements (DDAR/DDAF/
#          DDNV). The government-wide files are the executive branch. The
#          agency and subagency files keep every branch, with the branch and
#          EOP indicators as columns so a user can subset themselves.

library(arrow)
library(dplyr)
library(readr)

# Written straight into the export tree so 16_push_to_r2.R publishes it as the
# bucket's aggregates/ prefix with no change to 15 or 16. export/ is gitignored.
out_dir <- "export/aggregates"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# The column dictionary for every file in export/aggregates/, published beside
# them. It is versioned here and copied, since export/ is not in git.
invisible(file.copy(
  "aggregate/r2/aggregates_README.md",
  file.path(out_dir, "README.md"),
  overwrite = TRUE
))

emp <- open_dataset("emp_hive",
  partitioning = c("year", "month"),
  unify_schemas = TRUE
)
acc <- open_dataset("acc_hive",
  partitioning = c("year", "month"),
  unify_schemas = TRUE
)
sep <- open_dataset("sep_hive",
  partitioning = c("year", "month"),
  unify_schemas = TRUE
)

# Period keys. emp is a snapshot, so date is the as-of day (BZ Q4 is 12-13).
# acc/sep have mixed day/month grain across the eras, so year/month is their
# only consistent key.
period_emp <- c("date", "year", "month")
period_flow <- c("year", "month")

# Unit keys per level. Government-wide is the empty set: one row per period.
unit_gov <- character(0)
unit_agency <- c("agency_code_fwoc", "agency_fwoc")
unit_subagency <- c(
  unit_agency,
  "agency_subelement_code_fwoc", "agency_subelement_fwoc"
)

# Branch and EOP indicators, carried as descriptor columns on the agency and
# subagency rows. All are functions of the agency code and date
# (10_new_variables.R), so they describe a (period, unit) row rather than
# splitting it, with one exception handled by dd08_in_dd() below.
# count_level() asserts that no indicator splits a unit. The government rows
# carry none: they are the executive branch only, so the indicators would be
# constant.
indicators <- c(
  "exec_branch", "leg_branch", "jud_branch", "a1_court",
  "eop", "core_eop"
)

# dd08_in_dd(): a1_court for the agency level. Step 10 sets the flag for three
# agency codes (LT, JV, ZD) and two subelement codes: DD08, the U.S. Court of
# Appeals for the Armed Forces inside DoD, and JL03, the U.S. Tax Court, which
# OPM moved from its own code LT into the judicial code JL in 2013-09. DD08
# stays inside DD, so DD is coded 0 at the agency level; left alone the flag
# would split DD into two agency rows in every period. JL03 needs no such
# treatment: the other JL subelements last appear before 2013, so JL never
# holds both values in one period. count_level()'s duplicate check would stop
# the build if a rebuild changed that. The subagency level is not passed
# through this, so the DD08 row keeps its 1 there.
dd08_in_dd <- function(ds) {
  ds |>
    mutate(a1_court = if_else(
      agency_code_fwoc == "DD", cast(0L, int8()), a1_court
    ))
}

# count_level(): headcounts per period x unit for one hive.
#   ds      a hive opened with open_dataset()
#   period  the period key columns
#   unit    the unit key columns (empty for government-wide)
#   attrs   descriptor columns grouped alongside unit; must not split a row
# The group_by/summarize runs in Arrow and only the aggregate is collected.
count_level <- function(ds, period, unit, attrs) {
  out <- ds |>
    group_by(!!!syms(c(period, unit, attrs))) |>
    summarize(
      n_all   = n(),
      # nsftp_indicator_fwoc (11_harmonize_labels.R) needs two conditions: a
      # full-time schedule (F) and a permanent appointment type. 1 when both
      # hold, 0 when either fails even if the other code is unknown, NULL when
      # both codes are unknown or one condition holds and the other code is
      # unknown. na.rm so a NULL row counts in n_all but not in n_nsftp, the
      # same as a 0 row.
      n_nsftp = sum(nsftp_indicator_fwoc == 1L, na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect()

  # One row per (period, unit). A duplicate means an attr varies within a
  # unit-period, and the published file would double-count on a naive sum.
  dup <- anyDuplicated(out[c(period, unit)])
  if (dup > 0L) {
    stop(
      "Duplicate (period, unit) row at line ", dup, " -- an attribute ",
      "column splits a unit. Keys: ", paste(c(period, unit), collapse = ", ")
    )
  }

  out |>
    mutate(across(c(n_all, n_nsftp), as.integer)) |>
    arrange(!!!syms(c(unit, period)))
}

# Hives and their period keys, iterated below so each level is written the
# same way for all three.
hives <- list(
  emp = list(ds = emp, period = period_emp),
  acc = list(ds = acc, period = period_flow),
  sep = list(ds = sep, period = period_flow)
)

for (nm in names(hives)) {
  ds <- hives[[nm]]$ds
  period <- hives[[nm]]$period

  # Government-wide is the executive branch. The agency and subagency files
  # keep every branch and carry exec_branch for the user to filter on.
  gov <- count_level(
    ds |> filter(exec_branch == 1L),
    period, unit_gov,
    attrs = character(0)
  )
  agency <- count_level(dd08_in_dd(ds), period, unit_agency, attrs = indicators)
  subagency <- count_level(ds, period, unit_subagency, attrs = indicators)

  # na = "" so a missing label is an empty cell, never the string "NA" (a real
  # agency code).
  write_csv(gov, file.path(out_dir, paste0(nm, "_counts_gov.csv")), na = "")
  write_csv(agency, file.path(out_dir, paste0(nm, "_counts_agency.csv")), na = "")
  write_csv(subagency, file.path(out_dir, paste0(nm, "_counts_subagency.csv")), na = "")

  message(sprintf(
    "%s: %d gov rows, %d agency rows, %d subagency rows",
    nm, nrow(gov), nrow(agency), nrow(subagency)
  ))

  rm(gov, agency, subagency)
  gc()
}

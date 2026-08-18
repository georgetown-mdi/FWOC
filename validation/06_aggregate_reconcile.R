# Author: Mark Richardson and Claude Code
# Purpose: Ex.6 -- aggregate (Superset rate) reconciliation. The dashboard rate
#   tables are built by aggregate/agg_rates.R from the final hives and published
#   as parquet to aggregate_data/parquet/, which aggregate/push_to_gcs.R syncs to
#   GCS for the BigQuery external tables Superset reads. This is a CORRECTNESS
#   check, not a staleness check: it asserts two invariants of the PUBLISHED
#   parquet that re-running the builder cannot establish.
#     (A1) standard codes == FWOC codes on every row -- the join-safety invariant
#          the DoD fold promises and every Superset join depends on. prep()
#          overrides the standard code with the FWOC value only for DoD, on the
#          assumption that every OTHER code difference is DoD-only. A1 tests that
#          assumption, which a re-run would simply reproduce.
#     (A2) the three emp levels roll up exactly (subagency -> agency -> gov).
#          agg_rates.R builds each level with its own independent prep() ->
#          group_by() -> summarise() pass rather than rolling one up from
#          another, so this compares three independent computations -- catching a
#          wrong grouping vector as well as a partial or stale cross-file publish.
#   HARD: these guard numbers that publish to a live dashboard, and the build must
#   not reach the export steps with them broken.
#   Out of scope (V2): freshness vs the current hives, the count/appointee domain
#   guards, and full value-level reconciliation. prep() and the grp* vectors are
#   already shared (utils/agg_rate_functions.R) so a recompute could not diverge,
#   but with agg_rates.R wired into 00_build_fwoc.R a recompute compares the
#   builder against itself -- whether it is worth doing at all is the open
#   question, not how to build it.

library(arrow)
library(dplyr)
source("utils/validation_functions.R")

out_dir <- "validation_output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)  # ensure output dir exists for standalone runs
agg_dir <- "aggregate_data/parquet"

# The 10 agg_rates.R outputs (the publish dir also holds other builders' files
# -- agg_agency / agg_pol_* / agg_state -- which this driver ignores).
rate_files <- c("agg_emp_annual_gov", "agg_emp_annual_agency", "agg_emp_annual_subagency",
                "agg_acc_annual", "agg_sep_annual",
                "agg_emp_monthly_gov", "agg_emp_monthly_agency", "agg_emp_monthly_subagency",
                "agg_acc_monthly", "agg_sep_monthly")
paths   <- setNames(file.path(agg_dir, paste0(rate_files, ".parquet")), rate_files)
present <- file.exists(paths)

# Guard: every rate file must be on disk. Hard, and it gates the eight checks below
# -- absent files mean nothing is verified, which must not read as a pass. 00 runs
# aggregate/agg_rates.R immediately before the suite, so a missing file here means
# the builder exited clean without writing its output.
val_check("agg_present", all(present),
          detail = if (all(present)) "all 10 rate parquet files found"
                   else paste0("missing -- run aggregate/agg_rates.R: ",
                               paste(rate_files[!present], collapse = ",")))

if (all(present)) {
  A <- lapply(paths, read_parquet)

  # ---- A1: standard codes == FWOC codes (the DoD-fold join-safety invariant) ----
  # prep() overrides agency_code/subelement_code with the FWOC value only for DoD
  # (agency_code_fwoc == "DD"); the builder assumes all OTHER code differences are
  # DoD-only, so afterwards standard == FWOC on every row and Superset can join on
  # the standard keys. A non-DoD mismatch here is a real join-safety bug.

  # One entry per logical table: the annual + monthly files backing it, and whether
  # it carries a subelement column. Files live here rather than in a parallel
  # lookup so a table cannot be listed without them -- an unmatched lookup would
  # yield no files, leave the counters at zero, and record a PASS for a table that
  # was never opened.
  key_tables <- list(
    list(name = "emp_agency",    sub = FALSE,
         files = c("agg_emp_annual_agency", "agg_emp_monthly_agency")),
    list(name = "emp_subagency", sub = TRUE,
         files = c("agg_emp_annual_subagency", "agg_emp_monthly_subagency")),
    list(name = "acc",           sub = TRUE,
         files = c("agg_acc_annual", "agg_acc_monthly")),
    list(name = "sep",           sub = TRUE,
         files = c("agg_sep_annual", "agg_sep_monthly"))
  )

  for (kt in key_tables) { # Loop over tables
    n_ag <- 0L
    n_sub <- 0L
    bad_codes <- character(0)
    for (f in kt$files) { # Loop over annual and monthly versions
      df <- A[[f]]
      # Cases where the codes differ. NA on either side makes the comparison NA, which
      # filter() drops -- guard so a missing code counts rather than passing silently.
      m  <- df |> filter(is.na(agency_code) | is.na(agency_code_fwoc) |
                         agency_code != agency_code_fwoc)
      # Cumulative sum of code mismatches
      n_ag <- n_ag + nrow(m)
      # Cumulative set of bad codes
      bad_codes <- union(bad_codes, unique(m$agency_code))
      if (kt$sub) { # Check subagencies if present
        ms <- df |> filter(is.na(agency_subelement_code) | is.na(agency_subelement_code_fwoc) |
                           agency_subelement_code != agency_subelement_code_fwoc)
        n_sub <- n_sub + nrow(ms)
        bad_codes <- union(bad_codes, unique(ms$agency_subelement_code))
      }
    }
    val_check(paste0("agg_keyalign_", kt$name), n_ag == 0L && n_sub == 0L,
              detail = if (n_ag == 0L && n_sub == 0L) "standard codes == FWOC codes on every row"
                       else sprintf("%d agency + %d subelement mismatches; codes: %s",
                                    n_ag, n_sub, paste(head(bad_codes, 20), collapse = ",")))
  }

  # ---- A2: three-level rollup consistency (subagency -> agency -> gov) ----
  # The agency/gov files are built by their own prep() -> group_by() -> summarise()
  # passes, not rolled up from the subagency file, so this compares three
  # independent computations. It also catches a partial or stale cross-file publish.
  rollup_check <- function(fine, coarse, by, label) {
    # fine is the finer-grained table, coarse the one it should roll up to. Called
    # twice per grain, stepping down the chain: subagency -> agency, then agency ->
    # gov, so the agency table is coarse in the first call and fine in the second.
    # by is the key both share; whatever fine carries beyond it is the dimension
    # summing collapses.

    # Collapse that dimension away, summing emp_n into a distinctly named column so
    # it does not collide with coarse's own emp_n in the join below.
    roll <- fine |> group_by(across(all_of(by))) |> summarize(emp_n_roll = sum(emp_n), .groups = "drop")

    # Join the rolled-up totals to the coarse table from the build.
    cmp  <- full_join(roll, coarse, by = by)

    # Identify cases with a missing count or unequal counts.
    mism <- cmp |> filter(is.na(emp_n) | is.na(emp_n_roll) | emp_n != emp_n_roll)

    # Report results.
    val_check(paste0("agg_rollup_", label), nrow(mism) == 0,
              detail = if (nrow(mism) == 0) "rolls up exactly"
                       else sprintf("%d period/subgroup cells disagree", nrow(mism)))
  }

  # Annual and monthly differ in exactly two ways -- the filename suffix and the period
  # columns that key the join -- so one pass per grain covers both.
  for (grain in list(list(suffix = "annual",  keys = "year"),
                     list(suffix = "monthly", keys = c("year", "month")))) {

    sub_keys_ag <- c(grain$keys, "agency_code", "agency", "agency_code_fwoc", "agency_fwoc",
                     "nsftp_indicator_fwoc", "appointee")

    gov_keys    <- c(grain$keys, "nsftp_indicator_fwoc", "appointee")

    # subagency -> agency
    rollup_check(A[[paste0("agg_emp_", grain$suffix, "_subagency")]],
                 A[[paste0("agg_emp_", grain$suffix, "_agency")]],
                 sub_keys_ag, paste0("sub_agency_", grain$suffix))

    # agency -> gov
    rollup_check(A[[paste0("agg_emp_", grain$suffix, "_agency")]],
                 A[[paste0("agg_emp_", grain$suffix, "_gov")]],
                 gov_keys, paste0("agency_gov_", grain$suffix))
  }
}

val_finish(file.path(out_dir, "ex6_checks.csv"))

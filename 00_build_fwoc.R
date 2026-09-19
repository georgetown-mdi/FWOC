# Author: Mark Richardson and Claude Code
# Purpose: Build the Federal Workforce OpenCommons (FWOC) hives end to end, then
#          validate, document and publish them. Each step runs in a fresh Rscript
#          subprocess so memory is reclaimed between steps and no package state
#          leaks; the build halts on the first non-zero exit code.
#
#          The chain has four phases:
#            assemble + enrich   01 -> 15, ending in the final emp/acc/sep hives
#            aggregate           the Superset dashboard parquet
#            VALIDATE            the gate -- nothing past this point runs if a
#                                hard check fails
#            document + publish  the codebook, then the public export and R2 push
#
#          The gate sits BEFORE the codebook because codebook/01 is by far the
#          most expensive post-15 step (~177 full hive scans) and the likeliest
#          routine failure is Ex.5's history check. Failing first means not
#          paying for the codebook on a bad build. The trade: on a failed run the
#          committed codebook stays one cycle behind the hives.
#
# Prerequisites (see the README, "Acquiring the raw inputs"):
#   - Run setup/00_run_setup.R FIRST. It rebuilds the label tables and runs the
#     OPM-metadata gate (setup/check_opm_metadata.R), which halts when the OPM
#     download changed shape. That gate is deliberately outside this script.
#   - BuzzFeed FOIA data under buzz_feed/data/ (01/02 read it -- see below).
#   - 03 downloads the data.opm.gov files automatically.
#
# Run from the repo root:
#   source("00_build_fwoc.R")   or   Rscript 00_build_fwoc.R
#
# Progress + each step's stdout/stderr stream to ./fwoc_build_log_<timestamp>.txt

start_time <- Sys.time()
log_file   <- paste0("fwoc_build_log_",
                     format(start_time, "%Y%m%d_%H%M%S"), ".txt")

log_line <- function(msg) {
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
}

log_line(paste0("FWOC build started at ", format(start_time), "\n"))
log_line(paste0("Repo root: ", normalizePath("."), "\n"))
log_line(paste0("Log file:  ", normalizePath(log_file), "\n"))

# Refuse to build against label CSVs older than the OPM code tables they derive
# from: setup/build_labels.R stamps metadata/labels/labels_build_stamp.csv with each
# master's download date, and this compares that stamp against the dated files in
# metadata/code_tables/. Checked here, before step 01, because every labelling step
# downstream would otherwise silently use stale labels.
#
# code_table_functions.R is definitions-only and assert_labels_current() is base-R
# only, so sourcing it here costs nothing and needs no packages attached -- see that
# file's header. Keep it that way: top-level code there would run in this preamble.
source("utils/code_table_functions.R")
assert_labels_current()
log_line("Label freshness: metadata/labels/ is current with metadata/code_tables/\n")

run_step <- function(script, label) {
  # Write a time-stamped banner
  step_start <- Sys.time()
  log_line(paste0(
    "\n==== ", format(step_start), "  ", label,
    " (", script, ") ====\n"))

  # Shell redirection appends each subprocess's stdout/stderr to the log.
  cmd <- paste0("Rscript ", shQuote(script),
                " >> ", shQuote(log_file), " 2>&1")
  status <- system(cmd)

  elapsed <- difftime(Sys.time(), step_start, units = "mins")

  if (status == 0) {
    log_line(paste0("[OK] ", label, " finished in ",
                    round(as.numeric(elapsed), 1), " min\n"))
  } else {
    log_line(paste0("[ERROR] ", label,
                    " exited with status ", status, "\n",
                    "Halting at ", format(Sys.time()), ".\n",
                    "Fix the error and re-source from this step ",
                    "(comment out completed steps).\n"))
    stop(label, " failed (Rscript exit ", status, "); see ", log_file)
  }
}

# ====================================================================
# Assemble the raw sources into staging hives
# ====================================================================
# 01/02 parse the BuzzFeed FOIA releases into the BZ staging hives. Pre-2005 data
# does not change month to month, so they are a one-time historical build -- run
# them once on a fresh clone, then leave them commented for the monthly cycle.
#run_step("01_bz_hive_assembly_status.R",             "01 BuzzFeed status assembly")
#run_step("02_bz_hive_assembly_dynamic.R",            "02 BuzzFeed accessions/separations assembly")
run_step("03_fwd_download_opm_data_gov.R",            "03 Download data.opm.gov (FWD)")
run_step("04_fwd_hive_assembly_opm_data_gov.R",       "04 FWD assembly")

# ====================================================================
# Harmonize, label, derive, finalize
# ====================================================================
# There is no 06 here: the source repo's 06_label_codes.R became
# setup/build_labels.R, which runs in setup/00_run_setup.R before this script.
run_step("05_bz_compatibility.R",                     "05 BZ compatibility / staging")
run_step("07_bz_duty_station_labels.R",               "07 Duty station labeling")
run_step("08_bz_duty_station_state.R",                "08 BZ -> final amalgamation")
run_step("09_fwd_compatibility.R",                    "09 FWD -> final amalgamation")
run_step("10_agency_names.R",                         "10 Agency names")
run_step("11_new_variables.R",                        "11 New variables")
# Builds the PATCO crosswalk that 12 consumes (occupational_category_code_fwoc).
# Must run after 09 (final hives populated) and before 12; sources emp/acc/sep + raw BZ.
run_step("crosswalks/build_patco_crosswalk.R",        "11b PATCO crosswalk build")
run_step("12_harmonize_labels.R",                     "12 Harmonize labels")
run_step("13_duty_station_backfill.R",                "13 Duty station backfill")
run_step("14_bls_macro.R",                            "14 BLS macro")
run_step("15_finalize_hive.R",                        "15 Finalize hive")

# ====================================================================
# Aggregate: the Superset dashboard parquet
# ====================================================================
# Before the gate because validation/06 reconciles agg_rates.R's output, and all
# three are cheap relative to the hives. aggregate/push_to_gcs.R is deliberately
# NOT here -- pushing to the mdi-federal bucket stays a manual act.
run_step("aggregate/agg_rates.R",                     "Agg rates (acc/sep/emp, 3 levels)")
run_step("aggregate/agg_agency_subagency.R",          "Agg agency/subagency")
run_step("aggregate/agg_politicization.R",            "Agg politicization")

# ====================================================================
# VALIDATE -- the gate
# ====================================================================
# Exits non-zero if any hard check fails, so run_step() halts here and nothing
# below runs. Ex.5's history checks are hard: an unexplained row-count change in
# a frozen period stops the build. The failure detail prints each change as an
# allowlist row -- review them, add the ones you accept (with a reason) to
# metadata/validation/baselines/ex5_history_allowlist.csv, and re-run.
run_step("validation/00_run_validation.R",            "QA Validation suite (Ex.1-6)")

# ====================================================================
# Document: rebuild the codebook from the validated hives
# ====================================================================
# 01 is the heavy hive-reading step (~177 scans) and writes the inspection CSVs;
# 02 reads them, derives the coverage columns, and rewrites
# metadata/codebook/fwoc_codebook_table.csv plus gap_detail.csv.
run_step("codebook/01_codebook_inspection.R",         "Codebook 01 Inspection CSVs")
run_step("codebook/02_codebook_build.R",              "Codebook 02 Codebook build")

# ====================================================================
# Publish
# ====================================================================
# Both run only if every hard check above passed.
run_step("16_public_data_export.R",                   "16 Public data export")

# 17 carries its own gate: it stop()s unless FWOC_PUBLISH=1, so an unattended build
# cannot publish. Honour that gate here rather than calling it and failing -- a
# refusal is the normal outcome, and logging it as [ERROR] every month would train
# the operator to ignore real errors.
if (identical(Sys.getenv("FWOC_PUBLISH"), "1")) {
  run_step("17_push_to_r2.R",                         "17 Push public data to R2")
} else {
  log_line("\n[SKIP] 17 Push public data to R2 -- FWOC_PUBLISH is not set to 1\n")
}

total <- difftime(Sys.time(), start_time, units = "hours")
log_line(paste0("\nFWOC build complete at ", format(Sys.time()),
                " (", round(as.numeric(total), 2), " hours total)\n"))

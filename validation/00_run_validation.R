# Author: Mark Richardson and Claude Code
# Purpose: POST-build validation orchestrator. Runs the six exercise drivers,
#   each in a fresh Rscript subprocess (memory hygiene over the large hives),
#   continues past a failing driver so it reports ALL failures, aggregates every
#   driver's check rows into a run manifest, and exits non-zero if any driver
#   hard-failed (so a run_step() in 00_build_fwoc.R halts the build). Cheap,
#   deterministic checks first; the heavy overlap scan (Ex.1) last.
#
#   Ex.6 reads aggregate_data/parquet/, so aggregate/agg_rates.R must have run
#   first. 00_build_fwoc.R sequences it that way; running this suite standalone
#   on a fresh clone will fail Ex.6 until the aggregates are built.
#
#   Run from the repo root:
#     source("validation/00_run_validation.R")   or   Rscript validation/00_run_validation.R

start    <- Sys.time()
out_dir  <- "validation_output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(out_dir, paste0("validation_log_", format(start, "%Y%m%d_%H%M%S"), ".txt"))

log_line <- function(msg) {
  cat(msg)
  cat(msg, file = log_file, append = TRUE)
}

run_step <- function(script, label) {
  log_line(paste0("\n==== ", format(Sys.time()), "  ", label, " (", script, ") ====\n"))
  status <- system(paste0("Rscript ", shQuote(script), " >> ", shQuote(log_file), " 2>&1"))
  log_line(if (status == 0) paste0("[OK] ", label, "\n")
           else paste0("[FAIL] ", label, " exited ", status, "\n"))
  status == 0
}

steps <- list(
  c("validation/02_labels.R",               "Ex.2 Missing-label validation"),
  c("validation/03_reconcile.R",            "Ex.3 Row reconciliation"),
  c("validation/04_conformance.R",          "Ex.4 Codebook/schema conformance"),
  c("validation/05_sanity.R",               "Ex.5 Indicator/domain sanity + history"),
  c("validation/06_aggregate_reconcile.R",  "Ex.6 Aggregate (Superset rate) reconciliation"),
  c("validation/01_overlap.R",              "Ex.1 Overlap / FWD-revision")
)

log_line(paste0("FWOC validation started ", format(start), "\n"))
ok <- vapply(steps, function(s) run_step(s[1], s[2]), logical(1))

# Aggregate each driver's ex<N>_checks.csv into a single run manifest.
checks <- list.files(out_dir, pattern = "^ex._checks\\.csv$", full.names = TRUE)
if (length(checks)) {
  manifest <- dplyr::bind_rows(lapply(checks, function(f)
    dplyr::mutate(readr::read_csv(f, show_col_types = FALSE), file = basename(f))))
  readr::write_csv(manifest, file.path(out_dir, paste0("run_manifest_", format(start, "%Y%m%d_%H%M%S"), ".csv")), na = "")
}

log_line(paste0("\nValidation ", if (all(ok)) "PASSED" else "FAILED",
                " (", sum(ok), "/", length(ok), " drivers OK) -- log ", log_file, "\n"))
if (!all(ok)) quit(save = "no", status = 1)

# Author: Mark Richardson and Claude Code
# Purpose: Run the monthly setup/refresh chain in order, each step in a fresh
#          Rscript subprocess, halting on the first non-zero exit. Output streams
#          to the console -- setup is a sub-minute, human-reviewed step, so you
#          watch the metadata diff and label output live.
#
#          Chain: metadata gate -> build_labels -> get_bls_data. The gate
#          (check_opm_metadata.R) halts the run when the OPM download changed;
#          review the report it writes, then re-run with FWOC_METADATA_ACK=1.
#
# One-time (new repo, NOT monthly): setup/build_duty_station_labels.R parses the
#   frozen SCTFILE.TXT into ds_labels.RData -- run it once when standing up a
#   repo; it is deliberately not in the chain below.
#
# Prerequisites:
#   - metadata/code_tables/prior_downloads/ (used to diff the new OPM download when
#     present; absent on a first run, the gate no-ops and the chain proceeds)
#   - the current OPM code_tables masters + overlap_resolutions.csv (build_labels)
#   - BLS_USER_AGENT set in the environment (get_bls_data); see README
#
# Run from the project root (fwoc/). The gate halts the first run if OPM changed;
# review its report, then re-run with FWOC_METADATA_ACK set to acknowledge and
# proceed. Use whichever you work in:
#
#   Terminal:
#     Rscript setup/00_run_setup.R
#     FWOC_METADATA_ACK=1 Rscript setup/00_run_setup.R          # after reviewing the report
#
#   RStudio / R console:
#     source("setup/00_run_setup.R")
#     Sys.setenv(FWOC_METADATA_ACK = "1"); source("setup/00_run_setup.R")   # after review
#     Sys.unsetenv("FWOC_METADATA_ACK")                         # clear it so the gate re-arms
#
# Then review the metadata/ changes (git diff, or the RStudio Git pane), commit them,
# and build the hives.


start_time <- Sys.time()

# Run one setup script in a fresh Rscript; its output streams to the console so the
# monthly refresh can be reviewed live. Halt the chain on the first non-zero exit.
run_step <- function(script, label) {
  cat("\n==== ", label, " (", script, ") ====\n", sep = "")
  status <- system(paste("Rscript", shQuote(script)))
  if (status != 0L) {
    stop(label, " failed (Rscript exit ", status, "). Fix and re-run setup.",
      call. = FALSE
    )
  }
  cat("[OK] ", label, "\n", sep = "")
}

run_step("setup/check_opm_metadata.R", "Metadata gate (diff vs prior OPM download)")
run_step("setup/build_labels.R", "Build label CSVs")
run_step("setup/get_bls_data.R", "Download BLS macro series")

cat("\nSetup finished in ",
  round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1),
  "s\n",
  sep = ""
)

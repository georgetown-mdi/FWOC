# Author: Mark Richardson and Claude Code
# Purpose: Push the aggregate dashboard parquet to the GCS bucket that backs the
#          Superset / BigQuery external tables (dataset `mdi_federal`). Internal
#          ops step, run by hand after the agg_*.R builders refresh the local
#          parquet.
#
#   Refresh flow:
#     agg_rates.R (+ agg_agency_subagency.R, agg_politicization.R)
#       -> this script  -> BigQuery external tables pick up the new files.
#
#   To execute this file, run one of the following commands in a terminal
#   from the repo root:
#     Rscript aggregate/push_to_gcs.R                     # upload new/changed
#     Rscript aggregate/push_to_gcs.R --dry-run           # preview, no changes
#     Rscript aggregate/push_to_gcs.R --mirror            # also DELETES remote
#     Rscript aggregate/push_to_gcs.R --dry-run --mirror  # preview a mirror
#
#   --mirror deletes remote files that are absent locally. Always preview it
#   with --dry-run --mirror first.
#
#   Requires gcloud authenticated for the project behind FWOC_GCS_DEST
#   (`gcloud auth login`, or an active service account).

# --- config ---------------------------------------------------------------
# Written by the agg_*.R builders (gitignored).
local_dir <- "aggregate_data/parquet/"

# The destination bucket is org infrastructure, not public information, so it is
# read from the environment rather than committed. Set it in .Renviron (also
# gitignored) as a gs:// URI ending in a slash, e.g.
#   FWOC_GCS_DEST="gs://<bucket>/<path>/parquet/"
gcs_dest <- Sys.getenv("FWOC_GCS_DEST")

# Flags come from the command line; both default to FALSE when absent. To
# change the default, edit the assignment below (e.g. dry_run <- TRUE).
args <- commandArgs(trailingOnly = TRUE)
known_flags <- c("--dry-run", "--mirror")
dry_run <- "--dry-run" %in% args
mirror <- "--mirror" %in% args   # delete remote objects not present locally

# --- preflight ------------------------------------------------------------
# Flags are matched exactly, so a typo would otherwise be ignored silently --
# and a mistyped --dry-run means a real push when a preview was intended.
unknown_flags <- setdiff(args, known_flags)
if (length(unknown_flags)) {
  stop("Unrecognized flag(s): ", paste(unknown_flags, collapse = " "),
       "\n  Known flags: ", paste(known_flags, collapse = " "))
}

# Checked before anything else: an unset destination is a setup error, and
# without this the rsync would fail deep inside gcloud with a vaguer message.
if (!nzchar(gcs_dest)) {
  stop("FWOC_GCS_DEST is not set -- nowhere to push to.\n",
       "  Add it to .Renviron:  ",
       "FWOC_GCS_DEST=\"gs://<bucket>/<path>/parquet/\"")
}
if (!grepl("^gs://.*/$", gcs_dest)) {
  stop("FWOC_GCS_DEST must be a gs:// URI ending in a slash. Got: ", gcs_dest)
}

if (!nzchar(Sys.which("gcloud"))) {
  stop("gcloud not found on PATH. Install the Google Cloud CLI and run ",
       "`gcloud auth login`.")
}

if (!dir.exists(local_dir)) {
  stop("Local source not found: ", local_dir,
       "\nRun the agg_*.R builders first.")
}

# Refuse to sync an empty directory: with --mirror it would wipe the bucket.
# recursive = TRUE to match the -r on the rsync below, so the guard counts
# exactly the files the sync would send.
n_parquet <- length(list.files(local_dir, pattern = "\\.parquet$",
                               recursive = TRUE))
if (n_parquet == 0) {
  stop("No .parquet files in ", local_dir,
       " -- refusing to sync an empty directory.")
}

# --- build + run the rsync ------------------------------------------------
rsync_args <- c("storage", "rsync", "-r", local_dir, gcs_dest)
if (mirror) rsync_args <- c(rsync_args,
                            "--delete-unmatched-destination-objects")
if (dry_run) rsync_args <- c(rsync_args, "--dry-run")

message(sprintf("Syncing %d parquet file(s)", n_parquet))
message("  from: ", local_dir)
message("  to:   ", gcs_dest)
if (mirror) message("  mode: MIRROR (deletes remote files absent locally)")
if (dry_run) message("  mode: DRY-RUN (no changes)")
message("  cmd:  gcloud ", paste(rsync_args, collapse = " "), "\n")

status <- system2("gcloud", rsync_args)

if (status != 0) {
  stop("gcloud storage rsync failed (exit ", status, "). ",
       "If this is an auth error, run `gcloud auth login` ",
       "(or check the active service account).")
}

message(if (dry_run) "\nDry-run complete -- no changes made."
        else "\nPush complete.")

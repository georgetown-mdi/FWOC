# Author: Mark Richardson and Claude Code
# Purpose: Push the public data export (the format-based tree written by
#          15_public_data_export.R) to the Cloudflare R2 bucket that serves the
#          published FWOC data. Internal ops step, run by hand after 15 -- the
#          R2 counterpart of aggregate/push_to_gcs.R.
#
#   Publish flow:
#     15_public_data_export.R  (writes the tree under <local_root>)
#       -> this script  -> R2 bucket `fwoc`  (public downloads + S3 streaming).
#
#   To execute this file, run one of the following commands in a terminal
#   from the repo root:
#     FWOC_PUBLISH=1 Rscript 16_push_to_r2.R           # upload new/changed
#     Rscript 16_push_to_r2.R --dry-run                # preview, no changes
#     FWOC_PUBLISH=1 Rscript 16_push_to_r2.R --mirror  # also DELETES remote
#     Rscript 16_push_to_r2.R --dry-run --mirror       # preview a mirror
#
#   FWOC_PUBLISH=1 is required for any real upload -- it keeps an unattended
#   build from publishing as a side effect. --dry-run does not need it.
#
#   --mirror deletes remote files that are absent locally (sub-trees only, not
#   root-level files). Always preview it with --dry-run --mirror first.
#
#   This script publishes to the FWOC project's R2 bucket -- you will not have
#   write access to it. If you are reusing this script for your own data, set
#   up your own cloud storage and change `remote` below.

# --- config ---------------------------------------------------------------
# Local source = the export tree from 15_public_data_export.R -- must match its
# export_root, or this silently publishes a stale tree.
local_root <- "export"
remote <- "fwoc:fwoc"   # rclone remote `fwoc` -> R2 bucket `fwoc`

# The export tree maps 1:1 onto the bucket, but we sync each top-level entry on
# its own rather than syncing the root. A root sync would treat every key not in
# export/ as an extra and delete it under --mirror; per-prefix, anything the
# bucket holds that we do not publish is simply never touched.
#
# Derived from the tree, never hard-coded: a new top-level product publishes
# with no edit here. A written list fails silently in the one direction that
# matters -- it cannot tell you about something it was never told about.
entries    <- list.files(local_root, recursive = FALSE)
is_dir     <- dir.exists(file.path(local_root, entries))
subtrees   <- entries[is_dir]    # synced into fwoc:fwoc/<name>/
root_files <- entries[!is_dir]   # copied to  fwoc:fwoc/<name>

# Flags come from the command line; both default to FALSE when absent. To
# change the default, edit the assignment below (e.g. dry_run <- TRUE).
args <- commandArgs(trailingOnly = TRUE)
known_flags <- c("--dry-run", "--mirror")
dry_run <- "--dry-run" %in% args
mirror <- "--mirror" %in% args   # delete remote objects not present locally

# --- preflight ------------------------------------------------------------
# Flags are matched exactly, so a typo would otherwise be ignored silently.
# Checked ahead of the publish gate below: FWOC_PUBLISH=1 with a mistyped
# --dry-run would otherwise clear the gate and publish for real.
unknown_flags <- setdiff(args, known_flags)
if (length(unknown_flags)) {
  stop("Unrecognized flag(s): ", paste(unknown_flags, collapse = " "),
       "\n  Known flags: ", paste(known_flags, collapse = " "))
}

# Gated so an unattended build never publishes as a side effect. --dry-run is
# exempt: it changes nothing.
if (!dry_run && !identical(Sys.getenv("FWOC_PUBLISH"), "1")) {
  stop("Refusing to publish: FWOC_PUBLISH is not set to 1.\n",
       "  Re-run as:  FWOC_PUBLISH=1 Rscript 16_push_to_r2.R",
       if (mirror) " --mirror" else "",
       "\n  Or preview with:  Rscript 16_push_to_r2.R --dry-run")
}

if (!nzchar(Sys.which("rclone"))) {
  stop("rclone not found on PATH. Install rclone and configure the `fwoc` ",
       "remote (`rclone config`).")
}

if (!dir.exists(local_root)) {
  stop("Local source not found: ", local_root,
       "\nRun 15_public_data_export.R first.")
}

# Refuse to sync an empty tree: with --mirror it would wipe the bucket.
hive_dir <- file.path(local_root, "parquet", "hive")
n_parquet <- length(list.files(hive_dir, pattern = "\\.parquet$",
                               recursive = TRUE))
if (n_parquet == 0) {
  stop("No hive .parquet files under ", hive_dir,
       " -- refusing to sync an empty tree.")
}

# Deriving the list means the tree decides what publishes, so say out loud what
# must be in it. A MISSING sub-tree is fatal: 16 did not finish, and publishing
# anyway would leave the bucket holding this month's parquet beside last
# month's csv -- a self-inconsistent release, which is worse than a late one.
# An UNEXPECTED sub-tree is announced rather than blocked; that is the
# new-product case this derivation exists to serve, and only the operator can
# tell it from a stray directory.
expected <- c("parquet", "csv", "stata")
if (length(setdiff(expected, subtrees)) > 0) {
  stop("Export tree is missing: ",
       paste(setdiff(expected, subtrees), collapse = ", "),
       "\n  Re-run 15_public_data_export.R.")
}
for (extra in setdiff(subtrees, expected)) {
  message("  note: publishing unlisted sub-tree ", extra, "/")
}

# Presence is not enough. A sub-tree that exists but is empty passes the check
# above, and `sync` from an empty source DELETES that entire prefix from the
# bucket under --mirror. Checked per sub-tree so a half-written export cannot
# take out a product that published fine last run.
sub_n <- vapply(subtrees,
                function(s) length(list.files(file.path(local_root, s),
                                              recursive = TRUE)),
                integer(1))
if (any(sub_n == 0)) {
  stop("Empty sub-tree(s): ",
       paste(names(sub_n)[sub_n == 0], collapse = ", "),
       "\n  Re-run 15_public_data_export.R.")
}

# --- shared rclone flags --------------------------------------------------
# --checksum: compare by size + MD5 rather than size + mtime, so a rewritten
#   file with an unchanged size is still detected. The export archives are not
#   byte-reproducible -- they differ on every run of 15 -- so they re-upload
#   whether or not their contents changed.
# --fast-list: fewer S3 LIST calls (R2 bills per class-A/B operation).
# --transfers 8: how many files upload concurrently (rclone's default is 4).
#   This is network concurrency, not CPU cores -- each transfer spends most of
#   its time waiting on the network, so the useful ceiling comes from upstream
#   bandwidth, not from the machine's core count.
# --checkers: left at rclone's default. Checkers are the workers that compare
#   local against remote to decide what needs sending; that is metadata work
#   and rarely the bottleneck, so only the transfer count is raised here.
# --retries 5: re-attempt any file that fails (e.g. a transient R2 5xx) instead
#   of leaving a gap; the whole op is idempotent under --checksum.
# --stats-one-line: compact progress that stays readable in a captured log.
common <- c("--checksum", "--fast-list",
            "--transfers", "8",
            "--retries", "5",
            "--stats-one-line", "--stats", "15s")
if (dry_run) common <- c(common, "--dry-run")

# `sync` makes the remote match local (deletes extras); `copy` only
# adds/updates.
op <- if (mirror) "sync" else "copy"

run_rclone <- function(rclone_args) {
  message("  cmd:  rclone ", paste(rclone_args, collapse = " "))
  status <- system2("rclone", rclone_args)
  if (status != 0) {
    stop("rclone failed (exit ", status, "). ",
         "If this is an auth error, check the `fwoc` remote in ",
         "`rclone config`.")
  }
}

# --- run ------------------------------------------------------------------
message(sprintf("Publishing export tree to R2 (%s hive parquet file(s))",
                format(n_parquet, big.mark = ",")))
message("  from: ", local_root)
message("  to:   ", remote)
message("  op:   ", op,
        if (mirror) " (MIRROR -- deletes remote files absent locally)"
        else " (additive)")
if (dry_run) message("  mode: DRY-RUN (no changes)")
message("")

# Sub-trees: copy/sync each into its matching remote prefix. Every name came
# from list.files() above, so no existence check is needed here -- an entry
# that is not on disk cannot be in the list.
#
# With the current tree, the loop expands to one call per sub-tree:
#   rclone <op> export/csv      fwoc:fwoc/csv
#   rclone <op> export/parquet  fwoc:fwoc/parquet
#   rclone <op> export/stata    fwoc:fwoc/stata
# where <op> is copy, or sync under --mirror, plus the shared flags above.
for (sub in subtrees) {
  run_rclone(c(op, file.path(local_root, sub), file.path(remote, sub), common))
}

# Root-level files: always `copyto` (never sync -- syncing one file into the
# bucket root would delete every sibling key). --mirror does not apply here.
# Currently just agency_code_lookup.csv
for (f in root_files) {
  run_rclone(c("copyto", file.path(local_root, f),
               file.path(remote, f), common))
}

message(if (dry_run) "\nDry-run complete -- no changes made."
        else "\nPush complete.")

# Author: Mark Richardson and Claude Code
# Purpose: Export public data to a format-based tree that mirrors the R2 bucket:
#          parquet/hive/  live partitioned hives (API download)
#          parquet/zip/   zipped hives (download -> unzip -> open_dataset)
#          csv/           full acc/sep + per-agency employment, as zipped CSV
#          stata/         same, as zipped .dta
# Data coverage: 1973-present; emp_hive, acc_hive, and sep_hive partitioned
# by year/month
#
# Shape of the job: R writes plain .csv and .dta into a staging directory, and
# a separate step zips them in parallel and deletes the originals.
# Splitting write from zip is what makes the parallelism free -- Info-ZIP is
# single-threaded, but the ~1,000 items are independent, so `xargs -P` gets the
# whole machine without any R-level workers. Measured 12.3x on this box.
#
# Staging is /dev/shm, a RAM-backed filesystem. The intermediates are transient
# (written, zipped, deleted) and total ~2 TB across a run; sending them to a
# disk means ~4 TB of round-trip I/O for ~105 GB of output. Nothing survives
# staging, so nothing needs to be on a device.
#
# Staging is flushed whenever it exceeds a threshold, because ~2 TB does not
# fit in 124 GB of RAM. That is the only reason the flush is inside the loop
# rather than one sweep at the end.

# Load packages
library(arrow)
library(dplyr)
library(readr)
library(haven)


# ============================================================
#### Paths ####
# ============================================================

# The export tree lives at the repo root beside the hives it publishes. It is
# ~216 GB, so `export/` MUST be in .gitignore otherwise git would carry the
# whole tree.
export_root   <- "export"

hive_live_dir <- file.path(export_root, "parquet", "hive")  # unzipped (S3)
hive_zip_dir  <- file.path(export_root, "parquet", "zip")   # zipped (download)
csv_dir       <- file.path(export_root, "csv")              # acc.zip, sep.zip
csv_agency    <- file.path(export_root, "csv", "agency")    # per-agency emp CSV
dta_dir       <- file.path(export_root, "stata")            # acc.zip, sep.zip
dta_agency    <- file.path(export_root, "stata", "agency")  # per-agency emp dta

for (d in c(hive_live_dir, hive_zip_dir, csv_dir, csv_agency, dta_dir,
            dta_agency)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Staging is flat -- the job list below records where each archive goes, so the
# staging directory does not need to mirror the export tree. It is PID-tagged
# to the R session so the sweep below can tell a live run from the corpse of
# a killed one.
staging_root <- file.path("/dev/shm", paste0("fwoc_export_", Sys.getpid()))

# The headroom below is not slack: tmpfs pages ARE RAM, so staging competes
# with the page cache, and the page cache is what has to hold emp_hive's
# ~14.8 GB of parquet footers -- the thing staging moved to RAM to stop
# evicting.
#
# The flush check in write_export() runs AFTER an item is written, so
#
#     peak staging  <=  flush_threshold + largest single item
#
# Measured 2026-08-01 on a 2024-06 sample (1,177 B/row csv, 3,296 B/row dta),
# both files of an item are staged together:
#
#     acc  12,761,219 rows -> 15.0 GB csv + 42.1 GB dta = 57.1 GB
#     sep  12,462,646 rows -> 14.7 GB csv + 41.1 GB dta = 55.7 GB
#
# so, keeping 25 GB for the page cache:
#
#     flush_threshold <= 124 - 25 - 57.1 ~= 42 GB
#
# At 40 GB, acc and sep each exceed it alone and flush on their own (peak
# 57 GB); the ~1,000 small items batch to 40 GB and flush together. At the
# 90 GB this was first set to, acc (57.1) did NOT trip the check, sep landed on
# top of it, and staging peaked at 112.8 GB of a 124 GB tmpfs -- leaving 11 GB
# for the 14.8 GB of footers. Re-derive this if the row counts grow much.
flush_threshold <- 40e9

# If /dev/shm were not tmpfs, 40 GB of staging would land on a disk silently.
shm_fstype <- system2("stat", c("-f", "-c", "%T", "/dev/shm"), stdout = TRUE)
if (shm_fstype != "tmpfs") {
  stop("/dev/shm is ", shm_fstype, ", not tmpfs -- staging would hit a disk")
}

# A hard kill leaves staging resident in RAM until reboot. The finalizer covers
# clean exits and errors; this sweep covers the kill case on the next run.
for (d in list.files("/dev/shm", pattern = "^fwoc_export_[0-9]+$",
                     full.names = TRUE)) {
  pid <- suppressWarnings(as.integer(sub(".*_", "", basename(d))))
  if (!is.na(pid) && !dir.exists(file.path("/proc", pid))) {
    message("sweeping staging left by dead pid ", pid)
    unlink(d, recursive = TRUE, force = TRUE)
  }
}

dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
reg.finalizer(globalenv(), function(e) {
  unlink(staging_root, recursive = TRUE, force = TRUE)
}, onexit = TRUE)


# ============================================================
#### Stata variable renames (32-character limit) ####
# ============================================================

stata_renames <- c(
  agency_subelem_fwoc_experimental = "agency_subelement_fwoc_experimental",
  ann_adj_basic_pay_num       = "annualized_adjusted_basic_pay_num",
  consol_stat_area_code       = "consolidated_statistical_area_code",
  duty_stn_st_cntry_terr_code = "duty_station_state_country_territory_code",
  duty_stn_st_terr_abbr       = "duty_station_state_territory_abbreviation",
  ann_adj_basic_pay_real_2024 = "annualized_adjusted_basic_pay_real_2024",
  pa_eff_date_yyyymm          = "personnel_action_effective_date_yyyymm"
)


# ============================================================
#### Staging and the parallel zip flush ####
# ============================================================

# One row per staged file: the file itself, and the archive it belongs in. The
# archive name is NOT always derivable from the file name -- acc.csv goes into
# acc_csv.zip, so that the member a user extracts is still called acc.csv --
# which is why the pairing is recorded rather than computed at flush time.
job_src <- character(0)
job_zip <- character(0)

# Every archive this run produces. Unlike job_zip it is never cleared -- it is
# the record used to prune stale archives at the end (see section 6).
published <- character(0)

flush_zips <- function() {
  if (length(job_src) == 0) return(invisible(NULL))

  # The two vectors are positionally paired. sprintf() below RECYCLES a shorter
  # vector rather than erroring, so a mismatch would silently emit commands
  # zipping the wrong source into the wrong archive -- published, with nothing
  # raised anywhere. write_export() appends to both together, so this cannot
  # happen today; the assertion is what keeps it that way.
  stopifnot(length(job_src) == length(job_zip))

  # n-2, matching the convention 01 uses for setDTthreads. Measured cost of
  # holding two back is ~9% of the zip stage -- about 3 minutes on a full run --
  # and it leaves headroom for the OS and the SSD writeback. R itself is blocked
  # in system2() while this runs, so the spare cores are not for R.
  n_cores <- max(1L, parallel::detectCores() - 2L)
  message(sprintf("-- zipping %d staged files across %d cores --",
                  length(job_src), n_cores))

  # Writes a command line to zip_jobs.txt for each file to be zipped and
  # flushed. For VA_2024's CSV that line is (wrapped here; one line in the
  # file):
  #
  #   zip -j -q 'export/csv/agency/VA/VA_2024.zip'
  #             '/dev/shm/fwoc_export_123/VA_2024.csv'
  #     && rm   '/dev/shm/fwoc_export_123/VA_2024.csv'
  #
  #   -j     junks the path, so the member stored inside is VA_2024.csv rather
  #          than dev/shm/fwoc_export_123/VA_2024.csv -- without it, unzipping
  #          would recreate the staging tree on the user's machine.
  #   -q     quiet; otherwise every flush prints one "adding:" line per file.
  #   && rm  the safety property: rm runs only if zip exited 0, so a failed
  #          zip keeps its source rather than losing it mid-run.
  cmds <- sprintf("zip -j -q %s %s && rm %s",
                  shQuote(job_zip), shQuote(job_src), shQuote(job_src))
  cmd_file <- file.path(staging_root, "zip_jobs.txt")
  writeLines(cmds, cmd_file)

  # Runs one shell per line of zip_jobs.txt, n_cores at a time. Expanded, the
  # call is (wrapped here; one line in the shell):
  #
  #   xargs -a '/dev/shm/fwoc_export_123/zip_jobs.txt' -d '\n'
  #         -P 22 -I CMD sh -c CMD
  #
  # and each line of the file is then run as:
  #
  #   sh -c "zip -j -q 'export/csv/agency/VA/VA_2024.zip'
  #                    '/dev/shm/fwoc_export_123/VA_2024.csv'
  #            && rm   '/dev/shm/fwoc_export_123/VA_2024.csv'"
  #
  #   -a      read items from the file, so nothing has to be piped from R.
  #           2,000 commands is ~244 KB against an ARG_MAX of ~2 MB -- it
  #           would fit today, but not forever, and the failure is late.
  #   -d '\n' NOT for the delimiter: -I already makes that newline. It is
  #           here to DISABLE xargs' own quote and backslash processing.
  #           Without it xargs consumes the single quotes shQuote() added
  #           and re-splits the paths on spaces, so 'a b.csv' arrives as two
  #           arguments. R's "\\n" is the two characters \ and n, which is
  #           what xargs wants -- it does the escape interpretation itself.
  #   -P      how many run concurrently.
  #   -I CMD  substitute the line for the token CMD; implies one line per run.
  #   sh -c   required because the line contains &&, which is shell syntax.
  #           Without a shell, && and rm reach zip as literal arguments.
  st <- system2("xargs", c("-a", shQuote(cmd_file), "-d", shQuote("\\n"),
                           "-P", n_cores, "-I", "CMD", "sh", "-c", "CMD"))
  # Survivors are checked BEFORE the exit status, and the order matters. xargs
  # returns 123 when any child fails, so a status test first would always win
  # and report a bare number naming nothing -- while the surviving files, which
  # name exactly what failed, sit there unexamined. The && guard is what makes
  # this test possible: a file still present is a file whose zip did not exit 0.
  #
  # It is also the stronger test. A malformed command that silently did nothing
  # and returned 0 would leave the file staged but produce status 0, so the
  # survivor check catches it and the status check would not.
  left <- job_src[file.exists(job_src)]
  if (length(left) > 0) {
    stop(length(left), " of ", length(job_src), " file(s) failed to zip ",
         "(xargs status ", st, "), e.g. ", left[1])
  }

  # Nothing survived, so every file was zipped -- but xargs still reported a
  # problem. Rare, and worth surfacing rather than swallowing.
  if (st != 0L) {
    stop("parallel zip reported status ", st, " though every file was zipped")
  }

  file.remove(cmd_file) # Delete the text file of commands
  # Clear both halves of the job list at the global level (one CSV and one dta
  # per exported item, so these always empty together).
  job_src <<- character(0) # staged source files
  job_zip <<- character(0) # the archive each one goes into
  # invisible so the top-level call after the emp loops prints nothing; R
  # auto-prints a returned value at top level, and a bare NULL in the log is
  # noise. Same idiom as the early return above.
  invisible(NULL)
}

# Total bytes awaiting zip. Measures the files themselves rather than keeping a
# running total, so it cannot drift from what is actually staged.
staged_bytes <- function() {
  if (length(job_src) == 0) return(0)
  # Every queued path must still exist: file.size() returns NA for a missing
  # file, sum() propagates it, and the caller's `if (staged_bytes() > ...)` then
  # dies with "missing value where TRUE/FALSE needed", naming neither this
  # function nor the file. Entries are queued as they are written and cleared
  # as they are deleted, so this holds by construction -- assert it rather than
  # rely on that. na.rm would be the wrong fix: it would under-count and let
  # staging overrun the threshold.
  stopifnot(all(file.exists(job_src)))
  sum(file.size(job_src))
}

# Writes one item's two files into staging and records where each archive goes.
# The flush check sits here rather than in the callers so every item is covered
# by construction. Checking AFTER the write means staging peaks at the
# threshold plus one item -- bounded, and the largest item is acc at 57.1 GB
# (both files staged together). See the flush_threshold derivation above.
write_export <- function(df, basename, csv_out, dta_out,
                         csv_suffix = "", dta_suffix = "") {
  csv_src <- file.path(staging_root, paste0(basename, ".csv")) # staging path
  write_csv(df, csv_src, na = "")                # write the CSV to the tmpfs
  job_src <<- c(job_src, csv_src)                # add the path to job_src
  # Add the path to the zip job vector. The suffix goes on the ARCHIVE, not the
  # staged file -- that is what puts acc.csv inside acc_csv.zip.
  csv_zip <- file.path(csv_out, paste0(basename, csv_suffix, ".zip"))
  job_zip   <<- c(job_zip, csv_zip)
  published <<- c(published, csv_zip)

  dta_df  <- rename(df, any_of(stata_renames)) # rename variables for Stata
  dta_src <- file.path(staging_root, paste0(basename, ".dta")) # staging path
  write_dta(dta_df, dta_src)                     # write the dta to the tmpfs
  job_src <<- c(job_src, dta_src)                # add the path to job_src
  # Add the path to the zip job vector
  dta_zip <- file.path(dta_out, paste0(basename, dta_suffix, ".zip"))
  job_zip   <<- c(job_zip, dta_zip)
  published <<- c(published, dta_zip)

  # rename() shares column vectors with df rather than copying them, so this
  # drops a list skeleton, not data. The gc() is the part that matters -- it
  # reclaims haven's per-column conversion transients from write_dta before
  # the next item is collected.
  rm(dta_df)
  invisible(gc())

  # Check size of staging files and zip and flush once the threshold is crossed
  if (staged_bytes() > flush_threshold) flush_zips()
}


# ============================================================
#### 1. Parquet hives: rsync unzipped (API) + zip for download ####
# ============================================================

message("=== Parquet hives ===")
for (hive in c("emp_hive", "acc_hive", "sep_hive")) {
  message(hive, " (rsync)")
  # rsync, NOT `cp -al` hardlinks: 15_finalize_hive.R rewrites the hive by
  # unlink-and-create, so a hardlinked export tree would keep serving the
  # PREVIOUS generation's parquet files while passing every existence check.
  st <- system2("rsync", c("-a", "--delete",
                           paste0(hive, "/"),
                           file.path(hive_live_dir, hive, "/")))
  if (st != 0L) stop("rsync failed (status ", st, ") for ", hive)

  message(hive, " (zip)")
  zip_path <- file.path(hive_zip_dir, paste0(hive, ".zip"))
  # Must start from nothing, and the removal must be checked. zip UPDATES an
  # archive rather than replacing it, so a member that no longer exists on disk
  # SURVIVES: verified that dropping year=1991 from a hive and re-zipping
  # without deleting the archive leaves year=1991 inside it, and a downloader
  # unzips a partition the live hive does not have. The per-item archives below
  # are safe from this because their single member name never changes; these
  # are not, because the partition set does.
  if (file.exists(zip_path) && !file.remove(zip_path)) {
    stop("could not remove stale archive: ", zip_path)
  }
  # -r preserves the year=/month= partition folders (do NOT use -j here); -0
  # stores without recompressing (parquet is already compressed). Run from the
  # project root so the archive contains "<hive>/year=.../...".
  st <- system2("zip", c("-r", "-0", "-q", zip_path, hive))
  if (st != 0L) stop("zip failed (status ", st, ") for ", zip_path)
}


# ============================================================
#### 2. Accessions ####
# ============================================================

message("=== Accessions ===")
acc <- open_dataset("acc_hive", unify_schemas = TRUE) |> collect()
message("rows: ", format(nrow(acc), big.mark = ","))
write_export(acc, "acc", csv_dir, dta_dir,
             csv_suffix = "_csv", dta_suffix = "_dta")
rm(acc)
gc()


# ============================================================
#### 3. Separations ####
# ============================================================

message("=== Separations ===")
sep <- open_dataset("sep_hive", unify_schemas = TRUE) |> collect()
message("rows: ", format(nrow(sep), big.mark = ","))
write_export(sep, "sep", csv_dir, dta_dir,
             csv_suffix = "_csv", dta_suffix = "_dta")
rm(sep)
gc()


# ============================================================
#### 4. Employment -- tiered agency-level export ####
# ============================================================

message("=== Employment (tiered agency export) ===")
emp <- open_dataset("emp_hive", unify_schemas = TRUE)

# --- Tier definitions (V1) ---
by_year_codes   <- c("VA", "AR", "NV", "AF", "HS", "TR", "DD",
                     "AG", "DJ", "HE", "IN", "TD", "SZ", "CM")
by_decade_codes <- c("NN", "GS", "DL", "EP", "DN")
exclude_codes   <- c("00", "01", "04")

# One scan gives every agency's row count AND its year list, replacing the
# per-agency distinct(year) scans the tier loops would otherwise each run.
cy <- emp |> count(agency_code, year) |> collect()

row_counts <- cy |>
  group_by(agency_code) |>
  summarise(n = sum(n), .groups = "drop")

# Flag unclassified agencies whose single whole-agency file would be the
# largest thing in the export.
#
# The bound is the biggest file the by-year tier already produces, not a fixed
# row count. What matters is the size of one OUTPUT FILE, and for the whole
# tier that is the agency's entire row count -- so the honest comparison is
# against a file we already publish without concern.

year_file_rows <- cy |>
  filter(agency_code %in% by_year_codes) |>
  group_by(agency_code, year) |>
  summarise(n = sum(n), .groups = "drop")

unclassified <- row_counts |>
  filter(!agency_code %in% c(by_year_codes, by_decade_codes, exclude_codes))

large <- unclassified |> filter(n > max(year_file_rows$n))

if (nrow(large) > 0) {
  warning("Whole-agency file(s) larger than the biggest by-year file (",
    format(max(year_file_rows$n), big.mark = ","), " rows) ",
    "-- review tiering:\n",
    paste(sprintf("  %s: %s rows", large$agency_code,
                  format(large$n, big.mark = ",")), collapse = "\n"))
}

rm(large)

whole_codes <- unclassified |> pull(agency_code) |> sort()

# Extract a sorted vector of years for the agency code
years_for <- function(code) sort(cy$year[cy$agency_code == code])

# --- By year (14 agencies) ---
message("-- By year --")
for (code in by_year_codes) {
  # Each multi-file agency gets its own folder (csv/agency/VA/VA_2024.zip). The
  # R2 listing behind the Download page returns at most 1,000 entries per
  # folder, and a flat csv/agency/ holds more than that.
  csv_code_dir <- file.path(csv_agency, code)
  dta_code_dir <- file.path(dta_agency, code)
  dir.create(csv_code_dir, showWarnings = FALSE)
  dir.create(dta_code_dir, showWarnings = FALSE)
  for (yr in years_for(code)) {
    bn <- paste0(code, "_", yr) # Create a file name
    message(bn)
    chunk <- emp |> filter(agency_code == code, year == yr) |> collect()
    write_export(chunk, bn, csv_code_dir, dta_code_dir)
    rm(chunk)
    gc()
  }
}

# --- By decade (5 agencies) ---
message("-- By decade --")
for (code in by_decade_codes) {
  # Own folder per agency, as in the by-year tier (csv/agency/NN/NN_1970s.zip).
  csv_code_dir <- file.path(csv_agency, code)
  dta_code_dir <- file.path(dta_agency, code)
  dir.create(csv_code_dir, showWarnings = FALSE)
  dir.create(dta_code_dir, showWarnings = FALSE)
  # Get a vector of years for the agency
  yrs  <- years_for(code)
  # Create a vector of decades
  decs <- sort(unique(as.integer(floor(yrs / 10) * 10)))
  for (dec in decs) {
    dec_end <- as.integer(dec + 9L) # Create the decade's end
    bn <- paste0(code, "_", dec, "s") # Create a file name
    message(bn)
    chunk <- emp |>
      filter(agency_code == code, year >= dec, year <= dec_end) |>
      collect()
    if (nrow(chunk) > 0) write_export(chunk, bn, csv_code_dir, dta_code_dir)
    rm(chunk)
    gc()
  }
}

# --- Whole agency ---
# The other two tiers filter on `year`, a partition key, so arrow discards whole
# directories before reading. This tier has no year predicate, and `agency_code`
# is an ordinary column -- so `filter(agency_code == code)` prunes NOTHING, and
# each of the ~307 items opens all 331 parquet files (55 GB) to extract one
# small agency. Measured 2026-08-02: 9.7 h for the tier, 43% of a 22.5 h run.
#
# So scan once into an agency-partitioned copy of just this tier's rows, then
# read each agency back from its own directory. The same filter becomes a
# partition predicate against that copy, and the per-item cost collapses to the
# write. Projected ~15 min; NOT yet measured -- confirm on the next run.
message("-- Whole agency --")

# Store the whole-agency tier hive one directory above the repo root -- a ~6 GB
# derived intermediate has no business in a public tree. Deliberately NOT under
# staging_root: staged_bytes() sums only the job list, so a hive in /dev/shm
# would be invisible to the flush and silently overdraw the tmpfs budget.
small_root <- file.path("..", "whole-agency")

# A run killed mid-tier leaves this behind, and write_dataset() would then mix
# stale partitions with fresh ones -- its default overwrites same-named files
# but does not clear the directory. recursive = TRUE takes the whole tree.
if (dir.exists(small_root)) unlink(small_root, recursive = TRUE)

emp |>
  filter(agency_code %in% whole_codes) |>
  write_dataset(small_root,
                partitioning   = "agency_code",
                hive_style     = TRUE,
                use_dictionary = FALSE)

small <- open_dataset(small_root)

# Written in one pass, so the schema is uniform by construction -- no
# unify_schemas here, which is part of why the reads are cheap.
#
# Two things the round-trip could break, both silent:
#   1. Partitioning STORES agency_code as a directory name and drops it from the
#      data files. "NA" is a real OPM code (National Aeronautics and Space
#      Council), and a hive parser that reads it as a null would blank the
#      agency on every one of that agency's rows.
#   2. A column lost in the round-trip would simply be absent from the export.
stopifnot(
  !anyNA(small |> distinct(agency_code) |> collect() |> pull(agency_code)),
  setequal(names(small), names(emp))
)

# Partition keys come back LAST, not in their original position -- so restore
# the published column order that 15_finalize_hive.R established. Without this
# the whole-agency files would carry a different column order than the by-year
# and by-decade files.
emp_cols <- names(emp)

for (code in whole_codes) {
  message(code)
  chunk <- small |>
    filter(agency_code == code) |>
    collect() |>
    select(all_of(emp_cols))
  if (nrow(chunk) > 0) write_export(chunk, code, csv_agency, dta_agency)
  rm(chunk)
  gc()
}

rm(small)
unlink(small_root, recursive = TRUE)

# Whatever is still staged and under the threshold.
flush_zips()


# ============================================================
#### 5. Agency code lookup ####
# ============================================================

lookup <- emp |>
  distinct(agency_code, agency) |>
  collect() |>
  filter(!agency_code %in% exclude_codes)

# A code keeps every name it was published under, so a renamed agency returns
# more than one row and its single file covers them all.
#
# `agency` is NA wherever a row's date falls outside every label window for its
# code -- a code can outlive its label (AE keeps appearing after the Atomic
# Energy Commission is abolished) or its label can expire before the hive's
# first observation. A code named in some other period loses only a duplicate
# row when those are dropped below. A code with no named period at all would
# drop out of the lookup while its files stay published, so it takes its name
# from the label table directly, ignoring the window. The code set still comes
# from the hive -- this only supplies names for codes already in it.
unnamed <- setdiff(lookup$agency_code[is.na(lookup$agency)],
                   lookup$agency_code[!is.na(lookup$agency)])

fill <- read_csv("metadata/labels/agency_labels.csv",
                 col_types = cols(.default = "c"), na = "") |>
  filter(code %in% unnamed) |>
  # Label windows are split at reorganizations that often do not change the
  # name, so the same code-name pair recurs across rows. Dedupe on the pair and
  # drop the dates: a code with one name yields one row, and a code with
  # genuinely different names yields one row per name -- the same fan-out the
  # named codes above already have.
  distinct(agency_code = code, fill_label = label)

codes_before <- unique(lookup$agency_code)

lookup <- lookup |>
  left_join(fill, by = "agency_code") |>
  mutate(agency = coalesce(agency, fill_label)) |>
  select(-fill_label) |>
  filter(!is.na(agency)) |>
  distinct() |>
  arrange(agency_code, agency)

# Dropping unnamed rows must not drop a code: every published agency file needs
# an entry here.
if (!setequal(codes_before, lookup$agency_code)) {
  stop("agency_code_lookup lost code(s): ",
       paste(setdiff(codes_before, lookup$agency_code), collapse = ", "),
       "\n  No label in metadata/labels/agency_labels.csv covers them.")
}

# Lives at the bucket root (not under csv/agency/) to match the published
# layout.
write_csv(lookup, file.path(export_root, "agency_code_lookup.csv"), na = "")


# ============================================================
#### 6. Prune archives this run did not produce ####
# ============================================================

# An agency that disappears from the hive leaves its archive behind forever,
# and 17 republishes it -- the export tree accumulates data no longer in the
# hive it claims to publish.
#
# Pruning happens HERE, at the end, rather than by clearing the tree at the
# start, and the difference matters. 17 --mirror runs `rclone sync`, which
# DELETES remote objects absent locally. Wiping the tree up front would mean a
# run that died halfway left a genuinely incomplete tree, and a --mirror after
# it would delete the missing archives from R2 as well -- so the previous
# version would not survive on the bucket either. Pruning at the end inverts
# that: a failed run prunes nothing and leaves the last good tree intact, while
# a successful run ends holding exactly what it produced.
#
# Scoped to the two agency directories because they are the only place where
# the archive SET is data-dependent -- their membership follows the agencies
# and years found in the hive. If an agency-year vanished from the hive, the
# stale agency-year zip would remain after the monthly update. That scenario is
# unlikely but we guard against it here nonetheless. The more likely scenario is
# promotion of an agency to a finer tier -- whole-agency to decade, or decade to
# year. The old archive is not overwritten, because the new ones have different
# names, so the tree would publish current decade-level zips alongside a stale
# whole-agency one. Everywhere else the set is fixed or already managed:
#
#   csv/ , stata/   always exactly four archives (acc/sep x csv/dta), every run
#   parquet/hive/   kept in step by rsync --delete above
#   parquet/zip/    each archive removed and rebuilt above
#
# Widening the scope to csv/ and stata/ would be harmless -- those four are in
# `published` too -- but it would guard nothing.
#
# recursive = TRUE because the by-year and by-decade archives sit one level
# down, in a folder per agency. It also clears the archives of the earlier flat
# layout (csv/agency/VA_2024.zip): they are on disk, no run produces them any
# more, so they are stale.

existing <- list.files(c(csv_agency, dta_agency), pattern = "\\.zip$",
                       full.names = TRUE, recursive = TRUE)

stale <- setdiff(existing, published)

if (length(stale) > 0) {
  message("pruning ", length(stale), " archive(s) this run did not produce:")
  for (f in stale) message("  ", f)
  if (!all(file.remove(stale))) stop("failed to prune stale archive(s)")
}


message("=== Done ===")

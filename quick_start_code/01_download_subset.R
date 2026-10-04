############################################################
# 01_download_subset.R
#
# Downloads a subset of the Federal Workforce OpenCommons (FWOC) data: one
# dataset, the agencies you choose, and the range of months you choose. The
# subset is saved on your computer as Parquet files. The next script,
# 02_open_subset.R, shows how to open it and how to save a piece of it as CSV
# or Stata.
#
# HOW TO USE
#   1. Install the packages once: install.packages(c("arrow", "dplyr", "jsonlite"))
#   2. Edit the SETTINGS section below.
#   3. Run the whole script. It prints one line per month, then lists any
#      months that failed.
#
# WHEN NOT TO USE THIS SCRIPT
#   - You want a complete dataset. Download its zip file in your browser from
#     the Download page (https://opencommons.mdi.georgetown.edu), under
#     parquet/zip/, csv/, or stata/.
#   - You work only in Stata or with CSV files. The Download page has
#     ready-made CSV and Stata files: accessions and separations as single
#     files, and employment as one or more files per agency (csv/agency/ and
#     stata/agency/).
#
# HOW IT WORKS
#   The data are stored as one Parquet file per dataset per month, not per
#   agency. For each month you request, the script downloads that month's file,
#   keeps the rows for your agencies, saves them, and deletes the downloaded
#   file. Disk space is therefore needed only for your subset plus one month
#   (an employment month is about 250 MB).
#
# WARNING: THIS SCRIPT OVERWRITES FILES IT SAVED BEFORE
#   For each month you request, the script first deletes the file it saved for
#   that dataset and month on an earlier run (in OUTPUT_DIR), then downloads
#   the month again. This keeps your copy current when the data are updated.
#   It also means:
#     - If you change AGENCIES and run again, the earlier subset for those
#       months is replaced. To keep prior downloads, choose a different OUTPUT_DIR.
#     - If a month fails to download, its earlier file is already gone. Run
#       the script again to get the current version.
#     - Months outside START and END are not touched.
#
# PLEASE DOWNLOAD ONCE AND WORK LOCALLY
#   Every month downloaded is a billable read for the project. Pull the subset
#   you need once, then work from your local copy. Re-run the script when the
#   data are updated.
#
# SIZE AND TIME
#   Employment is large: about 3 GB is downloaded per year requested (from
#   2012 on), whichever agencies you keep. Accessions and separations are
#   small: about 0.03 GB per year.
############################################################

library(arrow)
library(dplyr)
library(jsonlite)


############################################################
# SETTINGS -- edit these
############################################################

# DATASET     "emp" = employment: a snapshot of every employee, quarterly from
#                     September 1973 to June 2011 and monthly after that.
#             "acc" = accessions: new hires and transfers into an agency,
#                     monthly from January 1982.
#             "sep" = separations: exits and transfers out of an agency,
#                     monthly from January 1982.
# AGENCIES    Two-character agency codes to keep. c() keeps every agency.
#             agency_code_lookup.csv on the Download page lists the codes.
# START, END  First and last month to download, written "YYYY-MM".
# OUTPUT_DIR  Folder to save into. It is created if it does not exist.

# Example 1: employment at the Department of Defense (DD) and the Department
# of Veterans Affairs (VA) in 2024. About 3 GB is downloaded. Setting START to
# "2010-01" and END to the current month would download about 44 GB.
DATASET <- "emp"
AGENCIES <- c("DD", "VA")
START <- "2024-01"
END <- "2024-12"
OUTPUT_DIR <- "fwoc_subset"

# Example 2: accessions for every agency, 2020 through 2024.
# DATASET <- "acc"
# AGENCIES <- c()
# START <- "2020-01"
# END <- "2024-12"
# OUTPUT_DIR <- "fwoc_subset"

# Example 3: separations from Veterans Affairs, 1982 through 2025.
# DATASET <- "sep"
# AGENCIES <- c("VA")
# START <- "1982-01"
# END <- "2025-12"
# OUTPUT_DIR <- "fwoc_subset"


############################################################
# SETUP
############################################################

stopifnot(DATASET %in% c("emp", "acc", "sep"))

OPENCOMMONS_URL <- "https://opencommons.mdi.georgetown.edu"

# R stops a download after 60 seconds by default, which is too short for an
# employment month on a slow connection. This allows one hour per file.
options(timeout = 3600)

# The subset is saved in OUTPUT_DIR/<dataset>/, for example fwoc_subset/emp/.
dataset_dir <- file.path(OUTPUT_DIR, DATASET)
dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)

# Each month's file is downloaded here, then deleted once its rows are saved.
download_path <- file.path(OUTPUT_DIR, paste0(DATASET, "_download_in_progress.parquet"))

# One row per requested month. The loop below fills in `rows` and `status`.
month_starts <- seq(
  as.Date(paste0(START, "-01")),
  as.Date(paste0(END, "-01")),
  by = "month"
)
download_log <- data.frame(
  year = as.integer(format(month_starts, "%Y")),
  month = as.integer(format(month_starts, "%m")),
  rows = NA_integer_,
  status = NA_character_
)


############################################################
# DOWNLOAD, SUBSET, AND SAVE ONE MONTH AT A TIME
############################################################

for (i in seq_len(nrow(download_log))) {
  y <- download_log$year[i]
  m <- download_log$month[i]

  # Where this month's rows are saved. The year=/month= folder names are the
  # same as in the full data. This layout is called "hive-style" partitioning:
  # the folder names are how Arrow knows each file's year and month, so that
  # open_dataset() can read the folder as one dataset.
  out_path <- file.path(
    dataset_dir, paste0("year=", y), paste0("month=", m), "part-0.parquet"
  )

  # Running the script again replaces what was saved before. Any month can
  # change when the data are updated: OPM revises earlier releases, and
  # variables are added or corrected. unlink() deletes this month's file from
  # an earlier run, if there is one.
  unlink(out_path)

  # tryCatch() keeps one failed month from stopping the whole run. The failure
  # is recorded in download_log and the loop moves on to the next month.
  result <- tryCatch(
    {
      # Ask OpenCommons what is stored for this month. The reply has one row
      # per file, with its name (Key), size in bytes (Size), and a temporary
      # download link (signedUrl). The link expires after 15 minutes, so it is
      # requested here, immediately before the download.
      prefix <- sprintf("parquet/hive/%s_hive/year=%d/month=%d/", DATASET, y, m)
      listing <- fromJSON(paste0(
        OPENCOMMONS_URL, "/r2/list?prefix=", URLencode(prefix, reserved = TRUE)
      ))
      files <- listing$Contents

      if (NROW(files) == 0) {
        # Not an error. Employment is quarterly (March, June, September,
        # December) before July 2011, and recent months may not be released yet.
        list(rows = NA_integer_, status = "no data for this month")
      } else {
        download.file(files$signedUrl[1], download_path, mode = "wb", quiet = TRUE)

        # Confirm the whole file arrived by comparing its size with the size
        # OpenCommons reported.
        if (file.size(download_path) != files$Size[1]) {
          stop("download incomplete")
        }

        # open_dataset() does not read the file yet. filter() and compute()
        # then read only the rows for the requested agencies into memory.
        # compute() keeps them as an Arrow table, so every variable is saved
        # with the same type it has in the full data. collect() would convert
        # them to an R data frame, which stores all integers as 32-bit.
        month_data <- open_dataset(download_path)
        if (length(AGENCIES) > 0) {
          month_data <- month_data |> filter(agency_code %in% AGENCIES)
        }
        month_data <- month_data |> compute()

        if (nrow(month_data) == 0) {
          list(rows = 0L, status = "no rows for these agencies")
        } else {
          dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
          write_parquet(month_data, out_path, compression = "zstd")
          list(rows = nrow(month_data), status = "saved")
        }
      }
    },
    error = function(e) {
      # Remove any partly written output.
      unlink(out_path)
      # A download error quotes the full download link, which ends in a long
      # signature beginning "?X-Amz". gsub() deletes that signature from the
      # message: the pattern matches "?X-Amz" and every character after it up
      # to the next quote mark or space.
      list(
        rows = NA_integer_,
        status = paste("FAILED:", gsub("\\?X-Amz[^' ]*", "", conditionMessage(e)))
      )
    }
  )

  unlink(download_path)
  download_log$rows[i] <- result$rows
  download_log$status[i] <- result$status
  message(sprintf("%s %d-%02d: %s", DATASET, y, m, result$status))
}


############################################################
# SUMMARY
############################################################

failed <- download_log[startsWith(download_log$status, "FAILED"), ]

message(
  sum(download_log$status == "saved"), " month(s) saved to ", dataset_dir, "; ",
  nrow(failed), " failed."
)

# List the months that failed, if any. Run the script again to retry them.
if (nrow(failed) > 0) {
  print(failed)
}

# Next: 02_open_subset.R.

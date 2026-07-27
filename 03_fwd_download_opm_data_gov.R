# Author: Mark Richardson and Claude Code
# Purpose: Download employment, accessions, and separations microdata from the
#          data.opm.gov API as Parquet, into per-dataset raw folders under
#          data_opm_gov/. Idempotent: for each dataset it lists every available
#          file from the API, compares against what is already on disk, and
#          downloads only what is missing or incomplete. Every published version
#          is kept.
#          Also writes metadata/fwd/file_manifest.csv, which does double duty: 04
#          reads it to pick the version OPM flags as current for each period, so
#          this script is a hard prerequisite for that one; and it is the committed
#          record of exactly which OPM files a release of the data was built from
#          (the downloads themselves are far too large to commit).
# API docs: https://data.opm.gov/get-data/api-access

library(httr2)
library(readr)

base_url <- "https://data.opm.gov/api/v1/files"

# TRUE if `path` is a structurally complete Parquet file. Parquet writes the magic
# bytes "PAR1" at BOTH ends, and the trailing copy sits after the metadata footer,
# so requiring both rules out a saved error body (no leading magic) and a download
# cut short part-way (leading magic intact, trailing magic missing).
is_parquet <- function(path) {
  if (!file.exists(path)) return(FALSE)
  n <- file.size(path)
  if (n < 8) return(FALSE)

  con <- file(path, "rb")
  on.exit(close(con))

  if (!identical(rawToChar(readBin(con, "raw", 4L)), "PAR1")) return(FALSE)
  seek(con, where = n - 4)
  identical(rawToChar(readBin(con, "raw", 4L)), "PAR1")
}

# sha256 of each file in `paths`, for the provenance manifest.
sha256_file <- function(paths) {
  vapply(paths,
         function(p) digest::digest(p, algo = "sha256", file = TRUE),
         character(1), USE.NAMES = FALSE)
}

# Flatten the API's JSON listing into one row per published file. Taking year,
# month and version from the API's own fields keeps them structured, rather than
# parsing them back out of the filename. vapply declares each column's type, so a
# field the API drops or changes shape fails here rather than silently becoming a
# list column; as.integer() and isTRUE() are likewise guards, not conversions --
# the API already sends version as a number and current as a boolean.
fwd_file_table <- function(file_list, dataset) {
  data.frame(
    dataset          = dataset,
    filename         = vapply(file_list, function(f) f$filename, character(1)),
    year             = vapply(file_list, function(f) f$year, character(1)),
    month            = vapply(file_list, function(f) f$month, character(1)),
    version          = vapply(file_list, function(f) as.integer(f$version), integer(1)),
    current          = vapply(file_list, function(f) isTRUE(f$current), logical(1)),
    publish_date     = vapply(file_list, function(f) f$publishDate, character(1)),
    stringsAsFactors = FALSE
  )
}

# List every available file for one dataset, diff against disk, download the rest,
# and return the manifest rows for that dataset. The bare List endpoint returns all
# published versions of each month; each version is a distinct filename, so "not
# already on disk as a complete file" pulls new months, any later OPM correction,
# and anything left truncated by an interrupted run. req_retry() backs off on
# transient/429 responses.
download_fwd <- function(dataset, sleep = 0.5) {

  dest_dir <- file.path("data_opm_gov", dataset)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  # get the list of files from the API
  file_list <- request(paste0(base_url, "/", dataset)) |>
    req_timeout(60) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json()

  # Subset to files not already on desk (after passing is_parquet() check)
  todo <- Filter(function(f) !is_parquet(file.path(dest_dir, paste0(f$filename, ".parquet"))),
                 file_list)

  message(sprintf("%s: %d files available, %d complete on disk, %d to download",
                  dataset, length(file_list), length(file_list) - length(todo), length(todo)))

  for (f in todo) {
    local_name <- paste0(f$filename, ".parquet")
    dest <- file.path(dest_dir, local_name)
    url <- paste0(base_url, "/", dataset, "/", f$year, "/", f$month, "/", f$version, "/download")

    message("  downloading ", local_name)

    # ok is TRUE only if the request succeeds AND the result validates as Parquet,
    # so it is FALSE two ways: the request errors, or it returns 200 with a body
    # that is not a complete Parquet file (an error page, or a truncated transfer).
    ok <- tryCatch({
      request(url) |>
        req_timeout(300) |>
        req_retry(max_tries = 3) |>
        req_perform(path = dest)
      is_parquet(dest)
    }, error = function(e) {
      message("    FAILED: ", conditionMessage(e))
      FALSE
    })

    if (!ok && file.exists(dest)) file.remove(dest) # drop partial / bad file
    Sys.sleep(sleep)
  }

  # Record the state of the local copy alongside what OPM published. Hashing every
  # run (not only on download) keeps the manifest a description of the bytes that
  # are actually present, so it stays true even if a file is altered out of band.
  manifest <- fwd_file_table(file_list, dataset)
  paths <- file.path(dest_dir, paste0(manifest$filename, ".parquet"))

  # Create an indicator variable for files available from API that are valid on disk.
  manifest$complete_on_disk <- vapply(paths, is_parquet, logical(1), USE.NAMES = FALSE)
  manifest$bytes <- NA_real_
  manifest$sha256 <- NA_character_

  have <- manifest$complete_on_disk

  # paths was built from manifest$filename, so it is parallel to the manifest rows:
  # `have` selects the same positions on both sides (subset assignment: only the
  # selected positions change). Each right-hand side returns exactly sum(have)
  # values, so nothing recycles or misaligns.
  manifest$bytes[have] <- file.size(paths[have])
  manifest$sha256[have] <- sha256_file(paths[have])

  manifest # Return manifest. req_perform() above writes the files to disk.
}

#### Employment ####

emp_manifest <- download_fwd("employment")

#### Accessions ####

acc_manifest <- download_fwd("accessions")

#### Separations ####

sep_manifest <- download_fwd("separations")

#### Write the provenance manifest ####

manifest <- rbind(emp_manifest, acc_manifest, sep_manifest)
manifest <- manifest[order(manifest$dataset, manifest$year,
                           manifest$month, manifest$version), ]

if (!dir.exists("metadata/fwd")) dir.create("metadata/fwd", recursive = TRUE)
write_csv(manifest, "metadata/fwd/file_manifest.csv", na = "")

message(sprintf("manifest: %d files across %d datasets -> metadata/fwd/file_manifest.csv",
                nrow(manifest), length(unique(manifest$dataset))))

#### Halt the build if anything is still missing ####

incomplete <- manifest$filename[!manifest$complete_on_disk]
if (length(incomplete) > 0) {
  stop(length(incomplete), " file(s) failed to download; re-run step 03 to resume:\n",
       paste(incomplete, collapse = "\n"))
}

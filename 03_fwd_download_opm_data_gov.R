# Author: Mark Richardson and Claude Code
# Purpose: Download employment, accessions, and separations microdata from the
#          data.opm.gov API as Parquet, into per-dataset raw folders under
#          data_opm_gov/. Idempotent: for each dataset it lists every available
#          file from the API, compares against what is already on disk, and
#          downloads only what is missing or incomplete. Every published version
#          is kept. Halts if a file already on disk comes back from the API with
#          a new publish date, the one case that download rule cannot see.
#          Also writes metadata/fwd/file_manifest.csv, which does double duty: 04
#          reads it to pick the version OPM flags as current for each period, so
#          this script is a hard prerequisite for that one; and it is the committed
#          record of exactly which OPM files a release of the data was built from
#          (the downloads themselves are far too large to commit).
# API docs: https://data.opm.gov/get-data/api-access

library(dplyr)
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

# Total size of a remote file, taken from a one-byte range request: the API answers with
# "Content-Range: bytes 0-0/<total>". It serves no ETag and no Last-Modified, and does not route
# HEAD (404), so this is the only way to size a published file without downloading it. NA when
# the request fails or the header is absent, which reads as "could not corroborate".
remote_size <- function(dataset, year, month, version) {
  url <- paste0(base_url, "/", dataset, "/", year, "/", month, "/", version, "/download")
  tryCatch({
    cr <- request(url) |>
      req_headers(Range = "bytes=0-0") |>
      req_timeout(60) |>
      req_retry(max_tries = 3) |>
      req_perform() |>
      resp_header("content-range")
    if (is.null(cr)) NA_real_ else as.numeric(sub(".*/", "", cr))
  }, error = function(e) NA_real_)
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

#### Prior manifest ####

# Read before anything downloads, because the write at the end replaces it. Comparing this run's
# publish dates against the last run's is the only way to notice OPM rewriting a file in place;
# NULL on a first run, which disables that check rather than failing it.
manifest_path  <- "metadata/fwd/file_manifest.csv"
prior_manifest <- if (file.exists(manifest_path)) {
  read_csv(manifest_path, na = "", show_col_types = FALSE, col_types = cols(.default = "c"))
} else {
  NULL
}

#### Employment ####

emp_manifest <- download_fwd("employment")

#### Accessions ####

acc_manifest <- download_fwd("accessions")

#### Separations ####

sep_manifest <- download_fwd("separations")

#### Assemble the provenance manifest ####

manifest <- rbind(emp_manifest, acc_manifest, sep_manifest)
manifest <- manifest[order(manifest$dataset, manifest$year,
                           manifest$month, manifest$version), ]

#### Halt if OPM rewrote a file in place ####

# A correction normally arrives as a new version, which is a new filename and so downloads under
# the rule in download_fwd(). The case that rule cannot see is the same version re-published under
# the same name: is_parquet() tests structure, not freshness, so the local copy is kept and nothing
# downstream can tell the difference -- 04 assembles whatever is on disk, and validation reconciles
# against that. publishDate is the only field the API offers that would move, so a drift on a file
# already held halts the build. The remote size is fetched only for the files that trip it, and
# only for the message: Parquet is compressed, so a rewrite can preserve the byte count, which
# makes size unsafe as a second condition but useful for judging what actually changed.
if (!is.null(prior_manifest)) {
  # Left join, not full: only a file in this run's listing can have a stale local copy, so rows
  # the prior manifest has and this one does not (a withdrawn or renamed file) are not wanted.
  # Pulling just the one prior column means a filename absent from the prior manifest arrives as
  # NA in publish_date_prior, so a single is.na() guard covers both "newly published" and "prior
  # row carried no date". cmp is scratch: the manifest written below is the untouched original.
  cmp   <- left_join(manifest, prior_manifest[, c("filename", "publish_date")],
                     by = "filename", suffix = c("", "_prior"))

  # Files we already held -- complete on disk now, and present in the prior manifest -- whose
  # publish date has moved. A file downloaded this run is excluded by the second guard, not the
  # first: it is complete on disk too, but has no prior row, so publish_date_prior is NA.
  moved <- cmp$complete_on_disk & !is.na(cmp$publish_date_prior) &
           cmp$publish_date != cmp$publish_date_prior

  # Guarded above, so moved carries no NA -- if (NA) would error rather than skip.
  if (any(moved)) {
    # Every flagged row. A moved publish date is the signal, not proof of a rewrite -- the sizes
    # below are what let the operator judge which it was.
    stale <- cmp[moved, ]
    # Size of each flagged file as OPM currently serves it, to set against the local bytes.
    stale$remote_bytes <- mapply(remote_size, stale$dataset, stale$year,
                                 stale$month, stale$version)

    # This halt sits before the write below, and must stay there. Writing first would record the
    # new publish dates, so the re-run would compare against them, find no drift, and proceed on
    # the stale local copies -- the run that reported the problem having destroyed the evidence
    # for it. The manifest and the local files are two halves of one state; neither moves alone.
    stop(nrow(stale), " file(s) already on disk have a new OPM publish date. Delete the local ",
         "copies and re-run step 03 to refresh them:\n",
         paste(sprintf("  %s.parquet  %s -> %s  local %s / remote %s bytes",
                       stale$filename, stale$publish_date_prior, stale$publish_date,
                       format(stale$bytes, big.mark = ","),
                       format(stale$remote_bytes, big.mark = ",")),
               collapse = "\n"))
  }
}

#### Write the provenance manifest ####

if (!dir.exists("metadata/fwd")) dir.create("metadata/fwd", recursive = TRUE)
write_csv(manifest, manifest_path, na = "")

message(sprintf("manifest: %d files across %d datasets -> metadata/fwd/file_manifest.csv",
                nrow(manifest), length(unique(manifest$dataset))))

#### Halt the build if anything is still missing ####

incomplete <- manifest$filename[!manifest$complete_on_disk]
if (length(incomplete) > 0) {
  stop(length(incomplete), " file(s) failed to download; re-run step 03 to resume:\n",
       paste(incomplete, collapse = "\n"))
}

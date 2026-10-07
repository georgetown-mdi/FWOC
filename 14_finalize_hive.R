# Author: Mark Richardson and Claude Code
# Purpose: Reorder variables to publication order and drop the columns that
#          carry no information. This is the build target: what survives here is
#          what gets published.
#
# The publication decision is driven by metadata/publish/variable_manifest.csv,
# a HAND-CURATED input.
#
# The manifest has one row per variable and one column per hive, each holding
# keep / drop / empty (variable not present in that hive). ROW ORDER IS THE
# PUBLISHED COLUMN ORDER -- do not sort this file.
#
# The codebook remains the documentation of what ships; that "published ==
# documented" agreement is asserted downstream (codebook/02, validation/)
# rather than here. The codebook is generated FROM these hives (codebook/01
# reads them, codebook/02 writes the table), so it cannot also be what selects
# them -- a column dropped here for being undocumented could never become
# documented. The manifest is an input, which is what keeps that from closing.

library(arrow)
library(dplyr)
library(readr)

# na = "" so a literal "NA"-style value is kept as a string rather than dropped
# to missing.
manifest <- read_csv("metadata/publish/variable_manifest.csv",
                     col_types = cols(.default = "c"), na = "")

hive_flags <- c("emp", "acc", "sep")

# Guard for missing columns. Every check below indexes the manifest by column
# name, and a name that is not there reads as NULL rather than erroring -- so a
# renamed column would slip past all of them and surface later as
# check_manifest() reporting every column in the hive as unlisted, which points
# at OPM rather than at this file. Caught here.
missing_cols <- setdiff(c("variable", hive_flags), names(manifest))
if (length(missing_cols) > 0) {
  stop("metadata/publish/variable_manifest.csv is missing required column(s): ",
       paste(missing_cols, collapse = ", "), ".")
}

rm(missing_cols)

if (any(is.na(manifest$variable)) || any(duplicated(manifest$variable))) {
  stop("metadata/publish/variable_manifest.csv has missing or duplicated ",
       "variable names -- it must have exactly one row per variable.")
}

# Guard for invalid flags
for (col in hive_flags) {
  bad <- setdiff(na.omit(unique(manifest[[col]])), c("keep", "drop"))
  if (length(bad) > 0) {
    stop("column '", col, "' of the variable manifest contains ",
         paste0("'", bad, "'", collapse = ", "),
         " -- only 'keep', 'drop' or an empty cell are allowed.")
  }
}

# col is the for() loop variable, which R leaves bound after the loop ends.
# Clearing the scratch objects leaves only `manifest` and the four functions
# live: vars_marked() and check_manifest() resolve names against this
# environment, so a stray binding could satisfy a typo instead of erroring.
rm(bad, col, hive_flags)

# Variable names for one hive + disposition, in manifest (= published) order.
# vars_marked(flag = "emp", mark = "keep") returns a character vector of
# variable names kept for emp in publication order from the manifest
vars_marked <- function(flag, mark) {
  manifest$variable[which(manifest[[flag]] == mark)]
}

# The manifest must account for every column actually present, in both
# directions. Either failure is silent otherwise: an unlisted column would be
# dropped without anyone deciding to drop it, and a vanished column would be
# published as absent without anyone noticing it went.
# ds is an arrow hive
# flag indexes the data set column in the manifest
# name is the label for the error message
check_manifest <- function(ds, flag, name) {
  keep <- vars_marked(flag, "keep") # vector of variables to keep
  drop <- vars_marked(flag, "drop") # vector of variables to drop
  have <- names(ds) # vector of variables in the hive

  # in the hive but in neither manifest column
  unlisted <- setdiff(have, c(keep, drop))
  # marked keep in the manifest but not in the hive
  gone     <- setdiff(keep, have)

  # Both directions are reported in one stop. Fixing one and re-running to
  # discover the other costs a second pass over every partition footer, which
  # open_dataset() must read to unify the schema.
  problems <- c(
    if (length(unlisted) > 0)
      paste0(length(unlisted), " column(s) present but not listed: ",
             paste(unlisted, collapse = ", "),
             " -- add a row for each marking '", flag, "' as keep or drop, ",
             "with a note saying why. Usually means OPM published a new ",
             "field."),
    if (length(gone) > 0)
      paste0(length(gone), " column(s) marked keep but absent: ",
             paste(gone, collapse = ", "),
             " -- either an upstream step (09-13) did not run, or the field ",
             "was withdrawn upstream.")
  )

  if (length(problems) > 0) {
    stop(name, " does not match metadata/publish/variable_manifest.csv; do ",
         "not publish until resolved --\n  ",
         paste(problems, collapse = "\n  "))
  }
  invisible(ds)
}

# path is a directory path to a hive
# flag indexes data set columns in the manifest
# name is the hive label used in the progress and error messages
finalize <- function(path, flag, name) {

  # open the hive and check the variables in the hive against the manifest
  ds <- open_dataset(path,
                     partitioning  = c("year", "month"),
                     unify_schemas = TRUE) |>
    check_manifest(flag, name)

  # Report columns kept and columns dropped. intersect() so the count reflects
  # drops that actually happen: a manifest drop entry for a column the hive no
  # longer has is inert, and should not be counted as a drop.
  dropped <- intersect(vars_marked(flag, "drop"), names(ds))
  message(sprintf("%s: keeping %d columns, dropping %d",
                  name, length(vars_marked(flag, "keep")), length(dropped)))

  # Drop the schema's metadata so no R attributes are published. A step that
  # converts a data.table to Arrow stores its class and indexes under the
  # metadata key "r", and write_dataset() copies a dataset's schema metadata
  # into every file it writes. ds$schema returns a copy, so clearing the copy's
  # metadata does not change ds; WithSchema() then applies the clean schema.
  clean_schema <- ds$schema
  clean_schema$metadata <- NULL
  ds <- ds$WithSchema(clean_schema)

  # Return a LAZY query over the hive -- not a materialized table -- keeping the
  # variables labelled keep in the manifest, in publication order. Nothing is
  # read until write_dataset() executes it, which is what lets this run at all.
  #
  # all_of(), not any_of(): check_manifest() has already proved every kept
  # column exists, so a permissive select could only hide a regression in that
  # guard. all_of() also emits columns in the order given, which is what makes
  # the manifest's row order the published column order.
  ds |> select(all_of(vars_marked(flag, "keep")))
}

# Stops if any Parquet file under path carries R attributes (metadata key "r").
# finalize() strips them, so a failure here means the strip stopped working.
# The published emp files once carried ~45 MB of stale data.table indexes in
# every footer (~15 GB in all), which this check exists to keep from recurring.
# Reads only each file's footer.
# path is a directory path to a hive
# name is the hive label used in the progress and error messages
check_no_r_attributes <- function(path, name) {
  # List every Parquet file in the hive
  files <- list.files(path, pattern = "\\.parquet$", recursive = TRUE,
                      full.names = TRUE)
  # Initialize an empty vector
  flagged <- character(0)
  # Check each partition for the r metadata in the footer; if r metadata exists
  # add the file path to the flagged vector
  for (f in files) {
    if ("r" %in% names(ParquetFileReader$create(f)$GetSchema()$metadata)) {
      flagged <- c(flagged, f)
    }
  }
  # Stop if any partitions carry the r metadata
  if (length(flagged) > 0) {
    stop(name, ": ", length(flagged), " of ", length(files),
         " file(s) carry R attributes in their metadata; do not publish ",
         "until resolved. First: ", flagged[1])
  }
  message(sprintf("%s: no R attributes in %d file(s)", name, length(files)))
  invisible(NULL)
}


# ============================================================
#### Employment ####
# ============================================================

emp <- finalize("emp_hive/", "emp", "emp_hive")

write_dataset(emp,
              path                   = "emp_hive",
              format                 = "parquet",
              partitioning           = c("year", "month"),
              hive_style             = TRUE,
              compression            = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary         = FALSE)

check_no_r_attributes("emp_hive", "emp_hive")

rm(emp)
gc()


# ============================================================
#### Accessions ####
# ============================================================

acc <- finalize("acc_hive/", "acc", "acc_hive")

write_dataset(acc,
              path                   = "acc_hive",
              format                 = "parquet",
              partitioning           = c("year", "month"),
              hive_style             = TRUE,
              compression            = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary         = FALSE)

check_no_r_attributes("acc_hive", "acc_hive")

rm(acc)
gc()


# ============================================================
#### Separations ####
# ============================================================

sep <- finalize("sep_hive/", "sep", "sep_hive")

write_dataset(sep,
              path                   = "sep_hive",
              format                 = "parquet",
              partitioning           = c("year", "month"),
              hive_style             = TRUE,
              compression            = "zstd",
              existing_data_behavior = "delete_matching",
              use_dictionary         = FALSE)

check_no_r_attributes("sep_hive", "sep_hive")

rm(sep)
gc()

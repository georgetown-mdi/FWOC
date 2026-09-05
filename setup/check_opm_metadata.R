# Author: Mark Richardson and Claude Code
# Purpose: PRE-build gate. Each month the OPM master code tables + FWD Data
#   Dictionary are re-downloaded; the prior versions are archived under
#   metadata/code_tables/prior_downloads/<YYYY_MM>/. This diffs current vs prior
#   (added/removed/changed codes, labels, date ranges) and, if anything changed,
#   writes a report and EXITS NON-ZERO so the rebuild is gated until a human
#   reviews and re-runs with FWOC_METADATA_ACK=1. Run before 00_build_fwoc.R.
#   If no prior metadata exists, exits 0.
#
#   Run from the project root:
#     Rscript setup/check_opm_metadata.R
#     FWOC_METADATA_ACK=1 Rscript setup/check_opm_metadata.R   # after review

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
source("utils/code_table_functions.R")

ct <- "metadata/code_tables/"
prior_root <- paste0(ct, "prior_downloads") # ct ends in "/", so paste0 avoids a doubled slash
latest_prior <- sort(list.dirs(prior_root, recursive = FALSE, full.names = TRUE), decreasing = TRUE)[1]
if (is.na(latest_prior)) {
  message("No prior_downloads/ to diff against -- first run / new repo, nothing to gate.")
  quit(save = "no", status = 0)
}
prior_dir <- paste0(latest_prior, "/")
message("Diffing current code_tables against ", latest_prior)

# Prior-version path, tolerant of a newly-added input with no prior yet -> NULL,
# which check_input() skips. required = FALSE tolerates ONLY absence; an ambiguous
# prior download still errors rather than being skipped as if it had no prior.
prior_file <- function(prefix, ext = "xlsx") {
  find_code_table(prior_dir, prefix, ext, required = FALSE)$file
}

# --- readers ---------------------------------------------------------------
date_from_name <- function(path) ymd(str_extract(basename(path), "\\d{8}"))
# Master xlsx: parse via the pipeline's own reader; blank the open date_to so the
# monthly download-date advance isn't flagged as a change.
read_master <- function(path) {
  dl <- date_from_name(path)
  read_code_table(path, dl) |> mutate(date_to = if_else(date_to == dl, as.Date(NA), date_to))
}
read_flat <- function(path) readr::read_csv(path, col_types = cols(.default = "c"), na = "", show_col_types = FALSE)
read_dict <- function(path) readxl::read_excel(path, col_types = "text")

# --- generic keyed diff ----------------------------------------------------
diff_keyed <- function(cur, prior, key) {
  kc <- function(df) do.call(paste, c(lapply(as.data.frame(df)[, key, drop = FALSE], as.character), sep = "|"))
  ck <- kc(cur)
  pk <- kc(prior)
  added <- cur[!(ck %in% pk), , drop = FALSE]
  removed <- prior[!(pk %in% ck), , drop = FALSE]
  both <- intersect(ck, pk)
  c_i <- match(both, ck)
  p_i <- match(both, pk)
  val_cols <- setdiff(intersect(names(cur), names(prior)), key)
  chg <- rep(FALSE, length(both))
  detail <- rep("", length(both))
  for (vc in val_cols) {
    a <- as.character(cur[[vc]])[c_i]
    b <- as.character(prior[[vc]])[p_i]
    d <- xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
    chg <- chg | d # element-wise OR comparison
    detail[d] <- paste0(ifelse(nzchar(detail[d]), paste0(detail[d], "; "), ""), vc, ": ", b[d], " -> ", a[d])
  }
  schema_note <- if (!identical(sort(names(cur)), sort(names(prior)))) {
    tibble(change = "columns_changed", key = "", detail = paste(setdiff(names(cur), names(prior)), collapse = ","))
  } else {
    NULL
  }
  bind_rows(
    if (nrow(added)) tibble(change = "added", key = kc(added), detail = ""), # Bind rows drops NULL
    if (nrow(removed)) tibble(change = "removed", key = kc(removed), detail = ""),
    if (any(chg)) tibble(change = "changed", key = both[chg], detail = detail[chg]),
    schema_note
  )
}

# --- one input: locate current + prior, diff, label the report rows --------
check_input <- function(name, cur_path, prior_path, reader, key = NULL) {
  if (is.null(prior_path) || !file.exists(prior_path) || !file.exists(cur_path)) {
    message(sprintf("  %-24s skipped (no prior/current)", name))
    return(NULL)
  }
  cur <- reader(cur_path)
  prior <- reader(prior_path)
  if (is.null(key)) key <- names(cur)[1]
  d <- diff_keyed(cur, prior, key)
  message(sprintf("  %-24s %d change(s)", name, nrow(d)))
  if (nrow(d)) mutate(d, input = name) else NULL
}

# --- inputs ----------------------------------------------------------------
masters <- c(
  "agency_subelement", "appointment_type_code", "education_level", "occupation",
  "occupational_category", "pay_plan", "supervisory_status", "work_schedule"
)

rows <- list()
for (m in masters) {
  rows[[m]] <- check_input(m, find_code_table(ct, m)$file, prior_file(m),
    read_master,
    key = c("code", "date_from")
  )
}
# DutyStations.Code is a unique row key; POI Code repeats across subelements, so key on both.
# The current DutyStations is OPM's two-sheet .xlsx; the archived prior is the flat csv
# (OPM's own through 2026-07, the build_labels-derived copy after). read_duty_stations()
# reads either form into the same shape, so the diff does not depend on which arrived.
rows[["DutyStations"]] <- check_input(
  "DutyStations",
  find_code_table(ct, "DutyStations", "xlsx")$file, prior_file("DutyStations", "csv"),
  read_duty_stations
)
rows[["POI"]] <- check_input("POI",
  find_code_table(ct, "POI", "csv")$file, prior_file("POI", "csv"), read_flat,
  key = c("POI Code", "Agency Subelement Code")
)
rows[["FWD Data Dictionary"]] <- check_input(
  "FWD Data Dictionary", "metadata/FWD Data Dictionary.xlsx",
  file.path(latest_prior, "FWD Data Dictionary.xlsx"), read_dict
)
# Seed the column shape: every check_input() returns NULL when nothing changed, and
# bind_rows() over all-NULL gives a 0-column tibble that write_csv emits as an empty
# file. Seeded, an all-clear run still leaves a headed report -- evidence it ran.
# The seed goes INSIDE the list: bind_rows(seed, rows) would read the named list as
# one data frame and recycle its elements as columns.
report <- bind_rows(c(
  list(tibble(change = character(), key = character(), detail = character(), input = character())),
  rows
))

# --- t-vs-t-1 overlap delta ------------------------------------------------
# Surface codes whose windows NEWLY overlap this month -- the conflicts that will hard-fail
# build_labels' guard and need a row in overlap_resolutions.csv. This is the check
# diff_keyed() structurally cannot do: it compares SETS of keys, so a row duplicating
# between downloads reports nothing -- and duplication is how conflicts actually arrive
# (GB and RZ in 2026-07 were variant labels landing on existing windows, not new windows).
#
# Every time-varying master, via the shared label_window_overlaps(), in build_labels' order:
# view first, THEN normalize. normalize_label_text() ends in distinct(), and the 00-collapse
# is what makes two rows identical (HS and HS00 share a label and window in 1970), so
# normalizing first would leave a phantom the build never sees. agency_subelement is checked
# twice -- raw (the 4-char subagency keyspace) and 00-collapsed (the 2-char agency keyspace,
# XX00 -> XX). The second view is not a nicety: most agencies exist ONLY as XX00, so the
# collapse is how they get a label at all, and the few codes carrying BOTH variants are the
# entire agency collision surface.
#
# KNOWN LIMIT -- the over-warn IS the blind spot, not a separate quirk. agency_view copies
# build_labels' filter and str_remove but not its drop-list or the AM/ST special cases, so
# it over-warns on those codes; and because overlap_delta() is setdiff(cur, prior), a code
# that over-warns EVERY month sits in the intersection and is suppressed permanently. The
# gate is therefore silent on exactly the codes whose handling is most intricate. Worst is
# ST, whose 1972 window build_labels EXTENDS to the download date: a new OPM ST window would
# hard-fail the build with no warning here. build_labels' assert_no_label_overlaps() is the
# authoritative backstop -- this gate is advisory only.
overlap_codes_master <- function(mfile, view_fn = identity) {
  if (is.null(mfile) || !file.exists(mfile)) {
    return(character(0))
  }
  # View before normalize: distinct() must run AFTER the collapse (see header).
  m <- read_code_table(mfile, date_from_name(mfile)) |>
    view_fn() |>
    normalize_label_text()
  unique(label_window_overlaps(m)$code)
}

agency_view <- function(m) {
  m |>
    filter(str_length(code) == 2 | str_detect(code, "00$")) |>
    mutate(code = str_remove(code, "00"))
}

overlap_delta <- function(name, view_fn) {
  setdiff(
    overlap_codes_master(find_code_table(ct, name, required = FALSE)$file, view_fn),
    overlap_codes_master(prior_file(name), view_fn)
  )
}

overlap_new <- list()
for (m in masters) {
  codes <- overlap_delta(m, identity)
  if (length(codes)) overlap_new[[m]] <- tibble(input = m, view = m, code = codes)
}
ag <- overlap_delta("agency_subelement", agency_view)
if (length(ag)) {
  overlap_new[["agency_00collapsed"]] <- tibble(
    input = "agency_subelement",
    view = "agency 00-collapsed", code = ag
  )
}
overlap_new <- bind_rows(overlap_new)

if (nrow(overlap_new)) {
  for (v in unique(overlap_new$view)) {
    codes <- overlap_new$code[overlap_new$view == v]
    message(sprintf(
      "  overlap [%-20s] %d NEW code(s): %s", v, length(codes),
      paste(codes, collapse = ", ")
    ))
  }
  report <- bind_rows(report, transmute(overlap_new,
    input,
    change = "new_overlap", key = code,
    detail = paste0(
      "new per-code overlapping window(s) [", view,
      "]; resolve in overlap_resolutions.csv"
    )
  ))
}

# --- write report + gate ---------------------------------------------------
tag <- format(find_code_table(ct, "agency_subelement")$download_date, "%Y_%m")
report_dir <- paste0(ct, "change_reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
report_path <- file.path(report_dir, paste0("changes_", tag, ".csv"))
write_csv(report, report_path, na = "")

if (nrow(report) == 0) {
  message("\nNo OPM metadata changes vs prior download. Safe to rebuild.")
} else {
  message("\n", nrow(report), " metadata change(s) detected. Review: ", report_path)
  if (!nzchar(Sys.getenv("FWOC_METADATA_ACK"))) {
    stop("OPM metadata changed; rebuild gated. Review the report, update the pipeline/codebook ",
      "if needed, then re-run with FWOC_METADATA_ACK=1.",
      call. = FALSE
    )
  }
  message("Acknowledged (FWOC_METADATA_ACK set) -- proceeding.")
}

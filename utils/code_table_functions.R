# Author: Mark Richardson and Claude Code
# Purpose: Locate and parse the OPM master code tables, and detect per-code
#          overlapping label windows. Shared by setup/build_labels.R (builds the
#          label CSVs; build-time overlap guard), setup/check_opm_metadata.R (diffs
#          a new monthly download against the prior), and validation/02_labels.R
#          (post-build overlap backstop), so all three read the tables and detect
#          overlaps identically. Most functions here assume the caller has attached
#          readxl/dplyr/stringr/lubridate; assert_labels_current() is deliberately
#          base-R-only, because 00_build_fwoc.R sources this file with nothing
#          attached. That works only because this file is definitions-only —
#          sourcing never evaluates the package calls. Keep it that way: any
#          top-level code here would break the orchestrator's package-free preamble.

# Find the single code table file matching a prefix and parse its download date
# from the filename (e.g., agency_subelement_20260507.xlsx). Returns
# list(file, download_date), or NULL when nothing matches and required = FALSE --
# for callers where an absent table is legitimate, e.g. the prior version of an
# input OPM only added this month. More than one match ALWAYS errors, whatever
# required is set to: an ambiguous download is a mistake to surface, never to skip.
find_code_table <- function(path, prefix, ext = "xlsx", required = TRUE) {
  pattern <- paste0("^", prefix, "_\\d{8}\\.", ext, "$") # Math prefix plus any 8 digits (i.e., download date)
  files <- list.files(path, pattern = pattern, full.names = FALSE)
  if (length(files) == 0) {
    if (!required) {
      return(NULL)
    }
    stop("No ", prefix, " file found in ", path)
  }
  if (length(files) > 1) {
    stop(
      "Multiple ", prefix, " files found: ",
      paste(files, collapse = ", "),
      ". Remove old downloads or move to prior_downloads/."
    )
  }
  date_str <- str_extract(files, "\\d{8}") # Get the download date
  list(
    file = paste0(path, files),
    download_date = ymd(date_str)
  )
}

# Read both tabs from an Excel code table, bind, and rename to label/
# date_from/date_to. CurrentValues rows get Through_Date = the download date.
read_code_table <- function(file_path, download_date) {
  cur <- read_excel(file_path,
    sheet = "CurrentValues",
    col_types = "text"
  ) |>
    rename_with(~ str_replace_all(., " ", "_")) |>
    mutate(
      From_Date = as.Date(From_Date),
      Through_Date = download_date
    )

  pst <- read_excel(file_path,
    sheet = "PastValues",
    col_types = "text"
  ) |>
    rename_with(~ str_replace_all(., " ", "_")) |>
    mutate(
      From_Date = as.Date(From_Date),
      Through_Date = as.Date(Through_Date)
    )

  bind_rows(cur, pst) |>
    rename(
      code = Code,
      label = Explanation,
      date_from = From_Date,
      date_to = Through_Date
    ) |>
    # Normalize a Windows-1252 round-trip artifact in the OPM source labels to
    # ASCII: a curly apostrophe (U+2019) and an inverted question mark (U+00BF)
    # standing in for the original character. The corruption is positional -- a
    # space-bounded U+00BF is a separator (-> hyphen), a letter-adjacent one is a
    # possessive apostrophe -- so no per-label hardcoding is needed.
    mutate(label = label |>
      str_replace_all(intToUtf8(0x2019), "'") |> # curly apostrophe -> '
      str_replace_all(paste0(" ", intToUtf8(0x00BF), " "), " - ") |> # " U+00BF " separator -> " - "
      str_replace_all(intToUtf8(0x00BF), "'")) |> # remaining U+00BF -> '
    select(code, label, date_from, date_to)
}

# ==========================================================================
# DutyStations: workbook -> flat CSV shape
# --------------------------------------------------------------------------
# From 2026-09 OPM's Duty Stations page downloads only as .xlsx, split into a "US"
# sheet and an "Other" (foreign) sheet, with the state column headed
# "State/Territory". Everything downstream -- 08_bz_duty_station_state.R,
# 13_duty_station_backfill.R, and the archived priors the gate diffs against --
# reads the one-sheet CSV shape OPM served before, so the workbook is stacked back
# into that shape here and setup/build_labels.R writes it out as
# DutyStations_<date>.csv beside the workbook. The reshape is a pure function so
# it is unit-testable without readxl.

# Stack the US and Other sheets and restore the flat-CSV column name. Only the
# two properties the reshape depends on are asserted; an added or dropped OPM
# column is left for the gate's diff to report rather than stopped here.
bind_duty_station_sheets <- function(us, other) {
  if (!identical(names(us), names(other))) {
    stop(
      "DutyStations US and Other sheets have different columns: ",
      paste(names(us), collapse = ", "), " vs ", paste(names(other), collapse = ", ")
    )
  }
  if (!"State/Territory" %in% names(us)) {
    stop("DutyStations sheets have no State/Territory column to rename to State.")
  }
  bind_rows(us, other) |>
    rename(State = `State/Territory`)
}

# Read a DutyStations table in either form: the OPM .xlsx (two sheets, stacked
# above) or the flat .csv (OPM's own download through 2026-07, and the derived
# copy build_labels writes thereafter). One reader lets the gate diff this month's
# workbook against last month's csv. All columns are read as character so the
# zero-padded nine-character Code survives; na = "" per the repo convention.
read_duty_stations <- function(path) {
  if (grepl("\\.xlsx$", path)) {
    sheet <- function(s) read_excel(path, sheet = s, col_types = "text")
    bind_duty_station_sheets(sheet("US"), sheet("Other"))
  } else {
    read_csv(path, col_types = cols(.default = "c"), na = "")
  }
}

# ==========================================================================
# Time-varying label integrity: no per-code overlapping date windows
# --------------------------------------------------------------------------
# A code with overlapping [date_from, date_to] windows matches >1 label row in
# the time-varying join, duplicating (inflating) hive rows. These helpers are the
# single shared implementation used by setup/build_labels.R (the generator's
# hard-fail guard), setup/check_opm_metadata.R (the t-vs-t-1 delta), and
# validation/02_labels.R (post-build backstop) -- one definition so they cannot
# drift. Callers must have dplyr + stringr attached.

# Rows whose window opens on/before an earlier window of the same code closes.
# Returns the later row of each overlapping pair (the one that "reopens" a span).
# The empty-case return carries a `code` column so callers can `distinct(code)`.
label_window_overlaps <- function(d) {
  if (!all(c("code", "date_from", "date_to") %in% names(d))) {
    return(tibble::tibble(code = character()))
  }
  # Parse strictly: a non-missing value that fails YYYY-MM-DD parsing is an error,
  # not a silently open-ended window (which coalesce(NA -> integer.max) would make
  # it). Genuine NA (empty date_to) stays open. Accepts Date or character input.
  as_date_strict <- function(x, col) {
    parsed <- if (inherits(x, "Date")) {
      x
    } else {
      suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
    }
    bad <- !is.na(x) & is.na(parsed)
    if (any(bad)) {
      stop("label_window_overlaps: ", sum(bad), " unparseable ", col, " value(s), e.g. '",
        as.character(x)[which(bad)[1]], "' for code ", d$code[which(bad)[1]],
        " (expected YYYY-MM-DD).",
        call. = FALSE
      )
    }
    parsed
  }
  from_i <- as.integer(as_date_strict(d$date_from, "date_from"))
  to_i <- coalesce(as.integer(as_date_strict(d$date_to, "date_to")), .Machine$integer.max)
  d |>
    mutate(.from = from_i, .to = to_i) |>
    filter(!is.na(.from)) |>
    arrange(code, .from) |>
    group_by(code) |>
    mutate(.prior_max_to = lag(cummax(.to))) |> # latest end among earlier windows
    ungroup() |>
    filter(!is.na(.prior_max_to) & .from <= .prior_max_to) |>
    select(-.from, -.to, -.prior_max_to)
}

# Collapse only PROVABLY-cosmetic OPM label variants that would otherwise create
# duplicate windows: internal whitespace runs, the "...NOTE: ORGANIZATION DOES NOT
# MAKE HR SUBMISSIONS..." documentation boilerplate, and a lone trailing period,
# then drop rows that have become exact duplicates. Conservative by design:
# anything it does not collapse (typos, renames, genuine name conflicts) is left
# for assert_no_label_overlaps() to hard-fail so a human resolves it. The boilerplate
# strip anchors on the distinctive verb phrase (ORGANIZATION ... DOES/HAS NOT MADE,
# HAS NOT MADE ... SUBMISSION, CODE MAY BE USED FOR OTHER DOCUMENTATION), not the
# bare noun "ORGANIZATION", so a real name like "U.S. ORGANIZATION FOR ..." is not
# truncated (verified: strips all 85 boilerplate labels, 0 real-name false hits).
normalize_label_text <- function(d) {
  d |>
    mutate(
      label = str_squish(label),
      label = str_remove(
        label,
        paste0(
          "\\.\\s+(NOTE:\\s*)?((THE\\s+)?ORGANIZATION\\b.*?",
          "(DOES NOT MAKE|HAS NOT MADE|WHOSE NAME)|",
          "HAS NOT MADE\\b.*?SUBMISSION|",
          "CODE MAY BE USED FOR OTHER DOCUMENTATION).*$"
        )
      ),
      label = str_squish(label),
      label = str_remove(label, "\\.$")
    ) |>
    distinct()
}

# Apply human-curated resolutions from overlap_resolutions.csv. For each
# (lookup, code, date_from, date_to) listed, keep only the row whose label ==
# keep_label and drop the other overlapping row(s) for that exact window; rows
# in windows not listed are untouched. Errors if a resolution matches no row
# (stale keep_label) so a typo in the resolution table surfaces loudly.
apply_overlap_resolutions <- function(d, lookup_name, resolutions) {

  # If this lookup has no rows in metadata/code_tables/overlap_resolutions.csv,
  # return the lookup table

  r <- resolutions |> filter(lookup == lookup_name)
  if (!nrow(r)) {
    return(d)
  }
  r <- r |> mutate(date_from = as.Date(date_from), date_to = as.Date(date_to))
  # Compare keep_label to the surviving label on a cosmetically-normalized form
  # (squish + drop a lone trailing period), so an OPM whitespace/trailing-period
  # tweak does not spuriously trip the miss check; a genuine wording change still
  # fails loudly (which is the point -- a human re-confirms which label to keep).

  canon <- function(s) str_remove(str_squish(s), "\\.$")

  # Confirm every code-label pair per window in resolution list has a valid label
  # to ensure no missing labels are created

  keyed <- d |> inner_join(r,
    by = c("code", "date_from", "date_to"),
    suffix = c("", ".resolution")
  ) # Get rows in d that match a code and date window in resolution list
  matched <- keyed |>
    filter(canon(label) == canon(keep_label)) |>
    distinct(code, date_from, date_to)
  wanted <- r |> distinct(code, date_from, date_to)
  miss <- anti_join(wanted, matched, by = c("code", "date_from", "date_to"))
  if (nrow(miss)) {
    stop(lookup_name, ": overlap_resolutions.csv keep_label matches no row for ",
      nrow(miss), " window(s), e.g. code ", miss$code[1], " ",
      miss$date_from[1], "..", miss$date_to[1], " -- fix keep_label.",
      call. = FALSE
    )
  }

  # Once check passed, drop every row that matches code and date-window in r except keep label

  d |>
    left_join(r |> transmute(code, date_from, date_to, .keeper = keep_label),
      by = c("code", "date_from", "date_to")
    ) |>
    filter(is.na(.keeper) | canon(label) == canon(.keeper)) |>
    select(-.keeper)
}

# Build-time hard-fail guard: stop() with a full listing if any code in `d` has
# overlapping windows. Called by the generator after normalization + resolutions.
assert_no_label_overlaps <- function(d, name) {
  bad <- unique(label_window_overlaps(d)$code)
  if (length(bad)) {
    offending <- d |>
      filter(code %in% bad) |>
      arrange(code, date_from)
    cat("\n*** OVERLAPPING LABEL WINDOWS in ", name, ": ",
      length(bad), " code(s) ***\n",
      sep = ""
    )
    print(as.data.frame(offending), row.names = FALSE)
    stop(
      name, ": ", length(bad), " code(s) have overlapping date windows, which ",
      "would inflate hive rows in the time-varying join. Resolve each in ",
      "metadata/code_tables/overlap_resolutions.csv ",
      "(lookup, code, date_from, date_to, keep_label), then re-run."
    )
  }
  invisible(d)
}

# ==========================================================================
# Label freshness: committed labels must not lag a code_tables refresh
# --------------------------------------------------------------------------
# Label building (setup/build_labels.R) is a MANUAL gated step, NOT part of
# 00_build_fwoc.R, so the committed metadata/labels/ can silently lag a newer
# code_tables download. build_labels stamps the download date it consumed per
# master; this fails the build loudly if any master's code_tables file is newer
# than its stamp -- so regenerated labels can never be quietly forgotten.
#
# Base R only, and paths are arguments: 00_build_fwoc.R calls this before step 01
# with no packages attached (see the file header), and the arguments make it
# testable against a tempdir fixture.
#
# Both the .xlsx masters and the flat .csv tables (POI, DutyStations) in the stamp
# are checked -- the file match below accepts either extension.
assert_labels_current <- function(stamp_path = "metadata/labels/labels_build_stamp.csv",
                                  ct_dir = "metadata/code_tables") {
  if (!file.exists(stamp_path)) {
    stop(
      "Missing ", stamp_path, " -- run setup/build_labels.R and commit ",
      "metadata/labels/ before building."
    )
  }
  stamp <- read.csv(stamp_path, colClasses = "character")
  stale <- character(0)
  for (i in seq_len(nrow(stamp))) {
    tb <- stamp$table[i]
    f <- list.files(ct_dir, pattern = paste0("^", tb, "_[0-9]{8}\\.(xlsx|csv)$"))
    if (!length(f)) next # a missing master surfaces later in the build
    cur_date <- max(as.Date(sub(".*_([0-9]{8})\\.(xlsx|csv)$", "\\1", f), "%Y%m%d"))
    stamp_date <- as.Date(stamp$download_date[i])
    if (is.na(stamp_date) || cur_date > stamp_date) stale <- c(stale, tb)
  }
  if (length(stale)) {
    stop(
      "Stale labels: code_tables newer than metadata/labels/ for ",
      paste(stale, collapse = ", "), ". Re-run setup/build_labels.R and commit ",
      "metadata/labels/ before building."
    )
  }
  invisible(TRUE)
}

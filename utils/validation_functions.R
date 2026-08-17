# Author: Mark Richardson and Claude Code
# Purpose: Shared helpers for the validation suite (validation/). A driver
#          sources this, records outcomes with val_check(), and ends with
#          val_finish(); a failed hard check makes val_finish() error so the
#          Rscript exits non-zero and the parent run_step() halts the suite.
#          Pure functions (namespaced calls), sourced into scripts that have
#          already attached arrow/dplyr -- mirrors utils/inspect_vars.R.

# Per-process accumulator (one driver = one Rscript). Re-sourcing resets it.
.val_env <- new.env(parent = emptyenv())
.val_env$results <- list()

# Record one check. severity = "hard" failures halt the suite; "soft" only log.
val_check <- function(name, condition, detail = "", severity = c("hard", "soft")) {

  # Validate severity against the candidates, and -- when the caller omits it --
  # collapse the default c("hard", "soft") to its first element. The assignment
  # matters: left as a length-2 vector, the tibble below would recycle and record
  # two rows for one check.
  severity <- match.arg(severity)

  pass <- isTRUE(condition)

  .val_env$results[[length(.val_env$results) + 1L]] <- tibble::tibble(
    check = name, pass = pass, severity = severity, detail = detail)
  cat(sprintf("[%s] %-4s %s%s\n",
              if (pass) "PASS" else "FAIL", severity, name,
              if (nzchar(detail)) paste0("  -- ", detail) else ""))
  invisible(pass)
}

# Write the driver's results and, on any hard failure, stop() -- which exits the
# Rscript non-zero (run_step halts) and errors clearly under an interactive source().
val_finish <- function(out_csv, fail_on_hard = TRUE) {
  res <- dplyr::bind_rows(.val_env$results)
  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(res, out_csv, na = "")
  message("wrote ", out_csv)

  fails <- res[!res$pass & res$severity == "hard", , drop = FALSE]
  if (nrow(fails) > 0 && fail_on_hard) {
    stop(nrow(fails), " hard check(s) failed: ",
         paste(fails$check, collapse = ", "), call. = FALSE)
  }
  invisible(res)
}

# Read a known-exception allowlist (all character, na=""); missing file = empty.
# An allowlist SUPPRESSES a check, so a malformed one halts rather than warns: an
# unquoted comma in a hand-written `reason` silently truncates the row, taking the
# recorded justification with it. trim_ws stays at its default here -- these files
# are hand-authored, so an incidental space after a comma should be forgiven.
load_allowlist <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  d <- readr::read_csv(path, col_types = readr::cols(.default = "c"), na = "",
                       show_col_types = FALSE)
  p <- readr::problems(d) # Get parsing problems
  if (nrow(p) > 0) {
    stop("malformed allowlist ", path, ": ", nrow(p), " parsing problem(s); first at row ",
         p$row[1], " (expected ", p$expected[1], ", got ", p$actual[1], ")", call. = FALSE)
  }
  d
}

# Published column -> Arrow type string for an open_dataset(), in the same
# spelling the codebook `Type` column uses (string, int8, int32, date32[day], ...).
arrow_schema_types <- function(ds) {
  flds <- ds$schema$fields
  tibble::tibble(column     = vapply(flds, function(f) f$name, character(1)),
                 arrow_type = vapply(flds, function(f) f$type$ToString(), character(1)))
}

# Seed a baseline CSV only if absent, so monthly reruns never clobber it.
write_baseline_if_absent <- function(df, path) {
  if (file.exists(path)) {
    message("baseline present: ", path)
    return(invisible(FALSE))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = "")
  message("seeded baseline: ", path)
  invisible(TRUE)
}

# Compare current counts to a stored baseline on `key`; flag |delta| > tol.
# Returns NULL when the baseline file is absent (caller seeds it instead).
compare_to_baseline <- function(current, baseline_path, key, value_var, tol = 0) {
  # A genuine empty string cannot survive the round trip: write_csv(na = "") renders
  # both NA and "" as an empty field, so "" comes back as NA and never rejoins its
  # current-side row -- a permanent changed cell in both directions. Unfixable on read
  # (the distinction is destroyed on write), so refuse input the baseline cannot
  # represent. Checked before the early return, so a first run refuses to SEED a
  # baseline that could never match itself.
  blank <- vapply(key, function(k) {
    v <- current[[k]]
    is.character(v) && any(!is.na(v) & v == "")
  }, logical(1))
  if (any(blank)) {
    stop("compare_to_baseline: key column(s) ", paste(key[blank], collapse = ", "),
         " contain empty strings, which the baseline CSV cannot distinguish from NA.",
         call. = FALSE)
  }
  if (!file.exists(baseline_path)) return(NULL)
  # trim_ws = FALSE: this file is machine-written and must round-trip exactly. A value
  # that is literally whitespace (FWD education_level_code is a single space over part
  # of the emp series) would otherwise be trimmed to "" and then turned into NA by
  # na = "", so it could never rejoin its current-side counterpart -- every run would
  # report it as a changed cell, in both directions, forever.
  base <- readr::read_csv(baseline_path, col_types = readr::cols(.default = "c"),
                          na = "", trim_ws = FALSE, show_col_types = FALSE)
  mk <- function(df, vcol) {
    # Subset df to columns in key and retain data.frame type
    out <- as.data.frame(df)[, key, drop = FALSE]
    # Convert all columns to character
    out[] <- lapply(out, as.character)
    # Add the value column as numeric, under the caller's name (val_now / val_base) so
    # the two sides do not collide in the join. Read from df, not out -- value_var is
    # not in key, so it was dropped above.
    out[[vcol]] <- suppressWarnings(as.numeric(df[[value_var]]))
    # Return the transformed data.frame
    out
  }
  # Join the current and baseline versions after transformation via mk(), on the
  # variables in key. FULL join: a cell present on only one side must survive, since an
  # appeared or vanished cell is itself a revision. Those are the rows that come back
  # NA, and the coalesce below reads that side as 0 so delta carries a sign -- current
  # minus seeded baseline, positive when FWD went up. changed then flags any delta
  # beyond tol (0 by default, so any movement at all).
  dplyr::full_join(mk(current, "val_now"), mk(base, "val_base"), by = key) |>
    dplyr::mutate(delta   = dplyr::coalesce(val_now, 0) - dplyr::coalesce(val_base, 0),
                  changed = abs(delta) > tol)
}

# Scan data.opm.gov source files -> current max version per (dataset, year, month).
# Files are <dataset>_<YYYYMM>_<version>.parquet; OPM retains superseded versions,
# so max(version) per period is the current state and a bump = OPM updated it.
# Reads the files themselves, NOT metadata/fwd/file_manifest.csv: the manifest is
# what 04 selects from, so validating against it would compare the build to its own
# provenance record. This scan is the independent observation -- do not consolidate.
scan_fwd_versions <- function(root = "data_opm_gov") {
  dirs <- c(emp = "employment", acc = "accessions", sep = "separations")
  pat  <- "^[a-z]+_([0-9]{4})([0-9]{2})_([0-9]+)\\.parquet$"
  parts <- lapply(names(dirs), function(ds) {

    # List files for one dataset directory
    f <- list.files(file.path(root, dirs[[ds]]), pattern = "\\.parquet$")

    # Keep only the names the pattern can parse. The sub() calls below return their input
    # UNCHANGED on a non-match rather than erroring, so an unparseable name would carry
    # through as an NA year/month/version row and poison the max() for its period. Warn
    # rather than drop in silence: the file this skips would be a real OPM download under an
    # unexpected name, and losing it here reads downstream as "period not updated" -- 05
    # would treat a period that did change as frozen, which is the direction that hides a
    # problem rather than reporting one.
    ok <- grepl(pat, f)

    # Warn if any file name is not ok
    if (any(!ok)) {
      warning("scan_fwd_versions: skipping ", sum(!ok), " unparseable file(s) in ",
              dirs[[ds]], ": ", paste(f[!ok], collapse = ", "), call. = FALSE)
    }

    # Subset f to properly formatted file names
    f <- f[ok]

    # No parseable files for this dataset -- return NULL, which bind_rows() drops.
    if (!length(f)) return(NULL)

    # Otherwise one row per parseable file in this dataset's directory. as.integer() is
    # load-bearing for the max() below: as strings, "9" sorts above "10".
    tibble::tibble(dataset = ds,
                   year    = as.integer(sub(pat, "\\1", f)),
                   month   = as.integer(sub(pat, "\\2", f)),
                   version = as.integer(sub(pat, "\\3", f)))
  })

  # Get the max version for each dataset-year-month
  dplyr::bind_rows(parts) |>
    dplyr::group_by(dataset, year, month) |>
    dplyr::summarise(version = max(version), .groups = "drop")
}

# Periods updated since the stored baseline (version increased or new period).
# Returns NULL if no baseline exists yet (first run). Does not write the baseline;
# the caller rolls it forward.
fwd_updated_periods <- function(current, baseline_path) {

  if (!file.exists(baseline_path)) return(NULL)

  # Prior run's max version per period -- the validation baseline, not 03's file_manifest.csv.
  prior <- readr::read_csv(baseline_path, na = "", show_col_types = FALSE,
                           col_types = readr::cols(dataset = "c", year = "i", month = "i", version = "i"))

  # Periods with no prior version (new) or whose max version increased. A decrease is not
  # flagged -- it fails this predicate and drops out; version_selection_fwd in 05 catches it.
  # LEFT join on `current`, so a period in the baseline but no longer on disk drops out
  # rather than being flagged -- deliberately: version_selection_fwd in 05 full-joins this
  # same scan against file_manifest.csv and fails hard on exactly that case.
  dplyr::left_join(current, prior, by = c("dataset", "year", "month"), suffix = c("", "_prior")) |>
    dplyr::filter(is.na(version_prior) | version > version_prior) |>
    dplyr::select(dataset, year, month)
}

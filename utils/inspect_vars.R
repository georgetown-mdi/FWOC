# Author: Mark Richardson and Claude Code

#### Function to inspect variables ####

inspectVar <- function(data, var, count = FALSE) {
  # Note: all_of() selects all variables in the vector

  if (count) {
    data |>
      count(across(all_of(var)), name = "n") |>
      arrange(desc(n)) |>
      collect()
  } else {
    data |>
      select(all_of(var)) |>
      distinct() |>
      collect()
  }
}

coverageByYearMonth <- function(data, var) {
  # Splice the column reference in before Arrow sees the expression;
  # `.data[[var]]` does not reliably resolve in arrow's dplyr binding
  # inside summarize()/mutate().
  #
  # `var` may be a single column name or a vector.
  # Single column → columns: year, month, n_total, n_na, n_not_na, all_na
  # Multiple cols → wide-format columns: year, month, n_total,
  #                  n_na_<v1>, ..., all_na_<v1>, ... (single arrow scan)

  if (length(var) == 1) {
    var_sym <- rlang::sym(var)
    return(
      data |>
        group_by(year, month) |>
        summarize(
          n_total = n(),
          n_na    = sum(is.na(!!var_sym)),
          .groups = "drop"
        ) |>
        mutate(
          n_not_na = n_total - n_na,
          all_na = if_else(n_total == n_na, 1L, 0L)
        ) |>
        arrange(year, month) |>
        collect()
    )
  }

  var_syms <- lapply(var, rlang::sym)

  # One `sum(is.na(<v>))` expression per variable -- the multi-var equivalent of
  # the single-var branch's n_na, named n_na_<v> so the columns stay distinct.
  na_exprs <- setNames(
    lapply(var_syms, function(s) rlang::expr(sum(is.na(!!s)))),
    paste0("n_na_", var)
  )

  all_na_exprs <- setNames(
    lapply(var, function(v) {
      n_na_sym <- rlang::sym(paste0("n_na_", v))
      rlang::expr(if_else(n_total == !!n_na_sym, 1L, 0L))
    }),
    paste0("all_na_", var)
  )

  data |>
    group_by(year, month) |>
    # !!!na_exprs is unpacked to create sum(is.na()) for each v in var
    summarize(n_total = n(), !!!na_exprs, .groups = "drop") |>
    # !!!all_na_exprs is unpacked to create if_else(n_total == n_na_<v>) for each v in var
    mutate(!!!all_na_exprs) |>
    arrange(year, month) |>
    collect()
}

#### Codebook coverage decomposition (used by codebook/02_codebook_build.R) ####
# These turn a coverageByYearMonth() table into the codebook's structured
# coverage fields. They live here rather than inside 02 so that 01 and 02 read as
# one workflow over a shared toolkit; 02 is currently the only caller.

fmt_ym <- function(y, m) sprintf("%d-%02d", y, m)

# Collapse a coverage slice's all-NA year-months into a compact run string
# ("YYYY-MM" singletons, "YYYY-MM to YYYY-MM" runs), comma-separated.
collapseNARuns <- function(cv_rows) {
  if (nrow(cv_rows) == 0) { # No observed year-months, return empty string
    return("")
  }
  na_mask <- cv_rows$v_all_na == 1
  # if no structurally missing year-months, return empty string
  if (!any(na_mask)) {
    return("")
  }
  runs <- rle(na_mask) # Get lengths and values of TRUE/FALSE runs
  pos <- 0
  gap_strings <- c()
  for (i in seq_along(runs$lengths)) { # loop over runs
    start_idx <- pos + 1 # starting index for this iteration's run
    end_idx <- pos + runs$lengths[i] # ending index for this iteration's run

    # if run value is TRUE, meaning run of structurally missing year-months
    if (runs$values[i]) {
      s_ym <- cv_rows$ym[start_idx] # get starting ym
      e_ym <- cv_rows$ym[end_idx] # get ending ym

      # If run is one period
      if (start_idx == end_idx) {
        gap_strings <- c(gap_strings, s_ym) # record the starting ym
      } else {
        # record the starting and ending ym
        gap_strings <- c(gap_strings, paste0(s_ym, " to ", e_ym))
      }
    }
    pos <- pos + runs$lengths[i] # set pos to end of this iteration's run
  }
  # Return the results as a single string separated by commas
  paste(gap_strings, collapse = ", ")
}

# Decompose a variable's year-month coverage into the structured codebook
# fields: era / date_from / date_to / gaps_flag / gap_detail / missing.
# All are computed on the *data window* (the months where the variable is not
# entirely NA), not the file's full span. `cv` is a coverageByYearMonth() table;
# `var_name` selects the n_na_<var>/all_na_<var> columns of a multi-var table
# (NULL for a single-var table's bare n_na/all_na columns).

analyzeCoverage <- function(cv, var_name = NULL) {

  # Normalize both table shapes onto v_na / v_all_na
  if (is.null(var_name)) {
    cv$v_na <- cv$n_na
    cv$v_all_na <- cv$all_na
  } else {
    cv$v_na <- cv[[paste0("n_na_", var_name)]]
    cv$v_all_na <- cv[[paste0("all_na_", var_name)]]
  }

  cv <- cv |>
    arrange(year, month) |>
    mutate(ym = fmt_ym(year, month))

  # Logical vector TRUE if variable is structurally NA (i.e., all NA) in a
  # year-month
  na_mask <- cv$v_all_na == 1

  # indices of year-months where variable is not structurally NA
  data_idx <- which(!na_mask)

  # Entirely-NA variable (should not happen for a real column): no window.
  if (length(data_idx) == 0) {
    return(list(
      era = NA_character_, date_from = NA_character_,
      date_to = NA_character_, gaps_flag = "No",
      gap_detail = "", missing = "No"
    ))
  }

  first_data_idx <- data_idx[1] # index of first observed year-month
  last_data_idx <- data_idx[length(data_idx)] # index of last observed year-month
  date_from <- cv$ym[first_data_idx] # Get first ym
  date_to <- cv$ym[last_data_idx] # Get last ym

  # Era: which OPM data eras the populated window touches. BZ = months on or
  # before 2004; FWD = months in 2005 or later.
  bz <- any(cv$year[data_idx] <= 2004)
  fwd <- any(cv$year[data_idx] >= 2005)
  era <- if (bz && fwd) "ALL" else if (bz) "BZ" else "FWD"

  # Interior Coverage Gaps: all-NA periods strictly INSIDE [date_from, date_to].
  # Leading/trailing all-NA (era boundaries) are excluded -- they are already
  # captured by Era + Date From/To.
  interior <- cv[first_data_idx:last_data_idx, ]
  # Get text listing structurally missing runs
  gap_detail <- collapseNARuns(interior)
  gaps_flag <- if (nchar(gap_detail) > 0) "Yes" else "No"

  # Missing Values: record-level partial NA within populated periods (months
  # that are entirely NA are excluded from this calculation).
  has_missing <- any(cv$v_na[data_idx] > 0)

  list(
    era = era, date_from = date_from, date_to = date_to,
    gaps_flag = gaps_flag, gap_detail = gap_detail,
    missing = if (has_missing) "Yes" else "No"
  )
}

#### Function to build an NA-treatment inventory CSV ####

buildInventory <- function(data, code_col, label_col = NULL, var_name = NULL,
                           date_col = "date",
                           first_of_month = FALSE,
                           out_dir = "metadata/na_treatment/inventory") {
  # Build a (code, label, date) inventory with row counts. Writes
  # <out_dir>/inventory_<var_name>.csv and returns the tibble.
  #
  # `code_col` and `label_col` may each be a single column name or a vector.
  # For variable groups (e.g., duty_station with 10+ cascading columns), pass
  # all the columns to inventory together and supply `var_name` for the file.
  #
  # `label_col` may be NULL for code-only variables (e.g., POI codes with no
  # label column).
  #
  # Grain is (code, label, date) so the downstream label-code check
  # (checkLabelGaps) can test each NA-label observation date precisely
  # against the OPM master's [date_from, date_to] windows. Aggregating to
  # (code, label) with min/max date produces false `unexpected` flags when
  # NA-label observations cluster at the ends of a valid window with a gap
  # in the middle (NH was an example).
  #
  # Date convention must match the dataset's label-join convention:
  #   * `first_of_month = FALSE` (default, for emp): uses `date_col`
  #     directly, which in emp_hive is end-of-period (quarter end for BZ,
  #     last-of-month for FWD). Matches the script 09/11 emp label joins.
  #   * `first_of_month = TRUE` (for acc/sep): derives
  #     make_date(year, month, 1L), matching the join_date convention
  #     scripts 09/11 use for acc/sep label joins.
  # See codebook/codebook_notes.md "Date conventions across hives".

  if (is.null(var_name)) {
    if (length(code_col) > 1) {
      stop("var_name must be supplied when code_col has multiple columns")
    }
    var_name <- code_col
  }
  if (length(var_name) != 1) {
    stop(
      "var_name must be a single string (used as the output file suffix), got length ",
      length(var_name)
    )
  }

  group_cols <- c(code_col, label_col) # c() drops NULL automatically
  # Turn strings into symbols that data-masking verbs can resolve
  group_syms <- lapply(group_cols, rlang::sym)

  # Set date to first of month for acc/sep or as-of date for emp
  if (first_of_month) {
    data <- data |> mutate(.bi_date = make_date(year, month, 1L))
  } else if (!is.null(date_col) && date_col %in% names(data)) {
    data <- data |> mutate(.bi_date = !!rlang::sym(date_col))
  } else {
    stop("Either set first_of_month = TRUE or supply a date_col that exists in `data`.")
  }

  # Generate a count of code-label pairs by date. Sort by frequency for reading, then
  # by the grouping columns to break ties: Arrow does not guarantee the order rows come
  # out of summarize(), and n_rows alone is nowhere near unique (most inventories are
  # >90% ties, with thousands of rows at n_rows = 1), so desc(n_rows) on its own leaves
  # the file in an arbitrary order that changes between runs on identical data. The
  # grouping columns plus the date are unique by construction, so this is a total order
  # and the written CSV is byte-reproducible -- which is what makes two runs diffable
  # after a monthly refresh. arrange() sorts in the C locale, so the order does not
  # depend on the machine.
  out <- data |>
    group_by(!!!group_syms, .bi_date) |>
    summarize(n_rows = n(), .groups = "drop") |>
    rename(date = .bi_date) |>
    arrange(desc(n_rows), !!!group_syms, date) |>
    collect()

  # Write the inventory to disk
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0("inventory_", var_name, ".csv"))
  readr::write_csv(out, out_path, na = "")
  message("wrote ", out_path)

  out
}

#### Function to write inspection outputs for codebook population ####

writeInspection <- function(cv, data, var, dataset,
                            out_dir = "metadata/codebook/inspection",
                            top_n = Inf) {
  # For codebook population (Date Coverage, Missing Values, Sentinel Codes).
  # Takes the already-computed coverage tibble (`cv`) and the dataset
  # (`data`) so it can derive per-variable distinct-value counts; does
  # NOT re-run coverageByYearMonth.  Keeps `cv` objects in the
  # interactive session intact.
  #
  # Writes:
  #   coverage_<dataset>_<joint_tag>.csv -- the supplied cv tibble
  #   values_<dataset>_<v>.csv           -- one per individual variable in
  #                                         `var`, distinct values with row
  #                                         counts (arranged desc by n)
  # The per-variable values file is the right place to spot sentinel codes
  # ("REDACTED", "#########", "NAME WITHHELD BY AGENCY", etc.).
  #
  # `top_n` caps the values file (default Inf = all distinct values).
  # Use a finite top_n for high-cardinality variables (pseudo_id, name)
  # where the full distinct table would be tens of millions of rows.

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  tag <- paste(var, collapse = "_") # Tag for the cv file name
  cv_path <- file.path(out_dir, paste0("coverage_", dataset, "_", tag, ".csv"))
  readr::write_csv(cv, cv_path, na = "")
  message("wrote ", cv_path)

  for (v in var) {
    v_sym <- rlang::sym(v)
    iv_q <- data |>
      count(!!v_sym, name = "n") |>
      arrange(desc(n))
    if (is.finite(top_n)) iv_q <- iv_q |> head(top_n)
    iv <- iv_q |> collect()

    iv_path <- file.path(out_dir, paste0("values_", dataset, "_", v, ".csv"))
    readr::write_csv(iv, iv_path, na = "")
    message("wrote ", iv_path)
  }

  invisible(cv)
}


#### Sentinel-code flagger ####

# Canonical sentinel lexicon (case-insensitive via the inline (?i) flag). Matches
# the string / punctuation placeholder forms OPM uses for redacted / missing /
# invalid / untranslatable values across the BZ and FWD eras:
#   *  ** *** *********   asterisk runs  (FWD suppression marker; BZ redaction variant)
#   ## ### #########      hash runs      (BZ redaction)
#   REDACTED, NO DATA REPORTED, INVALID, NAME WITHHELD BY AGENCY/OPM, NAME UNKNOWN,
#   TRANSLATION UNKNOWN, UNSPECIFIED, UNKNOWN, UNIDENTIFIED, NOT REPORTED/AVAILABLE/SPECIFIED
# Pure-numeric placeholders (0000 / 9999 / 00 / 99) are intentionally OUT of scope:
# they overlap legitimate indicator (0/1) and continuous values, so they are
# adjudicated case-by-case against the OPM lookup tables instead.
SENTINEL_LEXICON <- paste0("(?i)(", paste(c(
  "^\\*+$", "^#+$",
  "REDACTED", "NO DATA", "\\bINVALID\\b", "WITHHELD",
  "TRANSLATION UNKNOWN", "\\bUNSPECIFIED\\b", "^UNKNOWN$", "^UNK$",
  "NOT REPORTED", "NOT AVAILABLE", "NOT SPECIFIED",
  "\\bUNIDENTIFIED\\b", "NAME UNKNOWN"
), collapse = "|"), ")")

# Split a `Sentinel Codes` cell into its bare documented values, dropping the
# human "(invalid)" / "(from #########)" annotations so a literal value like
# "*" or "##" is matched exactly rather than as a loose substring.
sentinelTokens <- function(cell) {
  # if the cell is NA or an empty string, return a 0-length character vector
  if (is.na(cell) || cell == "") {
    return(character(0))
  }
  # Split the sentinel codes by commas and use [[1]] to extract the character
  # vector from the list returned by strsplit().
  toks <- trimws(strsplit(cell, ",", fixed = TRUE)[[1]])
  # Strip trailing parenthetical annotation from the token
  sub("\\s*\\(.*\\)$", "", toks)
}

flagSentinels <- function(cb_path = "metadata/codebook/fwoc_codebook_table.csv",
                          insp_dir = "metadata/codebook/inspection",
                          lexicon = SENTINEL_LEXICON,
                          warn = TRUE) {
  # Build-time self-check against silent sentinel drift. Scans every per-variable
  # values_<dataset>_<var>.csv dump written by writeInspection() for values that
  # match the sentinel lexicon, then reconciles them against the codebook's
  # `Sentinel Codes` column. Returns -- and by default warns about -- sentinel
  # values present in the FWOC data but not yet documented, so the manually
  # curated column cannot fall behind a new OPM release.
  #
  # Reads the frequency-ranked dumps, so high-cardinality variables capped via
  # top_n (pseudo_id, name, pay, CPI, dates, ...) are still covered: real
  # sentinels are high-frequency bulk markers that sit at the top of each dump,
  # well inside the cap. Numeric placeholder codes (0000 / 9999) are excluded by
  # the lexicon and adjudicated separately.

  # Read the codebook.
  cb <- readr::read_csv(cb_path, col_types = readr::cols(.default = "c"), na = "")

  # Get a named list -- one element per codebook row, named for its variable,
  # each element a character vector of that variable's sentinel tokens
  documented <- stats::setNames(lapply(cb$`Sentinel Codes`, sentinelTokens), cb$Variable)

  # Get the file paths for value dumps
  files <- list.files(insp_dir, pattern = "^values_(emp|acc|sep)_.*\\.csv$", full.names = TRUE)

  hits <- list()

  for (f in files) {
    # Extract the dataset and variable name from the file path
    base <- sub("\\.csv$", "", sub("^values_", "", basename(f)))

    # Extract the dataset from base
    dataset <- sub("_.*$", "", base)

    # Extract the variable name from base
    variable <- sub("^(emp|acc|sep)_", "", base)

    # Read the value dump
    d <- readr::read_csv(f,
      col_types = readr::cols(.default = "c"), na = "",
      show_col_types = FALSE
    )

    names(d)[1:2] <- c("value", "n") # Standardize the column names in the dump

    d$value <- iconv(d$value, "UTF-8", "UTF-8", sub = "") # drop stray non-UTF-8 bytes

    # Get logical vector that is TRUE if the value is not NA, not an empty string,
    # and is a sentinel value in the recorded lexicon
    keep <- !is.na(d$value) & d$value != "" & grepl(lexicon, d$value, perl = TRUE)

    if (!any(keep)) next # if no sentinels, move to next file

    have <- documented[[variable]] # get the sentinel tokens

    # if have is null, convert it to an empty character vector
    if (is.null(have)) have <- character(0)

    # Keep the rows whose value matched the lexicon but is not documented for
    # this variable -- the undocumented sentinels this function exists to report
    m <- d[keep & !(d$value %in% have), , drop = FALSE]

    if (nrow(m) > 0) { # if undocumented sentinels are found, add them to hits
      hits[[f]] <- data.frame(
        dataset = dataset, variable = variable,
        value = m$value, n = as.integer(m$n),
        in_codebook = variable %in% cb$Variable,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- if (length(hits) > 0) {
    do.call(rbind, hits)
  } else {
    data.frame(
      dataset = character(), variable = character(), value = character(),
      n = integer(), in_codebook = logical(), stringsAsFactors = FALSE
    )
  }
  out <- out[order(-out$n), , drop = FALSE]
  rownames(out) <- NULL

  if (warn) {
    if (nrow(out) > 0) {
      message(sprintf(
        paste0(
          "flagSentinels: %d undocumented sentinel value(s) across %d ",
          "variable(s) -- review and add to 'Sentinel Codes':"
        ),
        nrow(out), length(unique(out$variable))
      ))
    } else {
      message("flagSentinels: OK -- every sentinel-pattern value in the data is documented.")
    }
  }
  if (warn && nrow(out) > 0) print(out, row.names = FALSE)
  invisible(out)
}


#### Codebook ASCII helpers ####

# Detect non-ASCII bytes in a codebook data frame. Returns one row per
# offending (Variable, column) cell with a count and the offending bytes in
# hex. An empty data frame means the codebook is pure ASCII. The check is
# byte-level (charToRaw) so it is robust to strings that read_csv has
# mislabeled as UTF-8 while actually holding stray bytes (e.g. a Latin-1 0xa0).
codebookNonAscii <- function(cb) {

  # get names of the character columns
  cols <- names(cb)[vapply(cb, is.character, logical(1))]

  rows <- list()

  for (col in cols) {
    vals <- cb[[col]] # extract the column as a vector
    for (i in seq_along(vals)) {
      s <- vals[i] # get contents of a codebook cell
      if (is.na(s)) next
      hi <- as.integer(charToRaw(s)) # raw bytes -> integers, so next line can compare
      hi <- hi[hi > 127L] # any byte greater than 127 is not ascii
      if (length(hi) > 0L) { # one row per cell that contains non-ascii
        rows[[length(rows) + 1L]] <- data.frame(
          # Record variable name
          variable = if ("Variable" %in% names(cb)) cb[["Variable"]][i] else NA_character_,
          # Record the column name
          column = col,
          # Record number of bytes
          n_bytes = length(hi),
          # Record the bytes as a single string
          bytes = paste(sprintf("0x%02x", hi), collapse = " "),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  # If no non-ascii characters are found, return an empty data.frame
  if (length(rows) == 0L) {
    return(data.frame(
      variable = character(), column = character(),
      n_bytes = integer(), bytes = character(),
      stringsAsFactors = FALSE
    ))
  }
  # Else return the information about non-ascii characters
  do.call(rbind, rows)
}

# Map the common "smart"/non-ASCII punctuation that sneaks in when pasting text
# from OPM PDF/Word sources (e.g. the FWD data dictionary) to ASCII, so the
# published codebook stays pure ASCII. Run this deliberately after editing the
# codebook: 02_codebook_build.R refuses to write a codebook containing
# non-ASCII, and this is the tool that clears that error.
#
# NOTE: this fixes *validly-encoded* UTF-8 punctuation. A file already mangled
# to invalid bytes by a non-UTF-8 save (e.g. a stranded 0xa0) must first be
# re-saved as UTF-8. See codebook/codebook_notes.md.
normalizeCodebookText <- function(cb_path = "metadata/codebook/fwoc_codebook_table.csv",
                                  write = TRUE) {

  # Named vector that creates a mapping of common non-ascii character and their
  # ascii replacement.
  repl <- c(
    "\u2018" = "'", "\u2019" = "'", # single curly quotes
    "\u201C" = '"', "\u201D" = '"', # double curly quotes
    "\u2013" = "-", "\u2014" = "--", # en dash / em dash
    "\u2026" = "...", # horizontal ellipsis
    "\u00A0" = " " # non-breaking space
  )

  # Read the codebook
  cb <- readr::read_csv(cb_path, col_types = readr::cols(.default = "c"), na = "")
  # Get a vector of character columns in the codebook
  cols <- names(cb)[vapply(cb, is.character, logical(1))]

  total <- 0L
  for (col in cols) { # loop over character columns
    v <- cb[[col]] # extract the column as a vector
    for (k in seq_along(repl)) { # Loop over common cases in repl
      from <- names(repl)[k] # non-ascii
      to <- repl[[k]] # replacement for non-ascii
      # Get vector of elements of v (i.e, rows in the column) that have the
      # non-ascii character
      hits <- which(!is.na(v) & grepl(from, v, fixed = TRUE))

      if (length(hits) > 0L) {
        # Replace the non-ascii characters
        v[hits] <- gsub(from, to, v[hits], fixed = TRUE)
        total <- total + length(hits) # iterate the fix count
        message(sprintf(
          "  %-14s %d cell(s): U+%04X -> '%s'",
          col, length(hits), utf8ToInt(from)[1], to
        ))
      }
    }
    cb[[col]] <- v # write the column with the offending characters removed
  }

  if (total == 0L) {
    message("no smart-punctuation found; codebook unchanged")
  } else if (write) {
    # write the codebook with non-ascii characters removed
    readr::write_csv(cb, cb_path, na = "")
    message("normalized ", total, " cell(s); wrote ", cb_path)
  }

  # `repl` covers eight known characters; codebookNonAscii() flags ANY byte > 127.
  # So a residual here is a character this function cannot fix, and reporting it is
  # what stops the 02 -> normalize -> 02 loop: without it, a lone degree sign gets
  # "no smart-punctuation found" while 02_codebook_build.R keeps halting.
  residual <- codebookNonAscii(cb)
  if (nrow(residual) > 0L) {
    message(
      "NON-ASCII REMAINS in ", nrow(residual), " cell(s) -- outside `repl`, so this ",
      "function cannot fix them. Add them to `repl` or edit the codebook by hand; ",
      "02_codebook_build.R halts until they are gone."
    )
    print(residual)
  } else if (total > 0L) {
    message("codebook is now pure ASCII")
  }
  invisible(cb)
}


#### Function to check observed missing-label rows against master lookup ####

checkLabelGaps <- function(inventory_path, lookup_path,
                           code_col, label_col,
                           lookup_label_col = "label",
                           out_path = NULL) {
  # For each (code, label = missing, date) row in the inventory, classify
  # against the master lookup:
  #   - invalid:       code is not in the lookup
  #   - out_of_range:  code is in the lookup but the observation date falls
  #                    outside every [date_from, date_to] window for that code
  #   - unexpected:    code is in the lookup and the observation date falls
  #                    inside a [date_from, date_to] window — labeling bug
  #                    (the label should not be missing)
  #
  # A "missing" label is NA_character_ (originally empty in the CSV) or the
  # literal string "NA".
  #
  # Assumes the inventory was built with grain (code, label, date) by
  # buildInventory, and the date convention matches the dataset's label
  # join (see buildInventory `first_of_month` arg).

  # Read the inventory
  inv <- readr::read_csv(
    inventory_path,
    col_types = readr::cols(
      .default = "c",
      n_rows = "d",
      date = "D"
    ),
    na = ""
  )

  if (!"date" %in% names(inv)) {
    stop(
      "Inventory does not have a `date` column. Re-run buildInventory ",
      "to refresh the inventory with grain (code, label, date)."
    )
  }

  # Read the lookup table
  lkp <- readr::read_csv(lookup_path, col_types = "ccDD", na = "")

  # Get only codes with a missing label
  inv_gaps <- inv |>
    dplyr::filter(is.na(.data[[label_col]]) | .data[[label_col]] == "NA") |>
    dplyr::transmute(
      code = .data[[code_col]],
      date = date,
      n_rows = n_rows
    )

  if (nrow(inv_gaps) == 0) {
    message("No label-gap rows in inventory; nothing to check.")
    return(invisible(tibble::tibble()))
  }

  # Join each (code, date) to every lookup entry for that code. The
  # many-to-many is expected: a single inventory row can match multiple
  # lookup periods, and a single lookup period can match multiple inventory
  # dates. Overlap filtering happens in the summarize below.
  joined <- inv_gaps |>
    dplyr::left_join(
      lkp |> dplyr::rename(
        lkp_label = !!lookup_label_col,
        lkp_from = date_from,
        lkp_to = date_to
      ),
      by = "code",
      relationship = "many-to-many"
    )

  result <- joined |>
    dplyr::group_by(code, date, n_rows) |>
    dplyr::summarize(
      # Check if code is valid defined as in the lookup. Probe lkp_from, not
      # lkp_label: an unmatched code gets NA in every joined column, so any
      # of them detects the match -- but a MATCHED row may carry a genuinely
      # NA label (pay_plan has codes documented with a window and no label
      # text), and probing the label would report those as "not in lookup".
      # date_from is never NA in a real lookup row, so it means "matched".
      in_lookup = any(!is.na(lkp_from)),
      # Check if the date is in the date range
      in_range = any(date >= lkp_from & date <= lkp_to, na.rm = TRUE),
      # The label the lookup says should have applied (first in-range, non-NA
      # match). Every row here already has a missing label -- inv_gaps filtered
      # for that -- so this names the value that went missing, which is what
      # makes an `unexpected` row actionable.
      matching_label = dplyr::first(
        lkp_label[date >= lkp_from & date <= lkp_to & !is.na(lkp_label)]
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      status = dplyr::case_when(
        !in_lookup ~ "invalid", # Code is not in lookup, so the code is invalid
        in_lookup & in_range ~ "unexpected", # Code is valid and in range
        in_lookup & !in_range ~ "out_of_range"
      )
    ) |>
    dplyr::select(code, date, n_rows, status, matching_label) |>
    dplyr::arrange(dplyr::desc(n_rows), code, date)

  if (!is.null(out_path)) {
    if (!dir.exists(dirname(out_path))) {
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    }
    # na = "": `code` carries real OPM codes, and "NA" is one of them (National
    # Aeronautics and Space Council). Without it a missing value and that code
    # both write as bare NA and cannot be told apart on read-back.
    readr::write_csv(result, out_path, na = "")
    message("wrote ", out_path)
  }

  result
}

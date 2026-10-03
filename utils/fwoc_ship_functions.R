# Author: Mark Richardson and Claude Code
# Purpose: Pure helpers for setup/build_fwoc_ship_labels.R, which turns the OPM
#          agency and subagency label tables into the FWOC label tables.
#
# Every function takes and returns a label table with these columns, none NA:
#   code (chr), label (chr), date_from (Date), date_to (Date),
#   opm_label_misleading (lgl), opm_label_note (chr), reason (chr)
# reason is "" on a row that still shows OPM's label; otherwise it is the
# codebook's phrase for why the row differs.
#
# The functions do not call one another: the builder runs them in sequence. To
# walk one, create its arguments as objects and run its body line by line.
# Callers must have dplyr attached.

# Merge rows of one code that touch in time and are identical in every other
# column. OPM re-issues an unchanged label at its own table revisions (most codes
# have a row boundary at 1986-09-01 and 2005-02-01), so one name is often several
# rows. A gap in dates is never bridged.
merge_contiguous_rows <- function(d) {
  d |>
    arrange(code, date_from) |>
    group_by(code) |>
    mutate(
      # TRUE on the first row of a code and wherever anything changes
      new_run = is.na(lag(date_to)) |
        lag(date_to) + 1 != date_from |
        lag(label) != label |
        lag(opm_label_misleading) != opm_label_misleading |
        lag(opm_label_note) != opm_label_note |
        lag(reason) != reason,
      run = cumsum(new_run)
    ) |>
    # Each run becomes one row. summarize keeps only the grouping columns and
    # the columns it computes, so label, flag, note and reason are grouped on
    # to carry them through (they are identical within a run), and the dates
    # become the run's earliest start and latest end. A run is a set of
    # consecutive rows of one code whose dates touch and that are otherwise
    # identical.
    group_by(code, run, label, opm_label_misleading, opm_label_note, reason) |>
    summarize(
      date_from = min(date_from),
      date_to = max(date_to),
      .groups = "drop"
    ) |>
    select(
      code, label, date_from, date_to,
      opm_label_misleading, opm_label_note, reason
    ) |>
    arrange(code, date_from)
}

# Collapse OPM label changes that are two renderings of one name. `events` lists
# the admitted boundaries: code, event_date, opm_label_before, opm_label_after,
# reason. The rows on both sides of an admitted boundary form a run, and every
# row of a run takes the run's last (most recent) label. Runs chain: a change
# and its later reversal, both admitted, collapse to the final label.
#
# `d` must already be merged (merge_contiguous_rows), so that each boundary is a
# real change of label. Stops if an event is not a boundary in `d`, or if the
# labels on either side are not the ones the event records: OPM revises its
# history, so a code and date alone could come to mean a different change.
collapse_nominal_runs <- function(d, events) {
  events <- events |>
    transmute(
      code,
      date_from = event_date,
      opm_label_before, opm_label_after,
      event_reason = reason
    )

  unmatched <- anti_join(events, d, by = c("code", "date_from"))
  if (nrow(unmatched) > 0) {
    stop(
      "collapse_nominal_runs(): no row starts on the event date for: ",
      paste(unmatched$code, unmatched$date_from, collapse = "; ")
    )
  }

  d <- d |>
    arrange(code, date_from) |>
    group_by(code) |>
    mutate(prev_to = lag(date_to), prev_label = lag(label)) |>
    ungroup() |>
    left_join(events, by = c("code", "date_from")) |>
    mutate(is_event = !is.na(event_reason))

  # is.na(prev_to): the event sits on the code's first row, so nothing precedes it
  mismatch <- d |>
    filter(is_event) |>
    filter(
      is.na(prev_to) |
        prev_to + 1 != date_from |
        prev_label != opm_label_before |
        label != opm_label_after
    )
  if (nrow(mismatch) > 0) {
    stop(
      "collapse_nominal_runs(): the labels at the boundary are not the ones ",
      "the event records, or the rows do not touch, for: ",
      paste(mismatch$code, mismatch$date_from, collapse = "; ")
    )
  }

  d |>
    group_by(code) |>
    # a row that is not an event starts a new run; an event row joins the run
    # of the row before it
    mutate(run = cumsum(!is_event)) |>
    group_by(code, run) |>
    mutate(
      label = last(label),
      # the last row of a run of two or more is an event row, so this is NA
      # only for a run of one, which keeps its own reason
      reason = coalesce(last(event_reason), reason)
    ) |>
    ungroup() |>
    select(
      code, label, date_from, date_to,
      opm_label_misleading, opm_label_note, reason
    )
}

# Replace the label, flag, note and reason over stated date windows. `windows`
# has code, date_from, date_to, label, opm_label_misleading, opm_label_note,
# reason; a missing date_to means through the last day `d` covers the code.
#
# A window need not line up with OPM's rows (the Office of the Secretary of
# Health, Education, and Welfare ends on 1980-05-03, inside an OPM row), so each
# row is first cut into pieces at every date where a window starts or ends, and
# each piece then takes the values of the window that contains it, if any.
# Stops if two windows of a code overlap, or if `d` does not cover every day of
# a window (a window must never create coverage OPM lacks).
apply_label_windows <- function(d, windows) {
  code_end <- d |>
    group_by(code) |>
    summarize(code_end = max(date_to), .groups = "drop")

  # Each window holds the label, flag, note and reason to use in place of OPM's
  # from win_from to win_to. The columns are renamed with a win_ prefix so they
  # do not collide with d's columns in the joins below, and an empty end date
  # becomes the last day d covers the code.
  windows <- windows |>
    left_join(code_end, by = "code") |>
    transmute(
      code,
      win_from = date_from,
      win_to = coalesce(date_to, code_end),
      win_label = label,
      win_misleading = opm_label_misleading,
      win_note = opm_label_note,
      win_reason = reason
    )

  # Every date on which a piece can start: a row start, a window start, and the
  # day after a window ends
  cuts <- bind_rows(
    d |> transmute(code, cut = date_from),
    windows |> transmute(code, cut = win_from),
    windows |> transmute(code, cut = win_to + 1)
  ) |>
    distinct()

  # pieces takes the label rows and the windows, splits each row at every date
  # where a window starts or ends, and joins each window's values to the pieces
  # inside it. Pieces outside every window get NA in the win_ columns and keep
  # OPM's label. Nothing is overwritten here; the final mutate does that.
  pieces <- d |>
    inner_join(cuts, by = join_by(code, between(y$cut, x$date_from, x$date_to))) |>
    group_by(code, date_from) |> # the pieces of one original row
    arrange(cut, .by_group = TRUE) |>
    mutate(
      piece_from = cut,
      # a piece ends the day before the next one starts; the last ends with the row
      piece_to = coalesce(lead(cut) - 1, date_to)
    ) |>
    ungroup() |>
    left_join(
      windows,
      by = join_by(code, between(x$piece_from, y$win_from, y$win_to))
    )

  # After the cuts, each piece is unique by code and piece_from. The left join
  # to windows repeats a piece only if its first day lies inside two windows,
  # so a repeated code and piece_from means two windows of that code overlap.
  overlapping <- pieces |> filter(duplicated(pieces[c("code", "piece_from")]))
  if (nrow(overlapping) > 0) {
    stop(
      "apply_label_windows(): windows overlap for code(s): ",
      paste(unique(overlapping$code), collapse = ", ")
    )
  }

  # Count the days of each window that pieces cover: the summed length of the
  # pieces inside it
  days_covered <- pieces |>
    filter(!is.na(win_from)) |>
    group_by(code, win_from) |>
    summarize(
      days = sum(as.integer(piece_to - piece_from) + 1L),
      .groups = "drop"
    )

  # Join windows with the days covered by their pieces, then compare with the
  # window's own length in days. Keep windows with no matching piece
  # (is.na(days)) or with fewer days covered than the window spans. No match
  # means d has no rows for the window; unequal days means part of the window
  # falls in a gap or outside the dates d covers.
  uncovered <- windows |>
    left_join(days_covered, by = c("code", "win_from")) |>
    filter(is.na(days) | days != as.integer(win_to - win_from) + 1L)

  # Stop if any window is not fully covered
  if (nrow(uncovered) > 0) {
    stop(
      "apply_label_windows(): the label table does not cover every day of ",
      "the window for: ",
      paste(uncovered$code, uncovered$win_from, collapse = "; ")
    )
  }

  pieces |>
    mutate(
      # TRUE for a piece that lies inside a window
      in_window = !is.na(win_from),
      # a piece inside a window takes the window's values; any other piece
      # keeps the values it had
      label = if_else(in_window, win_label, label),
      opm_label_misleading = if_else(in_window, win_misleading, opm_label_misleading),
      opm_label_note = if_else(in_window, win_note, opm_label_note),
      reason = if_else(in_window, win_reason, reason)
    ) |>
    transmute(
      # back to the seven standard columns, with each piece's own dates in
      # place of the original row's
      code, label,
      date_from = piece_from,
      date_to = piece_to,
      opm_label_misleading, opm_label_note, reason
    ) |>
    arrange(code, date_from)
}

# The codebook's table of differences: one row per FWOC row whose label differs
# from an OPM label in force during any part of its span, with those OPM labels
# listed in date order. `opm` is the OPM label table (code, label, date_from,
# date_to); `level` is "agency" or "subagency".
ship_differences <- function(ship, opm, level) {
  opm <- opm |>
    transmute(code, opm_label = label, opm_from = date_from, opm_to = date_to)

  ship |>
    inner_join(
      opm,
      by = join_by(code, overlaps(x$date_from, x$date_to, y$opm_from, y$opm_to))
    ) |>
    filter(opm_label != label) |>
    arrange(code, date_from, opm_from) |>
    group_by(code, label, date_from, date_to, reason, opm_label_note) |>
    summarize(
      opm_labels_in_span = paste(unique(opm_label), collapse = " | "),
      .groups = "drop"
    ) |>
    transmute(
      level = level,
      code,
      fwoc_label = label,
      date_from, date_to,
      opm_labels_in_span, reason, opm_label_note
    ) |>
    arrange(code, date_from)
}

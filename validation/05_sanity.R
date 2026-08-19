# Author: Mark Richardson and Claude Code
# Purpose: Ex.5 -- indicator/domain sanity + historical-count stability. Per dataset:
#   binary indicators take only {0,1,NA} and aren't degenerate (single-level, soft);
#   continuous vars (pay, length of service, BLS macro) sit within plausible bounds (soft);
#   month/quarter are in range and mutually
#   consistent; the era time column is never null; and final per-period row counts
#   for "frozen" months (those OPM did NOT re-version this cycle -- from the source
#   file versions; the first run seeds both baselines and defers the check) are unchanged
#   vs the prior run -- hard for all three, checked separately. An unexplained change
#   halts the suite and holds both baselines at their prior values; a row in
#   metadata/validation/baselines/ex5_history_allowlist.csv accepts a reviewed change
#   and lets them roll forward.
#   No duplicate-pseudo_id check -- duplicate ids are an expected feature of the data.

library(arrow)
library(dplyr)
source("utils/validation_functions.R")

out_dir   <- "validation_output"
hist_path <- "metadata/validation/baselines/ex5_history_counts.csv"

# int8 flags that are strictly 0/1/NA (excludes quarter, which is 1-4).
binary_indicators <- c(
  "cfo_act_agency_indicator", "nsftp_indicator", "nsftp_indicator_fwoc",
  "veteran_indicator", "drp_indicator", "drp_indicator_fwoc",
  "opm_label_misleading_agency", "opm_label_misleading_subagency",
  "exec_branch", "leg_branch", "jud_branch", "a1_court", "eop", "core_eop")

# Continuous vars with a plausible [lo, hi]; out-of-range values are data-quality outliers
# (the cfo_act class at the value level). The pay bounds are set from the OPM salary tables and
# the statutory ceilings. OPM publishes this column as basic pay plus any basic pay supplement
# AFTER any applicable cap, so a legitimate value cannot exceed the ceiling of its own pay
# system: EX-I is the highest scheduled rate in the tables ($246,400 in 2024, with the GS
# payable max capped at EX-IV and SES at EX-II/III), and the outer ceiling for any federal
# civilian is the 38 U.S.C. 7431(e)(4) cap on VA physician base + market pay -- the President's
# pay, $400,000. Nearly the whole observed high tail is that one population (VA occupation 0602
# on pay plans VM/AD), sitting above the nominal cap because the cap binds on compensation
# actually paid while this column is an annualized rate.
#
# The real-2024 bound does the real work. No single nominal bound can be grounded across the
# series, because the ceiling itself moves: EX-I runs from $60,000 (1969) to $253,100 (2026),
# so one constant is either too tight for the present or useless for the BZ era -- where
# 1980s wage-grade records many times over their era's ceiling clear even a $1M bound
# untouched. Deflating to 2024 dollars indexes the ceiling instead of guessing at it, so 7e5
# (headroom over the highest legitimate value observed, a 2025 VA physician) applies uniformly
# to both eras. 6e5 nominal is kept as a coarse FWD-era screen. LoS at a full-career 70, rates
# 0-100%, CPI positive. grade is excluded -- its range is pay-plan-specific, so one bound misleads.
numeric_bounds <- list(
  list(col = "length_of_service_years",                 lo = 0, hi = 70),
  list(col = "annualized_adjusted_basic_pay_num",       lo = 0, hi = 6e5),
  list(col = "annualized_adjusted_basic_pay_real_2024", lo = 0, hi = 7e5),
  list(col = "cpi_u",                                   lo = 0, hi = 1000),
  list(col = "cpi_index",                               lo = 0, hi = 5),
  list(col = "unrate_us",                               lo = 0, hi = 100),
  list(col = "unrate_state",                            lo = 0, hi = 100))

datasets <- list(
  list(ds = "emp", hive = "emp_hive", time = "date"),
  list(ds = "acc", hive = "acc_hive", time = "acc_sep"),
  list(ds = "sep", hive = "sep_hive", time = "acc_sep"))

all_counts <- list()

for (d in datasets) {
  ds   <- open_dataset(d$hive, partitioning = c("year", "month"), unify_schemas = TRUE)
  cols <- names(ds)

  # Binary domain: distinct values must be a subset of {0,1,NA}.
  degenerate <- character(0)
  for (col in intersect(binary_indicators, cols)) {
    # Pull vector as a variable
    vals <- ds |> distinct(!!rlang::sym(col)) |> collect() |> pull()
    lv   <- vals[!is.na(vals)]  # non-NA levels actually present
    bad  <- setdiff(lv, c(0L, 1L)) # Get any non-NA levels not 0/1
    val_check(sprintf("binary_%s_%s", d$ds, col), length(bad) == 0,
              detail = if (length(bad) == 0) "in {0,1,NA}" else paste0("unexpected: ", paste(bad, collapse = ",")))
    if (length(lv) == 1) degenerate <- c(degenerate, sprintf("%s(only %d)", col, lv))
  }
  # Degenerate binary (soft): a flag with non-NA data but only one level (all 0 or all 1) is
  # suspect -- e.g. a miscoded recode collapsing every value to 0 (the cfo_act bug, 2026-06,
  # which passed the domain check since {0,NA} is valid). Soft: some flags are legitimately
  # one-sided/rare in a given hive, so surface for review rather than block.
  val_check(sprintf("binary_degenerate_%s", d$ds), length(degenerate) == 0, severity = "soft",
            detail = if (length(degenerate) == 0) "all binaries have both levels (or are all-NA)"
                     else paste0("single-level: ", paste(degenerate, collapse = ", ")))

  # Numeric plausibility (soft): each continuous var within its [lo, hi]; out-of-range surfaces
  # a data-quality outlier (e.g. impossible tenure or pay), not a structural failure.
  for (nb in numeric_bounds) {
    if (!nb$col %in% cols) next
    s <- ds |> filter(!is.na(!!rlang::sym(nb$col))) |>
      summarise(seen_lo = min(!!rlang::sym(nb$col)), seen_hi = max(!!rlang::sym(nb$col)),
                n_oob = sum(as.integer(!!rlang::sym(nb$col) < nb$lo | !!rlang::sym(nb$col) > nb$hi))) |>
      collect()
    val_check(sprintf("range_%s_%s", d$ds, nb$col), s$n_oob == 0, severity = "soft",
              detail = sprintf("seen [%s, %s]; bound [%s, %s]; %s out-of-bound",
                               format(s$seen_lo, scientific = FALSE), format(s$seen_hi, scientific = FALSE),
                               format(nb$lo, scientific = FALSE), format(nb$hi, scientific = FALSE),
                               format(s$n_oob, big.mark = ",")))
  }

  # month/quarter domain + mutual consistency (tiny distinct table, checked in R).
  mq    <- ds |> count(month, quarter) |> collect()
  ok_mq <- all(mq$month %in% 1:12) && all(mq$quarter %in% 1:4) &&
           all(mq$quarter == ((mq$month - 1L) %/% 3L) + 1L) # Derive the quarter from the month
  val_check(sprintf("month_quarter_%s", d$ds), ok_mq,
            detail = if (ok_mq) "valid + consistent" else "invalid month/quarter combo")

  # Era time column never null (emp: date; acc/sep: coalesced eff date).
  n_null <- if (d$time == "date")
    ds |> filter(is.na(date)) |> count() |> collect() |> pull(n)
  else
    ds |> filter(is.na(eff_date) & is.na(personnel_action_effective_date_yyyymm)) |>
      count() |> collect() |> pull(n)
  val_check(sprintf("time_not_null_%s", d$ds), n_null == 0, detail = sprintf("%d null-time rows", n_null))

  all_counts[[d$ds]] <- ds |> count(year, month) |> collect() |>
    transmute(dataset = d$ds, year, month, n_rows = n)
}

# Sort before this reaches either write below. Arrow's count() does not guarantee row
# order -- two calls in one session return different orders -- so without this the
# baseline is rewritten in an arbitrary order on every roll-forward and two runs cannot
# be diffed. (dataset, year, month) is unique, so this is a total order and the file is
# byte-reproducible. arrange() sorts in the C locale, so it does not depend on the
# machine. fwd_source_versions.csv needs no equivalent: scan_fwd_versions() ends in a
# grouped summarise(), which returns groups already sorted.
cur_counts <- bind_rows(all_counts) |> arrange(dataset, year, month)

# Periods OPM re-versioned this cycle are allowed to change; everything else is
# "frozen". Compare the live source versions to the stored version baseline. NULL means that
# baseline is absent -- a state to seed or halt on, never to guess through. The version
# baseline is NOT rolled forward here: it and the count baseline are two halves of one cycle
# state, and `updated` is what permits a count change to pass. Rolling the version baseline
# forward on a run that then fails would leave the next run seeing no version bumps while the
# count baseline still shows the change -- turning a legitimate OPM update into a second
# unexplained failure alongside the real one. Both files therefore move together, or neither
# does, below.

ver_path <- "metadata/validation/baselines/fwd_source_versions.csv"
cur_ver  <- scan_fwd_versions() # Get the max version for each dataset-year-month
updated  <- fwd_updated_periods(cur_ver, ver_path) # Get the updated periods

# The guard above treats "highest version on disk" as a stand-in for "the version in the hive",
# but 04 assembles whichever version the manifest flags `current`. Those are two different facts,
# so assert they agree rather than leaving it to OPM's publishing habit: every period's current
# version is also its highest today, and nothing enforces that. A rollback would break the
# stand-in silently and in the dangerous direction -- `updated` would excuse a period 04 did not
# actually re-assemble. Reading the manifest here is not the circularity flagged at
# validation_functions.R:137; that warns against deriving the expected counts from the build's own
# record. This compares two independent records of the same fact -- what is on disk, and what 04
# selected -- and fails when they disagree.

# 03's provenance manifest -- OPM's file listing as recorded at download time.
man <- readr::read_csv("metadata/fwd/file_manifest.csv", na = "", show_col_types = FALSE,
                       col_types = readr::cols(dataset = "c", year = "i", month = "i",
                                               version = "i", current = "l"))

# Subset to the manifest's current-flagged rows, taking the first three characters of the
# manifest's dataset names ("employment" -> "emp") to build a join key matching cur_ver.
sel <- man |>
  filter(current) |>
  transmute(dataset = stringr::str_sub(dataset, 1, 3),
            year, month, v_current = version)

# Disagreements, three ways: a manifest period missing from disk (deleted source file), a
# period on disk with no current-flagged manifest row, or a max-on-disk that is not the
# version main script 04 selected.
ver_bad <- full_join(cur_ver, sel, by = c("dataset", "year", "month")) |>
  filter(is.na(version) | is.na(v_current) | version != v_current)

val_check("version_selection_fwd", nrow(ver_bad) == 0,
          detail = if (nrow(ver_bad) == 0)
                     sprintf("current == max(version) on disk across %d periods", nrow(sel))
                   else paste0(nrow(ver_bad), " period(s) disagree: ",
                               paste(sprintf("%s %s-%s disk=%s current=%s", ver_bad$dataset,
                                             ver_bad$year, ver_bad$month,
                                             ver_bad$version, ver_bad$v_current), collapse = ", ")))

# Historical-count stability: frozen periods must not change between monthly runs.
cmp <- compare_to_baseline(cur_counts, hist_path,
                           key = c("dataset", "year", "month"), value_var = "n_rows")

# Two baseline files carry state from one monthly run to the next, and this check needs both:
#   ex5_history_counts.csv  -- last run's row count per period. compare_to_baseline() above
#                              diffed today's counts against it; `cmp` flags every period that moved.
#   fwd_source_versions.csv -- last run's max source-file version per period. fwd_updated_periods()
#                              diffed today's versions against it; `updated` lists what OPM
#                              re-published this cycle.
# A count that moved is only a problem if OPM did NOT re-publish that period, so `cmp` says which
# periods moved and `updated` says which were allowed to. Either file missing comes back NULL and
# leaves the question unanswerable -- hence the three branches.

if (is.null(cmp) && is.null(updated)) {
  # Neither file exists -- a first run. Seed both from today and defer the check to next month.
  write_baseline_if_absent(cur_counts, hist_path)
  readr::write_csv(cur_ver, ver_path, na = "")   # seed both halves together
  val_check("history_stability", TRUE, "seeded both baselines (first run)")
} else if (is.null(cmp) || is.null(updated)) {
  # Exactly one file exists. Not a first run, so do not seed and do not guess -- neither
  # automatic recovery is safe:
  #   re-seed the missing one from today -- discards the real history held by the file that
  #     survived, and silently blesses whatever changed in the meantime;
  #   keep the survivor and carry on -- compares months-old counts against a version record
  #     that starts today, so every period OPM revised in the gap reads as unexplained.
  # Both files are committed, so the operator restores the missing one from git, or deletes
  # both to re-seed deliberately.
  val_check("history_stability", FALSE,
            detail = paste0(basename(if (is.null(cmp)) hist_path else ver_path),
                            " missing while its counterpart is present -- restore it or",
                            " delete both to re-seed"))
} else {
  # Both files present -- the normal path. A period OPM re-published this cycle is allowed to
  # have moved; every other period is "frozen" and must still match last run's count. Keys go
  # to character because compare_to_baseline() stringifies both sides of its join.
  allow <- transmute(updated, dataset, year = as.character(year), month = as.character(month), allow = TRUE)
  # Frozen periods whose count moved: everything cmp flagged, minus what `updated` allows.
  unexpected_changes <- cmp |> filter(changed) |>
    left_join(allow, by = c("dataset", "year", "month")) |> filter(is.na(allow))

  # Rendered as allowlist rows so a reviewed change can be pasted straight into
  # ex5_history_allowlist.csv; the reason column still has to be written by hand.
  fmt <- function(df) paste(sprintf("%s,%s,%s,%+d", df$dataset, df$year, df$month, df$delta),
                            collapse = " | ")

  # Reviewed exceptions live in a committed allowlist, like ex2's and ex3's -- not an
  # environment variable. Two things follow from that choice. The acceptance is a durable,
  # reviewable record carrying its own reason, where an env var left none: the flag cleared
  # the check AND rolled the baselines forward, erasing what it had been comparing against.
  # And the key includes `delta`, so an accepted change never excuses a DIFFERENT later
  # change to the same period -- which is what makes rows safe to leave in the file forever
  # rather than pruning them after each roll-forward.
  hist_allow <- load_allowlist("metadata/validation/baselines/ex5_history_allowlist.csv")
  if (nrow(hist_allow) > 0) {
    hist_allow <- transmute(hist_allow, dataset, year, month,
                            delta = suppressWarnings(as.numeric(delta)))
    unexpected_changes <- anti_join(unexpected_changes, hist_allow,
                                    by = c("dataset", "year", "month", "delta"))
  }

  # One check per dataset, not one pooled: OPM can re-publish accessions for a period and not
  # separations, and a pooled row hides which of the three moved. All three hard -- a routine OPM
  # revision arrives with a version bump and was already absorbed by `allow` above, and an in-place
  # rewrite of a file already on disk halts back in 03, so a frozen count that moved has no benign
  # source left to attribute it to.
  for (dsn in c("emp", "acc", "sep")) {
    chg <- filter(unexpected_changes, dataset == dsn)
    val_check(sprintf("history_stability_%s", dsn), nrow(chg) == 0,
              detail = if (nrow(chg) == 0) sprintf("%s frozen periods unchanged", dsn)
                       else paste0(nrow(chg), " ", dsn, " period(s) changed -- investigate, or ",
                                   "add to ex5_history_allowlist.csv after review: ", fmt(chg)))
  }

  # Roll both files forward together, and only when nothing unexplained moved in ANY of the three
  # datasets -- unexpected_changes spans all of them, so one clean dataset is not enough. Advancing
  # over a dirty one would write the drift in as the new baseline, and the check, having erased
  # what it was comparing against, would never fire on it again. The two files are one state and
  # cannot move separately -- see the note above ver_path.
  # An allowlisted change is already gone from unexpected_changes, so accepting one
  # releases the roll-forward exactly as a genuinely-unchanged cycle does.
  if (nrow(unexpected_changes) == 0) {
    readr::write_csv(cur_counts, hist_path, na = "")
    readr::write_csv(cur_ver, ver_path, na = "")
  }
}

val_finish(file.path(out_dir, "ex5_checks.csv"))

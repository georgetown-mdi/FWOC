# Author: Mark Richardson and Claude Code
# Purpose: Build the per-(year, month) occupational-series -> PATCO
#          (occupational category) crosswalk used to derive
#          occupational_category_code_fwoc in the emp hive. data.opm.gov omitted
#          PATCO for 2005-2009; the raw BuzzFeed FOIA status hive carries it for
#          those years, so the gap is sourced from raw BZ status and every other
#          year from the final emp_hive's own published value.
#
#          Scope: built from emp/STATUS only -- the dense as-of snapshot that
#          covers all active codes each period -- and applied (in script 11) to
#          the emp hive only. The same crosswalk can be applied to acc/sep by
#          users who want PATCO there; we do not do that in-pipeline because
#          acc/sep are sparse flows and a first-of-month label would inherit the
#          emp as-of code with extra slippage.
#
#          PATCO is NOT a clean function of the 4-digit series: 140 of the 2,119
#          series carry two categories at once (e.g. 0303 is both Technical and
#          Clerical at a single snapshot), confirmed in FWD data and in the BZ
#          era. So the crosswalk is LAYERED and EXACT:
#            Level 1 -- (year, month, series)                  where that is 1:1
#            Level 2 -- (year, month, series, pay_plan, grade) for split series
#          Cells still ambiguous go to the diagnostics file and are left blank.
#
#          Key = (year, month), not the full date: it aligns BZ and FWD
#          despite their different day-of-month (BZ Q4 = 12-13, FWD =
#          12-31), matches the hive partitioning, and -- since emp has one
#          snapshot per year-month -- is date-level for emp. (Finer eff_date
#          granularity was tested on BZ acc/sep and buys ~0: the per-position
#          split is intra-day, and a first-of-month label convention for
#          acc/sep data collapses the only within-month transitions anyway.)
#
#          Run from 00_build_fwoc.R after step 10 (final emp_hive populated and
#          enriched) and before step 11, which consumes the CSVs written here.
#          Construction: pushed-down arrow distinct() per source, then
#          vectorized counting in R.

library(arrow)
library(dplyr)
library(readr)

gap_years <- 2005:2009                       # FWD PATCO gap -> source from raw BZ status
fwd_start <- 2005                            # first FWD year; BZ supplies everything before it

emp <- open_dataset("emp_hive/",
                    partitioning  = c("year", "month"),
                    unify_schemas = TRUE)

# The raw BZ status hive is partitioned year/quarter, not year/month. month is a
# regular column there (values 3/6/9/12), so it still supplies the (year, month)
# key this crosswalk is built on.
bzs <- open_dataset("buzz_feed/bz_status_hive_raw",
                    partitioning  = c("year", "quarter"),
                    unify_schemas = TRUE)

cols <- c("year", "month", "occupational_series_code", "occupational_category_code",
          "pay_plan_code", "grade")

# Drop the redaction sentinel ("*") and blank/NA codes before deciding "clean".
keep_real <- function(d) {
  d |> filter(!is.na(occupational_category_code),
              occupational_category_code != "", occupational_category_code != "*",
              !is.na(occupational_series_code),
              occupational_series_code != "", occupational_series_code != "*")
}

# gap_years is an assumption about OPM's data, not a fact of this pipeline. OPM
# could backfill PATCO for 2005-2009 in any refresh, or stop publishing it for a
# new year, and the hardcoded range would then be wrong with nothing downstream
# to say so: a filled gap means we keep preferring BZ over OPM's own values, and
# a widened gap means 12 leaves those years NA. Checked against the data rather
# than trusted. No tolerance is needed -- the split is absolute, with exactly 0
# real PATCO rows in every gap year and >= 99.9% in every other FWD year.
patco_years <- emp |>
  filter(year >= fwd_start) |>
  select(all_of(cols)) |>
  keep_real() |>          # same definition of "real" used to build the crosswalk
  count(year) |>
  collect()

fwd_years    <- emp |> filter(year >= fwd_start) |> distinct(year) |> collect()
observed_gap <- sort(setdiff(fwd_years$year, patco_years$year))

# Report both directions at once; fixing one and rerunning to discover the other
# wastes a full pass over emp_hive.
#
# setdiff(x, y) is "in x, not in y", so the two calls below read:
#   setdiff(gap_years, observed_gap) -- assumed a gap, but the data has PATCO
#   setdiff(observed_gap, gap_years) -- data has no PATCO, but not assumed a gap
gap_drift <- c(
  if (length(setdiff(gap_years, observed_gap)) > 0)
    paste0("OPM now publishes PATCO for year(s) ",
           paste(setdiff(gap_years, observed_gap), collapse = ", "),
           " -- drop them from gap_years so the FWD values are used, not BZ"),
  if (length(setdiff(observed_gap, gap_years)) > 0)
    paste0("no PATCO in FWD year(s) ",
           paste(setdiff(observed_gap, gap_years), collapse = ", "),
           " -- add them to gap_years or 11 will leave them NA")
)
if (length(gap_drift) > 0) {
  stop("gap_years no longer matches the data --\n  ", paste(gap_drift, collapse = "\n  "))
}

# Non-gap years: final emp_hive (the exact values 12 applies the crosswalk to).
nongap <- emp |>
  filter(!(year %in% gap_years)) |>
  select(all_of(cols)) |>
  keep_real() |> distinct() |> collect()

# Gap years (2005-2009): raw BZ status carries PATCO; rename to the final schema.
gap <- bzs |>
  filter(year %in% gap_years) |>
  select(year, month,
         occupational_series_code   = occ_code,
         occupational_category_code = occ_cat_code,
         pay_plan_code, grade) |>
  keep_real() |> distinct() |> collect()

# Bind nongap (FWD) to gap (BZ) to make a complete crosswalk because we create
# an _fwoc patco variable to preserve the FWD and BZ versions with more coverage
obs <- bind_rows(nongap, gap)              # disjoint year ranges; each already distinct

key_ym  <- c("year", "month", "occupational_series_code")
key_ymg <- c("year", "month", "occupational_series_code", "pay_plan_code", "grade")

# ---- Level 1: (year, month, series) that map to exactly one category ----
ser <- obs |>
  group_by(pick(all_of(key_ym))) |>
  summarise(k         = n_distinct(occupational_category_code),
            # first_cat rather than assigning back to the source column, for the
            # same reason as the Level 2 summarise below: it keeps any later
            # expression reading the group's values, not first()'s scalar.
            first_cat = first(occupational_category_code),  # valid when k == 1
            .groups   = "drop")

lvl1       <- ser |> filter(k == 1) |>
  select(all_of(key_ym), occupational_category_code = first_cat)
split_keys <- ser |> filter(k >  1) |> select(all_of(key_ym))

# ---- Level 2: for split (year, month, series), (... pay_plan, grade) clean cells ----
cell <- obs |>
  semi_join(split_keys, by = key_ym) |>
  group_by(pick(all_of(key_ymg))) |>
  summarise(k         = n_distinct(occupational_category_code),
            cats      = paste(sort(unique(occupational_category_code)),
                              collapse = "|"),
            # first_cat, NOT occupational_category_code. summarise() evaluates
            # sequentially, so assigning back to the source column would make
            # cats order-dependent: any expression after it would see the single
            # first() value instead of the group's values, and cats would record
            # the winner rather than the competitors -- the one thing this
            # diagnostics file exists to capture. A distinct name makes that
            # impossible rather than merely avoided. Renamed back on the way out.
            first_cat = first(occupational_category_code),  # valid when k == 1
            .groups   = "drop")

lvl2  <- cell |> filter(k == 1) |>
  select(all_of(key_ymg), occupational_category_code = first_cat)
unres <- cell |> filter(k >  1) |> select(all_of(key_ymg), competing_categories = cats)

# Attach the human-readable category label (stable code->label) for QA readability.
# 12 selects only the code columns it needs, so this never reaches the hive.
cat_lab <- read_csv("metadata/labels/occ_cat_labels.csv",
                    col_types = cols(.default = "c"), na = "") |>
  distinct(occupational_category_code = code, occupational_category = label)

lvl1 <- lvl1 |> left_join(cat_lab, by = "occupational_category_code")
lvl2 <- lvl2 |> left_join(cat_lab, by = "occupational_category_code")

# ---------------------------------------------------------------------------
# Self-check: 2005-2009 emp gap coverage (aggregate to cells first).
#
# Runs BEFORE the writes on purpose. The CSVs are committed inputs that 12 reads,
# so a build that fails this check must leave the previous good ones in place
# rather than overwrite them with a crosswalk that no longer covers the gap.
# ---------------------------------------------------------------------------
gapcells <- emp |>
  filter(year %in% gap_years) |>
  count(year, month, occupational_series_code, pay_plan_code, grade) |>
  collect() |>
  left_join(lvl1 |> select(all_of(key_ym),  l1 = occupational_category_code), by = key_ym) |>
  left_join(lvl2 |> select(all_of(key_ymg), l2 = occupational_category_code), by = key_ymg)
gapcells$covered <- !is.na(gapcells$l1) | !is.na(gapcells$l2)

tot     <- sum(gapcells$n)
gap_cov <- 100 * sum(gapcells$n[gapcells$covered]) / tot # percentage of gap-year rows covered by the crosswalk

message("\n=== crosswalk summary (emp/status only, key = year, month) ===")
message(sprintf("obs distinct rows: %d  | Level 1: %d  | Level 2: %d  | unresolved: %d",
                nrow(obs), nrow(lvl1), nrow(lvl2), nrow(unres)))
message(sprintf("2005-2009 emp gap coverage: %.3f%% of %d rows (L1 %.2f%%, L2 adds %.2f%%)",
                gap_cov, tot,
                100 * sum(gapcells$n[!is.na(gapcells$l1)]) / tot,
                100 * sum(gapcells$n[is.na(gapcells$l1) & !is.na(gapcells$l2)]) / tot))
message(sprintf("residual left blank: %d rows (%.3f%%)",
                sum(gapcells$n[!gapcells$covered]),
                100 * sum(gapcells$n[!gapcells$covered]) / tot))

# Enforced, not just reported. The gap rows of this crosswalk are built from BZ
# status but applied by 11 to FWD emp rows, so Level 2's
# (series, pay_plan_code, grade) key has to match across two eras' formatting
# conventions. If either side ever changes how grade or pay_plan_code is padded,
# L2 stops matching, coverage collapses, and 11 leaves five years of
# occupational_category_code_fwoc NA with no error anywhere in the build. The
# floor is what turns that from silent into fatal. Measured 98.065% on the
# 2026-07 build; 95% sits well below normal drift and well above a real break.
gap_cov_floor <- 95
if (gap_cov < gap_cov_floor) {
  # %g, not %d: gap_cov_floor is a double in R, and %d would abort with a
  # sprintf format error instead of reporting the coverage problem.
  stop(sprintf("2005-2009 PATCO gap coverage is %.3f%%, below the %g%% floor. ",
               gap_cov, gap_cov_floor),
       "At or near 0% the BZ gap source is empty -- check that 01 still writes ",
       "2005-2009 to bz_status_hive_raw (it writes every BZ year; the pre-2005 ",
       "scoping happens later, in 05). Otherwise the Level 2 key ",
       "(occupational_series_code + pay_plan_code + grade) has most likely ",
       "stopped matching between the BZ source and the FWD emp rows -- compare ",
       "grade and pay_plan_code formatting on both sides. ",
       "The existing crosswalk CSVs have been left untouched.")
}

write_csv(lvl1,  "metadata/crosswalks/occ_series_patco_crosswalk.csv",       na = "")
write_csv(lvl2,  "metadata/crosswalks/occ_series_grade_patco_crosswalk.csv", na = "")
write_csv(unres, "metadata/crosswalks/occ_series_patco_unresolved.csv",      na = "")

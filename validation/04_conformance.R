# Author: Mark Richardson and Claude Code
# Purpose: Ex.4 -- codebook/schema conformance + partition continuity. Per dataset:
#   (a) published columns == the codebook keep-list for that hive (no extra/missing);
#   (b) each column's Arrow type == the codebook `Type`;
#   (c) the build incorporated the latest available FWD month, no year-month is
#       missing from the hive's cadence-aware calendar, and the newest month is not
#       an implausibly short drop. Writes per-dataset schema + partition CSVs.

library(arrow)
library(dplyr)
source("utils/validation_functions.R")

out_dir <- "validation_output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)  # ensure output dir exists for standalone runs
cb <- readr::read_csv("metadata/codebook/fwoc_codebook_table.csv",
                      col_types = readr::cols(.default = "c"), na = "", show_col_types = FALSE)

datasets <- list(
  list(ds = "emp", hive = "emp_hive", fwd = "data_opm_gov/fwd_emp_hive", flag = "Employment", cadence = "emp"),
  list(ds = "acc", hive = "acc_hive", fwd = "data_opm_gov/fwd_acc_hive", flag = "Accessions", cadence = "monthly"),
  list(ds = "sep", hive = "sep_hive", fwd = "data_opm_gov/fwd_sep_hive", flag = "Separations", cadence = "monthly")
)

ym      <- function(df) df$year * 100L + df$month   # year-month as a sortable integer
to_date <- function(y, m) as.Date(sprintf("%04d-%02d-01", y, m))  # (year, month) -> first-of-month Date

for (d in datasets) {
  # open the hive
  ds   <- open_dataset(d$hive, partitioning = c("year", "month"), unify_schemas = TRUE)

  # get the hive schema on disk
  got  <- arrow_schema_types(ds)

  # get a tibble of Variable names and schema type for this hive from the codebook
  want <- cb |> filter(.data[[d$flag]] == "x") |> transmute(column = Variable, type_cb = Type)

  # write the joined got-want report
  readr::write_csv(full_join(got, want, by = "column") |> arrange(column),
                   file.path(out_dir, sprintf("ex4_schema_%s.csv", d$ds)), na = "")

  # (a) keep-list: published columns must equal the codebook's per-hive variable set.

  # get variables in codebook for this hive that are not on disk
  missing <- setdiff(want$column, got$column)

  # get variables on disk that are not in the codebook
  extra   <- setdiff(got$column, want$column)

  val_check(sprintf("keeplist_%s", d$ds), length(missing) == 0 && length(extra) == 0,
            detail = sprintf("missing=[%s] extra=[%s]",
                             paste(missing, collapse = " "), paste(extra, collapse = " ")))

  # (b) types: every shared column's Arrow type matches the codebook Type.
  # is.na(type_cb) is folded in deliberately. filter() keeps only TRUE rows, so a
  # blank Type cell makes `arrow_type != type_cb` NA and drops the row -- the column
  # would ship with no documented type and the check would still report "all types
  # match". A published column with no Type IS a conformance failure, so it belongs
  # in `mism`; it renders as col(int32!=NA).

  # get variables in codebook whose type is missing or whose type does not
  # match the type on disk
  mism <- got |> inner_join(want, by = "column") |> filter(is.na(type_cb) | arrow_type != type_cb)

  val_check(sprintf("types_%s", d$ds), nrow(mism) == 0,
            detail = if (nrow(mism) == 0) "all types match"
                     else paste(sprintf("%s(%s!=%s)", mism$column, mism$arrow_type, mism$type_cb),
                                collapse = ", "))

  # (c) partition continuity: the latest FWD source month must have landed in the hive.

  # get year-month counts from hive
  pc <- ds |> count(year, month) |> collect() |> arrange(year, month)

  readr::write_csv(transmute(pc, dataset = d$ds, year, month, n_rows = n),
                   file.path(out_dir, sprintf("ex4_partitions_%s.csv", d$ds)), na = "")

  # get the max year-month in FWD source
  stg_max <- open_dataset(d$fwd, partitioning = c("year", "month"), unify_schemas = TRUE) |>
    count(year, month) |> collect() |> ym() |> max()

  # get the max year-month in the hive
  fin_max <- max(ym(pc))

  val_check(sprintf("latest_month_%s", d$ds), fin_max >= stg_max,
            detail = sprintf("final max %d vs FWD source max %d", fin_max, stg_max))

  # Full partition presence: no year-month missing from (or unexpected in) the hive's
  # expected calendar. Cadence-aware -- emp is quarterly (months 3,6,9,12) through
  # 2011-06 then monthly; acc/sep are monthly throughout.
  # A cadence change at the OPM source trips this and prompts updating the spec here.
  # We check (year, month) partition presence, not the hive's end-of-period as-of date.
  # Any day works so long as it's the same everywhere; we use the first of the month.

  # Get a vector of observed periods on disk
  present <- sort(unique(to_date(pc$year, pc$month)))

  if (d$cadence == "monthly") {
    # get a vector of expected periods based on beginning date, end date,
    # and cadence
    expected <- seq(min(present), max(present), by = "month")
  } else {                                          # quarterly thru 2011-06, then monthly
    # get a vector of expected periods based on beginning date, end date,
    # and cadence accounting for the transition from quarterly reporting to
    # monthly reporting
    q_part <- seq(min(present), as.Date("2011-06-01"), by = "3 months")
    m_part <- seq(as.Date("2011-07-01"), max(present), by = "month")
    expected <- c(q_part, m_part)
  }

  # get expected periods not on disk
  missing <- expected[!(expected %in% present)] # %in% keeps Date class (setdiff would strip it)

  # get periods on disk that are unexpected
  extra   <- present[!(present %in% expected)]

  # Report results
  val_check(sprintf("partitions_%s", d$ds), length(missing) == 0 && length(extra) == 0,
            detail = if (length(missing) == 0 && length(extra) == 0) "no gaps in the partition calendar"
                     else sprintf("missing=[%s] extra=[%s]",
                                  paste(format(missing, "%Y-%m"), collapse = " "),
                                  paste(format(extra, "%Y-%m"), collapse = " ")))

  # Size anomaly (soft): is the newest period grossly off its recent typical size, high or
  # low? Low guards source-side truncation (a partial/cut-short download); high guards
  # duplication/inflation. 03_reconcile.R catches neither -- it only confirms source ==
  # final and takes a bad source as ground truth. Band is loose (0.5x-2x of the prior-6
  # median) since flows fluctuate seasonally, so only a gross deviation warns.

  # Sort FWD year-months on disk
  fwd_pc <- filter(pc, year >= 2005) |> arrange(year, month)

  if (nrow(fwd_pc) >= 7) {

    last_n <- tail(fwd_pc$n, 1) # get the most recent n

    med6   <- median(tail(fwd_pc$n, 7)[1:6]) # get median of the previous 6 periods

    # Fire if the most recent n is less than half the median or greater than
    # twice the median
    val_check(sprintf("size_%s", d$ds), last_n >= 0.5 * med6 && last_n <= 2 * med6, severity = "soft",
              detail = sprintf("newest=%d vs prior-6 median=%.0f", last_n, med6))
  }
}

val_finish(file.path(out_dir, "ex4_checks.csv"))

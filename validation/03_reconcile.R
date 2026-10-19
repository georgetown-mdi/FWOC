# Author: Mark Richardson, Benjamin Reese, and Claude Code
# Purpose: Ex.3 -- row-count reconciliation. Assembly must neither gain nor lose
#          rows: source counts (BZ raw FOIA hives, year<2005; FWD data.opm.gov
#          staging hives) must equal the amalgamated-hive counts for the same
#          year/month. Writes a per-comparison CSV and asserts every diff is 0,
#          or matches an allowlisted known exception.

library(arrow)
library(dplyr)
source("utils/validation_functions.R")

out_dir   <- "validation_output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)  # ensure output dir exists for standalone runs
allowlist <- load_allowlist("metadata/validation/baselines/ex3_reconcile_allowlist.csv")

# Six source->final comparisons, one per (dataset, era). BZ source = raw FOIA hive
# (filtered <2005, the slice that reaches the final hive); FWD source = the full
# data.opm.gov staging hive, which starts at 2005 and so needs no filter. Final is
# filtered to match the era.
specs <- list(
  list(ds = "emp", era = "BZ", src = "buzz_feed/bz_status_hive_raw", part = c("year", "quarter"), fin = "emp_hive"),
  list(ds = "emp", era = "FWD", src = "data_opm_gov/fwd_emp_hive",    part = c("year", "month"),   fin = "emp_hive"),
  list(ds = "acc", era = "BZ", src = "buzz_feed/bz_acc_hive_raw",     part = c("year", "quarter"), fin = "acc_hive"),
  list(ds = "acc", era = "FWD", src = "data_opm_gov/fwd_acc_hive",    part = c("year", "month"),   fin = "acc_hive"),
  list(ds = "sep", era = "BZ", src = "buzz_feed/bz_sep_hive_raw",     part = c("year", "quarter"), fin = "sep_hive"),
  list(ds = "sep", era = "FWD", src = "data_opm_gov/fwd_sep_hive",    part = c("year", "month"),   fin = "sep_hive")
)

# Count rows by (year, month). `yr`: "lt" = year<2005, "ge" = year>=2005, "none".
count_ym <- function(path, part, yr = c("none", "lt", "ge")) {
  yr <- match.arg(yr)
  d <- open_dataset(path, partitioning = part, unify_schemas = TRUE)
  if (yr == "lt") d <- filter(d, year < 2005)
  if (yr == "ge") d <- filter(d, year >= 2005)
  d |> count(year, month) |> collect()
}

al <- allowlist |>
  mutate(year = as.integer(year), month = as.integer(month),
         expected_diff = as.integer(expected_diff))

for (s in specs) {
  src <- count_ym(s$src, s$part,             yr = if (s$era == "BZ") "lt" else "none")
  fin <- count_ym(s$fin, c("year", "month"), yr = if (s$era == "BZ") "lt" else "ge")

  cmp <- full_join(src, fin, by = c("year", "month"), suffix = c("_src", "_fin")) |>
    mutate(dataset = s$ds, era = s$era,
           n_src = coalesce(n_src, 0L), n_fin = coalesce(n_fin, 0L),
           diff  = n_src - n_fin) |>
    arrange(year, month) |>
    select(dataset, era, year, month, n_src, n_fin, diff)

  readr::write_csv(cmp, file.path(out_dir, sprintf("ex3_reconcile_%s_%s.csv", s$ds, s$era)), na = "")

  # A nonzero diff passes only if it matches an allowlisted expected_diff.
  bad <- cmp |>
    filter(diff != 0) |>
    left_join(al, by = c("dataset", "era", "year", "month")) |>
    filter(is.na(expected_diff) | diff != expected_diff)

  val_check(
    sprintf("reconcile_%s_%s", s$ds, s$era),
    nrow(bad) == 0,
    detail = if (nrow(bad) == 0) "all periods reconcile"
             else paste0(nrow(bad), " unexplained: ",
                         paste(sprintf("%d-%02d %+d", bad$year, bad$month, bad$diff), collapse = ", ")))
}

val_finish(file.path(out_dir, "ex3_checks.csv"))

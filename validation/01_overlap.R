# Author: Mark Richardson, Benjamin Reese, and Claude Code
# Purpose: Validate the BZ data using the period where BZ and FWD overlap.
#   The raw BZ FOIA hives retain 2005+ rows (the staging cut to <=2004 happens in script
#   05), so BZ and FWD overlap: emp 2005Q1-2017Q1, acc/sep 2005Q1-2016Q2. BZ is
#   quarterly, FWD monthly -> align on (year, quarter) (emp: FWD quarter-end month;
#   acc/sep: FWD summed to quarter). Duplicated pseudo_ids are kept.
#   acc/sep stop at 2016Q2: the BZ dynamic FOIA ends mid-quarter at 2016-08
#   (eff_date max), so calendar Q3 2016 is partial (Jul-Aug only) and is dropped.
#   Duty location is compared as two variables rather than one, because the sources
#   carry the 9-digit code over different spans: duty_loc9 covers 2010Q1-2014Q2, where
#   both sides have it, and duty_loc2 the 2-char state/country/territory code across
#   the whole overlap (loc_canon, VAR_WIN).
#
#   The FIRST run is a discovery run: it writes agency- and variable-level BZ-vs-FWD
#   count tables for review and SEEDS the FWD baseline (expected discrepancies aren't
#   known a priori; only sep code SI, excluded below, is). Every run after that: BZ is
#   frozen, so any FWD overlap count that changed vs the seeded baseline and isn't in
#   ex1_known_discrepancies is an OPM revision -- reported, never fatal. Nothing in this
#   exercise halts the build; it is a discovery and record-keeping step.

library(arrow)
library(dplyr)
library(stringr)
source("utils/validation_functions.R")
source("utils/derive_functions.R")     # add_los_ord(), shared with 08_fwd_compatibility.R

out_dir  <- "validation_output"
base_dir <- "metadata/validation/baselines"
known    <- load_allowlist(file.path(base_dir, "ex1_known_discrepancies.csv"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)  # ensure output dir exists for standalone runs

# Create the endpoints of the quarters for which each data set overlaps
yqi <- function(y, q) as.integer(y) * 10L + as.integer(q)  # (year,quarter) -> sortable int

OVERLAP <- list(emp = c(yqi(2005, 1), yqi(2017, 1)),
                acc = c(yqi(2005, 1), yqi(2016, 2)),
                sep = c(yqi(2005, 1), yqi(2016, 2)))

# Per-variable window overrides (else the dataset window applies), and the sole gate on
# which quarters reach the output -- the preps compute every quarter and let these
# windows do the cutting, so widening a comparison is a one-line change here.
# duty_loc9 is the only variable that cannot span the full overlap: FWD publishes no
# 9-digit duty_station_code before 2010 and BZ stops carrying one after 2014-Q2.
VAR_WIN <- list(duty_loc9 = c(yqi(2010, 1), yqi(2014, 2)))

# FWD length_of_service_years is binned by the SHARED add_los_ord() (utils), the same
# helper 09_ uses, so the comparison below cannot be thrown off by two definitions of
# the brackets drifting apart. Sharing is safe here because the independent side is BZ:
# a wrong bucketing would show up as a large BZ-vs-FWD discrepancy, not a silent pass.
# BZ los_level is mixed-format across releases ("5 - 9 years" vs "5-9",
# "Less than 1 year", "35 or more"); map all to the canonical compact brackets.

los_canon <- function(x) case_when(
  x %in% c("UNSP", "Unspecified", "UNSPECIFIED") ~ "UNSPECIFIED",
  x == "Less than 1 year" ~ "< 1",
  x == "35 or more" ~ "35+",
  TRUE ~ str_replace_all(str_remove(x, " years$"), " - ", "-"))

# BZ age (age_75, age_65 fallback for 2014-2016) -> FWD age_bracket, upper-cased:
# FWD's age_bracket is mixed-case across periods ("65 or more" vs "65 OR MORE").

age_harm <- function(a) str_to_upper(case_when(
  a == "15-19" ~ "Less than 20",
  a %in% c("65-69", "70-74", "75+") ~ "65 or more",
  a == "UNSP" ~ "Unspecified", TRUE ~ a))

# Duty-station codes comingle FIPS (US states), 2-char OPM country codes, and Q
# territory codes, plus sentinels that vary by source and column: runs of # or * (BZ
# duty_stat, and BZ state_code as ## / *), a bare *, and the literal word REDACTED
# (FWD, and BZ state_code from 2014-Q3). Canonicalize both sides to one comparable
# token. EVERY sentinel form collapses to SENTINEL: whether BZ's # runs and * runs
# carry different meanings is unresolved, so this comparison does not distinguish
# them. A redaction is a value the source chose to withhold, so it gets a token; a
# missing value is not, so NA stays NA and is counted as NA -- the same treatment
# every other variable in this comparison gets. Code width is likewise NOT
# normalized: both sides carry full-width codes with leading zeros intact, so a width
# difference would be a real change in what OPM publishes and belongs in the output
# table rather than repaired out of sight.

loc_canon <- function(x) case_when(
  str_detect(x, "^[*#]+$") | x == "REDACTED" ~ "SENTINEL",
  TRUE ~ x)  # NA stays NA; numeric FIPS, alpha country, Q territory codes pass through

# ---- prep: harmonize each side to shared column names, keyed (year, quarter) ----

prep_emp_bz <- function() {
  d <- open_dataset("buzz_feed/bz_status_hive_raw", partitioning = c("year", "quarter"), unify_schemas = TRUE) |>
    filter(year >= 2005)
  has_ds <- all(c("duty_stat", "state_code") %in% names(d))
  d <- d |> mutate(
    agency_subelement_code     = subagency_code,
    agency_code                = str_sub(subagency_code, 1, 2),
    age_bracket                = age_harm(coalesce(age_75, age_65)),
    length_of_service_ord      = los_canon(los_level),
    education_level_code        = if_else(str_detect(education_code, "^[0-9]$"), paste0("0", education_code), education_code),
    occupational_series_code    = occ_code,
    occupational_category_code  = occ_cat_code,
    supervisory_status_code     = super_stat_code,
    appointment_type_code       = appt_type_code,
    work_schedule_code          = work_sched_code,
    nsftp_indicator             = if_else(as.integer(nsftp_code) == 1, 1L, 0L))
  # BZ carries the 9-digit duty_stat only through 2014-Q2 and the 2-char state_code
  # only from 2014-Q3 (the two are exactly complementary -- never both populated), so
  # the 2-char series has to be stitched from both. Truncate BEFORE canonicalizing:
  # BZ's sentinels are repeated characters, so #########/********* survive str_sub as
  # ##/** and still match loc_canon; canonicalizing first would leave "SE".
  if (has_ds)
    d <- d |> mutate(
      duty_loc9 = loc_canon(duty_stat),
      duty_loc2 = loc_canon(if_else(year * 100L + month <= 201406L,
                                    str_sub(duty_stat, 1, 2), state_code)))
  list(data = d, has_ds = has_ds)
}

prep_emp_fwd <- function() {
  open_dataset("data_opm_gov/fwd_emp_hive", partitioning = c("year", "month"), unify_schemas = TRUE) |>
    filter(month %in% c(3L, 6L, 9L, 12L)) |>                       # quarter-end snapshot
    add_los_ord() |>
    # Note: case_when() is first-match wins, meaning once a row returns TRUE it
    # is not recoded by subsequent conditions.
    mutate(quarter     = case_when(month <= 3 ~ 1L, month <= 6 ~ 2L, month <= 9 ~ 3L, month <= 12 ~ 4L),
           agency_code = str_sub(agency_subelement_code, 1, 2),
           age_bracket = str_to_upper(age_bracket),
           nsftp_indicator = case_when(nsftp_indicator == "Y" ~ 1L, nsftp_indicator == "N" ~ 0L, TRUE ~ NA_integer_),
           # The 2-char comingled code is one variable OPM published under two names:
           # duty_station_state_code through 2014, and the state/country/territory
           # column from 2010. They coexist 2010-2014 and agree on every row, so the
           # coalesce spans the rename and covers the whole overlap. Do not rebuild it
           # from duty_station_state_code + duty_station_country_code: country_code
           # reads US for Puerto Rico / Guam / the Virgin Islands, collapsing the
           # RQ/GQ/VQ codes both sources otherwise keep. Nor take the first 2 chars of
           # duty_station_code -- FWD's sentinel is the word REDACTED, which truncates
           # to a bogus "RE"; the 2-char columns carry it whole.
           duty_loc9 = loc_canon(duty_station_code),
           duty_loc2 = loc_canon(coalesce(duty_station_state_country_territory_code,
                                          duty_station_state_code)))
}

# acc/sep: BZ raw lacks education/supervisory/nsftp and the duty state_code column,
# so those are dropped here; the category code is added.
prep_dyn_bz <- function(path, cat_in, cat_out, drop_si = FALSE) {
  d <- open_dataset(path, partitioning = c("year", "quarter"), unify_schemas = TRUE) |> filter(year >= 2005)
  if (drop_si) d <- d |> filter(is.na(!!sym(cat_in)) | !!sym(cat_in) != "SI")
  d |> mutate(
    agency_subelement_code      = subagency_code,
    agency_code                 = str_sub(subagency_code, 1, 2),
    age_bracket                 = age_harm(age_75),
    length_of_service_ord       = los_canon(los_level),
    occupational_series_code    = occ_code,
    occupational_category_code  = occ_cat_code,
    appointment_type_code       = appt_type_code,
    work_schedule_code          = work_sched_code,
    !!cat_out := !!sym(cat_in))
}

prep_dyn_fwd <- function(path) {
  open_dataset(path, partitioning = c("year", "month"), unify_schemas = TRUE) |>
    add_los_ord() |>
    mutate(quarter     = case_when(month <= 3 ~ 1L, month <= 6 ~ 2L, month <= 9 ~ 3L, month <= 12 ~ 4L),
           agency_code = str_sub(agency_subelement_code, 1, 2),
           age_bracket = str_to_upper(age_bracket))
}

# ---- compare one harmonized variable, restricted to the overlap window ----

agg <- function(ds, var) ds |> mutate(value = !!sym(var)) |> count(year, quarter, value) |> collect()

compare_var <- function(bz, fwd, var, dataset) {
  # bz is the prepped bz source data
  # fwd is the prepped fwd source data
  # var is a string variable name to be compared
  # dataset is a string indicating the data set: emp, acc, or sep

  # Use the variable's own window when VAR_WIN overrides it, else the dataset's
  # overlap window
  win <- if (!is.null(VAR_WIN[[var]])) VAR_WIN[[var]] else OVERLAP[[dataset]]

  # Join the two count tables on (year, quarter, value). FULL join: a value that
  # appears on only one side must survive, since a code present in BZ and absent
  # from FWD (or vice versa) is exactly the discrepancy being looked for -- an
  # inner join would silently drop it. The coalesce below reads that side as 0.
  full_join(rename(agg(bz, var), n_bz = n), rename(agg(fwd, var), n_fwd = n),
            by = c("year", "quarter", "value")) |>
    # Subset to the evaluation window
    filter(yqi(year, quarter) >= win[1], yqi(year, quarter) <= win[2]) |>
    mutate(dataset = dataset, variable = var,
           n_bz = coalesce(n_bz, 0L), n_fwd = coalesce(n_fwd, 0L), diff = n_bz - n_fwd) |>
    # Load-bearing, and separate on purpose: nsftp_indicator is built as 1L/0L in
    # both preps, so its `value` arrives integer while every other variable's is
    # character. run_dataset() binds all the variables into one frame, and that
    # bind errors on a character/integer mix -- so cast before it gets there.
    mutate(value = as.character(value)) |>
    select(dataset, variable, year, quarter, value, n_bz, n_fwd, diff)
}

# Compare vers in bz and fwd in dataset (emp, acc, sep)
run_dataset <- function(dataset, bz, fwd, vars) {
  # Arrow's count() does not guarantee row order, so sort before writing. These CSVs are
  # read by hand during the discovery review and they also feed the FWD baseline, and both
  # jobs need two runs to be diffable. arrange() sorts in the C locale, so the order does
  # not depend on the machine. (variable, year, quarter, value) is unique within a dataset,
  # so the sort is a total order and the file is byte-reproducible.

  # v supplies each element of vars to compare_var() in lapply()
  out <- bind_rows(lapply(vars, function(v) compare_var(bz, fwd, v, dataset))) |>
    arrange(variable, year, quarter, value)
  readr::write_csv(out, file.path(out_dir, sprintf("ex1_overlap_%s.csv", dataset)), na = "")
  out
}

# ---- run all three datasets ----

# Prep emp_bz data and create has_ds logical variable (both returned in a list)
emp_bz <- prep_emp_bz()

# Create a vector of variables to compare
emp_vars <- c("agency_code", "agency_subelement_code", "age_bracket", "length_of_service_ord",
              "education_level_code", "pay_plan_code", "supervisory_status_code",
              "appointment_type_code", "work_schedule_code", "occupational_series_code",
              "occupational_category_code", "nsftp_indicator",
              # A bare if returns NULL when false and c() drops NULL, so the two
              # duty_loc entries simply do not enter the vector when BZ lacks the
              # columns -- compare_var() is never asked for a variable that is absent.
              if (emp_bz$has_ds) c("duty_loc9", "duty_loc2"))

val_check("ex1_emp_duty_loc", emp_bz$has_ds, severity = "soft",
          detail = if (!emp_bz$has_ds) "duty_loc skipped (state_code/duty_stat absent from BZ raw)"
                   else paste("duty_loc9 compared 2010-Q1..2014-Q2 (FWD publishes no 9-digit code",
                              "before 2010, BZ none after 2014-Q2); duty_loc2 spans the full overlap.",
                              "Expect BZ > FWD redaction; see ex1_known_discrepancies."))

# duty_loc9's 2010-Q1 start (VAR_WIN) rests on FWD publishing no 9-digit
# duty_station_code before 2010. OPM backfills retroactively, so assert it: without
# this, a backfill would silently leave five comparable years out of the comparison
# rather than announcing that the window can be widened. Soft -- a backfill is good
# news about the data, not a build failure. One column read, no row scan.

# prep the fwd data
ds_pre2010 <- open_dataset("data_opm_gov/fwd_emp_hive", partitioning = c("year", "month"),
                           unify_schemas = TRUE) |>
  filter(year <= 2009) |>
  summarise(rows = n(), populated = sum(!is.na(duty_station_code), na.rm = TRUE)) |>
  collect()

val_check("ex1_fwd_duty_station_pre2010", ds_pre2010$populated == 0, severity = "soft",
          detail = if (ds_pre2010$populated == 0)
                     "FWD publishes no 9-digit duty_station_code before 2010; duty_loc9 correctly starts 2010-Q1"
                   else paste0(ds_pre2010$populated, " of ", ds_pre2010$rows,
                               " pre-2010 FWD rows now carry duty_station_code -- OPM backfilled;",
                               " widen the duty_loc9 window in VAR_WIN"))

# Create a vector of variables for comparison in acc/sep data
dyn_vars <- c("agency_code", "agency_subelement_code", "age_bracket", "length_of_service_ord",
              "pay_plan_code", "appointment_type_code", "work_schedule_code",
              "occupational_series_code", "occupational_category_code")

out_emp <- run_dataset("emp", emp_bz$data, prep_emp_fwd(), emp_vars)

out_acc <- run_dataset("acc",
                       prep_dyn_bz("buzz_feed/bz_acc_hive_raw", "acc_code", "accession_category_code"),
                       prep_dyn_fwd("data_opm_gov/fwd_acc_hive"),
                       c(dyn_vars, "accession_category_code"))

out_sep <- run_dataset("sep",
                       prep_dyn_bz("buzz_feed/bz_sep_hive_raw", "sep_code", "separation_category_code", drop_si = TRUE),
                       prep_dyn_fwd("data_opm_gov/fwd_sep_hive"),
                       c(dyn_vars, "separation_category_code"))

# ---- FWD revision guard: seed on first run, report un-allowlisted changes monthly ----

# Determine whether changes in the BZ-vs-FWD comparison are due to revisions in FWD.
# The working rule is that the current OPM version is authoritative, so a revision is
# never an error. What this guard protects is the RECORD: the discrepancies measured
# during the discovery review and written into ex1_known_discrepancies.csv with specific
# numbers. A revision inside the overlap silently falsifies those numbers, and this is
# what says so. Scope is the overlap window only, because that is all the comparison
# covers -- 05_sanity.R watches the full FWD span for re-versioned periods.

# Bind the three comparison tables, then reduce to the FWD side: drop the BZ columns
# and any cell FWD has no rows for. A cell that later falls to zero likewise leaves
# this table, which is how the baseline detects it as vanished.
fwd_all <- bind_rows(out_emp, out_acc, out_sep) |> filter(n_fwd > 0) |>
  select(dataset, variable, year, quarter, value, n_fwd)

# Path to the baseline; compare_to_baseline() reads it, and only if it exists
base_path <- file.path(base_dir, "ex1_fwd_overlap_baseline.csv")

# Compare current FWD counts to baseline counts if baseline exists
cmp <- compare_to_baseline(fwd_all, base_path,
                           key = c("dataset", "variable", "year", "quarter", "value"), value_var = "n_fwd")

# if no baseline counts (i.e., cmp = NULL), write current counts as the baseline
if (is.null(cmp)) {
  write_baseline_if_absent(fwd_all, base_path)
  val_check("ex1_fwd_overlap", TRUE, "V1: seeded FWD overlap baseline (report-only)", severity = "soft")
} else {
  # Extract changes from baseline and compare to known discrepancies
  changed <- cmp |> filter(changed)
  if (nrow(known) > 0) {
    # An allowlist row suppresses a changed cell only WITHIN its
    # [year_from/quarter_from .. year_to/quarter_to] window -- a revision OUTSIDE the
    # documented window must still surface (else a future genuine OPM revision is
    # masked). A blank `value` wildcards all values of that dataset+variable (a
    # wholesale retroactive backfill, e.g. education_level_code 2009-2016); a
    # non-blank `value` matches that value only.

    # Reshape the allowlist for the join: rename value -> allow_value so it does not
    # collide with changed$value, and fold the four window columns into two sortable
    # yqi integers for the range test below.
    k <- known |> transmute(dataset, variable, allow_value = value,
                            yq_from = yqi(year_from, quarter_from),
                            yq_to   = yqi(year_to, quarter_to))

    # Subset changed to known changes
    suppressed <- changed |>
      mutate(yq = yqi(year, quarter)) |>
      inner_join(k, by = c("dataset", "variable"), relationship = "many-to-many") |>
      filter(is.na(allow_value) | allow_value == "" | allow_value == value,
             yq >= yq_from, yq <= yq_to) |>
      distinct(dataset, variable, year, quarter, value)

    # Subset changed to new changes
    changed <- anti_join(changed, suppressed,
                         by = c("dataset", "variable", "year", "quarter", "value"))
  }

  # Record the results of the comparison. SOFT: the current OPM version is authoritative
  # by working rule, so a revision is new truth rather than a defect and must not halt
  # the build. What a failure here means is that the figures recorded in
  # ex1_known_discrepancies.csv were measured against superseded data and should be
  # re-checked against this run's ex1_overlap_*.csv.
  val_check("ex1_fwd_overlap", nrow(changed) == 0, severity = "soft",
            detail = if (nrow(changed) == 0) "FWD overlap counts unchanged vs baseline"
                     else paste0(nrow(changed), " FWD cell(s) changed (revision): ",
                                 paste(head(sprintf("%s/%s %s-Q%s %s:%+d", changed$dataset, changed$variable,
                                       changed$year, changed$quarter, changed$value, changed$delta), 10), collapse = "; ")))
}

# Bind the results, write them to disk, then halt on any failed hard check -- the
# stop() exits Rscript non-zero, which is what makes the orchestrator stop. The CSV
# is written first, so the record survives the halt.
val_finish(file.path(out_dir, "ex1_checks.csv"))

# Author: Mark Richardson and Claude Code
# Purpose: Ex.2 -- missing-label validation. For every OPM code->label pair, a row
#   whose code is present but label is NA must be either an invalid code or a valid
#   code with no valid label for that as-of date -- never a valid, in-range code
#   (which would be a labeling bug). Reuses the engine in utils/inspect_vars.R:
#   buildInventory() -> checkLabelGaps() classifies each gap invalid/out_of_range/
#   unexpected; we assert no un-allowlisted `unexpected`. We also assert every
#   observed code (sentinels aside) is a documented OPM code in the master lookup --
#   the `invalid` class, surfaced as its own per-variable check (soft). Each pair is
#   checked in every hive it appears in (emp uses the end-of-period date; acc/sep use
#   first-of-month, per their label-join convention); acc/sep category codes also
#   get a static membership check. FWD-source-labeled pairs have no OPM lookup and
#   are out of scope. A separate integrity check asserts each lookup has no per-code
#   overlapping date windows (which would inflate rows in the time-varying join --
#   see setup/build_labels.R).

library(arrow)
library(dplyr)
source("utils/inspect_vars.R")
source("utils/validation_functions.R")
source("utils/code_table_functions.R")   # shared label_window_overlaps()

out_dir   <- "validation_output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)  # ensure output dir exists for standalone runs
inv_dir   <- "metadata/na_treatment/inventory"
chk_dir   <- "metadata/na_treatment/checks"
lab_dir   <- "metadata/labels"
allowlist  <- load_allowlist("metadata/validation/baselines/ex2_label_gap_allowlist.csv")
code_allow <- load_allowlist("metadata/validation/baselines/ex2_invalid_code_allowlist.csv")

emp <- open_dataset("emp_hive", partitioning = c("year", "month"), unify_schemas = TRUE)
acc <- open_dataset("acc_hive", partitioning = c("year", "month"), unify_schemas = TRUE)
sep <- open_dataset("sep_hive", partitioning = c("year", "month"), unify_schemas = TRUE)

# Time-varying OPM pairs and their [date_from, date_to] lookup tables
pairs <- list(
  list(code = "agency_code",                 label = "agency",                 var = "agency",                 lkp = "agency_labels.csv"),
  list(code = "agency_subelement_code",      label = "agency_subelement",      var = "agency_subelement",      lkp = "subagency_labels.csv"),
  list(code = "agency_code_fwoc",            label = "agency_fwoc",            var = "agency_fwoc",            lkp = "agency_labels_fwoc.csv"),
  list(code = "agency_subelement_code_fwoc", label = "agency_subelement_fwoc", var = "agency_subelement_fwoc", lkp = "subagency_labels_fwoc.csv"),
  list(code = "agency_code_fwoc",            label = "agency_fwoc_experimental",            var = "agency_fwoc_experimental",            lkp = "agency_labels_fwoc_experimental.csv"),
  list(code = "agency_subelement_code_fwoc", label = "agency_subelement_fwoc_experimental", var = "agency_subelement_fwoc_experimental", lkp = "subagency_labels_fwoc_experimental.csv"),
  list(code = "education_level_code",        label = "education_level",        var = "education_level",        lkp = "ed_labels.csv"),
  list(code = "appointment_type_code",       label = "appointment_type",       var = "appointment_type",       lkp = "appt_type_labels.csv"),
  list(code = "pay_plan_code",               label = "pay_plan",               var = "pay_plan",               lkp = "pay_plan_labels.csv"),
  list(code = "supervisory_status_code",     label = "supervisory_status",     var = "supervisory_status",     lkp = "super_stat_labels.csv"),
  list(code = "work_schedule_code",          label = "work_schedule",          var = "work_schedule",          lkp = "work_sched_labels.csv"),
  list(code = "occupational_series_code",    label = "occupational_series",    var = "occupational_series",    lkp = "occ_labels.csv"),
  list(code = "occupational_category_code",  label = "occupational_category",  var = "occupational_category",  lkp = "occ_cat_labels.csv")
)

# emp joins labels on the end-of-period date; acc/sep on first-of-month.
hives <- list(list(name = "emp", ds = emp, fom = FALSE),
              list(name = "acc", ds = acc, fom = TRUE),
              list(name = "sep", ds = sep, fom = TRUE))

unexp <- list()
undoc <- list()
for (i in seq_along(hives)) {
  H <- hives[[i]]
  cols <- names(H$ds)
  for (j in seq_along(pairs)) {
    p <- pairs[[j]]
    if (!all(c(p$code, p$label) %in% cols)) next   # skip a pair absent from this hive
    vn <- paste0(p$var, "_", H$name)               # namespace the inventory/check files per hive

    # Build the code-label count by date inventory
    inv <- buildInventory(data = H$ds, code_col = p$code, label_col = p$label,
                          var_name = vn, first_of_month = H$fom)

    res <- checkLabelGaps(
      inventory_path = file.path(inv_dir, paste0("inventory_", vn, ".csv")),
      lookup_path    = file.path(lab_dir, p$lkp),
      code_col       = p$code, label_col = p$label,
      out_path       = file.path(chk_dir, paste0("check_", vn, ".csv")))

    # `unexpected` = valid code, observation date inside a label window, yet NA label.
    # Get bad (i.e., unexpected missing labels)
    bad <- res |> filter(status == "unexpected") |> mutate(dataset = H$name, variable = p$var)
    # If allowlist has entries, remove them from the bad list
    if (nrow(allowlist) > 0)
      bad <- anti_join(bad, filter(allowlist, variable == p$var), by = c("variable", "code"))

    # Accumulate the bad list
    unexp[[vn]] <- bad

    val_check(paste0("labels_", H$name, "_", p$var), nrow(bad) == 0,
              detail = if (nrow(bad) == 0) "no unexpected gaps"
                       else paste0(nrow(bad), " unexpected: ", paste(unique(bad$code), collapse = ",")))

    # Code validity: every observed code must be a documented OPM code (present in the
    # master lookup, ignoring the date window -- date-aware validity is the gap check's
    # job). The gap check only inspects NA-label rows and asserts `unexpected`; this
    # asserts the codes themselves -- the `invalid` class -- as its own soft line per
    # variable. Sentinels (REDACTED / #### / *) are deliberately absent from the
    # masters, so drop them here; the gap check and sentinel flagger cover those.

    # Get a vector of valid codes
    lkp_codes <- readr::read_csv(file.path(lab_dir, p$lkp), na = "", show_col_types = FALSE,
                                 col_types = readr::cols(.default = "c")) |>
      distinct(code) |> pull(code)

    # Get a vector of observed codes
    obs_codes <- unique(inv[[p$code]])

    # Drop NA codes and codes that match sentinel lexicon
    obs_codes <- obs_codes[!is.na(obs_codes) & !grepl(SENTINEL_LEXICON, obs_codes, perl = TRUE)]

    # Get the observed codes not in the lookup vector
    miss <- tibble::tibble(dataset = H$name, variable = p$var,
                           code = setdiff(obs_codes, lkp_codes))

    # Remove any codes in the allowlist
    if (nrow(code_allow) > 0)
      miss <- anti_join(miss, filter(code_allow, variable == p$var), by = c("variable", "code"))

    # Accumulate the invalid codes
    undoc[[vn]] <- miss
    val_check(paste0("codes_", H$name, "_", p$var), nrow(miss) == 0, severity = "soft",
              detail = if (nrow(miss) == 0) "all observed codes documented"
                       else paste0(nrow(miss), " undocumented: ", paste(head(miss$code, 20), collapse = ",")))
  }
}

readr::write_csv(bind_rows(unexp), file.path(out_dir, "ex2_unexpected_summary.csv"), na = "")
readr::write_csv(bind_rows(undoc), file.path(out_dir, "ex2_undocumented_codes.csv"), na = "")

# ---- label-table integrity: per-code date windows must not overlap ----
# A code with overlapping [date_from, date_to] windows matches >1 label row in the
# time-varying join, inflating hive rows. For the GENERATED lookups,
# setup/build_labels.R prevents that at build time: normalize_label_text() collapses
# provably-cosmetic variants, metadata/code_tables/overlap_resolutions.csv resolves
# genuine conflicts by hand, and assert_no_label_overlaps() hard-fails on anything
# left -- so for those this is a post-build backstop, as it is for the two
# *_labels_fwoc.csv files, which setup/build_fwoc_ship_labels.R guards the same way.
# The two *_labels_fwoc_experimental.csv files are different: nothing in the repo
# writes them, so they get no build-time guard and this check is the ONLY one they
# receive. 03_reconcile.R is the end-to-end row-count check either way.
ov_allow <- load_allowlist("metadata/validation/baselines/ex2_overlap_allowlist.csv")

# label_window_overlaps() is the shared detector in utils/code_table_functions.R
# (sourced above). setup/build_labels.R's build-time guard and
# setup/check_opm_metadata.R's pre-build check call the same function, so where a
# generator does guard a lookup, its guard and this one cannot drift. It also parses
# dates strictly, which an inline copy using bare as.Date() could not: as.Date()
# infers its format from the first element, so a malformed date in any later row
# becomes a silent NA, then an integer.max "open" window that swallows every later
# window of that code and hard-fails the build on an overlap that does not exist.
overlapping_codes <- function(lkp_path) {
  d <- readr::read_csv(lkp_path, col_types = readr::cols(.default = "c"), na = "",
                       show_col_types = FALSE)
  label_window_overlaps(d) |> distinct(code)
}

# gather the lookup file named in each pair, then check each unique one
lkps <- character(length(pairs))
for (i in seq_along(pairs)) lkps[i] <- pairs[[i]]$lkp
lkps <- unique(lkps)

ov_all <- list()
for (i in seq_along(lkps)) {
  lk   <- lkps[i]
  stem <- gsub("_labels|\\.csv", "", lk) # agency_labels_fwoc.csv -> agency_fwoc
  bad  <- overlapping_codes(file.path(lab_dir, lk))
  if (nrow(ov_allow) > 0)
    bad <- anti_join(bad, filter(ov_allow, lookup == lk), by = "code")
  if (nrow(bad) > 0) ov_all[[lk]] <- transmute(bad, lookup = lk, code)
  val_check(paste0("overlap_", stem), nrow(bad) == 0,
            detail = if (nrow(bad) == 0) "no overlapping windows"
                     else paste0(nrow(bad), " code(s) overlap: ", paste(bad$code, collapse = ",")))
}
readr::write_csv(bind_rows(ov_all), file.path(out_dir, "ex2_label_overlaps.csv"), na = "")

# acc/sep category: static (dateless) lookup -> any code absent from the lookup is
# an undocumented/invalid code (soft: surfaced for review, not a labeling bug).
static <- list(
  list(ds = acc, code = "accession_category_code",  lkp = "acc_labels.csv", var = "accession_category"),
  list(ds = sep, code = "separation_category_code", lkp = "sep_labels.csv", var = "separation_category"))
for (i in seq_along(static)) {
  s     <- static[[i]]
  lkp   <- readr::read_csv(file.path(lab_dir, s$lkp), col_types = "cc", na = "")
  codes <- s$ds |> filter(!is.na(!!rlang::sym(s$code))) |> distinct(!!rlang::sym(s$code)) |> collect()
  unmatched <- anti_join(codes, lkp, by = s$code)
  val_check(paste0("labels_", s$var), nrow(unmatched) == 0, severity = "soft",
            detail = if (nrow(unmatched) == 0) "all codes documented"
                     else paste0("undocumented: ", paste(unmatched[[s$code]], collapse = ",")))
}

val_finish(file.path(out_dir, "ex2_checks.csv"))

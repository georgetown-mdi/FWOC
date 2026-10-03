# Author: Mark Richardson and Claude Code

library(readr)
library(dplyr)

source("utils/inspect_vars.R")

insp <- "metadata/codebook/inspection"

cb <- read_csv("metadata/codebook/fwoc_codebook_table.csv",
               col_types = cols(.default = "c"), na = "")

# The codebook is ASCII-only (see codebook/codebook_notes.md). Fail loudly
# if any non-ASCII slips in -- almost always "smart" punctuation pasted from an
# OPM PDF/Word source (curly quotes, en/em dashes, non-breaking spaces). We do
# NOT silently clean here: write_csv is lossless on clean input, so a clean
# committed file stays clean. To fix, run normalizeCodebookText() (defined in
# utils/inspect_vars.R), then re-run this script.

bad <- codebookNonAscii(cb)

if (nrow(bad) > 0) {
  cat("Non-ASCII characters found in the codebook (must be ASCII-only):\n")
  print(bad)
  stop("Codebook contains non-ASCII. Run normalizeCodebookText() to fix it, ",
       "then re-run 02_codebook_build.R.")
}

# Store a per-variable result. A variable can be computed from more than one
# coverage file (eff_date and personnel_action_effective_date_yyyymm each have
# an acc and a sep file); when that happens, assert the two agree rather than
# silently taking the first.
addResult <- function(v, res) {
  prev <- results[[v]]
  if (!is.null(prev)) {
    same <- identical(prev$era, res$era) &&
            identical(prev$date_from, res$date_from) &&
            identical(prev$date_to, res$date_to) &&
            identical(prev$gaps_flag, res$gaps_flag) &&
            identical(prev$missing, res$missing)
    if (!same) {
      stop("Coverage mismatch for '", v, "' across its source files: (",
           prev$era, " ", prev$date_from, "..", prev$date_to,
           " gaps=", prev$gaps_flag, " missing=", prev$missing, ") vs (",
           res$era, " ", res$date_from, "..", res$date_to,
           " gaps=", res$gaps_flag, " missing=", res$missing, ")")
    }
    return(invisible())
  }
  results[[v]] <<- res
}

# ====================================================================
# Process coverage CSVs
# ====================================================================

results <- list()

# Single-variable coverage CSVs
single_files <- list(
  list(f = "coverage_emp_duty_station_code.csv",        v = "duty_station_code"),
  list(f = "coverage_emp_duty_station_city.csv",        v = "duty_station_city"),
  list(f = "coverage_emp_snapshot_yyyymm.csv",          v = "snapshot_yyyymm"),
  list(f = "coverage_emp_cfo_act_agency_indicator.csv", v = "cfo_act_agency_indicator"),
  list(f = "coverage_emp_grade.csv",                    v = "grade"),
  list(f = "coverage_emp_veteran_indicator.csv",        v = "veteran_indicator"),
  list(f = "coverage_emp_pres_admin.csv",               v = "pres_admin"),
  list(f = "coverage_emp_pseudo_id.csv",                v = "pseudo_id"),
  list(f = "coverage_emp_name.csv",                     v = "name"),
  list(f = "coverage_emp_duty_station_state_country_territory_code.csv",
       v = "duty_station_state_country_territory_code"),
  list(f = "coverage_emp_personnel_office_identifier_code.csv",
       v = "personnel_office_identifier_code"),
  list(f = "coverage_emp_service_computation_date_leave.csv",
       v = "service_computation_date_leave"),
  list(f = "coverage_acc_eff_date.csv",                 v = "eff_date"),
  list(f = "coverage_acc_appointment_not_to_exceed_date.csv",
       v = "appointment_not_to_exceed_date"),
  list(f = "coverage_acc_pathways_group.csv",           v = "pathways_group")
)

for (item in single_files) {
  # Read the coverage file
  cv <- read_csv(file.path(insp, item$f), show_col_types = FALSE)
  addResult(item$v, analyzeCoverage(cv))
}

# Multi-variable coverage CSVs
multi_files <- list(
  list(f = "coverage_emp_date_year_month_quarter.csv",
       vars = c("date", "year", "month", "quarter")),
  list(f = "coverage_emp_agency_agency_code.csv",
       vars = c("agency", "agency_code")),
  list(f = "coverage_emp_agency_subelement_code_agency_subelement.csv",
       vars = c("agency_subelement_code", "agency_subelement")),
  list(f = "coverage_emp_agency_code_fwoc_agency_fwoc_agency_fwoc_experimental.csv",
       vars = c("agency_code_fwoc", "agency_fwoc", "agency_fwoc_experimental")),
  list(f = paste0("coverage_emp_agency_subelement_code_fwoc_agency_subelement_fwoc_",
                  "agency_subelement_fwoc_experimental.csv"),
       vars = c("agency_subelement_code_fwoc", "agency_subelement_fwoc",
                "agency_subelement_fwoc_experimental")),
  list(f = "coverage_emp_department_code_department.csv",
       vars = c("department_code", "department")),
  list(f = "coverage_emp_duty_station_country_code_duty_station_country.csv",
       vars = c("duty_station_country_code", "duty_station_country")),
  list(f = "coverage_emp_duty_station_state_code_duty_station_state_duty_station_state_abbreviation.csv",
       vars = c("duty_station_state_code", "duty_station_state",
                "duty_station_state_abbreviation")),
  list(f = "coverage_emp_duty_station_county_code_duty_station_county.csv",
       vars = c("duty_station_county_code", "duty_station_county")),
  list(f = "coverage_emp_consolidated_statistical_area_code_consolidated_statistical_area.csv",
       vars = c("consolidated_statistical_area_code", "consolidated_statistical_area")),
  list(f = "coverage_emp_core_based_statistical_area_code_core_based_statistical_area.csv",
       vars = c("core_based_statistical_area_code", "core_based_statistical_area")),
  list(f = "coverage_emp_age_75_age_bracket.csv",
       vars = c("age_75", "age_bracket")),
  list(f = "coverage_emp_length_of_service_ord_length_of_service_years.csv",
       vars = c("length_of_service_ord", "length_of_service_years")),
  list(f = "coverage_emp_education_level_code_education_level.csv",
       vars = c("education_level_code", "education_level")),
  list(f = "coverage_emp_education_level_code_education_level_bracket_education_level_bracket_fwoc.csv",
       vars = c("education_level_bracket", "education_level_bracket_fwoc")),
  list(f = "coverage_emp_annualized_adjusted_basic_pay_annualized_adjusted_basic_pay_num_annualized_adjusted_basic_pay_real_2024.csv",
       vars = c("annualized_adjusted_basic_pay", "annualized_adjusted_basic_pay_num",
                "annualized_adjusted_basic_pay_real_2024")),
  list(f = "coverage_emp_locality_pay_area_code_locality_pay_area.csv",
       vars = c("locality_pay_area_code", "locality_pay_area")),
  list(f = "coverage_emp_pay_basis_code_pay_basis.csv",
       vars = c("pay_basis_code", "pay_basis")),
  list(f = "coverage_emp_appointment_type_code_appointment_type.csv",
       vars = c("appointment_type_code", "appointment_type")),
  list(f = "coverage_emp_pay_plan_code_pay_plan.csv",
       vars = c("pay_plan_code", "pay_plan")),
  list(f = "coverage_emp_supervisory_status_code_supervisory_status.csv",
       vars = c("supervisory_status_code", "supervisory_status")),
  list(f = "coverage_emp_work_schedule_code_work_schedule.csv",
       vars = c("work_schedule_code", "work_schedule")),
  list(f = "coverage_emp_nsftp_indicator_nsftp_indicator_fwoc.csv",
       vars = c("nsftp_indicator", "nsftp_indicator_fwoc")),
  list(f = "coverage_emp_occupational_series_code_occupational_series.csv",
       vars = c("occupational_series_code", "occupational_series")),
  list(f = "coverage_emp_occupational_group_code_occupational_group.csv",
       vars = c("occupational_group_code", "occupational_group")),
  list(f = "coverage_emp_occupational_category_code_occupational_category.csv",
       vars = c("occupational_category_code", "occupational_category")),
  list(f = "coverage_emp_occupational_category_code_fwoc_occupational_category_fwoc.csv",
       vars = c("occupational_category_code_fwoc", "occupational_category_fwoc")),
  list(f = "coverage_emp_stem_occupation_type_stem_occupation.csv",
       vars = c("stem_occupation_type", "stem_occupation")),
  list(f = "coverage_emp_bargaining_unit_code_bargaining_unit_bargaining_unit_status.csv",
       vars = c("bargaining_unit_code", "bargaining_unit", "bargaining_unit_status")),
  list(f = "coverage_emp_flsa_category_code_flsa_category.csv",
       vars = c("flsa_category_code", "flsa_category")),
  list(f = "coverage_emp_position_occupied_code_position_occupied.csv",
       vars = c("position_occupied_code", "position_occupied")),
  list(f = "coverage_emp_step_or_rate_type_code_step_or_rate_type.csv",
       vars = c("step_or_rate_type_code", "step_or_rate_type")),
  list(f = "coverage_emp_tenure_code_tenure.csv",
       vars = c("tenure_code", "tenure")),
  list(f = "coverage_emp_exec_branch_leg_branch_jud_branch_a1_court_eop_core_eop.csv",
       vars = c("exec_branch", "leg_branch", "jud_branch", "a1_court", "eop", "core_eop")),
  list(f = "coverage_emp_cpi_u_cpi_index_unrate_us_unrate_state.csv",
       vars = c("cpi_u", "cpi_index", "unrate_us", "unrate_state")),
  list(f = "coverage_acc_accession_category_code_accession_category.csv",
       vars = c("accession_category_code", "accession_category")),
  list(f = "coverage_acc_personnel_action_effective_date_yyyymm.csv",
       vars = "personnel_action_effective_date_yyyymm"),
  list(f = "coverage_sep_separation_category_code_separation_category.csv",
       vars = c("separation_category_code", "separation_category")),
  list(f = "coverage_sep_drp_indicator_drp_indicator_fwoc.csv",
       vars = c("drp_indicator", "drp_indicator_fwoc")),
  list(f = "coverage_sep_eff_date.csv",
       vars = "eff_date"),
  list(f = "coverage_sep_personnel_action_effective_date_yyyymm.csv",
       vars = "personnel_action_effective_date_yyyymm"),
  # POI + territory: derived by 01_codebook_inspection.R (run 01 before 02).
  list(f = "coverage_emp_duty_station_state_territory_duty_station_state_territory_abbreviation.csv",
       vars = c("duty_station_state_territory",
                "duty_station_state_territory_abbreviation")),
  list(f = "coverage_emp_personnel_office_name_poi_city_poi_state_country.csv",
       vars = c("personnel_office_name", "poi_city", "poi_state_country"))
)

for (item in multi_files) {
  cv <- read_csv(file.path(insp, item$f), show_col_types = FALSE)
  for (v in item$vars) {
    na_col <- paste0("n_na_", v)
    if (na_col %in% names(cv)) {
      res <- analyzeCoverage(cv, v)
    } else if ("n_na" %in% names(cv)) {
      res <- analyzeCoverage(cv)
    } else {
      cat("WARNING: cannot find NA column for", v, "in", item$f, "\n")
      next
    }
    addResult(v, res)
  }
}

# ====================================================================
# Build the new codebook columns and write the CSV
# ====================================================================

# No hardcoded fallback: every codebook variable's coverage is derived by
# 01_codebook_inspection.R (including the POI / duty-station-territory columns,
# whose coverage is OPM-controlled). Run 01 before 02 so all coverage CSVs
# exist; the missing_vars check below errors clearly if any are absent.

# Hand-maintained FWOC correction columns: the CSV is the source of truth for
# their coverage (it tracks agency/subagency, which is always present). The
# build neither derives nor overwrites them -- excused from the guard here, and
# their existing values restored after getField below.
skip <- c("opm_label_misleading_agency", "case_agency_experimental",
          "func_id_agency_experimental", "reorg_id_agency_experimental",
          "opm_label_misleading_subagency", "case_subagency_experimental",
          "func_id_subagency_experimental", "reorg_id_subagency_experimental")

# Ensure the coverage results generated above cover all variables
missing_vars <- setdiff(cb$Variable, c(names(results), skip))
if (length(missing_vars) > 0) {
  cat("ERROR: no coverage data or fallback for:\n")
  cat(paste(" ", missing_vars, collapse = "\n"), "\n")
  stop("Every codebook variable needs a coverage result before writing.")
}

# Aligned vectors in codebook row order. NA era (entirely-NA var) -> "".
getField <- function(field) {
  vapply(cb$Variable, function(v) {
    # get the result of analyzeCoverage() for field for variable v
    val <- results[[v]][[field]]
    # convert val to character or populate missing/empty values with an
    # empty string
    if (is.null(val) || is.na(val)) "" else as.character(val)
  },
  FUN.VALUE = character(1), # Require each call to return a character vector of length 1
  USE.NAMES = FALSE) # Don't name the vector
}

era_v  <- getField("era")
from_v <- getField("date_from")
to_v   <- getField("date_to")
gaps_v <- getField("gaps_flag")
miss_v <- getField("missing")

# Preserve hand-maintained rows: restore their existing codebook values so the
# build never blanks them (getField returned "" for skip vars, which are not
# in `results`).

# Vector whose element is TRUE if the row should be skipped in the
# automated codebook build because the variable is hand-maintained.
keep <- cb$Variable %in% skip

restoreSkip <- function(vec, col) {
  # if any keep = TRUE rows exist and the col is in the codebook
  if (any(keep) && col %in% names(cb)) {
    val <- cb[[col]][keep] # get the values of col for the hand-maintained rows
    val[is.na(val)] <- "" # Write any NA to empty
    vec[keep] <- val # Write the hand-maintained rows into vec to preserve values
  }
  vec
}
era_v  <- restoreSkip(era_v,  "Era")
from_v <- restoreSkip(from_v, "Date From")
to_v   <- restoreSkip(to_v,   "Date To")
gaps_v <- restoreSkip(gaps_v, "Interior Coverage Gaps")
miss_v <- restoreSkip(miss_v, "Missing Values")

# Report what changed vs the codebook currently on disk. On the very first
# run the new columns do not exist yet (the codebook still has Date Coverage),
# so each is reported as a new column.
reportChange <- function(col, new_vec) {
  if (!(col %in% names(cb))) {
    cat(sprintf("  + %-15s (new column)\n", col))
    return(invisible())
  }
  old_vec <- cb[[col]]
  old_vec[is.na(old_vec)] <- ""
  cat(sprintf("    %-15s %d changed\n", col, sum(old_vec != new_vec)))
}

cat("=== codebook coverage rebuild ===\n")
cat("variables:", nrow(cb), " | with coverage data:", length(results), "\n")
reportChange("Era", era_v)
reportChange("Date From", from_v)
reportChange("Date To", to_v)
reportChange("Interior Coverage Gaps", gaps_v)
reportChange("Missing Values", miss_v)

# Write the new coverage columns
# Drop the old single column and any prior copies of the new columns
# (idempotent re-run), set fresh values, and position the four coverage
# columns immediately before Missing Values.
cb_out <- cb |>
  select(-any_of(c("Date Coverage", "Era", "Date From", "Date To",
                   "Coverage Gaps", "Interior Coverage Gaps")))
cb_out$Era                      <- era_v
cb_out$`Date From`              <- from_v
cb_out$`Date To`                <- to_v
cb_out$`Interior Coverage Gaps` <- gaps_v
cb_out$`Missing Values`         <- miss_v
cb_out <- cb_out |>
  relocate(Era, `Date From`, `Date To`, `Interior Coverage Gaps`,
           .before = `Missing Values`)

write_csv(cb_out, "metadata/codebook/fwoc_codebook_table.csv", na = "")
cat("wrote metadata/codebook/fwoc_codebook_table.csv\n")

# ====================================================================
# Per-variable coverage gap detail -> metadata/codebook/gap_detail.csv
# ====================================================================
# The specific year-months, inside each variable's [Date From, Date To]
# window, where the variable is entirely absent. Read by the "Coverage gap
# detail" table in docs/fwoc_codebook.qmd. Variables without interior gaps
# are omitted. (This replaces the former hand-prose .md dictionary; the
# column legend now lives in docs/fwoc_codebook.qmd.)

# Get rows/variables that have an interior coverage gap (i.e., gaps_flag = Yes)
gap_rows <- cb$Variable[vapply(cb$Variable,
                               function(v) identical(results[[v]]$gaps_flag, "Yes"),
                               logical(1))]

# Create a table that contains information about the rows/variables and the
# gap periods
gap_detail <- data.frame(
  Variable      = gap_rows,
  Era           = unname(vapply(gap_rows, function(v) results[[v]]$era,        character(1))),
  `Date From`   = unname(vapply(gap_rows, function(v) results[[v]]$date_from,  character(1))),
  `Date To`     = unname(vapply(gap_rows, function(v) results[[v]]$date_to,    character(1))),
  `Gap periods` = unname(vapply(gap_rows, function(v) results[[v]]$gap_detail, character(1))),
  check.names      = FALSE,
  stringsAsFactors = FALSE
)

# Write the table and report the results
write_csv(gap_detail, "metadata/codebook/gap_detail.csv", na = "")
cat(sprintf("wrote metadata/codebook/gap_detail.csv (%d variables with gaps)\n",
            nrow(gap_detail)))

# ====================================================================
# Sentinel self-check (flagSentinels, defined in utils/inspect_vars.R)
# ====================================================================
# Reconciles every sentinel-pattern value in the inspection value dumps against
# the codebook's `Sentinel Codes` column and warns about any that are present in
# the data but undocumented -- so a marker the lexicon already knows cannot turn
# up in a new variable, or sit in a cell nobody updated, without being reported.
# (Numeric placeholder codes are out of scope; see the lexicon note in
# inspect_vars.R.)

# Key blind spot: Flags known sentinels if they are not listed for a variable
# but will not flag unknown sentinels

flagSentinels()

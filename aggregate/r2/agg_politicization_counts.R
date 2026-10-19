# Author: Mark Richardson and Claude Code
# Purpose: Per-period counts of political appointees and of the candidate
#          denominators behind politicization ratios, from the emp hive at the
#          agency and subagency levels, published to the R2 bucket as CSV
#          under export/aggregates/. One row per period x unit, wide: four
#          appointee numerators (all, PAS, Schedule C, noncareer SES) and two
#          denominators (managers, career SES). The NSFTP and all-employee
#          denominators are in the counts files (agg_counts.R), joined on the
#          period and unit keys. No ratios are computed; a user divides
#          whichever pair they want.
#
#          The appointee and denominator definitions are the ones behind the
#          Superset politicization dashboard (aggregate/agg_politicization.R
#          and prep() in utils/agg_rate_functions.R), restated here because
#          prep() also filters to the executive branch and drops agencies
#          00/01/04, which this file does not. If a definition changes there,
#          change it here too.
#
#          Every flag is three-valued. A code that is missing or that OPM
#          reports as not specified (* or **) is unknown, and no flag is ever 0
#          by relying on an unknown code: a single-code flag is 1 when the
#          reported code matches, 0 when it does not, NULL when it is unknown.
#          The one composite (all appointees, an OR of two codes) is 1 when
#          either reported code fires, 0 only when both codes are reported and
#          neither fires, NULL otherwise.
#
#          Executive branch only. Agencies are the FWOC codes, so the military
#          departments roll up into DD. The two EOP indicators ride along as
#          columns; the branch indicators are constant within the executive
#          branch and are left out.

library(arrow)
library(dplyr)
library(readr)

# Written straight into the export tree so 16_push_to_r2.R publishes it as the
# bucket's aggregates/ prefix with no change to 15 or 16. export/ is gitignored.
out_dir <- "export/aggregates"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Executive branch only, so the government-wide politicization literature's
# universe. Rows with no FWOC agency code have a NULL exec_branch and drop here.
emp <- open_dataset("emp_hive",
  partitioning = c("year", "month"),
  unify_schemas = TRUE
) |>
  filter(exec_branch == 1L)

period <- c("date", "year", "month")

unit_agency <- c("agency_code_fwoc", "agency_fwoc")
unit_subagency <- c(
  unit_agency,
  "agency_subelement_code_fwoc", "agency_subelement_fwoc"
)

# EOP indicators, carried as descriptor columns. Both are functions of the
# agency code and date (10_new_variables.R), so they describe a (period, unit)
# row rather than splitting it; pol_level() asserts that. The branch
# indicators are omitted: within the executive branch exec_branch is 1 and
# leg_branch and jud_branch are 0 on every row, and a1_court marks only the
# U.S. Court of Appeals for the Armed Forces (DD08), which the counts files
# already carry at the subagency level.
indicators <- c("eop", "core_eop")

# OPM's "not specified" sentinels. Appointment type and pay plan use both,
# supervisory status uses *. A row carrying one is unknown, not a known
# non-member, and is treated below exactly like a missing code.
not_specified_codes <- c("*", "**")

# pol_level(): appointee and denominator counts per period x unit.
#   ds     the emp hive opened with open_dataset()
#   unit   the unit key columns
#   attrs  descriptor columns grouped alongside unit; must not split a row
# The flags are int8 0/1 with NULL where a source code is unknown (see the
# header), and every sum is na.rm, so an unknown row counts in n_all but in no
# numerator or denominator. The mutate/group_by/summarize runs in Arrow and
# only the aggregate is collected.
pol_level <- function(ds, unit, attrs) {
  out <- ds |>
    # Appointment type codes are published from 1982; earlier years cannot
    # separate appointees from careerists.
    filter(year >= 1982L) |>
    mutate(
      # Each source code with its not-specified sentinels turned into NULL, so
      # that every flag below reads a reported code or nothing. == and | on a
      # NULL then propagate it (NULL == "EX" is NULL, FALSE | NULL is NULL,
      # TRUE | NULL is TRUE), which is what makes the flags three-valued. The
      # flags use == rather than %in% because %in% answers FALSE for NULL.
      appt_code = if_else(
        appointment_type_code %in% not_specified_codes,
        NA_character_, appointment_type_code
      ),
      pay_plan_known = if_else(
        pay_plan_code %in% not_specified_codes,
        NA_character_, pay_plan_code
      ),
      supv_code = if_else(
        supervisory_status_code %in% not_specified_codes,
        NA_character_, supervisory_status_code
      ),
      # All appointees: appointment type 34/44/55 or pay plan EX. Either
      # reported signal is enough for a 1, so an EX row with no appointment
      # type is still an appointee. A 0 needs both codes reported and neither
      # firing: a GS pay plan does not rule out an appointee (Schedule C
      # appointees are routinely GS-graded), so a GS row with an unknown
      # appointment type stays NULL.
      is_app_all = cast(
        appt_code == "34" | appt_code == "44" | appt_code == "55" |
          pay_plan_known == "EX",
        int8()
      ),
      # PAS: Presidential appointment with Senate confirmation, pay plan EX.
      is_app_pas = cast(pay_plan_known == "EX", int8()),
      # Schedule C: 34 is the permanent variant, retired 1993-03-31, and 44 the
      # nonpermanent one. Counting them together keeps the series continuous
      # across that code change.
      is_app_sc = cast(appt_code == "44" | appt_code == "34", int8()),
      # Noncareer SES.
      is_app_na_ses = cast(appt_code == "55", int8()),
      # Managers: any reported supervisory status other than 8 (ALL OTHER
      # POSITIONS). Not restricted to NSFTP; the literature sets no convention.
      # This is a denylist, so a code OPM adds later counts as a manager until
      # this rule is revisited.
      is_denom_mngr = cast(supv_code != "8", int8()),
      # Career SES.
      is_denom_ca_ses = cast(appt_code == "50", int8())
    ) |>
    group_by(!!!syms(c(period, unit, attrs))) |>
    summarize(
      n_app_all = sum(is_app_all, na.rm = TRUE),
      n_app_pas = sum(is_app_pas, na.rm = TRUE),
      n_app_sc = sum(is_app_sc, na.rm = TRUE),
      n_app_na_ses = sum(is_app_na_ses, na.rm = TRUE),
      n_denom_mngr = sum(is_denom_mngr, na.rm = TRUE),
      n_denom_ca_ses = sum(is_denom_ca_ses, na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect()

  # One row per (period, unit). A duplicate means an indicator varies within a
  # unit-period, and the published file would double-count on a naive sum.
  dup <- anyDuplicated(out[c(period, unit)])
  if (dup > 0L) {
    stop(
      "Duplicate (period, unit) row at line ", dup, " -- an indicator ",
      "column splits a unit. Keys: ", paste(c(period, unit), collapse = ", ")
    )
  }

  out |>
    mutate(across(starts_with("n_"), as.integer)) |>
    arrange(!!!syms(c(unit, period)))
}

agency <- pol_level(emp, unit_agency, attrs = indicators)
subagency <- pol_level(emp, unit_subagency, attrs = indicators)

# na = "" so a missing label is an empty cell, never the string "NA" (a real
# agency code).
write_csv(agency, file.path(out_dir, "politicization_agency.csv"), na = "")
write_csv(subagency, file.path(out_dir, "politicization_subagency.csv"), na = "")

message(sprintf(
  "politicization: %d agency rows, %d subagency rows",
  nrow(agency), nrow(subagency)
))

rm(agency, subagency)
gc()

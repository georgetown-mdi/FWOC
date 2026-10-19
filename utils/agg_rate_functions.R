# Author: Mark Richardson and Claude Code
# Purpose: Shared scoping for the Superset aggregates. prep() and the grp*
#   grouping vectors are sourced by every aggregate/agg_*.R builder and by the
#   aggregate reconciliation check, so all of them apply the IDENTICAL universe
#   and scoping (exec branch, FWOC-agency filter, the 00/01/04 drop, the
#   unknown-code rule, appointee, DoD fold, unlabelled-code sentinel) from one
#   definition -- a divergence would surface as phantom drift. Pure
#   definitions, no side effects; the caller attaches arrow + dplyr.
#
#   Unknown-code rule: a code that is missing or that OPM reports as not
#   specified (* or **) is unknown, and no flag is ever 0 on the strength of an
#   unknown code. prep() exposes appt_code, pay_plan_known and supv_code, the
#   source codes with their sentinels turned into NULL, and every flag built
#   here or in aggregate/agg_politicization.R reads those rather than the raw
#   columns. Same rule as aggregate/r2/agg_politicization_counts.R and as
#   nsftp_indicator_fwoc in 11_harmonize_labels.R.

# Full grain for the acc/sep numerator tables (keeps nsftp + appointee
# subgroups).
grp <- c(
  "agency_code", "agency",
  "agency_code_fwoc", "agency_fwoc",
  "agency_subelement_code", "agency_subelement",
  "agency_subelement_code_fwoc", "agency_subelement_fwoc",
  "nsftp_indicator", "nsftp_indicator_fwoc", "appointee"
)

# Subagency emp-denominator grain: agency/subagency keys PLUS the two filter
# subgroups (nsftp_indicator_fwoc, appointee). These let Superset build a single
# correct denominator for each forced filter combination by SUM-ing emp_n over
# the matching subgroups (see superset_rate_queries.md, internal). The
# unfiltered "all/all" rate is just the sum over every subgroup. Standard
# nsftp_indicator is NOT carried here (the rate filters use the FWOC variant).
grp_sub <- c(
  "agency_code", "agency",
  "agency_code_fwoc", "agency_fwoc",
  "agency_subelement_code", "agency_subelement",
  "agency_subelement_code_fwoc", "agency_subelement_fwoc",
  "nsftp_indicator_fwoc", "appointee"
)

grp_agency <- c(
  "agency_code", "agency",
  "agency_code_fwoc", "agency_fwoc"
)

# OPM's "not specified" sentinels. Appointment type and pay plan use both,
# supervisory status uses *. A row carrying one is unknown, not a known
# non-member, and prep() treats it exactly like a missing code.
not_specified_codes <- c("*", "**")

prep <- function(ds) {
  ds |>
    filter(exec_branch == 1L) |>
    filter(!is.na(agency_fwoc) & !is.na(agency_code_fwoc)) |>
    filter(!(agency_code_fwoc %in% c("00", "01", "04"))) |>
    mutate(
      # Each source code with its not-specified sentinels turned into NULL, so
      # that appointee below, and the politicization flags in
      # aggregate/agg_politicization.R, read a reported code or nothing. == and
      # | on a NULL then propagate it (NULL == "EX" is NULL, FALSE | NULL is
      # NULL, TRUE | NULL is TRUE). The flags use == rather than %in% because
      # %in% answers FALSE for NULL. Exposed, not dropped, so every builder
      # applies one rule for what counts as an unknown code.
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
      # appointee: 1 = appointment type 34/44/55 or pay plan EX, 0 = careerist,
      # NULL = undetermined. Three-valued: either reported signal is enough for
      # a 1, so an EX row with no appointment type is an appointee. A 0 needs
      # both codes reported and neither firing. Anything else is NULL.
      #
      # KEEP the NULL -- do NOT coalesce it to 0/1. A GS pay plan does not rule
      # out an appointee (Schedule C appointees are routinely GS-graded), so a
      # GS row with an unknown appointment type is genuinely unknown.
      # Undetermined rows must stay out of both the careerist and appointee
      # denominators while still counting under All, so the total stays
      # correct. See superset_rate_queries.md (internal).
      appointee = cast(
        appt_code == "34" | appt_code == "44" | appt_code == "55" |
          pay_plan_known == "EX",
        int8()
      ),
      # DoD-only consolidation: for the DoD group (agency_code_fwoc == "DD"),
      # adopt the FWOC values so AR/AF/NV fold into DD and the service names
      # nest down to the subelement (DDAR/DDAF/DDNV). Non-DoD standard
      # labels/codes are untouched, and both standard and FWOC variants are
      # retained. Code differences are DoD-only, so afterwards standard codes
      # equal FWOC codes for every row and the standard keys are safe to
      # join on.
      agency = if_else(agency_code_fwoc == "DD", agency_fwoc, agency),
      agency_subelement = if_else(agency_code_fwoc == "DD",
        agency_subelement_fwoc, agency_subelement
      ),
      agency_subelement_code = if_else(agency_code_fwoc == "DD",
        agency_subelement_code_fwoc,
        agency_subelement_code
      ),
      agency_code = if_else(
        agency_code_fwoc == "DD", agency_code_fwoc, agency_code
      )
    ) |>
    mutate(
      # A code with no label (OPM's "**" not-reported sentinel, BZ-era codes
      # absent from the OPM table, and any FWOC label-window gap) would
      # otherwise surface as a single <NULL> entry in every Superset dropdown.
      # Naming it by code keeps each such bucket distinct and visible. The
      # code itself is never changed, so joins are unaffected. A separate
      # mutate() so it runs after the DoD fold and DD** is named from the
      # folded code.
      agency_subelement = if_else(
        is.na(agency_subelement) & !is.na(agency_subelement_code),
        paste0(agency_subelement_code, ": No label reported"),
        agency_subelement
      ),
      agency_subelement_fwoc = if_else(
        is.na(agency_subelement_fwoc) & !is.na(agency_subelement_code_fwoc),
        paste0(agency_subelement_code_fwoc, ": No label reported"),
        agency_subelement_fwoc
      )
    )
}

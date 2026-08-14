# Author: Mark Richardson and Claude Code
# Purpose: Shared scoping for the Superset aggregates. prep() and the grp*
#   grouping vectors are sourced by every aggregate/agg_*.R builder and by the
#   aggregate reconciliation check, so all of them apply the IDENTICAL universe
#   and scoping (exec branch, FWOC-agency filter, the 00/01/04 drop, appointee,
#   DoD fold) from one definition -- a divergence would surface as phantom
#   drift. Pure definitions, no side effects; the caller attaches arrow + dplyr.

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

prep <- function(ds) {
  ds |>
    filter(exec_branch == 1L) |>
    filter(!is.na(agency_fwoc) & !is.na(agency_code_fwoc)) |>
    filter(!(agency_code_fwoc %in% c("00", "01", "04"))) |>
    mutate(
      # appointee: 1 = appointment type 34/44/55 or pay plan EX, 0 = careerist,
      # NULL = undetermined.
      #
      # Only %in% is wrapped, because the two operators disagree on missing
      # values: %in% answers FALSE, == answers NULL. Wrapping makes both
      # propagate, so `|` gives 1 when either signal fires and NULL when neither
      # was observed.
      #
      # KEEP the NULL -- do NOT coalesce it to 0/1. A GS pay plan does not rule
      # out an appointee (Schedule C appointees are routinely GS-graded), so a
      # GS row with no appointment type is genuinely unknown. Undetermined rows
      # must stay out of both the careerist and appointee denominators while
      # still counting under All, so the total stays correct. See
      # superset_rate_queries.md (internal).
      appointee = cast(
        if_else(
          is.na(appointment_type_code),
          NA,
          appointment_type_code %in% c("34", "44", "55")
        ) | pay_plan_code == "EX",
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
    )
}

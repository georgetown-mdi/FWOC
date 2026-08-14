# Author: Mark Richardson and Claude Code
# Purpose: Employment counts by agency and subagency for Superset, written to
#          agg_agency.parquet. One row per date x agency x subagency x
#          appointment type x pay-plan-EX flag x NSFTP status x appointee, with
#          n as the headcount. Column order is pinned to the BigQuery external
#          table and asserted before the write.

library(arrow)
library(dplyr)

# prep(): the shared universe + scoping (exec branch, FWOC-agency filter, the
# 00/01/04 drop, appointee, DoD fold) used by every aggregate builder, so this
# script cannot drift from agg_rates.R.
source("utils/agg_rate_functions.R")

# Local staging for the dashboard aggregates; aggregate/push_to_gcs.R syncs
# parquet/ from here to GCS. Gitignored -- derived, rebuilt on demand.
agg_path <- "aggregate_data/"
dir.create(file.path(agg_path, "parquet"),
           recursive = TRUE, showWarnings = FALSE)

emp <- open_dataset("emp_hive",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

ag_at <- emp |>
  # prep(): exec-branch filter, FWOC-agency filter, the 00/01/04 drop,
  # appointee, and the DoD fold. The fold must precede count() so the
  # consolidated AR/AF/NV rows aggregate into DD.
  prep() |>
  # Published by this table only, so it is derived here rather than in prep().
  mutate(ex_pay_plan = cast(pay_plan_code == "EX", int8())) |>
  # appointee is in the grain but cannot split a group: it is a function of
  # appointment_type_code and pay_plan_code, and ex_pay_plan carries the only
  # part of pay_plan_code it reads.
  count(date, year, month,
        agency_code, agency,
        agency_code_fwoc, agency_fwoc,
        agency_subelement_code, agency_subelement,
        agency_subelement_code_fwoc, agency_subelement_fwoc,
        pres_admin,
        appointment_type_code,
        appointment_type,
        ex_pay_plan,
        nsftp_indicator_fwoc,
        appointee) |>
  # appointee last, matching the published BigQuery schema.
  relocate(appointee, .after = n) |>
  arrange(agency_fwoc, agency_subelement_fwoc, year, month) |>
  compute()

# Assert the variable order to match the BigQuery schema.
agg_agency_cols <- c(
  "date", "year", "month",
  "agency_code", "agency",
  "agency_code_fwoc", "agency_fwoc",
  "agency_subelement_code", "agency_subelement",
  "agency_subelement_code_fwoc", "agency_subelement_fwoc",
  "pres_admin",
  "appointment_type_code", "appointment_type",
  "ex_pay_plan", "nsftp_indicator_fwoc",
  "n", "appointee"
)

if (!identical(names(ag_at), agg_agency_cols)) {
  stop("agg_agency.parquet column order changed -- do not publish until ",
       "resolved.\n  expected: ", paste(agg_agency_cols, collapse = ", "),
       "\n  got:      ", paste(names(ag_at), collapse = ", "))
}

write_parquet(ag_at,
              paste0(agg_path, "parquet/agg_agency.parquet"),
              use_dictionary = FALSE)

rm(ag_at)
gc()

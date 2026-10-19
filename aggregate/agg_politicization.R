# Author: Mark Richardson, Benjamin Reese, and Claude Code
# Purpose: Stacked politicization tables for Superset at three aggregation
#          levels (gov, agency, subagency). Users choose a denominator
#          (NSFTP, Managers, Career SES) and filter by appointee type
#          (All, PAS, SC, NA-SES). Metric: SUM(n_app) / MAX(n_denom)

library(arrow)
library(dplyr)
library(tidyr)

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

# Grouping keys for the politicization tables: agency/subagency identifiers
# only. The appointee types and denominators are the n_app_*/n_denom_* columns,
# not grouping keys, so these deliberately differ from the grp_sub in
# utils/agg_rate_functions.R -- named pol_* so sourcing that file cannot shadow
# them, and so a reader is not misled into thinking they are the same grain.
pol_grp_sub <- c("agency_code", "agency",
                 "agency_code_fwoc", "agency_fwoc",
                 "agency_subelement_code", "agency_subelement",
                 "agency_subelement_code_fwoc", "agency_subelement_fwoc")

pol_grp_ag <- c("agency_code", "agency",
                "agency_code_fwoc", "agency_fwoc")

# Subagency-level flat counts
flat <- emp |>
  prep() |>
  filter(year >= 1982L) |>
  mutate(
    # prep() already derives this as `appointee` (pay plan EX or appt type
    # 34/44/55). Aliased rather than restated so the two cannot drift apart.
    is_all = appointee,
    # The single-code flags read prep()'s appt_code, pay_plan_known and
    # supv_code, which are the raw codes with OPM's not-specified sentinels
    # turned into NULL. Each is therefore 1 when the reported code matches, 0
    # when it does not, NULL when the code is unknown. The sums below use
    # na.rm = TRUE, so an unknown row sits outside both the numerator and the
    # denominator rather than being reported as a definite non-member.
    is_pas = cast(pay_plan_known == "EX", int8()),
    # Both codes are Schedule C: 34 is the permanent variant, retired
    # 1993-03-31, and 44 the nonpermanent one. Counting them together keeps the
    # SC series continuous across that code change.
    is_sc = cast(appt_code == "44" | appt_code == "34", int8()),
    is_na_ses = cast(appt_code == "55", int8()),
    # Derived in 11_harmonize_labels.R under the same unknown-code rule.
    is_nsftp = cast(nsftp_indicator_fwoc == 1L, int8()),
    # Managers = any reported supervisory status other than 8 (ALL OTHER
    # POSITIONS). Deliberately NOT restricted to nsftp_indicator_fwoc == 1 --
    # the literature sets no convention, so this counts every manager.
    #
    # This is a denylist, so any code it does not name counts as a manager --
    # including codes OPM adds later, and codes whose meaning changes. Check new
    # supervisory status codes against this rule rather than assuming they fall
    # through.
    is_mngr = cast(supv_code != "8", int8()),
    is_ca_ses = cast(appt_code == "50", int8())
  ) |>
  group_by(date, year, month, !!!syms(pol_grp_sub)) |>
  summarize(
    n_app_all    = sum(is_all, na.rm = TRUE),
    n_app_pas    = sum(is_pas, na.rm = TRUE),
    n_app_sc     = sum(is_sc, na.rm = TRUE),
    n_app_na_ses = sum(is_na_ses, na.rm = TRUE),
    n_denom_nsftp  = sum(is_nsftp, na.rm = TRUE),
    n_denom_mngr   = sum(is_mngr, na.rm = TRUE),
    n_denom_ca_ses = sum(is_ca_ses, na.rm = TRUE)
  ) |>
  collect()

# Pivot to stacked format
stack <- function(df) {
  app <- df |>
    # each row is a count of appointee type
    pivot_longer(cols = starts_with("n_app_"),
                 names_to = "appointee_type",
                 names_prefix = "n_app_",
                 values_to = "n_app") |>
    select(-starts_with("n_denom_"))

  denom <- df |>
    # each row is a count for a denominator type
    select(-starts_with("n_app_")) |>
    pivot_longer(cols = starts_with("n_denom_"),
                 names_to = "denom",
                 names_prefix = "n_denom_",
                 values_to = "n_denom")

  app_labels <- c(all = "All", pas = "PAS", sc = "SC", na_ses = "NA-SES")
  denom_labels <- c(nsftp = "NSFTP", mngr = "Managers", ca_ses = "Career SES")

  # For each observed date-agency, crossing() creates a row for each
  # appointee_type-denom pair. Then left_joins() join the appointee counts in
  # app and the denominator counts in denom. Next mutate() recodes
  # appointee_type and denom to display labels and replaces any NAs in counts
  # with 0s.
  crossing(
    df |>
      select(-starts_with("n_app_"), -starts_with("n_denom_")) |>
      distinct(),
    tibble(appointee_type = names(app_labels)),
    tibble(denom = names(denom_labels))
  ) |>
    left_join(app,
              by = c(names(df)[!grepl("^n_", names(df))], "appointee_type")) |>
    left_join(denom,
              by = c(names(df)[!grepl("^n_", names(df))], "denom")) |>
    mutate(
      appointee_type = app_labels[appointee_type],
      denom = denom_labels[denom],
      n_app = replace_na(n_app, 0L),
      n_denom = replace_na(n_denom, 0L)
    )
}

# Subagency level
pol_sub <- stack(flat) |>
  arrange(agency_fwoc, agency_subelement_fwoc, year, month, denom,
          appointee_type)

# Agency level

flat_ag <- flat |> # Sum to the agency-level
  group_by(date, year, month, !!!syms(pol_grp_ag)) |>
  summarize(across(starts_with("n_"), sum), .groups = "drop")

pol_ag <- stack(flat_ag) |>
  arrange(agency_fwoc, year, month, denom, appointee_type)

# Government-wide level

flat_gov <- flat |> # Sum to the government-wide level
  group_by(date, year, month) |>
  summarize(across(starts_with("n_"), sum), .groups = "drop")

pol_gov <- stack(flat_gov) |>
  arrange(year, month, denom, appointee_type)

write_parquet(pol_gov,
              paste0(agg_path, "parquet/agg_pol_gov.parquet"),
              use_dictionary = FALSE)
write_parquet(pol_ag,
              paste0(agg_path, "parquet/agg_pol_agency.parquet"),
              use_dictionary = FALSE)
write_parquet(pol_sub,
              paste0(agg_path, "parquet/agg_pol_subagency.parquet"),
              use_dictionary = FALSE)

rm(flat, flat_ag, flat_gov, pol_sub, pol_ag, pol_gov)
gc()

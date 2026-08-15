# Author: Mark Richardson and Claude Code
# Purpose: Long-format accession/separation count tables and emp denominator
#          tables at three aggregation levels (gov, agency, subagency) for
#          Superset virtual datasets.
#          Annual: Dec emp (year Y) -> year Y+1 acc/sep.
#          Monthly: month-end emp -> next month (from July 2011).
#          Superset virtual dataset joins emp to sep/acc on the level keys
#          (gov: year[,month]; agency: + agency_code; subagency:
#          + agency_subelement_code), then: metric = SUM(n) / MAX(emp_n).
#          DoD nesting (prep): AR/AF/NV fold into DD with the service names
#          nested to the subelement (DDAR/DDAF/DDNV), so standard codes equal
#          FWOC codes everywhere and the standard keys are safe to join on.
#          The emp denominators carry nsftp_indicator_fwoc + appointee subgroups
#          so Superset can build a single correct denominator per forced filter
#          combination (nsftp only/all x appointee all/only/non-only) by SUM-ing
#          emp_n over the matching subgroups; the acc/sep numerators keep full
#          grain. Superset forces an agency (+ subagency) filter and one value
#          of each rate filter, so emp_n is constant within each charted period
#          and MAX(emp_n) is exact. See superset_rate_queries.md (internal, not
#          in this repo) for the join SQL.

library(arrow)
library(dplyr)
library(lubridate)

# prep() + the grp* grouping vectors; shared with the aggregate reconciliation
# check so both scope identically (see utils/agg_rate_functions.R).
source("utils/agg_rate_functions.R")

# Local staging for the dashboard aggregates; aggregate/push_to_gcs.R syncs
# parquet/ from here to GCS. Gitignored -- derived, rebuilt on demand.
agg_path <- "aggregate_data/"
dir.create(file.path(agg_path, "parquet"),
           recursive = TRUE, showWarnings = FALSE)

emp <- open_dataset("emp_hive",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)
acc <- open_dataset("acc_hive",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)
sep <- open_dataset("sep_hive",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

# Last year/month present in a hive.
last_period <- function(ds) {
  y <- as.integer(ds |> summarize(m = max(year)) |> collect() |> pull(m))
  m <- as.integer(ds |>
                    filter(year == y) |>
                    summarize(m = max(month)) |>
                    collect() |> pull(m))
  c(y, m)
}

# All three hives must end at the same period. max_yr/max_month bound every
# table built below, so a hive ending earlier than the others would be quietly
# truncated to their cutoff instead of reported. The starts differ by design
# (emp reaches back to 1973, acc/sep begin in 1982) -- only the end is asserted.
periods <- list(emp = last_period(emp),
                acc = last_period(acc),
                sep = last_period(sep))

if (length(unique(periods)) != 1L) {
  stop("emp/acc/sep do not end at the same period -- do not build until ",
       "resolved:\n  ",
       paste(sprintf("%s %d-%02d", names(periods),
                     vapply(periods, `[`, integer(1), 1L),
                     vapply(periods, `[`, integer(1), 2L)),
             collapse = "\n  "))
}

max_yr <- periods$emp[1]
max_month <- periods$emp[2]
complete_year <- max_month == 12L

###########################################################################
# ANNUAL
###########################################################################

yr_cutoff <- if (complete_year) max_yr else max_yr - 1L

# Subagency-level emp (finest grain)
emp_a_sub <- emp |>
  prep() |>
  filter(month == 12L) |>
  count(year, !!!syms(grp_sub)) |>
  rename(emp_n = n) |>
  # Shift year by 1 so Dec emp of year Y-1 is the denominator for year Y
  # acc/sep. After this, year names the TARGET year, so the filter below is
  # about acc/sep years, not emp years.
  mutate(year = year + 1L) |>
  filter(year >= 1982L, year <= yr_cutoff) |>
  collect()

# Agency-level emp (keep the nsftp_fwoc/appointee subgroups for
# filterable rates)
emp_a_ag <- emp_a_sub |>
  group_by(year, !!!syms(grp_agency), nsftp_indicator_fwoc, appointee) |>
  summarize(emp_n = sum(emp_n), .groups = "drop")

# Government-wide emp (keep the nsftp_fwoc/appointee subgroups)
emp_a_gov <- emp_a_sub |>
  group_by(year, nsftp_indicator_fwoc, appointee) |>
  summarize(emp_n = sum(emp_n), .groups = "drop")

# Accessions
acc_a <- acc |>
  prep() |>
  filter(year <= yr_cutoff) |>
  count(year, !!!syms(grp), accession_category_code, accession_category) |>
  collect() |>
  arrange(agency_fwoc, agency_subelement_fwoc, year, accession_category_code)

# Separations
sep_a <- sep |>
  prep() |>
  # SI (Termination or Removal - Discipline/Performance) is published in the BZ
  # era only; FWD does not carry it. Dropped in both eras so the separation
  # series is consistent across the boundary rather than losing a category at
  # 2005.
  filter(separation_category_code != "SI") |>
  filter(year <= yr_cutoff) |>
  count(year, !!!syms(grp),
        separation_category_code, separation_category,
        drp_indicator, drp_indicator_fwoc) |>
  collect() |>
  arrange(agency_fwoc, agency_subelement_fwoc, year, separation_category_code)

write_parquet(emp_a_gov,
              paste0(agg_path, "parquet/agg_emp_annual_gov.parquet"),
              use_dictionary = FALSE)
write_parquet(emp_a_ag,
              paste0(agg_path, "parquet/agg_emp_annual_agency.parquet"),
              use_dictionary = FALSE)
write_parquet(emp_a_sub,
              paste0(agg_path, "parquet/agg_emp_annual_subagency.parquet"),
              use_dictionary = FALSE)
write_parquet(acc_a,
              paste0(agg_path, "parquet/agg_acc_annual.parquet"),
              use_dictionary = FALSE)
write_parquet(sep_a,
              paste0(agg_path, "parquet/agg_sep_annual.parquet"),
              use_dictionary = FALSE)

rm(emp_a_sub, emp_a_ag, emp_a_gov, acc_a, sep_a)
gc()

###########################################################################
# MONTHLY
###########################################################################

# Subagency-level emp (finest grain)

emp_m_sub <- emp |>
  prep() |>
  filter(year >= 2011L) |>
  count(year, month, !!!syms(grp_sub)) |>
  rename(emp_n = n) |>
  collect() |>
  # Shift year/month by 1 so emp count of t-1 is the denominator for acc/sep
  # count of t. After this, year/month name the TARGET month, so the filter
  # below is about acc/sep periods, not emp months.
  mutate(
    # Parse a date string to year-month-day and increment by one calendar
    # month. %m+% rather than + so an end-of-month day would roll back to the
    # last valid day instead of returning NA.
    dt = ymd(paste(year, month, "01")) %m+% months(1),
    year = as.integer(year(dt)),
    month = as.integer(month(dt))
  ) |>
  select(-dt) |>
  filter(year > 2011L | (year == 2011L & month >= 7L))

# Agency-level emp (keep the nsftp_fwoc/appointee subgroups for
# filterable rates)
emp_m_ag <- emp_m_sub |>
  group_by(year, month, !!!syms(grp_agency), nsftp_indicator_fwoc, appointee) |>
  summarize(emp_n = sum(emp_n), .groups = "drop")

# Government-wide emp (keep the nsftp_fwoc/appointee subgroups)
emp_m_gov <- emp_m_sub |>
  group_by(year, month, nsftp_indicator_fwoc, appointee) |>
  summarize(emp_n = sum(emp_n), .groups = "drop")

# Accessions
acc_m <- acc |>
  prep() |>
  filter(year > 2011L | (year == 2011L & month >= 7L)) |>
  count(year, month, !!!syms(grp),
        accession_category_code, accession_category) |>
  collect() |>
  arrange(agency_fwoc, agency_subelement_fwoc, year, month,
          accession_category_code)

# Separations
sep_m <- sep |>
  prep() |>
  # SI (Termination or Removal - Discipline/Performance) is published in the BZ
  # era only; FWD does not carry it. Dropped in both eras so the separation
  # series is consistent across the boundary rather than losing a category at
  # 2005.
  filter(separation_category_code != "SI") |>
  filter(year > 2011L | (year == 2011L & month >= 7L)) |>
  count(year, month, !!!syms(grp),
        separation_category_code, separation_category,
        drp_indicator, drp_indicator_fwoc) |>
  collect() |>
  arrange(agency_fwoc, agency_subelement_fwoc, year, month,
          separation_category_code)

write_parquet(emp_m_gov,
              paste0(agg_path, "parquet/agg_emp_monthly_gov.parquet"),
              use_dictionary = FALSE)
write_parquet(emp_m_ag,
              paste0(agg_path, "parquet/agg_emp_monthly_agency.parquet"),
              use_dictionary = FALSE)
write_parquet(emp_m_sub,
              paste0(agg_path, "parquet/agg_emp_monthly_subagency.parquet"),
              use_dictionary = FALSE)
write_parquet(acc_m,
              paste0(agg_path, "parquet/agg_acc_monthly.parquet"),
              use_dictionary = FALSE)
write_parquet(sep_m,
              paste0(agg_path, "parquet/agg_sep_monthly.parquet"),
              use_dictionary = FALSE)

rm(emp_m_sub, emp_m_ag, emp_m_gov, acc_m, sep_m)
gc()

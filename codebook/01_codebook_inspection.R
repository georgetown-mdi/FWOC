# Author: Mark Richardson and Claude Code

# Codebook population workflow (step 1 of 2)
# ------------------------------------------
# Working codebook CSV:        metadata/codebook/fwoc_codebook_table.csv
# Per-variable inspection CSVs: metadata/codebook/inspection/
#
# This script (01) reads the hives and, for each variable (or related group),
# calls writeInspection() which writes:
#   coverage_<dataset>_<vars>.csv  -- year-month coverage (joint)
#   values_<dataset>_<v>.csv       -- distinct values + counts, per variable
# These are the inputs for the codebook's Era / Date From / Date To /
# Interior Coverage Gaps / Missing Values / Sentinel Codes columns.
#
# Run it only when the underlying hives change (it is the heavy, hive-reading
# step). Then run codebook/02_codebook_build.R, which reads these
# inspection CSVs, derives the structured coverage columns, and writes
# fwoc_codebook_table.csv (and gap_detail.csv).
#
# Label-gap checking is not here. validation/02_labels.R runs the
# buildInventory() -> checkLabelGaps() pair over all three hives and asserts the
# result; this script only inspects coverage and values, and asserts nothing.

# Load packages
library(arrow)
library(dplyr)

# Load functions
source("utils/inspect_vars.R")

# Open data
emp <- open_dataset("emp_hive",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)


acc <- open_dataset("acc_hive",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

sep <- open_dataset("sep_hive",
                    partitioning = c("year", "month"),
                    unify_schemas = TRUE)

#### Date ####

cv <- coverageByYearMonth(data = emp,
                          var = c("date", "year", "month", "quarter"))

writeInspection(cv, emp, c("date", "year", "month", "quarter"), "emp")

#### snapshot ####

cv <- coverageByYearMonth(data = emp,
                          var = "snapshot_yyyymm")

writeInspection(cv, emp, "snapshot_yyyymm", "emp")

#### pseudo_id ####

# pseudo_id has tens of millions of distinct values; cap values
# inspection at top 200 by frequency to spot sentinels / duplicate
# hot spots without dumping the whole distinct table.
cv <- coverageByYearMonth(data = emp,
                          var = "pseudo_id")

writeInspection(cv, emp, "pseudo_id", "emp", top_n = 200)

#### name ####

cv <- coverageByYearMonth(data = emp,
                          var = "name")

writeInspection(cv, emp, "name", "emp", top_n = 200)

#### agency ####

cv <- coverageByYearMonth(data = emp,
                          var = c("agency", "agency_code"))

writeInspection(cv, emp, c("agency", "agency_code"), "emp")

#### subagency ####

cv <- coverageByYearMonth(data = emp,
                          var = c("agency_subelement", "agency_subelement_code"))

writeInspection(cv, emp, c("agency_subelement_code", "agency_subelement"), "emp")

#### education_level ####

cv <- coverageByYearMonth(data = emp,
                          var = c("education_level", "education_level_code"))

writeInspection(cv, emp, c("education_level_code", "education_level"), "emp")

#### appointment_type ####

cv <- coverageByYearMonth(data = emp,
                          var = c("appointment_type", "appointment_type_code"))

writeInspection(cv, emp, c("appointment_type_code", "appointment_type"), "emp")

#### pay_plan ####

cv <- coverageByYearMonth(data = emp,
                          var = c("pay_plan", "pay_plan_code"))

writeInspection(cv, emp, c("pay_plan_code", "pay_plan"), "emp")

#### grade ####

cv <- coverageByYearMonth(data = emp, var = "grade")
writeInspection(cv, emp, "grade", "emp")

#### supervisory_status ####

cv <- coverageByYearMonth(data = emp,
                          var = c("supervisory_status_code", "supervisory_status"))

writeInspection(cv, emp, c("supervisory_status_code", "supervisory_status"), "emp")

#### work_schedule ####

cv <- coverageByYearMonth(data = emp,
                          var = c("work_schedule_code", "work_schedule"))

writeInspection(cv, emp, c("work_schedule_code", "work_schedule"), "emp")

#### nsftp ####

cv <- coverageByYearMonth(data = emp,
                          var = c("nsftp_indicator", "nsftp_indicator_fwoc"))
writeInspection(cv, emp, c("nsftp_indicator", "nsftp_indicator_fwoc"), "emp")

#### agency_fwoc ####

cv <- coverageByYearMonth(data = emp,
                          var = c("agency_code_fwoc", "agency_fwoc"))
writeInspection(cv, emp, c("agency_code_fwoc", "agency_fwoc"), "emp")

#### subagency_fwoc ####

cv <- coverageByYearMonth(data = emp,
                          var = c("agency_subelement_code_fwoc", "agency_subelement_fwoc"))
writeInspection(cv, emp, c("agency_subelement_code_fwoc", "agency_subelement_fwoc"), "emp")

#### department ####

cv <- coverageByYearMonth(data = emp,
                          var = c("department_code", "department"))
writeInspection(cv, emp, c("department_code", "department"), "emp")

#### cfo_act_agency_indicator ####

cv <- coverageByYearMonth(data = emp, var = "cfo_act_agency_indicator")
writeInspection(cv, emp, "cfo_act_agency_indicator", "emp")

#### duty_station_code ####

cv <- coverageByYearMonth(data = emp, var = "duty_station_code")
writeInspection(cv, emp, "duty_station_code", "emp", top_n = 500)

#### duty_station_country ####

cv <- coverageByYearMonth(data = emp,
                          var = c("duty_station_country_code", "duty_station_country"))
writeInspection(cv, emp, c("duty_station_country_code", "duty_station_country"), "emp")

#### duty_station_state ####

cv <- coverageByYearMonth(data = emp,
                          var = c("duty_station_state_code", "duty_station_state",
                                  "duty_station_state_abbreviation"))
writeInspection(cv, emp, c("duty_station_state_code", "duty_station_state",
                            "duty_station_state_abbreviation"), "emp")

#### duty_station_county ####

cv <- coverageByYearMonth(data = emp,
                          var = c("duty_station_county_code", "duty_station_county"))
writeInspection(cv, emp, c("duty_station_county_code", "duty_station_county"), "emp")

#### duty_station_city ####

cv <- coverageByYearMonth(data = emp, var = "duty_station_city")
writeInspection(cv, emp, "duty_station_city", "emp", top_n = 500)

#### duty_station_state_country_territory_code ####

cv <- coverageByYearMonth(data = emp, var = "duty_station_state_country_territory_code")
writeInspection(cv, emp, "duty_station_state_country_territory_code", "emp")

#### duty_station_state_territory ####

# Derived in 12_duty_station_backfill.R from OPM duty-station data; coverage is
# OPM-controlled, so derive it here rather than hardcoding in the build.
cv <- coverageByYearMonth(data = emp,
                          var = c("duty_station_state_territory",
                                  "duty_station_state_territory_abbreviation"))
writeInspection(cv, emp, c("duty_station_state_territory",
                            "duty_station_state_territory_abbreviation"), "emp")

#### consolidated_statistical_area ####

cv <- coverageByYearMonth(data = emp,
                          var = c("consolidated_statistical_area_code",
                                  "consolidated_statistical_area"))
writeInspection(cv, emp, c("consolidated_statistical_area_code",
                            "consolidated_statistical_area"), "emp")

#### core_based_statistical_area ####

cv <- coverageByYearMonth(data = emp,
                          var = c("core_based_statistical_area_code",
                                  "core_based_statistical_area"))
writeInspection(cv, emp, c("core_based_statistical_area_code",
                            "core_based_statistical_area"), "emp")

#### locality_pay_area ####

cv <- coverageByYearMonth(data = emp,
                          var = c("locality_pay_area_code", "locality_pay_area"))
writeInspection(cv, emp, c("locality_pay_area_code", "locality_pay_area"), "emp")

#### age ####

cv <- coverageByYearMonth(data = emp,
                          var = c("age_75", "age_bracket"))
writeInspection(cv, emp, c("age_75", "age_bracket"), "emp")

#### length_of_service ####

cv <- coverageByYearMonth(data = emp,
                          var = c("length_of_service_ord", "length_of_service_years"))
writeInspection(cv, emp, c("length_of_service_ord", "length_of_service_years"), "emp")

#### education_level_bracket ####

# education_level_code repeats from the education_level block above so the joint
# coverage table shows the code and its brackets together. That rewrites
# values_emp_education_level_code.csv with identical content -- deliberate, not a bug.
cv <- coverageByYearMonth(data = emp,
                          var = c("education_level_code", "education_level_bracket", "education_level_bracket_fwoc"))
writeInspection(cv, emp, c("education_level_code", "education_level_bracket", "education_level_bracket_fwoc"), "emp")

#### annualized_adjusted_basic_pay ####

cv <- coverageByYearMonth(data = emp,
                          var = c("annualized_adjusted_basic_pay",
                                  "annualized_adjusted_basic_pay_num",
                                  "annualized_adjusted_basic_pay_real_2024"))
writeInspection(cv, emp, c("annualized_adjusted_basic_pay",
                            "annualized_adjusted_basic_pay_num",
                            "annualized_adjusted_basic_pay_real_2024"), "emp", top_n = 200)

#### pay_basis ####

cv <- coverageByYearMonth(data = emp,
                          var = c("pay_basis_code", "pay_basis"))
writeInspection(cv, emp, c("pay_basis_code", "pay_basis"), "emp")

#### occupational_series ####

cv <- coverageByYearMonth(data = emp,
                          var = c("occupational_series_code", "occupational_series"))
writeInspection(cv, emp, c("occupational_series_code", "occupational_series"), "emp")

#### occupational_group ####

# No published lookup table for occupational_group
cv <- coverageByYearMonth(data = emp,
                          var = c("occupational_group_code", "occupational_group"))
writeInspection(cv, emp, c("occupational_group_code", "occupational_group"), "emp")

#### occupational_category ####

cv <- coverageByYearMonth(data = emp,
                          var = c("occupational_category_code", "occupational_category"))
writeInspection(cv, emp, c("occupational_category_code", "occupational_category"), "emp")

#### occupational_category_fwoc ####
# FWOC-derived: uniform per-year series[+pay_plan+grade] PATCO rule (11_harmonize_labels.R)
# that also fills the 2005-2009 FWD gap. No source lookup exists, so it is absent from
# validation/02_labels.R's pairs -- coverage here is the only inspection it gets.

cv <- coverageByYearMonth(data = emp,
                          var = c("occupational_category_code_fwoc", "occupational_category_fwoc"))
writeInspection(cv, emp, c("occupational_category_code_fwoc", "occupational_category_fwoc"), "emp")

#### stem_occupation ####

cv <- coverageByYearMonth(data = emp,
                          var = c("stem_occupation_type", "stem_occupation"))
writeInspection(cv, emp, c("stem_occupation_type", "stem_occupation"), "emp")

#### bargaining_unit ####

cv <- coverageByYearMonth(data = emp,
                          var = c("bargaining_unit_code", "bargaining_unit",
                                  "bargaining_unit_status"))
writeInspection(cv, emp, c("bargaining_unit_code", "bargaining_unit",
                            "bargaining_unit_status"), "emp", top_n = 500)

#### flsa_category ####

cv <- coverageByYearMonth(data = emp,
                          var = c("flsa_category_code", "flsa_category"))
writeInspection(cv, emp, c("flsa_category_code", "flsa_category"), "emp")

#### personnel_office_identifier_code ####

cv <- coverageByYearMonth(data = emp, var = "personnel_office_identifier_code")
writeInspection(cv, emp, "personnel_office_identifier_code", "emp", top_n = 200)

#### personnel_office_name / poi_city / poi_state_country ####

# These cascade from personnel_office_identifier_code via the POI master lookup;
# coverage tracks OPM's publication of the POI code (drop the code and these go
# NA), so derive it here rather than hardcoding. High-cardinality text -> the
# values files are capped via top_n.
cv <- coverageByYearMonth(data = emp,
                          var = c("personnel_office_name", "poi_city",
                                  "poi_state_country"))
writeInspection(cv, emp, c("personnel_office_name", "poi_city",
                            "poi_state_country"), "emp", top_n = 200)

#### position_occupied ####

cv <- coverageByYearMonth(data = emp,
                          var = c("position_occupied_code", "position_occupied"))
writeInspection(cv, emp, c("position_occupied_code", "position_occupied"), "emp")

#### step_or_rate_type ####

cv <- coverageByYearMonth(data = emp,
                          var = c("step_or_rate_type_code", "step_or_rate_type"))
writeInspection(cv, emp, c("step_or_rate_type_code", "step_or_rate_type"), "emp")

#### service_computation_date_leave ####

cv <- coverageByYearMonth(data = emp, var = "service_computation_date_leave")
writeInspection(cv, emp, "service_computation_date_leave", "emp", top_n = 200)

#### tenure ####

cv <- coverageByYearMonth(data = emp,
                          var = c("tenure_code", "tenure"))
writeInspection(cv, emp, c("tenure_code", "tenure"), "emp")

#### veteran_indicator ####

cv <- coverageByYearMonth(data = emp, var = "veteran_indicator")
writeInspection(cv, emp, "veteran_indicator", "emp")

#### branch_indicators ####

cv <- coverageByYearMonth(data = emp,
                          var = c("exec_branch", "leg_branch", "jud_branch",
                                  "a1_court", "eop", "core_eop"))
writeInspection(cv, emp, c("exec_branch", "leg_branch", "jud_branch",
                            "a1_court", "eop", "core_eop"), "emp")

#### pres_admin ####

cv <- coverageByYearMonth(data = emp, var = "pres_admin")
writeInspection(cv, emp, "pres_admin", "emp")

#### economic_indicators ####

cv <- coverageByYearMonth(data = emp,
                          var = c("cpi_u", "cpi_index", "unrate_us", "unrate_state"))
writeInspection(cv, emp, c("cpi_u", "cpi_index", "unrate_us", "unrate_state"), "emp", top_n = 200)

#### eff_date (acc) ####

cv <- coverageByYearMonth(data = acc, var = "eff_date")
writeInspection(cv, acc, "eff_date", "acc", top_n = 200)

#### personnel_action_effective_date_yyyymm (acc) ####

cv <- coverageByYearMonth(data = acc, var = "personnel_action_effective_date_yyyymm")
writeInspection(cv, acc, "personnel_action_effective_date_yyyymm", "acc")

#### accession_category ####

cv <- coverageByYearMonth(data = acc,
                          var = c("accession_category_code", "accession_category"))
writeInspection(cv, acc, c("accession_category_code", "accession_category"), "acc")

#### appointment_not_to_exceed_date ####

cv <- coverageByYearMonth(data = acc, var = "appointment_not_to_exceed_date")
writeInspection(cv, acc, "appointment_not_to_exceed_date", "acc", top_n = 200)

#### pathways_group ####

# Accessions only. OPM ships it in the accession and separation files from
# 2015-01, but never in the employment file; 14_finalize_hive.R keeps it on acc
# and drops it from sep, where no row is populated (see the manifest note).
cv <- coverageByYearMonth(data = acc, var = "pathways_group")
writeInspection(cv, acc, "pathways_group", "acc")

#### eff_date (sep) ####

cv <- coverageByYearMonth(data = sep, var = "eff_date")
writeInspection(cv, sep, "eff_date", "sep", top_n = 200)

#### personnel_action_effective_date_yyyymm (sep) ####

cv <- coverageByYearMonth(data = sep, var = "personnel_action_effective_date_yyyymm")
writeInspection(cv, sep, "personnel_action_effective_date_yyyymm", "sep")

#### separation_category ####

cv <- coverageByYearMonth(data = sep,
                          var = c("separation_category_code", "separation_category"))
writeInspection(cv, sep, c("separation_category_code", "separation_category"), "sep")

#### drp_indicator ####

cv <- coverageByYearMonth(data = sep,
                          var = c("drp_indicator", "drp_indicator_fwoc"))
writeInspection(cv, sep, c("drp_indicator", "drp_indicator_fwoc"), "sep")

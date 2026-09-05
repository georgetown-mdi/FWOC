# Author: Mark Richardson and Claude Code
# Purpose: Build the OPM code->label maps and write them to metadata/labels/*.csv
#   (consumed by 10_agency_names.R / 12_harmonize_labels.R). Does not touch any
#   hive data.
#
#   The OPM master code tables may change on the monthly metadata refresh, so the
#   labels are regenerated, reviewed, and committed as part of that gated refresh
#   -- the pipeline then consumes the committed CSVs.
#
#   Monthly metadata-refresh order (run from the fwoc/ project root):
#     Rscript setup/check_opm_metadata.R          # gate: diff t vs t-1
#     FWOC_METADATA_ACK=1 Rscript setup/check_opm_metadata.R   # after review
#     Rscript setup/build_labels.R                # regenerate + guard labels; also
#                                                 # derives DutyStations_<date>.csv
#     # review `git diff` of metadata/labels/, commit, then build the hives
#
#   Time-varying tables (agency, subagency, education, occupation, ...) are guarded
#   against per-code overlapping date windows, which would match >1 label row in
#   the time-varying join and DUPLICATE (inflate) hive rows. The guard hard-fails
#   and lists the offending codes; resolve each genuine conflict in
#   metadata/code_tables/overlap_resolutions.csv (byte-identical duplicate rows and
#   provably-cosmetic variants -- whitespace, the OPM "does not make submissions"
#   boilerplate, trailing periods -- are collapsed automatically and need no entry).
#
#   All metadata lookup tables are checked for unique keys to prevent hive row
#   inflation on joins during the build

library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(stringr)

path <- "metadata/code_tables/"

# find_code_table()/read_code_table() and the overlap helpers below live in a
# shared util so this builder, setup/check_opm_metadata.R, and validation/
# 02_labels.R detect overlaps identically.
source("utils/code_table_functions.R")

# Human-curated resolutions for genuine (non-cosmetic) overlapping windows.
resolutions <- read_csv(paste0(path, "overlap_resolutions.csv"),
  col_types = cols(.default = "c"), na = ""
)

# normalize (agency/subagency cosmetic variants) -> collapse exact duplicates ->
# apply resolutions -> hard-fail guard. Returns the cleaned table; stop()s if any
# overlap survives.
# Assumes resolutions exists in the global environment
clean_and_guard <- function(d, lookup_name, normalize = FALSE) {
  if (normalize) d <- normalize_label_text(d)
  # Collapse byte-identical rows before the guard: an exact-duplicate (code, label,
  # window) is pure redundancy that would otherwise trip assert_no_label_overlaps
  # with no fix expressible in overlap_resolutions.csv (which chooses BETWEEN
  # differing labels). normalize_label_text already distinct()s; this also covers
  # the tables built with normalize = FALSE.
  d <- distinct(d)
  d <- apply_overlap_resolutions(d, lookup_name, resolutions)
  assert_no_label_overlaps(d, lookup_name)
  d
}

# ============================================================
#### Agency and subagency ####
# ============================================================

ct_agy <- find_code_table(path, "agency_subelement")
sub <- read_code_table(file_path = ct_agy$file, download_date = ct_agy$download_date)

agy_map <- sub |>
  filter(str_length(code) == 2 | str_detect(code, "00$")) |>
  arrange(code, date_from) |>
  filter(!(code %in% c(
    "AH00",
    "AR00",
    "DL00",
    "DN00",
    "EM00",
    "FQ00",
    "HF00",
    "HU00",
    "RH00",
    "SM00",
    "VA00"
  ))) # Vector of codes to drop to avoid duplicates (e.g., VA00 is a duplicate for VA)

subagy_map <- sub |>
  filter(str_length(code) == 4) |>
  mutate(label = str_to_upper(label))

subagy_map <- clean_and_guard(subagy_map, "subagency_labels.csv", normalize = TRUE)
write_csv(subagy_map, file = "metadata/labels/subagency_labels.csv", na = "")

# Special adjustments for USAID (AM)

# AM is the code from 08/01/1979 to 09/30/2020 after which there is no longer
# a valid two-character code AM. AM00 maps to "U.S. Agency for International
# Development" beginning 10/01/2020. There exist AM00 codes and labels before
# 10/01/2020

agy_map <- agy_map |>
  filter(!(code == "AM00" &
    (label == "U.S. INTERNATIONAL DEVELOPMENT COOPERATION AGENCY" |
      label == "AGENCY FOR INTERNATIONAL DEVELOPMENT")))

# Special adjustments for the Department of State (ST)

# ST is a valid two-character code for "DEPARTMENT OF STATE" from 06/01/1972 to
# 05/31/1986 and from 03/01/2002 to the present. ST00 is a valid code for
# "Department of State" from 08/1/1995 to present.

# Need to make ST a valid code from 06/01/1972 to the present

agy_map <- agy_map |>
  mutate(date_to = if_else(code == "ST" & date_from == "1972-06-01",
    as.Date(max(sub$date_to, na.rm = TRUE)), date_to
  )) |>
  filter(code != "ST00") |>
  filter(!(code == "ST" & date_from == "2002-03-01"))

# Remove "00" and make all labels upper case

agy_map <- agy_map |>
  mutate(
    code = str_remove(code, "00"),
    label = str_to_upper(label)
  )

table(is.na(agy_map$code)) # No missing codes

agy_map <- clean_and_guard(agy_map, "agency_labels.csv", normalize = TRUE)
write_csv(agy_map, file = "metadata/labels/agency_labels.csv", na = "")

rm(sub)

# ============================================================
#### Education ####
# ============================================================

ct_ed <- find_code_table(path, "education_level")
ed_map <- read_code_table(file_path = ct_ed$file, download_date = ct_ed$download_date) |>
  mutate(code = str_pad(code, width = 2, pad = "0"))

# Strip trailing explanatory text after the period to keep labels concise.
ed_map <- ed_map |>
  mutate(label = str_remove(label, "\\..*"))

table(is.na(ed_map$code)) # No missing codes

ed_map <- clean_and_guard(ed_map, "ed_labels.csv")
write_csv(ed_map, file = "metadata/labels/ed_labels.csv", na = "")

# ============================================================
#### Occupation ####
# ============================================================

ct_occ <- find_code_table(path, "occupation")
occ_map <- read_code_table(file_path = ct_occ$file, download_date = ct_occ$download_date)

occ_map <- occ_map |>
  mutate(label = str_remove(label, "\\s*\\(.*"))

# Note: GAO EVALUATOR and AUTOPSY ASSISTANT have overlapping dates
occ_map |>
  filter(label == "GAO EVALUATOR" | label == "AUTOPSY ASSISTANT") |>
  arrange(label, date_from)

n <- nrow(occ_map)

occ_map <- occ_map |>
  filter(!(label == "GAO EVALUATOR" &
    (date_from == ymd("1990-01-01") |
      date_to == ymd("1989-12-31"))
  )) |>
  filter(!(label == "AUTOPSY ASSISTANT" &
    date_to == ymd("2018-02-28")))

n - nrow(occ_map) # 3 rows dropped

occ_map |>
  filter(label == "GAO EVALUATOR" | label == "AUTOPSY ASSISTANT") |>
  arrange(label, date_from) # correct cases retained

occ_map <- clean_and_guard(occ_map, "occ_labels.csv")
write_csv(occ_map, file = "metadata/labels/occ_labels.csv", na = "")

# ============================================================
#### Occupation category ####
# ============================================================

# Note: No past values of occupational categories

ct_oc <- find_code_table(path, "occupational_category")
occ_cat_map <- read_code_table(file_path = ct_oc$file, download_date = ct_oc$download_date)

occ_cat_map <- occ_cat_map |>
  mutate(label = str_remove(label, "\\..*"))

table(is.na(occ_cat_map$code)) # No missing codes

occ_cat_map <- clean_and_guard(occ_cat_map, "occ_cat_labels.csv")
write_csv(occ_cat_map, file = "metadata/labels/occ_cat_labels.csv", na = "")

# ============================================================
#### Pay plan ####
# ============================================================

ct_pp <- find_code_table(path, "pay_plan")
pp_map <- read_code_table(file_path = ct_pp$file, download_date = ct_pp$download_date) |>
  filter(!is.na(code))

pp_map <- pp_map |>
  mutate(label = str_remove(label, "\\s*(CODE IS )?FOR USE BY.*"))

table(is.na(pp_map$code)) # 0 NA

# Vector of duplicated labels
dup_pp_labels <- c(
  "ATTORNEYS AND LAW CLERKS",
  "COMMISSIONED CORPS OF THE PUBLIC HEALTH SERVICE",
  "DEFENSE INTELLIGENCE SENIOR EXECUTIVE SERVICE",
  "EVALUATOR AND EVALUATOR RELATED",
  "GRADED TRADESMEN AND CRAFTSMEN (EXCLUDING LITHOGRAPHERS AND PRINTERS)",
  "LEAD LITHOGRAPHERS AND PRINTERS", # JQ: subsumed dup (overlap check)
  "LEADERS OF TRADESMEN AND CRAFTSMEN", # JL: subsumed dup (overlap check)
  "NONSUPERVISORY LITHOGRAPHERS AND PRINTERS",
  "SUPERVISORS OF TRADESMEN AND CRAFTSMEN",
  "SUPERVISORY LITHOGRAPHERS AND PRINTERS"
)

pp_map |>
  filter(label %in% dup_pp_labels) |>
  arrange(label, date_from)

# All cases are duplicates with a shorter date range subsumed by the longer one
# with the same start date

n <- nrow(pp_map)

pp_map <- pp_map |>
  filter(!((label %in% dup_pp_labels) &
    date_to != ct_pp$download_date))

n - nrow(pp_map) # 8 rows removed

pp_map |>
  filter(label %in% dup_pp_labels) |>
  arrange(label, date_from) # Correct cases retained

pp_map <- clean_and_guard(pp_map, "pay_plan_labels.csv")
write_csv(pp_map, file = "metadata/labels/pay_plan_labels.csv", na = "")

# ============================================================
#### Supervisory status ####
# ============================================================

ct_sp <- find_code_table(path, "supervisory_status")
sp_map <- read_code_table(file_path = ct_sp$file, download_date = ct_sp$download_date)

sp_map <- sp_map |>
  mutate(label = str_remove(label, "\\..*"))

table(is.na(sp_map$code)) # No missing codes

sp_map <- clean_and_guard(sp_map, "super_stat_labels.csv")
write_csv(sp_map, file = "metadata/labels/super_stat_labels.csv", na = "")

# ============================================================
#### Type of appointment ####
# ============================================================

ct_ta <- find_code_table(path, "appointment_type_code")
ta_map <- read_code_table(file_path = ct_ta$file, download_date = ct_ta$download_date)

table(is.na(ta_map$code)) # No missing codes

ta_map <- clean_and_guard(ta_map, "appt_type_labels.csv")
write_csv(ta_map, file = "metadata/labels/appt_type_labels.csv", na = "")

# ============================================================
#### Work schedule ####
# ============================================================

ct_wk <- find_code_table(path, "work_schedule")
wk_map <- read_code_table(file_path = ct_wk$file, download_date = ct_wk$download_date)

wk_map <- wk_map |>
  mutate(label = str_remove(label, "\\..*"))

table(is.na(wk_map$code)) # No missing codes

wk_map <- clean_and_guard(wk_map, "work_sched_labels.csv")
write_csv(wk_map, file = "metadata/labels/work_sched_labels.csv", na = "")

# ============================================================
#### Personnel office identifier ####
# ============================================================

# POI is a flat CSV (no Current/Past Values sheets). Join key in hive is
# personnel_office_identifier_code + agency_subelement_code + agency_subelement.
# Not time-varying (no date windows)

ct_poi <- find_code_table(path, "POI", ext = "csv")
poi_map <- read_csv(ct_poi$file, col_types = "cccccc", na = "") |>
  rename(
    personnel_office_identifier_code = `POI Code`,
    agency_subelement_code = `Agency Subelement Code`,
    agency_subelement = `Agency Subelement Name`,
    personnel_office_name = `Personnel Office Name`,
    poi_city = City,
    poi_state_country = `State/Country`
  )

if (any(duplicated(poi_map |>
                   select(personnel_office_identifier_code,
                          agency_subelement_code,
                          agency_subelement)))) {
  stop("Duplicated POI key: personnel_office_identifier_code + agency_subelement_code + agency_subelement.")
}

write_csv(poi_map, file = "metadata/labels/poi_labels.csv", na = "")

# ============================================================
#### Accession type labels ####
# ============================================================

acc_codes <- read_csv(file = "metadata/code_tables/DTacc.txt") |>
  rename(
    accession_category_code = ACC,
    accession_category = ACCT
  ) |>
  add_row(
    accession_category_code = "A",
    accession_category = "Accession"
  )

if (any(duplicated(acc_codes$accession_category_code))) {
  stop("Duplicated accession codes.")
}

write_csv(acc_codes, file = "metadata/labels/acc_labels.csv", na = "")

# ============================================================
#### Separation type labels ####
# ============================================================

sep_codes <- read_csv(file = "metadata/code_tables/DTsep.txt") |>
  rename(
    separation_category_code = SEP,
    separation_category = SEPT
  ) |>
  add_row(
    separation_category_code = "S",
    separation_category = "Separation"
  ) |>
  add_row(
    separation_category_code = "SI",
    separation_category = "Termination or Removal (Discipline/Performance)"
  )

if (any(duplicated(sep_codes$separation_category_code))) {
  stop("Duplicated separation codes.")
}

write_csv(sep_codes, file = "metadata/labels/sep_labels.csv", na = "")

# ============================================================
#### Duty Station codes ####
# ============================================================

# From 2026-09 OPM serves Duty Stations only as a two-sheet .xlsx (see
# read_duty_stations() in utils/code_table_functions.R). 08_bz_duty_station_state.R
# and 13_duty_station_backfill.R still read the flat CSV, so it is derived here and
# written beside the workbook under the same download date. find_code_table()
# rejects >1 csv, so an old derived CSV must be archived to prior_downloads/ along
# with its workbook -- the archived copy is what next month's gate diffs against.
ct_ds <- find_code_table(path, "DutyStations", ext = "xlsx")
ds_map <- read_duty_stations(ct_ds$file)

if (any(duplicated(ds_map$Code))) {
  stop("Duplicated Duty Station codes.")
}

write_csv(ds_map,
  file = paste0(path, "DutyStations_", format(ct_ds$download_date, "%Y%m%d"), ".csv"),
  na = ""
)

# ============================================================
#### Provenance stamp (freshness guard) ####
# ============================================================

# Record which OPM code-table download each master's labels were built from. Because
# label building is a MANUAL gated step (not part of 00_build_fwoc.R), the build
# reads this stamp and hard-fails if the committed metadata/labels/ lag a newer
# code_tables refresh -- so regenerated labels can never be silently forgotten.
# tribble keeps each table name next to its own download date on one row, so the
# two can never drift out of alignment the way parallel vectors can.
labels_stamp <- tibble::tribble(
  ~table,                   ~download_date,
  "agency_subelement",      ct_agy$download_date,
  "education_level",        ct_ed$download_date,
  "occupation",             ct_occ$download_date,
  "occupational_category",  ct_oc$download_date,
  "pay_plan",               ct_pp$download_date,
  "supervisory_status",     ct_sp$download_date,
  "appointment_type_code",  ct_ta$download_date,
  "work_schedule",          ct_wk$download_date,
  "POI",                    ct_poi$download_date,
  "DutyStations",           ct_ds$download_date
)

write_csv(labels_stamp, file = "metadata/labels/labels_build_stamp.csv", na = "")

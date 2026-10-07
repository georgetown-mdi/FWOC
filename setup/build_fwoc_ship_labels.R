# Author: Mark Richardson and Claude Code
# Purpose: Build the FWOC agency and subagency label tables
#   (metadata/labels/agency_labels_fwoc.csv, subagency_labels_fwoc.csv; consumed
#   by 09_agency_names.R) and the codebook's table of differences from OPM
#   (metadata/codebook/opm_fwoc_differences.csv). Does not touch any hive data.
#
#   The FWOC labels are OPM's labels (metadata/labels/agency_labels.csv,
#   subagency_labels.csv, built by setup/build_labels.R) with three kinds of
#   change, each listed row by row in a reviewed table under
#   metadata/labels/fwoc_ship/:
#     nominal_events.csv  OPM label changes that are two renderings of one name;
#                         both sides take the later label
#     relabels.csv        labels replaced outright: a label that names the wrong
#                         unit, an OPM typo or placeholder, a statutory name kept
#     os_lineages.csv     Office of the Secretary codes, standardized to
#                         "OFFICE OF THE SECRETARY OF <DEPARTMENT>"
#   This script holds only the mechanics. To change a label, edit a table and
#   re-run; never edit the output files.
#
#   Run from the fwoc/ project root, after setup/build_labels.R whenever the OPM
#   masters change:
#     Rscript setup/build_fwoc_ship_labels.R
#   The script stops if an admitted boundary in nominal_events.csv no longer
#   matches OPM's labels (OPM revises its history) or if a window in the other
#   two tables falls outside OPM's coverage of the code.

library(dplyr)
library(readr)
library(stringr)

source("utils/code_table_functions.R") # assert_no_label_overlaps()
source("utils/fwoc_ship_functions.R")

in_path <- "metadata/labels/fwoc_ship/"

# ============================================================
#### Inputs ####

# na = "": the agency code "NA" is a real code, and an empty date_to in
# relabels.csv / os_lineages.csv means "through the end of OPM's coverage".
opm_agy <- read_csv("metadata/labels/agency_labels.csv",
  col_types = "ccDD", na = ""
)
opm_subagy <- read_csv("metadata/labels/subagency_labels.csv",
  col_types = "ccDD", na = ""
)

events <- read_csv(paste0(in_path, "nominal_events.csv"),
  col_types = cols(.default = "c", event_date = "D"), na = ""
)

relabels <- read_csv(paste0(in_path, "relabels.csv"),
  col_types = cols(
    .default = "c", date_from = "D", date_to = "D",
    opm_label_misleading = "l"
  ),
  na = ""
) |>
  select(-source)

# The Office of the Secretary rows are all subagency rows, never misleading,
# and carry no note; add those columns so they stack with relabels.
os_lineages <- read_csv(paste0(in_path, "os_lineages.csv"),
  col_types = cols(.default = "c", date_from = "D", date_to = "D"),
  na = ""
) |>
  mutate(
    level = "subagency",
    opm_label_misleading = FALSE,
    opm_label_note = ""
  ) |>
  select(-source)

windows <- bind_rows(relabels, os_lineages)

# ============================================================
#### Agency ####

agy_start <- opm_agy |>
  mutate(opm_label_misleading = FALSE, opm_label_note = "", reason = "")

agy_ship <- agy_start |>
  merge_contiguous_rows() |>
  collapse_nominal_runs(filter(events, level == "agency")) |>
  apply_label_windows(filter(windows, level == "agency") |> select(-level)) |>
  merge_contiguous_rows()

# ============================================================
#### Subagency ####

# 09_agency_names.R recodes every Air Force, Army and Navy subelement to DDAF,
# DDAR or DDNV, so those three codes need a label row and the departments' own
# subelement codes are never looked up. OPM's table has no row for the three
# codes; each takes the label and dates of OPM's agency row.
mil_dept_rows <- opm_agy |>
  filter(code %in% c("AF", "AR", "NV")) |>
  mutate(code = case_when(
    code == "AF" ~ "DDAF",
    code == "AR" ~ "DDAR",
    code == "NV" ~ "DDNV"
  ))

subagy_start <- opm_subagy |>
  filter(!str_sub(code, 1, 2) %in% c("AF", "AR", "NV")) |>
  bind_rows(mil_dept_rows) |>
  mutate(opm_label_misleading = FALSE, opm_label_note = "", reason = "")

subagy_ship <- subagy_start |>
  merge_contiguous_rows() |>
  collapse_nominal_runs(filter(events, level == "subagency")) |>
  apply_label_windows(filter(windows, level == "subagency") |> select(-level)) |>
  merge_contiguous_rows()

# ============================================================
#### Checks ####

# The FWOC tables must cover exactly the codes and days the starting tables do:
# the aggregates drop records whose FWOC label is missing, so lost coverage
# would silently lose records. Days are counted per code on each side.
agy_days <- full_join(
  agy_start |> group_by(code) |>
    summarize(start_days = sum(as.integer(date_to - date_from) + 1L)),
  agy_ship |> group_by(code) |>
    summarize(ship_days = sum(as.integer(date_to - date_from) + 1L)),
  by = "code"
)
subagy_days <- full_join(
  subagy_start |> group_by(code) |>
    summarize(start_days = sum(as.integer(date_to - date_from) + 1L)),
  subagy_ship |> group_by(code) |>
    summarize(ship_days = sum(as.integer(date_to - date_from) + 1L)),
  by = "code"
)
stopifnot(
  !anyNA(agy_days), all(agy_days$start_days == agy_days$ship_days),
  !anyNA(subagy_days), all(subagy_days$start_days == subagy_days$ship_days)
)

# Overlapping windows would match a record to two labels and duplicate hive rows.
assert_no_label_overlaps(agy_ship, "agency_labels_fwoc.csv")
assert_no_label_overlaps(subagy_ship, "subagency_labels_fwoc.csv")

# ============================================================
#### Outputs ####

# reason is not written to the label files; it goes to the differences table.
write_csv(select(agy_ship, -reason),
  file = "metadata/labels/agency_labels_fwoc.csv", na = ""
)
write_csv(select(subagy_ship, -reason),
  file = "metadata/labels/subagency_labels_fwoc.csv", na = ""
)

differences <- bind_rows(
  ship_differences(agy_ship, opm_agy, "agency"),
  ship_differences(subagy_ship, opm_subagy, "subagency")
)
write_csv(differences, file = "metadata/codebook/opm_fwoc_differences.csv", na = "")

cat(
  "agency rows:", nrow(agy_ship), "| subagency rows:", nrow(subagy_ship),
  "| rows differing from OPM:", nrow(differences), "\n"
)
print(count(differences, level, reason), n = Inf)

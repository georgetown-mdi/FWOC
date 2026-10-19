# FWOC aggregate tables

Per-period counts built from the FWOC employment, accession and separation
hives. These are the hives summed up, nothing more: no filters, no lagging,
no ratios. They exist so that common questions can be answered without
downloading the microdata.

All files are plain CSV with a header row. An empty cell is a missing value.

## Files

| File | Source hive | One row per |
|---|---|---|
| `emp_counts_gov.csv` | employment | period (executive branch) |
| `emp_counts_agency.csv` | employment | period x agency |
| `emp_counts_subagency.csv` | employment | period x subagency |
| `acc_counts_gov.csv` | accessions | period (executive branch) |
| `acc_counts_agency.csv` | accessions | period x agency |
| `acc_counts_subagency.csv` | accessions | period x subagency |
| `sep_counts_gov.csv` | separations | period (executive branch) |
| `sep_counts_agency.csv` | separations | period x agency |
| `sep_counts_subagency.csv` | separations | period x subagency |
| `politicization_agency.csv` | employment | period x agency (executive branch) |
| `politicization_subagency.csv` | employment | period x subagency (executive branch) |

## Periods

Employment rows are snapshots. `date` is the as-of day of the snapshot, and
`year` and `month` are its calendar year and month. Snapshots are quarterly
through June 2011 and monthly from July 2011. The snapshot day varies by era
(for example, the fourth-quarter snapshot in the FOIA years falls on
December 13), so use `date` when you need the exact day.

Accession and separation rows are flows. They carry `year` and `month` only,
because the underlying effective dates are day-level before 2005 and
month-level from 2005. A row counts the actions effective in that calendar
month.

Nothing is shifted. An accession count for a month sits beside the employment
snapshot for that same month, not the prior one. If you compute a rate,
choose and align the denominator period yourself.

## Agencies and subagencies

Units are the FWOC agency and subagency codes and labels
(`agency_code_fwoc`, `agency_fwoc`, `agency_subelement_code_fwoc`,
`agency_subelement_fwoc`). Under the FWOC codes the Departments of the Army,
Navy and Air Force are subelements of the Department of Defense (`DDAR`,
`DDNV`, `DDAF` under `DD`) rather than separate agencies. The codebook
describes how the FWOC codes differ from OPM's.

Labels change over time. A unit keeps one code but may carry different labels
in different periods, so group on the code, not the label, when following a
unit through time.

Rows whose agency could not be assigned a FWOC code appear with an empty code
and label. They are included so that the agency and subagency files sum to
the hives.

## Indicator columns

Agency and subagency rows in the counts files carry descriptor columns taken
from the hives. All are 0/1 integers, empty where the agency code is missing.
The politicization files cover the executive branch only and carry just `eop`
and `core_eop`, since the branch indicators do not vary within it.

| Column | Meaning |
|---|---|
| `exec_branch` | Agency is in the executive branch |
| `leg_branch` | Agency is in the legislative branch |
| `jud_branch` | Agency is in the judicial branch |
| `eop` | Agency is in the Executive Office of the President in that period |
| `core_eop` | Agency is a core EOP component in that period |
| `a1_court` | Unit is an Article I court |

No branch filter has been applied to the agency and subagency counts files.
They include the handful of legislative and judicial agencies that report to
OPM. Filter on `exec_branch` to match the usual executive-branch universe.

The U.S. Court of Appeals for the Armed Forces is an Article I court that sits
inside the Department of Defense as subelement `DD08`. It is left in `DD`
rather than broken out as its own agency, so in the agency files `DD` carries
`a1_court` of 0. In the subagency files the `DD08` row carries 1. The codebook
explains why the court counts as executive branch.

The government files are the executive branch only (`exec_branch` equal to
1) and carry no indicator columns. They equal the agency files summed over
rows with `exec_branch` equal to 1.

## Count columns

| Column | Files | Meaning |
|---|---|---|
| `n_all` | `*_counts_*` | Every record in the period |
| `n_nsftp` | `*_counts_*` | Records with `nsftp_indicator_fwoc` equal to 1 (non-seasonal, full-time, permanent) |
| `n_app_all` | `politicization_*` | Political appointees: appointment type 34, 44 or 55, or pay plan EX |
| `n_app_pas` | `politicization_*` | Presidential appointees with Senate confirmation (pay plan EX) |
| `n_app_sc` | `politicization_*` | Schedule C appointees (appointment type 34 or 44) |
| `n_app_na_ses` | `politicization_*` | Noncareer Senior Executive Service (appointment type 55) |
| `n_denom_mngr` | `politicization_*` | Managers: any reported supervisory status other than 8 (All other positions). Under OPM's current codes that is 2 supervisor or manager, 4 supervisor, 5 management official, 6 leader and 7 team leader |
| `n_denom_ca_ses` | `politicization_*` | Career Senior Executive Service (appointment type 50) |

A record whose source code is missing or not specified (OPM's `*` or `**`)
counts in `n_all` but in none of the subset columns, so the subsets need not
sum to `n_all` and a subset can be smaller than it would be with complete
data. This matters most for `n_nsftp`
before 1986: OPM did not report an appointment type for about 14 percent of
employment records from 1973 to 1981 and for a declining share through 1985,
and those records are excluded from `n_nsftp` even where their work schedule
is full-time.

The politicization files start in 1982, when appointment type codes become
available, and cover the executive branch only. They report counts only. Divide the appointee column you want by
the denominator you want to form a ratio. For an all-employee or NSFTP
denominator, take `n_all` or `n_nsftp` from the employment counts file at the
same level, joined on `date` and the agency or subagency code.

The three appointee categories are not mutually exclusive. A small number of
records carry pay plan EX together with a Schedule C or noncareer SES
appointment type, so `n_app_pas`, `n_app_sc` and `n_app_na_ses` added
together can slightly exceed `n_app_all`, which counts each record once.
Use `n_app_all` for the total.

## Provenance

Built by `aggregate/r2/agg_counts.R` and
`aggregate/r2/agg_politicization_counts.R` in the FWOC repository, from the
same hives published in the `parquet/` and `csv/` prefixes of this bucket.
The definitions of the appointee and denominator columns match the FWOC
politicization dashboard.

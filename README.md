# FWOC

The Federal Workforce OpenCommons (FWOC) is a public platform for curated United States federal civilian employment data, hosted by Georgetown University's Massive Data Institute at <https://opencommons.mdi.georgetown.edu>. It is not affiliated with, or endorsed by, the Office of Personnel Management or the United States Government. This repository holds the R/Arrow pipeline that builds the FWOC microdata: it assembles records released by the Office of Personnel Management (OPM) from 1973 to the present into three harmonized, labeled Parquet datasets, validates them, and exports them for publication. It also holds the codebook and example code for working with the published data.

The repository contains code, documentation, and small metadata only. The source data are described below and the curated data are described and available on the [FWOC Download page](https://opencommons.mdi.georgetown.edu/fwoc/download-page).

## Using the published data

You do not need this pipeline to use the data. The [FWOC Download page](https://opencommons.mdi.georgetown.edu/fwoc/download-page) serves each dataset as zipped Parquet, CSV, and Stata files.

- `quick_start_code/01_download_subset.R` provides a script to download a bespoke subset (one dataset, selected agencies, a range of months) as Parquet files, and `02_open_subset.R` shows how to open the subset without loading it into memory and how to save a piece as CSV or Stata. Both need only the `arrow`, `dplyr`, and `jsonlite` packages.
- `docs/fwoc_codebook.qmd` is the codebook: one entry per published variable, with definitions, types, coverage, and value labels. It is rendered to PDF with Quarto and published on the site. The table it is built from is `metadata/codebook/fwoc_codebook_table.csv`.

## The data

The pipeline combines two sources of OPM administrative records:

| Source | Short name | Period used | Status |
|---|---|---|---|
| OPM records released under three FOIA requests and [placed in the public domain by BuzzFeed News](https://archive.org/details/opm-federal-employment-data) | BZ | 1973-09 through 2004 | Frozen |
| [Federal Workforce Data](https://data.opm.gov/), OPM's data site | FWD | 2005 to present | Updated monthly with a two-month lag |

The BuzzFeed releases run to 2017-03, but only their pre-2005 rows are in the final datasets. The 2005–2017 overlap between the two sources is used to validate one against the other.

The pipeline produces three datasets ("hives"), each partitioned as `year=yyyy/month=m/`:

| Hive | Contents | Coverage |
|---|---|---|
| `emp_hive` | Employment: end-of-period snapshots of the workforce | Quarterly from 1973-09 through 2011-06, monthly thereafter |
| `acc_hive` | Accessions: employees joining an agency | 1982 to present |
| `sep_hive` | Separations: employees leaving an agency | 1982 to present |

Where FWOC publishes an alternative version of an OPM variable, the FWOC version carries a `_fwoc` suffix and sits beside the OPM original. For example, `agency` is OPM's agency name and `agency_fwoc` is FWOC's. `metadata/codebook/fwoc_codebook_table.csv` documents every published variable, including the few OPM columns the pipeline edits in place, and `metadata/codebook/opm_fwoc_differences.csv` lists each place the FWOC agency and subagency labels differ from OPM's.

## Repository layout

```
00_build_fwoc.R     Runs the whole build in order
01_ ... 16_         Numbered pipeline steps
setup/              Builds the pipeline's inputs: label tables, BLS series, renv.lock
utils/              Helper functions sourced by the scripts
crosswalks/         Builds the occupational category (PATCO) crosswalk
aggregate/          Builds the aggregate tables behind the FWOC dashboards
validation/         Post-build validation suite
codebook/           Rebuilds the codebook table from the built hives
docs/               The codebook and user guide, as Quarto documents (_quarto.yml renders them from the root)
quick_start_code/   Example scripts for downloading and opening the published data
metadata/           Small committed inputs: OPM code tables, label tables, crosswalks,
                    validation baselines, the codebook table, the variable manifest
```

The repository root is the project root. Run every script from there; all paths in the code are relative to it.

Generated at the root, and ignored by git: `buzz_feed/`, `data_opm_gov/`, `emp_hive/`, `acc_hive/`, `sep_hive/`, `aggregate_data/`, `export/`, `validation_output/`, `logs/`.

## What the pipeline was built with

None of this is a hard requirement; it is the environment the pipeline was developed in and built with.

- **R.** The R version and the packages used for the latest build are recorded in `renv.lock`. `renv::restore()` installs the same set.
- **Linux**, on a machine with 24 cores and 250 GB of RAM. Only the export step (15) depends on the platform: it stages files in `/dev/shm` and calls `zip`, `xargs`, and `rsync`. Steps 01–14, the validation suite, and the codebook scripts use only R.
- **Memory.** The scripts never load a whole hive: every step either stays lazy in Arrow or processes the employment hive one partition at a time.
- **Disk** (as of the October 2026 build, with data through July 2026; every monthly release adds to these). The three final hives together are about 57 GB of Parquet: `emp_hive` is 55 GB, `acc_hive` and `sep_hive` under 1 GB each. Rebuilding them takes about 180 GB in all, with roughly 60 GB of BuzzFeed data and staging and 55 GB of OPM downloads alongside the hives. The public export adds about 220 GB.
- **Time** (most recent build, 2026-10-04). Steps 01 through the codebook took 9.5 hours: about 9 hours to the final hives (steps 01–14), then 4 minutes for the aggregates, 9 minutes for validation, and 18 minutes for the codebook. The public export (step 15) ran separately on 2026-10-06 and took about 9 hours. Steps 01 and 02 are one-time; without them a monthly build is about an hour shorter.
- **Quarto**, only to render the codebook. Render it from the repository root with `quarto render docs/fwoc_codebook.qmd`; it reads `metadata/codebook/gap_detail.csv`, which `codebook/02_codebook_build.R` writes and git ignores, so on a fresh clone the build must run first.

## Acquiring the raw inputs

**BuzzFeed FOIA data.** Download the three releases from the [Internet Archive](https://archive.org/details/opm-federal-employment-data) and arrange them under `buzz_feed/data/`, one folder per release, each split into `dod/` and `non-dod/`:

```
buzz_feed/data/
  1973-09-to-2014-06/    SCTFILE.TXT, dod/, non-dod/   (each with status/ and dynamic/)
  2014-09-to-2016-09/    dod/, non-dod/                (each with status/, accessions/, separations/)
  2016-12-to-2017-03/    dod/, non-dod/                (each with status/)
```

`SCTFILE.TXT` is OPM's code table file from the first release. `setup/build_duty_station_labels.R` parses it into `metadata/labels/duty_station/ds_labels.RData`, which is committed, so that script needs running only if you want to regenerate the file.

**Federal Workforce Data.** Step 03 downloads the employment, accessions, and separations files from the [data.opm.gov API](https://data.opm.gov/get-data/api-access) into `data_opm_gov/`. No API key is needed. The step downloads only what is missing, keeps every version OPM has published, and records the files it used in `metadata/fwd/file_manifest.csv`.

**OPM code tables.** The code tables and data dictionary that the labels are built from are committed under `metadata/code_tables/`, each with its download date in the file name. For a monthly refresh, download the current tables from data.opm.gov into that folder and move the previous set into `metadata/code_tables/prior_downloads/<YYYY_MM>/`, so the setup chain can compare the two. Current versions are available at [OPM's EHRI Data Standards page](https://data.opm.gov/data-standards/ehri-data-standards).

**BLS series.** `setup/get_bls_data.R` downloads CPI-U and the national and state unemployment rates from the Bureau of Labor Statistics. BLS requires a descriptive User-Agent that identifies you, so set one before running setup, for example in `~/.Renviron`:

```
BLS_USER_AGENT="Your Name your@email.org"
```

## Running the pipeline

### 1. Setup

```
Rscript setup/00_run_setup.R
```

Setup takes under a minute and is meant to be watched. It writes `renv.lock`, compares the OPM code tables against the previous download, rebuilds the label tables in `metadata/labels/`, and downloads the BLS series.

If OPM's code tables changed since the previous download, setup writes a change report to `metadata/code_tables/change_reports/` and stops. Review the report, then acknowledge it and re-run:

```
FWOC_METADATA_ACK=1 Rscript setup/00_run_setup.R
```

Review the resulting changes under `metadata/` and commit them before building.

### 2. Build

```
Rscript 00_build_fwoc.R
```

The build runs each step in a fresh R process, appends all output to `logs/fwoc_build_log_<timestamp>.txt`, and halts at the first step that fails.

**First run on a fresh clone:** steps 01 and 02, which parse the BuzzFeed releases, are commented out in `00_build_fwoc.R` because that data never changes. Uncomment them for the first build, then comment them out again for the monthly cycle.

The build has four phases:

| Phase | Steps | What it does |
|---|---|---|
| Assemble | 01–04 | Parse the BuzzFeed releases and download and assemble the FWD files into staging hives |
| Harmonize and enrich | 05–14 | Bring the two sources to one schema, write the final hives, attach labels, derive the FWOC variables, join the BLS series, and set the published column order |
| Aggregate and validate | `aggregate/`, `validation/` | Build the dashboard tables, then run the validation suite |
| Document and publish | `codebook/`, 15, 16 | Rebuild the codebook table, export the public files, and upload them |

Validation is a gate: if any hard check fails, the codebook, export, and upload steps do not run.

**If you only want the hives, stop after validation.** The final `emp_hive`, `acc_hive`, and `sep_hive` are complete once step 14 and the validation suite have run. The last phase exists to publish the FWOC site: the codebook scripts rebuild the codebook table, step 15 writes the download files, and step 16 uploads them. To stop there, comment out the two `codebook/` lines and the step 15 line in `00_build_fwoc.R`; step 16 never runs unless `FWOC_PUBLISH=1` is set. Keep the `aggregate/` steps, because validation exercise 6 checks their output.

### 3. Validation

`validation/00_run_validation.R` runs six exercises and writes its reports to `validation_output/`:

| Exercise | Checks |
|---|---|
| 1. Overlap | BZ against FWD over the period both cover, and FWD revisions against the recorded baseline |
| 2. Labels | Every valid, in-range code has a label |
| 3. Reconciliation | Row counts in the final hives equal the source counts for every period |
| 4. Conformance | Published columns and types match the codebook, and no period is missing |
| 5. Sanity and history | Indicator and domain checks, and row counts of past periods against the recorded history |
| 6. Aggregates | The dashboard rate tables reconcile to the hives |

The files in `metadata/validation/baselines/` record the known, accepted discrepancies.

### 4. Export and publication

These steps produce the files served by the FWOC site. They are not needed to rebuild or use the hives locally.

Step 15 writes the public export to `export/`, in the layout the FWOC Download page provides: `parquet/hive/` (the partitioned hives), `parquet/zip/` (each hive zipped), and `csv/` and `stata/` (accessions and separations as single files, employment as one or more files per agency under `agency/<code>/`).

Step 16 and `aggregate/push_to_gcs.R` upload to the FWOC project's own cloud storage. They are included for transparency and you will not have write access to those buckets. To publish your own build, point them at storage you control. Step 16 refuses to upload unless `FWOC_PUBLISH=1` is set, so a routine build never publishes by accident; `Rscript 16_push_to_r2.R --dry-run` previews an upload without changing anything.

### Environment variables

| Variable | Used by | Purpose |
|---|---|---|
| `BLS_USER_AGENT` | `setup/get_bls_data.R` | Identifies you to the BLS download server; required |
| `FWOC_METADATA_ACK` | `setup/check_opm_metadata.R` | Set to `1` to acknowledge a reviewed change in the OPM code tables |
| `FWOC_PUBLISH` | `00_build_fwoc.R`, `16_push_to_r2.R` | Set to `1` to allow the upload of the public export (requires `rclone`) |
| `FWOC_GCS_DEST` | `aggregate/push_to_gcs.R` | Destination bucket for the dashboard tables (requires `gcloud`) |

## Working with the hives

A locally built hive opens exactly like a downloaded subset: point `open_dataset()` at the hive folder (`emp_hive`, `acc_hive`, or `sep_hive`), filter and aggregate in Arrow, and `collect()` only the reduced result. `quick_start_code/02_open_subset.R` shows this step by step. The employment hive is far too large to load whole on most machines.

`CLAUDE.md` records the conventions the pipeline code follows, including the Arrow idioms that keep it within memory. It is written for AI coding assistants and is also a useful summary for contributors.

## License

The code is released under the MIT License; see `LICENSE`. The license covers the code in this repository, not the OPM records the pipeline processes.

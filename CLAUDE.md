# CLAUDE.md

<!-- Guidance for Claude Code in this repo. `README.md` is the human-facing guide; this file is the agent-facing architecture + conventions. Keep it concise — it loads into every session. -->

## What this is

An R/Arrow ETL pipeline that assembles United States federal civilian employment microdata (~1973–present) into three hive-partitioned Parquet datasets.

There are two data sources:
1. BuzzFeed (BZ) — 1973-09 to 2017-03; frozen.
2. Federal Workforce Data (FWD) — data.opm.gov, 2005–present; updated monthly (a new month, plus irregular revisions of prior releases).

There are three types of data:
1. `emp_hive`: stock — end-of-period snapshots (`employment`).
2. `acc_hive`: flow — employees joining a government agency (`accessions`).
3. `sep_hive`: flow — employees leaving a government agency (`separations`).

Only BZ's pre-2005 rows reach the final hives.

The only top-level era split is BZ vs FWD — branch on that, not on BZ's three releases.

## Repository layout

The repository root **is** the project root — run scripts from here and reference paths root-relative (`metadata/...`, `utils/...`, `source("utils/x.R")`).

```
setup/     Builders of inputs to the main ETL pipeline scripts
utils/     Reusable helpers sourced by the scripts
metadata/  Static inputs used by the main ETL pipeline scripts
tests/     testthat suite for the pure helpers (run by CI)
```

## Data model (descriptive)

- **Dates**
  - emp:
    - end-of-period (BZ Q4 = `12-13`, not `12-31`).
    - emp data has day-level time grain.
    - snapshots are quarterly through 2011-06 and monthly thereafter.
    - `date` contains the as-of date for snapshots across BZ and FWD eras.
    - label-joins use `date`.
  - acc/sep:
    - label-joins use first-of-month for a consistent key across the acc/sep hives — their grain is mixed (day-level pre-2005 BZ, month-level 2005+ FWD).
    - `eff_date` is BZ-only, `personnel_action_effective_date_yyyymm` is FWD-only; use year/month (present for all rows) as the unified time key.
  - Checks must match the dataset's own join convention.

- **Partitioning**
  - All hives are partitioned by `year=yyyy/month=mm/` (month is not zero padded).

- **Names**
  - `subagency`, `subelement`, and `agency_subelement` are interchangeable.
  - Stock data are called `employment` in FWD, `status` in BZ.
  - Flow data are called `accessions` and `separations` in FWD; BZ combines them as `dynamic` through 2014-06.

## Conventions

### The hives are huge — keep pipelines lazy; don't materialize a full hive

Materializing the whole `emp_hive` in memory needs on the order of hundreds of GB to low-TB of RAM — beyond typical machines. `collect()`, `compute()`, `as.data.frame()`, `as_tibble()`, and `pull()` pull the *current* result into memory; only `write_dataset()` streams to disk without materializing. A full-hive `collect()` OOMs on any ordinary machine (and where RAM is constrained it may **hang** rather than raise R's allocation error).

- Keep pipelines lazy end-to-end: `open_dataset() |> filter() |> mutate() |> … |> write_dataset()`. To transform-and-save large data, **end in `write_dataset()`, never `collect()`**.
- `collect()` is safe **only on a reduced result**: after `group_by() |> summarize()` (the aggregate is small), after `filter()`/`head(n)` narrows to a known-small slice, or on one partition (`filter(year == Y, month == M)`).
- Unsure of a result's size? Check first — `ds |> count() |> collect()` or `nrow(ds)` (Parquet metadata, no data pages) — and don't collect unless it's obviously small.
- If a transform can't be expressed in Arrow, iterate **one `(year, month)` partition at a time** (one period fits), then `rm()` + `gc()`.

### Arrow / dplyr

- Read hives with `open_dataset(..., unify_schemas = TRUE)`.
- Write hives with `write_dataset(..., partitioning = c("year", "month"), hive_style = TRUE, compression = "zstd", existing_data_behavior = "delete_matching", use_dictionary = FALSE)`. Two args are load-bearing for correctness:
  - **`use_dictionary = FALSE`** — per-file dictionaries give the same string different indices across partitions, so `unify_schemas` then fails or mis-aligns categories.
  - **`existing_data_behavior = "delete_matching"`** — the default (`"overwrite"`) leaves a prior run's stale files in a partition dir, duplicating rows on re-read; `delete_matching` clears each written partition first, so step re-runs and rebuilds stay idempotent.
- These forms are load-bearing — get them wrong and you either silently mis-resolve, pull the dataset into RAM (an OOM trap), or hit a cryptic error:
  - `!!sym(var)` in data-masking verbs, **not** `.data[[var]]`.
  - `TRUE ~ col` in `case_when`, **not** `.default = col`.
  - every element of `by=` named if any is.
  - `arrow_table(data, schema = )` matches **by position** — new columns append to the tibble end, so the schema must follow.

<!--
Human "why" for the last two traps above (Claude acts on the terse rules as written; this note is for maintainers):

by= naming: Arrow's dplyr join binding cannot mix named and unnamed keys the way base dplyr can. If any key is renamed (its column name differs between the two tables), every key must be spelled out as "k" = "k" — even the ones identical on both sides. The mixed form throws a cryptic error where the missing column prints as an empty backtick pair, because Arrow mis-parses the unnamed element as a zero-length name.
- e.g., left_join(x, y, by = c("a" = "b", "k" = "k"))

arrow_table(schema=): Arrow binds schema fields to data columns by position, not by name (field 1 to column 1, field 2 to column 2, and so on). When an OPM release adds a variable, our mutate() lands it as the last tibble column, so its field must be appended in the last schema position too. Miss it and every column from that point on silently gets the wrong type and name — no error is raised, just a mistyped or mislabeled column downstream. Alternative: omit the explicit schema and let Arrow infer types by name.
-->

### Missing values & reading

- Every readr read/write of a text file (CSV/TSV/pipe/fixed-width) passes `na = ""`: a literal `"NA"` is a real code (e.g., the National Aeronautics and Space Council agency), an empty cell means missing. Text has no typed null, so the default spells the real code and missing identically; Parquet has a typed null and is unaffected.
- Read code variables as character (`col_types = cols(.default = "c")`) to avoid mapping sentinels to NA (e.g., `**` for a two-digit numeric code) and to preserve zero-padded codes.

### Code style

- Tidyverse style: snake_case, single-space assignment, native `|>` pipe, no aligned `=`/`~` columns. Run `styler` (tidyverse) on new/edited files.
- Prefer the engine over row-wise R: express transforms in Arrow / `data.table` / vectorized dplyr rather than per-row loops or applies — that (not loop-vs-apply) is the performance lever on large data.
- Clarity over cleverness: extract a named, testable helper rather than a complex inline anonymous function.
- Comments are high-signal — explain the *why* and the non-obvious OPM/domain quirk, not the *what*.
- `rm()` + `gc()` between heavy steps; `setDTthreads(parallel::detectCores() - 2)` per step.

### Tests

Pure helpers (`utils/*` + pure transforms) get a `testthat` test with a tiny fixture (build it in a `tempdir()` when simple). IO/hive pipeline steps are **not** unit-tested — `validation/` is their regression check. The suite runs with `Rscript tests/run_tests.R` (also run in CI).

### Surfacing decisions

Flag non-obvious or judgment-call design choices in your message so a human can review before they land.

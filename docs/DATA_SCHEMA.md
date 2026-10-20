# Data Schema (Frozen Snapshot, Update-Friendly)

## Snapshot Philosophy
- The deployed app reads only from `app/data/` (frozen snapshot).
- Update path: rerun build script to regenerate snapshot artifacts, then redeploy.
- Store enough metadata to reproduce the snapshot later (source notes, date, year range, transform version).

## Files (Recommended)
- `app/data/snapshot_meta.json`
- `app/data/schools.csv` (dimension table)
- `app/data/divisions_polygons.json` (division polygons as lon/lat rings; converted to `sp` at runtime)
- `app/data/school_metrics.csv` (fact table, long)
- `app/data/division_metrics.csv` (fact table, long; pre-aggregated)
- `app/data/school_subject_perf.csv` (historical accreditation-export ACR and Percent Passing only; annual SOL uses the long metric tables)
- `app/data/division_subject_perf.csv` (subject-level performance rollups)
- `app/data/metric_defs.csv` (metric catalog: labels, units, formatting)

## Tables (Recommended Columns)

### `schools` (one row per school)
- `school_id` (string, stable join key; keep both state and NCES IDs if available)
- `school_name`
- `division_id`, `division_name`
- `lat`, `lon`
- `school_level` (Elementary/Middle/High/Other)
- `enrollment` (by year lives in metrics; include latest here optionally for convenience)
- `open_status` (optional)

### `divisions` (one row per division)
- `division_id`, `division_name`
- `geometry` (stored as `divisions_polygons.json`, built into `sp` polygons for leaflet)

### `school_metrics` (long fact table)
- `school_id`
- `year` (integer; 2026 means school year 2025–26 for annual SOL)
- `metric_id` (string)
- `value` (numeric; `NA` when suppressed/missing)
- `unit` (optional denormalization)
- `suppressed` (bool)
- `numerator` (numeric, optional)
- `denominator` (numeric, optional)
- `source` (short string; optional)

### `division_metrics` (pre-aggregated long fact table)
- `division_id`
- `year`
- `metric_id`
- `value`
- `suppressed` (bool; TRUE if effectively not reportable)
- `n_included` (int)
- `n_suppressed` (int)
- `agg_method` (string: `total_rate`, `weighted_mean`, or `mean_subjects` in the current build)

### `metric_defs` (catalog)
- `metric_id`
- `category` (`test`/`behavior`/`demographics`)
- `label_short`, `label_long`
- `unit` (`%`, `per_100`, `index`, etc.)
- `better_direction` (`higher_better`, `lower_better`, `neutral`)
- `format` (e.g., `pct_1`, `num_1`, `int`)
- `palette` (e.g., `viridis`, `magma`)

## Versioning
- `snapshot_meta.json` includes:
  - `snapshot_date`
  - `year_min`, `year_max`
  - `transform_version` (bump when schema changes)
  - `sources` (list of source identifiers + notes)

## Performance Grain (Subject-Level)
For performance, the pipeline also emits subject-level tables at the grain:
- `school × school_year × subject × subgroup` in `school_subject_perf.csv`
- `division × school_year × subject × subgroup` in `division_subject_perf.csv`

These tables include:
- `acr_rate` (Accreditation Combined Rate; 0–100)
- `pass_rate` (historical School Quality Profiles Percent Passing; 0–100)

Annual SOL school metrics use `test_sol_annual_ela`, `test_sol_annual_math`, `test_sol_annual_science`, and `test_sol_annual_overall` in `school_metrics.csv`. The division table uses the same IDs. School subject rates store VDOE test numerators and denominators; the overall measure is an app-defined mean across available subjects and has no single test denominator. The source Excel files stay in ignored `data-raw/input/manual/`; `data-raw/annual_sol_school.csv.gz` is the tracked, checked extract used by the builder. `data-raw/annual_sol_sources.json` records the original workbook hashes.

The seven `demo_*_pct` metrics use VDOE Fall Membership race counts in 2025 and 2026. Their school `numerator` is a reported race count and `denominator` is the checked school total from the same source. Explicit blank counts are suppressed. The original CSVs stay in ignored `data-raw/input/manual/`; the builder reads the tracked `data-raw/fall_membership_race_school.csv.gz` and its source-hash manifest. Earlier 2023–24 demographic metrics use normalized School Quality Profiles subgroup percentages, with their own `source` value.

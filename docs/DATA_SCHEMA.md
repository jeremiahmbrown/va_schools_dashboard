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
- `app/data/school_subject_perf.csv` (subject-level performance: ACR + SOL pass rate)
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
- `year` (integer, e.g. 2020..2024)
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
- `agg_method` (string: `total_rate`, `enrollment_weighted_mean`, `tests_taken_weighted_mean`)

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
- `pass_rate` (SOL Pass Rate / Percent Passing; 0–100)

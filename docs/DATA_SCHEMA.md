# Data Schema (Frozen Snapshot, Update-Friendly)

## Snapshot Philosophy
- The deployed app reads only from `app/data/` (frozen snapshot).
- Update path: rerun build script to regenerate snapshot artifacts, then redeploy.
- Store enough metadata to reproduce the snapshot later (source notes, date, year range, transform version).

## Files (Recommended)
- `app/data/snapshot_meta.json`
- `app/data/schools.csv` (dimension table)
- `app/data/divisions_polygons.json` (division polygons as lon/lat outer `rings` and interior `holes`; converted to `sp` at runtime)
- `app/data/school_metrics.csv` (fact table, long)
- `app/data/enrollment_by_year.csv` (mapped school, year, and reported enrollment for the Profile)
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
- `enrollment_latest` (latest reported count; the Profile uses the year-specific table)
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

### `enrollment_by_year` (one row per mapped school and year)
- `school_id`, `year`, `enrollment`
- 2022 uses the checked Fall Membership school total. Later years use the reported SQP school total.
- The Profile shows the selected year's count. Division enrollment sums mapped schools with a count for that year.

### `division_metrics` (pre-aggregated long fact table)
- `division_id`
- `year`
- `metric_id`
- `value`
- `suppressed` (bool; TRUE if effectively not reportable)
- `n_included` (int)
- `n_suppressed` (int)
- `agg_method` (string: includes `total_rate`, `weighted_mean`, `mean_subjects`, and `reported_division_rate`)

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

Annual SOL school metrics use `test_sol_annual_ela`, `test_sol_annual_math`, `test_sol_annual_science`, and `test_sol_annual_overall` in `school_metrics.csv`. The division table uses the same IDs. For 2022, the school and division workbooks report subject rates without test counts; the checked extracts are `data-raw/annual_sol_2022_school.csv.gz` and `data-raw/annual_sol_2022_division.csv.gz`. The 2022 division subject rows use `agg_method = reported_division_rate`. For 2023–26, school subject rates store VDOE test numerators and denominators in `data-raw/annual_sol_school.csv.gz`, and division subjects are calculated from matched school counts. Every `Overall` value is an app-defined mean across available subjects and has no single test denominator. The source Excel files stay in ignored `data-raw/input/manual/`; source hashes are recorded in `data-raw/annual_sol_2022_sources.json` and `data-raw/annual_sol_sources.json`.

The seven `demo_*_pct` metrics use VDOE Fall Membership race counts in 2022–2026. Their school `numerator` is a reported race count and `denominator` is the checked school total from the same source. Explicit blank counts are suppressed. The original CSVs stay in ignored `data-raw/input/manual/`; the builder reads the tracked `data-raw/fall_membership_historical_race_school.csv.gz` for 2022–24 and `data-raw/fall_membership_race_school.csv.gz` for 2025–26, with source-hash manifests. The 2022 SBAR rates also use `data-raw/fall_membership_2022_enrollment.csv.gz` as the school enrollment denominator. The 2023 gender export omits `Other`, so the historical race loader requires its race total to match separate SQP enrollment for that year.

The `behavior_sbar_oss_actions_per_100` and `behavior_sbar_iss_actions_per_100` rows store sanction actions as `numerator` and checked same-year enrollment as `denominator`. Their tracked source is `data-raw/sbar_school_sanctions.csv.gz`; 2022 uses Fall Membership enrollment, while 2023–26 use SQP enrollment. These action measures are separate from the earlier student-count suspension measures.

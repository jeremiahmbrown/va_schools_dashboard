# Map Spec (Virginia Public Schools Shiny Dashboard)

## Default View
- Geography: Virginia.
- Layers enabled by default:
  - School points (shown only when zoomed in).
  - School division choropleth (simplified polygons behind points, with clear district borders).
- Defaults:
  - Year: most recent in snapshot.
  - Metric group: Performance Index.
  - Metric: Performance Index.

## Filters / Controls
- Year selector (last 5 years in snapshot).
- Layer toggles:
  - `Schools (points)`
  - `Divisions (choropleth)`
- Metric selectors:
  - Category: `Performance Index`, `Behavior`, `Demographics`
  - Metric within category
- Optional (recommended) filters:
  - School level: `Elementary`, `Middle`, `High`, `Other/Unknown`
  - Division (dropdown/search)
  - Search school name
  - Threshold filter:
    - Minimum Performance Index
    - Maximum suspensions per 100
    - Apply to schools, divisions, or both

## Metrics (Map-First)
### Performance Index (Primary)
- Performance Index (0-100): mean of accreditation combined rates across English, Math, and Science for All Students.
- Optional additional test metrics:
  - ELA pass rate (%)
  - Math pass rate (%)
  - ELA+Math composite pass rate (%), weighted by tests taken (if counts available).

### Behavior
- Suspension rate per 100 students:
  - `susp_per_100 = 100 * suspensions / enrollment`
  - Prefer totals-based division aggregation when numerators are available.
- Optional (later) additional behavior metrics:
  - Incident/referral rate per 100 students (definitions may vary by reporting system).

### Demographics
- Race/ethnicity share of enrollment (%):
  - `% Black`, `% Hispanic`, `% White`, `% Asian`, `% Two+` (as available).

## Interaction Model
- Hover: quick tooltip (school/division name + selected metric + year + suppression note).
- Zoom behavior:
  - Zoomed out (`<= 7`): show division polygons only (no rollup points).
  - Zoomed in (`> 7`): show individual school points with school-name + selected-metric hover labels.
- Click school point:
  - Popup: current-year metric value + a few key attributes (enrollment, division, school level).
  - Details panel: 5-year trend line for selected metric, plus a table of key metrics.
- Click division polygon:
  - Zooms/fits to division extent.
  - Does not hide nearby divisions/schools; map remains statewide.
  - Details panel switches to division summaries (5-year trend + distribution of school values).

## Aggregation Rules (Division Choropleth)
- Test index / pass rates:
  - Prefer weighting by `tests_taken` if available; else enrollment-weighted mean.
- Behavior rates:
  - Compute division-wide rate from totals: `100 * sum(incidents) / sum(enrollment)` when numerators are available.
  - If only per-school rates exist, use enrollment-weighted mean and clearly label method.
- Demographics:
  - Compute from totals: `% group = 100 * sum(group_enrollment) / sum(enrollment)`.

## Missing / Suppressed Data
- Store `suppressed` explicitly.
- Rendering:
  - `suppressed == TRUE` or missing => do not color by numeric scale; show neutral “No data” style.
  - Tooltip clarifies “Suppressed / not reported”.
- Aggregations exclude suppressed numeric values; tooltips include `n_included` and `n_suppressed` counts.

## Visual Encoding
- Sequential palettes for all metrics (higher = darker by default).
- Legend shows metric units only.
- Optional (future): threshold bands (e.g., state average) and quantile mode for visual comparison.

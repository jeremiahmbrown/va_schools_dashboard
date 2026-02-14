# Virginia Public Schools Dashboard (R Shiny)

## Goal
Build an interactive R Shiny dashboard that maps and compares **Virginia public schools** using:
- test performance (primary)
- behavioral indicators
- demographics (race/ethnicity emphasis)

Non-goals (unless explicitly requested): student-level data, any PII, predictive modeling, or school “rankings” that imply causal claims.

## Deployment / Data Refresh (Current Decision)
- Host on `shinyapps.io`.
- Use a **frozen snapshot** dataset for the deployed app, but keep `data-raw/` build scripts so updating to newer years is a repeatable rerun + redeploy.

## Data Principles
- Prefer authoritative, reproducible sources (e.g., Virginia DOE “School Quality Profiles” downloads; NCES for IDs/geo if needed).
- Store data in a **tidy long** form for metrics: one row per `school_id` + `year` + `metric_id`.
- Treat suppression/“too small to report” as first-class: keep a `suppressed` flag; render as `NA` and never impute by default.
- Use rates normalized by enrollment (e.g., incidents per 100 students) when comparing schools.
- Any aggregation to divisions must document weighting (default: enrollment-weighted mean).

## Map Product Spec (Default)
- Map supports **both**:
  - point layer for schools (lat/lon) with clustering at low zoom
  - division choropleth layer (polygons) for enrollment-weighted summaries
- Default color metric: **overall performance index** (selected metric + year).
- Click behavior:
  - click school point -> show popup + details panel (time series and metric table)
  - click division polygon -> filter/highlight schools in that division

## Project Structure (Default)
- `app/` Shiny app entry + modules (deploy this directory to `shinyapps.io`)
- `R/` reusable functions (data transforms, scales, validation)
- `data-raw/` scripts to fetch/clean/build datasets (idempotent, deterministic)
- `data/` small processed artifacts used by the app (`.rds`/`.parquet`)
- `docs/` methodology notes and metric definitions

## Conventions
- Use `renv` for dependency management.
- Use `arrow`/`parquet` for larger processed data; use `.rds` for small objects.
- Prefer `leaflet` for mapping; use `sp` polygons in the frozen snapshot to avoid system GIS dependencies, and optionally migrate to `sf` later.
- Keep UI copy plain-language and include “what this means” tooltips per metric.
- Keep the deployed snapshot under `app/data/` so `shinyapps.io` bundles it with the app.

## Runbook (Expected)
- Data build: `Rscript data-raw/build_data.R`
- App run: `R -e "shiny::runApp('app', port = 3838)"`
- Deploy: `R -e "rsconnect::deployApp('app')"`

## Testing (Lightweight)
- Add `testthat` tests for metric computations and suppression handling.
- Add a small fixture dataset (a few schools, 2 years) for fast tests.

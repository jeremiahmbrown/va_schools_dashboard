# Virginia Public Schools Dashboard (R Shiny)

Interactive Shiny dashboard to explore Virginia public schools via:
- Test performance (default: overall performance index)
- Behavioral indicators (default: suspensions per 100 students)
- Demographics (race/ethnicity %)

This repo is set up for **shinyapps.io** deployment with a **frozen snapshot** dataset bundled under `app/data/`.

## Quickstart
1. Install R packages (see `scripts/bootstrap.R`).
2. Run locally:
   - `R -e "shiny::runApp('va_schools_dashboard/app', port = 3838, launch.browser = FALSE)"`

## Data Model
See `va_schools_dashboard/docs/DATA_SCHEMA.md`.

## Map Behavior
See `va_schools_dashboard/docs/MAP_SPEC.md`.

## Updating Data Later
- Replace the snapshot files in `va_schools_dashboard/app/data/` using repeatable scripts in `va_schools_dashboard/data-raw/`.
- Redeploy to shinyapps.io.


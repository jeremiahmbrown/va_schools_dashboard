# Virginia Public Schools Dashboard (R Shiny)

Interactive Shiny dashboard to explore Virginia public schools via a map + drill-down details:
- Division boundaries (county + independent city school divisions) as polygons
- Individual schools as points (shown when zoomed in)
- A small set of key metrics (performance, suspensions, demographics)

This repo is set up for **shinyapps.io** deployment with a **frozen snapshot** dataset bundled under `app/data/`.

## Live App
https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/

## What’s On The Map
- **Divisions (polygons):** Virginia public school division boundaries (includes independent cities).
- **Schools (points):** Individual public schools with NCES geocoded locations (shown at higher zoom levels).
- **Hover/click:** Hover shows the currently selected metric; clicking selects an entity and updates the charts below the map.

## Metrics
This dashboard focuses on three categories:

### Performance Index (0–100)
The dashboard’s **Performance Index** is a simplified, repeatable summary of school-level accreditation performance:

1. For each `school-year`, pull **Accreditation Combined Rate** values reported by VDOE School Quality Profiles for:
   - English
   - Math
   - Science
   using the **All Students** subgroup.
2. Compute the **mean** of the available subject values for that school-year.
3. Cap at `100`.

Division-level Performance Index values are aggregated from school-level values using **enrollment-weighted** methods when denominators are available.

Important notes:
- This **Performance Index is not an official VDOE accreditation label**. It is a compact index to support map-based exploration.
- The underlying accreditation fields come from VDOE’s School Quality Profiles exports.

### Suspensions per 100
Suspensions are reported as **suspensions per 100 students**:

`100 * (short-term suspensions + long-term suspensions) / enrollment`

Counts come from VDOE School Quality Profiles “Learning Climate” exports, and enrollment comes from VDOE enrollment exports.

### Demographics (Race/Ethnicity)
Demographics are race/ethnicity percentage estimates derived from VDOE subgroup percentage exports and normalized to sum to 100 across the included categories.

## Where The Accreditation Data Comes From
Accreditation-related fields (including Accreditation Combined Rate) are sourced from **VDOE School Quality Profiles** download exports:
- https://schoolquality.virginia.gov/download-data

### What Is “Accreditation Combined Rate”?
VDOE’s School Quality Profiles exports include **Accreditation Combined Rate** values for core subjects. In general terms, the related **Combined Rate** is designed to give equal credit for:
- Students who pass SOL tests, and
- Students who do not pass but still demonstrate sufficient growth/progress.

This dashboard uses those reported values (for the **All Students** subgroup) as the building block for the Performance Index.

### How Virginia Accredits Schools (High-Level)
Virginia’s accreditation system (Standards of Accreditation) evaluates multiple indicators and assigns levels (commonly described as Level One/Two/Three). Schools are typically reported as **Accredited** or **Accredited with Conditions** based on performance across those indicators. For official definitions and the current rules, refer to VDOE.

For background on how Virginia accredits schools (Standards of Accreditation):
- https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/accreditation-federal-reports/soa-school-accreditation

For glossary definitions (including “Combined Rate”):
- https://schoolquality.virginia.gov/glossary

## Quickstart
1. Install R packages (see `scripts/bootstrap.R`).
2. Run locally:
   - `R -e "shiny::runApp('va_schools_dashboard/app', port = 3838, launch.browser = FALSE)"`

## Deploy (shinyapps.io)
Deploy from the repo root so `renv.lock` is included:
- `R -e "rsconnect::deployApp(appDir = 'va_schools_dashboard', appPrimaryDoc = 'app/app.R')"`

## Data Sources
- VDOE School Quality Profiles downloads: https://schoolquality.virginia.gov/download-data
- VDOE Standards of Accreditation overview: https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/accreditation-federal-reports/soa-school-accreditation
- NCES EDGE geocodes (school + LEA points): https://nces.ed.gov/programs/edge/
- U.S. Census TIGER/Line school district boundaries (used for division polygons): https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

## Data Model
See `va_schools_dashboard/docs/DATA_SCHEMA.md`.

## Map Behavior
See `va_schools_dashboard/docs/MAP_SPEC.md`.

## Updating Data Later
- Rebuild the frozen snapshot using scripts in `va_schools_dashboard/data-raw/` (this will download fresh source files and regenerate `app/data/*`).
- Commit updated `app/data/*` and redeploy to shinyapps.io.
- Redeploy to shinyapps.io.

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

### Performance (ACR + SOL Pass Rate)
This dashboard supports two **subject-level** performance measures for **All Students**, for:
- English (ELA)
- Math
- Science

#### Accreditation Combined Rate (ACR)
**Accreditation Combined Rate (ACR)** is sourced from VDOE School Quality Profiles accreditation exports. In general terms, the related **Combined Rate** is designed to give equal credit for:
- Students who pass SOL tests, and
- Students who do not pass but still demonstrate sufficient growth/progress (see VDOE glossary).

#### SOL Pass Rate (Proficiency-Only)
**SOL Pass Rate** in this app is sourced from the same VDOE accreditation exports as **Percent Passing** (proficiency-only).

#### “Overall” In The App
The app provides an **Overall** view for each performance metric (ACR or Pass Rate):
- `Overall = mean(subject values)` across English (ELA), Math, and Science for the selected school-year/division-year.
- If one subject is missing/suppressed, the mean is taken over available subjects and the app tracks how many subjects were present.

Important notes:
- These “Overall” composites are **app-defined** for convenience and are **not** official VDOE accreditation labels.
- The app does **not** create any derived indicator from `ACR − Pass`. Public data do not decompose ACR into separate “pass vs growth” counts at the same grain.

### Suspensions per 100
Suspensions are reported as **suspensions per 100 students**:

`100 * (short-term suspensions + long-term suspensions) / enrollment`

Counts come from VDOE School Quality Profiles “Learning Climate” exports, and enrollment comes from VDOE enrollment exports.

### Demographics (Race/Ethnicity)
Demographics are race/ethnicity percentage estimates derived from VDOE subgroup percentage exports and normalized to sum to 100 across the included categories.

## Computation & Aggregation
### Division Aggregation (Performance)
- Division **subject-level** ACR and Pass Rate values are computed as **enrollment-weighted averages across schools**.
- Division **Overall** values are computed as the **mean across the division’s subject-level values** for the selected metric.

### Limitations
- Public exports may suppress values for small groups; suppressed/missing values appear as “No data”.
- Public data do not provide a clean decomposition of ACR into separate “pass vs growth” counts at the same grain.

## Where The Accreditation Data Comes From
Accreditation-related fields (including **Accreditation Combined Rate** and **Percent Passing**) are sourced from **VDOE School Quality Profiles** download exports:
- https://schoolquality.virginia.gov/download-data

### What Is “Accreditation Combined Rate”?
VDOE’s School Quality Profiles exports include **Accreditation Combined Rate** values for core subjects. In general terms, the related **Combined Rate** is designed to give equal credit for:
- Students who pass SOL tests, and
- Students who do not pass but still demonstrate sufficient growth/progress.

This dashboard uses those reported values (for the **All Students** subgroup) as the building block for the app’s ACR and Overall composites.

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

## Data Sources
- VDOE School Quality Profiles downloads: https://schoolquality.virginia.gov/download-data
- VDOE Standards of Accreditation overview: https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/accreditation-federal-reports/soa-school-accreditation
- NCES EDGE geocodes (school + LEA points): https://nces.ed.gov/programs/edge/
- U.S. Census TIGER/Line school district boundaries (used for division polygons): https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

## Data Model
See `va_schools_dashboard/docs/DATA_SCHEMA.md`.

## Map Behavior
See `va_schools_dashboard/docs/MAP_SPEC.md`.

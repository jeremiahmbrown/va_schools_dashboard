# Virginia Public Schools Dashboard (R Shiny)

Interactive Shiny dashboard to explore Virginia public schools via a map + drill-down details:
- Division boundaries (county + independent city school divisions) as polygons
- Individual schools as points (shown when zoomed in)
- A small set of key metrics (performance, behavior/attendance, demographics)

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

### Behavior & Attendance
The detail page includes a “Behavior & Attendance” panel. Metrics are displayed for the **selected year** and include division/state **percentiles**.

#### Percentiles
Percentiles are empirical ranks among peers for the same year:
- **State percentile:** compared to all Virginia schools (or divisions) with reportable values.
- **Division percentile:** compared to schools within the same division (only shown for schools).

For metrics where **lower values are better** (e.g., suspensions), percentiles are **inverted** so:
- Higher percentile = better (lower adverse outcome).

#### Suspensions / Incidents / Expulsions (per 100)
Discipline outcomes are reported as **per 100 students** using VDOE counts and school enrollment totals:

`100 * (short-term suspensions + long-term suspensions) / enrollment`

When available, the app also shows:
- Short-term vs long-term suspensions per 100
- Suspendable incidents per 100 (short/long and total)
- Expulsions per 100 and expellable incidents per 100

Counts come from VDOE School Quality Profiles “School Environment / Learning Climate” exports, and enrollment comes from VDOE enrollment exports.

#### Chronic absenteeism (%)
Chronic absenteeism is shown as the **percent of students missing 10%+ of school days** (VDOE School Quality Profiles export).

### Demographics (Race/Ethnicity)
Race/ethnicity is displayed as a **horizontal 100% stacked bar**:
- No in-bar labels (to keep tiny segments readable)
- The legend lists each category with its percentage value
- If the legend becomes long, the app can collapse to “Top N + Other” (implementation is designed to support this)

Values are derived from VDOE subgroup percentages and normalized to sum to 100 across the included categories.

### Student Needs & Programs
The detail page includes a “Student needs & programs” panel (when metrics are available in the snapshot). Current snapshot metrics include:
- Meal eligibility (%)
- Breakfast participation among eligible students (%)
- Lunch participation among eligible students (%)

## Computation & Aggregation
### Division Aggregation (Performance)
- Division **subject-level** ACR and Pass Rate values are computed as **enrollment-weighted averages across schools**.
- Division **Overall** values are computed as the **mean across the division’s subject-level values** for the selected metric.

### Limitations
- Public exports may suppress values for small groups; suppressed/missing values appear as “No data”.
- Public data do not provide a clean decomposition of ACR into separate “pass vs growth” counts at the same grain.
- Program/needs percentages may be reported without denominators in public exports; aggregation may use enrollment-weighted fallbacks when necessary.

## Attendance Zones (Optional)
The map supports an optional “Attendance zones” overlay:
- **Division sources (when available):** a curated registry is seeded for the top 15 divisions by enrollment under `app/data/zones/zone_sources.csv`.
- **NCES SABS fallback:** can be generated from the NCES School Attendance Boundary Survey (SABS) dataset.

Boundaries may be incomplete or outdated; verify with the school division.

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
- NCES School Attendance Boundary Survey (SABS): https://nces.ed.gov/surveys/sabs/

## Data Model
See `va_schools_dashboard/docs/DATA_SCHEMA.md`.
See `va_schools_dashboard/docs/DATA_INVENTORY.md` for an automatically generated inventory of what metrics exist in the current snapshot.

## Map Behavior
See `va_schools_dashboard/docs/MAP_SPEC.md`.

# Snapshot refresh and verification record

Checked 2026-10-20 in Ubuntu on branch `improve-data-refresh-and-ui`.
The public shinyapps.io app still has the old snapshot until a release is made.

## Build result

- `Rscript data-raw/build_snapshot.R` completed from validated cached public
  sources. A separate run with `VA_DASH_REFRESH_VDOE=1` fetched live VDOE CSVs
  through a verified TLS bundle. The build now rejects HTTP errors, HTML,
  wrong columns, invalid years, and unusually short school-year files.
- Output: 1,810 schools, 120,678 school metric rows, 9,010 division metric rows,
  15,858 school subject rows, and 1,170 division subject rows. There are 120
  saved attendance-zone JSON files. The build preserved them in the bundle.
- The NCES 2024–25 name join matched 1,810 of 1,828 distinct VDOE school names
  (99.0%). The 18 unmatched school/division names are listed in
  `app/data/snapshot_meta.json` under `nces_match.unmatched`; they have no map
  point or school metric in this snapshot. Eleven schools were added to the map
  compared with the baseline; one old school, Richmond Virtual Academy, left.
- `snapshot_meta.json` contains SHA-256, size, and cache modified time for
  every current and archived input file. These dates describe the local file,
  not the official VDOE publication date.

## Valid values by measure

| Measure | 2023 | 2024 | 2025 | 2026 |
| --- | ---: | ---: | ---: | ---: |
| Chronic absence | 1,794 | 1,783 | 1,793 | 1,794 |
| Meal eligibility | 1,780 | 1,769 | 1,776 | 1,780 |
| Total suspensions per 100 | 470 | 515 | — | — |
| App mean of ACR | 1,777 | 1,781 | — | — |
| App mean of SOL pass rate | 1,773 | 1,776 | — | — |

Each number counts schools with a finite, unsuppressed value. The data year
2026 means 2025–26 for school-year exports. Historical accreditation exports
use their own year field. Fall enrollment through 2025–26 supports the school
lookup's latest enrollment field, but the app has no separate enrollment map
metric.

## Material differences from the old snapshot

- 2025 and 2026 attendance and meal rows now appear. The old builder dropped
  them because it took all years from accreditation exports, which stop at
  2024.
- The live VDOE service now drops 2022–23 enrollment, attendance, and meal
  rows. The builder retains those years from the original public raw files in
  `data-raw/historical/`; current downloads take precedence for overlapping
  years.
- The old vector suppression check treated some suppressed counts as usable.
  Total suspension values are now reportable for 470 schools in 2023 and 515
  in 2024, versus 1,794 and 1,800 before the fix. A total is missing if a
  needed component is suppressed. Suppressed numeric values are not published.
- Among 83,614 school metric cells with finite values in both snapshots,
  none changed numerically. Differences are new years, updated school coverage,
  and correct missing/suppressed handling.
- Science ACR is missing for every available historical year. The app's ACR
  mean therefore uses English and Math. Science SOL pass rate is available.
  The app and metric definition now say this directly.

## Checks run

- R parse checks for the builder, app, and percentile helper passed.
- `Rscript tests/check_snapshot.R` passed: school/division unique keys, linked
  IDs, suppression flags, valid percentage ranges, metric-specific years,
  preserved zones, percentile direction/ties/missing peers, and recomputed
  2026 division absence and meal rates from school numerators.
- `Rscript tests/check_source_validation.R` passed with valid, HTML, short,
  invalid-year, future-year, and changed-column source fixtures.
- `renv::status()` reported no issues after the lockfile update.
- Local Shiny started on WSL and loaded in the Windows in-app browser through
  `http://localhost:3838`. Desktop view showed OpenStreetMap tiles with
  attribution, division polygons, a 2026 attendance legend, and Campbell
  County's 2026 division details. School search found and selected Rocktown
  High. At 390 × 844, the phone-only "Jump to map" link moved the view from
  the controls to the map. The map, polygons, legend, and OpenStreetMap
  attribution were visible there. The source and zone notes were collapsed.

## Release limits

- Official VDOE 2025–26 SOL files are listed on the [SOL results page](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results),
  but direct XLS retrieval returned HTTP 403 in this environment. Those files
  have not been imported. Old ACR, a new SOL pass series, and official SPSF
  scores must remain separate.
- Discipline exports stop at 2023–24. The app must show missing values for
  2025–26 instead of implying zero incidents.
- Geocodes use NCES 2024–25 data; division polygons use Census 2024 data.
  Attendance-zone files can be incomplete or older. The 18 unmatched school
  names need review when newer geocodes are available.
- The public app has not been redeployed. The same URL and deployment account
  are documented in `docs/MAINTENANCE.md`; check the live URL only after a
  reviewed release.

# Lexington City division outline release — 2026-10-24

Git commit `b3ee0f6` was deployed to the existing Shiny app at
<https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/>. The app ID remains
`16676824`; the new bundle ID is `12735111`. The data snapshot date remains
`2026-10-22` because this release changes only the district map geometry and
its direct ID lookup.

The [Census 2024 elementary district file](https://www2.census.gov/geo/tiger/TIGER2024/ELSD/tl_2024_51_elsd.zip)
has one Virginia polygon: Lexington City Public Schools (`5102220`, grades
KG–12). It adds that outline to the 135 unified district polygons. The map
now preserves the 17 interior holes in the unified source. Rockbridge County's
source polygon meets Lexington at its boundary. The former snapshot omitted
the hole and filled Lexington with Rockbridge geometry.

Two Rockbridge County schools have NCES school points inside Lexington City.
The app now gives a polygon its direct division ID where that ID exists. A
school point's position does not change its school division or the polygon's
metric label.

## Checks

- The new JSON has 136 unique polygons and 17 interior holes. All 135
  existing outer rings and names are unchanged.
- `tests/check_division_polygons.R`, `tests/check_snapshot.R`, and
  `tests/check_zones.R` passed in Ubuntu. The focused check confirms that
  Lexington school points fall inside its outline and outside the Rockbridge
  polygon hole. `renv::status()` found no dependency issues.
- A local 1280 × 800 headless Edge check found 136 division paths. Lexington
  and Rockbridge search results opened separate profiles. Their map popups
  showed 91.9% and 71.4% for the 2026 annual SOL overall measure.
- The same browser check passed on the public app after deployment, with the
  same path count, profile labels, and values. This is automated browser
  evidence; it is not a separate owner device check.

## Rollback and remaining work

The preceding Git commit is `0fba3fe`, and the preceding confirmed bundle is
`12734339`. If needed, deploy that commit to the same app and verify the live
map. Attendance-zone coverage (`va_schools_dashboard-nlf`), the Roanoke City
source overlap (`va_schools_dashboard-9ht`), and unmatched school geocodes
(`va_schools_dashboard-4qu`) remain open. Division outlines do not imply a
school's attendance assignment.

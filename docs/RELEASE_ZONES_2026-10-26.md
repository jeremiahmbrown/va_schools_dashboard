# Interim statewide attendance-zone release — 2026-10-26

The checked attendance-zone batch was deployed to the existing public app at
<https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/> (app ID `16676824`).
The final live bundle is `12740245`, from Git commit `cd3b4054ab43f00c805c3c9d69436ad0b9a1f131`.
The first boundary bundle, `12738277` from commit `840146fb46540ff48dc920cc3da1e90206052f19`,
exposed a missing static-asset route and was replaced by the final bundle.

## Included data and limits

The app now has nine division GIS files and 111 dated NCES SABS files; ten
divisions have no zone file. Of 1,810 schools, 881 have a linked attendance
polygon and 929 do not. This release adds checked public GIS polygons for
Bedford, Botetourt, Montgomery, Salem, Arlington, Fairfax, and Prince William.
Three named older SABS polygons remain as fallbacks in Bedford and Salem.
Franklin, Craig, and Floyd retain dated SABS sources with checked school links.

Source years vary. Arlington GIS records have a 2022–23 year field. Fairfax
standard zones are 2026–27 and have measured high-school edge overlaps.
Prince William elementary zones are labeled 2025–26; its middle and high
sources are labeled current without a separate year. The 2026–27 Woodbridge
elementary polygon is held for a school-snapshot update. The app warns users
to confirm an address with the division. See [zone coverage](ZONES_COVERAGE.md)
for feature counts, geometry reports, exclusions, and source links.

## Checks

- Before release, the staged importer gate and Fairfax and Prince William
  deterministic dry runs passed. The statewide audit found zero invalid school
  IDs and zero empty geometries.
- `tests/check_zones.R`, `tests/check_snapshot.R`,
  `tests/check_source_validation.R`, `tests/check_metric_availability.R`, and
  `tests/check_division_polygons.R` passed in the Ubuntu checkout.
  `renv::status()` reported no issues and Git whitespace checks passed.
- The rsconnect file list contained 192 files, including the new app zone
  assets, and excluded `data-raw`, docs, tests, and editor settings.
- On the first live bundle, the app loaded and showed the Prince William
  source note, but a phone-sized browser showed a blank map. The live
  `dashboard.css` URL returned **Not Found**. A repository-root deployment did
  not serve `app/www` at the asset paths used by the page.
- The hotfix registers `app/www` as `dashboard-assets` and uses those paths in
  the page. In a local repository-root run, both CSS and JavaScript URLs
  returned HTTP 200. The snapshot and zone checks passed again.
- After the hotfix, the public page loaded at a 390 × 844 viewport. The map
  and Virginia division shapes were visible; the map stage measured 366 ×
  565 CSS pixels. Selecting Potomac View Elementary showed its profile and
  polygon after zooming in. The public snapshot still read `2026-10-22`.
  This is an automated browser check; the owner's phone view remains a
  separate observation.

## Recovery and remaining work

Bundle `12738277` immediately preceded the hotfix but has the broken asset
route. The last documented bundle before this boundary release is `12734339`
from 2026-10-24; it was not tested as a restoration today and might have the
same asset-route issue. If a data rollback is needed, prepare and test a new
bundle that retains the static-asset fix and removes this boundary batch.

The statewide research epic and its cohort beads remain open. Bead
`va_schools_dashboard-nlf.6.3` tracks the later Prince William Woodbridge
boundary. The next large-division validation is Loudoun County.

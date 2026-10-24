# Roanoke attendance-zone speed release — 2026-10-24

Git commit `16368bd` was deployed to the existing Shiny app at
<https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/>. The app ID remains
`16676824`; the new bundle ID is `12734339`. shinyapps.io reported the app as
running. The public page returned HTTP 200 and still showed the `2026-10-22`
data snapshot.

The app now reads checked, ready-to-render level files for Roanoke City and
County. It uses them only when the combined source file's normalized MD5
matches the cache manifest. A changed source uses the original filter path
until the cache is rebuilt. The source boundaries and school assignments did
not change. See [zone coverage](ZONES_COVERAGE.md) for the rebuild command.

## Checks

- `tests/check_snapshot.R` and `tests/check_zones.R` passed in Ubuntu.
  The latter compares every cached feature with its source and checks stale
  cache rejection and Windows line endings.
- `renv::status()` found no issues; `git diff --check` passed.
- On the local app, a 1280 × 800 headless Edge check measured browser
  change-to-overlay time of 0.04–0.06 seconds for both divisions. Before the
  cache, the same check measured 1.8–2.0 seconds for City and 3.2–3.4 seconds
  for County.
- On the public app, the same level switches took 0.11–0.17 seconds. City
  showed 17 elementary, 5 middle, and 2 high features. County showed 20 at
  each level. Each switch replaced the prior layer. These are automated
  measurements from one browser and connection, not a device-wide guarantee.

## Rollback and open geography work

The preceding Git commit is `199c0e6`, and the preceding confirmed bundle is
`12734298`. If needed, deploy that commit to the same app and verify the live
map and snapshot date. Current zone boundaries still have the source-date and
coverage limits in [zone coverage](ZONES_COVERAGE.md). The Roanoke City overlap,
missing statewide zones, Lexington City outline, and unmatched schools remain
separate Beads work.

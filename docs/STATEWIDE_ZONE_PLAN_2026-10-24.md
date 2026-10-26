# Statewide school attendance-zone work plan

Plan date: 2026-10-24. Durable work is tracked in Beads epic
`va_schools_dashboard-nlf`. The division-by-division research record is
`data-raw/zone_source_inventory.csv`. This document gives the shared method;
the Beads status and inventory rows show what remains to do.

## Baseline and scope

The frozen local snapshot has 130 divisions and 1,810 schools. Six divisions
have division GIS files. Of the **124 remaining divisions**, 114 use the old
NCES SABS 2015–16 survey and ten have no zone file. There are 652 linked schools
statewide. A linked survey polygon is historical evidence, not confirmation
of a present-day assignment. The 2026-10-20 effort estimate of 280–810 hours
was based on 128 divisions; it is a rough planning range and has not been
re-estimated from the new four-division import.

Progress on 2026-10-25: Arlington is the first statewide pilot. Its 29
neighborhood polygons are integrated locally, leaving 123 divisions in the
frozen work cohorts. Current active files are seven division GIS, 113 SABS,
and ten divisions with no file. The source inventory preserves the original
cohort assignments so a refresh does not silently move tasks. The Arlington
source carries a 2022-23 year field even though APS links its locator for
current assignments and publishes 2026-27 maps. Keep that conflict visible.

Fairfax County is the next large Northern Virginia validation, tracked in
`va_schools_dashboard-nlf.6.1`. Its 188 official 2026–27 standard attendance
polygons are integrated locally and link 185 of 195 snapshot schools. Current
active files are eight division GIS, 112 SABS, and ten divisions with no file;
813 schools have a matched zone. Of the original 124 remaining divisions, 122
still need a source disposition. Fairfax high-school edges have measured
overlaps, so the app warns users to confirm addresses with FCPS. See
`docs/ZONES_COVERAGE.md` and the tracked geometry report for details.

Prince William County is the next high-enrollment division, tracked in
`va_schools_dashboard-nlf.6.2`. Its 94 official locator polygons are integrated
locally and link 93 of 95 snapshot schools. The elementary source is labeled
2025–26; middle and high sources are labeled current without a separate year.
The 2026–27 Woodbridge elementary layer cannot be linked to the frozen school
list and is held in blocked follow-up `va_schools_dashboard-nlf.6.3`. Current
active files are nine division GIS, 111 SABS, and ten divisions with no file;
881 schools have a matched zone. The other 121 divisions from the original
124 still need a local source disposition. See `docs/ZONES_COVERAGE.md` for
the source-year and Quantico coverage limits.

The objective is to obtain and integrate adopted, public, machine-readable
attendance polygons for each remaining division **where they exist**. Each
division must finish with either a validated local integration or a dated,
evidence-backed reason that no usable public polygon was found. Do not draw a
zone from a school point, use a proposal as an adopted boundary, or present a
school district outline or bus eligibility area as a school attendance zone.

## Start or resume in another chat

1. Use `/home/jeremiah/code/va_schools_dashboard` in Ubuntu/WSL as the
   authoritative Git, R, Beads, and data-build checkout. The Windows
   `C:\Users\jerem\Documents\Codex\va-public-schools-dashboard\source`
   directory is a visible copy. Compare the two before moving edits. Leave
   the unrelated untracked `.vscode/` folder alone.
2. Run `bd prime`, `bd show va_schools_dashboard-nlf`, and `bd ready`. Read this
   file and `data-raw/zone_source_inventory.csv`. Then inspect `git status`;
   the preceding neighboring-division work is still local and uncommitted.
3. Run `python3 data-raw/build_zone_source_inventory.py` to refresh current
   file and match counts. It preserves research fields and frozen cohorts.
   A changed division list or snapshot size stops the script for review.
4. Work the next open Beads child. Update its notes and the relevant inventory
   rows after every division, including a source rejection or blocked case.
   Use `--append-notes` to preserve prior Beads evidence. Do not close a cohort
   bead until each assigned row has a disposition.
5. Run `python3 data-raw/audit_zones.py`,
   `Rscript --vanilla tests/check_zones.R`,
   `Rscript --vanilla tests/check_snapshot.R`, and `git diff --check` after
   each integration batch. Check the local app map at elementary, middle, and
   high levels and when a school is selected. A public deployment and live
   address check are separate gates.

## Beads work map

| Bead | Work | Inventory cohort |
| --- | --- | --- |
| `va_schools_dashboard-nlf.1` | Inventory and safe import contract | All 130 |
| `va_schools_dashboard-nlf.2` | Ten divisions with no zone file | `no_file` |
| `va_schools_dashboard-nlf.3` | Nine divisions with only unclassified survey zones | `unknown_only` |
| `va_schools_dashboard-nlf.4` | Arlington source and import pilot | Arlington in `high_enrollment` |
| `va_schools_dashboard-nlf.5` | Reusable staged importer and audit gates | Shared |
| `va_schools_dashboard-nlf.6` | Twenty high-enrollment survey divisions | `high_enrollment` |
| `va_schools_dashboard-nlf.6.1` | Fairfax large-division validation | Fairfax in `high_enrollment` |
| `va_schools_dashboard-nlf.6.2` | Prince William snapshot-aligned validation | Prince William in `high_enrollment` |
| `va_schools_dashboard-nlf.6.3` | PWCS 2026–27 Woodbridge reconciliation after a school snapshot update | Prince William follow-up; blocked |
| `va_schools_dashboard-nlf.7` | Thirty standard divisions | `standard_1` |
| `va_schools_dashboard-nlf.8` | Thirty standard divisions | `standard_2` |
| `va_schools_dashboard-nlf.9` | Twenty-five standard divisions | `standard_3` |
| `va_schools_dashboard-nlf.10` | Statewide audit and source-request drafts | All cohorts |
| `va_schools_dashboard-nlf.11` | Separate public release review | Release gate |

The cohorts are fixed from the 2026-10-24 snapshot. `no_file` and
`unknown_only` come first. The next 20 by total latest enrollment form
`high_enrollment`, and the other 85 are split into three cohorts in enrollment
order. Arlington has its own pilot bead and must not be imported twice.
Each inventory row names the relevant bead.

## Source decision for one division

1. Search the school division and local government GIS first. Record the
   official evidence URL, polygon service URL, owner, retrieval date, source
   edit date, and effective school year if stated. Check board decisions when
   a new boundary is proposed. A page that only locates an address is evidence
   of a mapping service, not proof that bulk polygon export is available.
2. Distinguish neighborhood attendance areas from option schools, special
   programs, transportation/walk zones, and district outlines. A school can
   correctly have no geographic neighborhood zone. Keep that exclusion
   explicit. Do not link a historical campus polygon to a consolidated school
   without direct evidence of the new area.
3. If the source is usable, download raw files into ignored
   `data-raw/input/zones/<division_id>/` and record SHA-256, URL, and date in
   the inventory or source manifest. Query all pages from ArcGIS services;
   a first page at the server record limit is not complete data.
4. Stage a GeoJSON FeatureCollection with one feature per school and level,
   using the current `app/data/schools.csv` IDs. Check source feature count,
   source school names, unmatched and duplicate IDs, levels, CRS/WGS84 bounds,
   empty and invalid geometry, same-level overlaps, and large coverage gaps.
   Repair geometry with documented GDAL/GEOS steps. Keep ambiguous polygons
   out of the active file until resolved. Make the effective year or unknown
   date visible in the app note and polygon popup.
5. Replace `app/data/zones/division/<division_id>.json` atomically only after
   all checks pass. Preserve the prior SABS file as a dated fallback. Build
   checked level caches, run the local tests, and inspect a few areas and
   selected schools on the map. Record counts, exclusions, validation output,
   and remaining questions in the inventory row and Beads child.
6. If no adopted downloadable polygon is found, set the row to
   `needs_official_file`, `no_geographic_zone`, or `blocked` with evidence and
   a next action. Draft an official data request if useful; sending it to a
   division requires user authorization.

Use `not_started`, `source_candidate`, `source_validated`,
`integrated_local`, `needs_official_file`, `no_geographic_zone`, or `blocked`
in `research_status`. `integrated_local` is not a public release claim.

## Import safety

`data-raw/build_zones_division_sources.R` is a legacy scaffold. It writes a
`division_id`/`features`/`rings` object, while the app loads GeoJSON features
with `geometry` and `properties`; it also lacks current school crosswalk and
publication checks. **Do not run it against the frozen snapshot.** Bead
`va_schools_dashboard-nlf.5` must replace or repair that path before it is
used. The existing Roanoke and neighboring Python builders are reference
examples for explicit counts, school IDs, geometry repair, and atomic writes.
The [verified import contract](ZONE_IMPORT_CONTRACT.md) records the staged
source, explicit crosswalk, and validation rules for this work.

The first pilot is Arlington. [APS says its locator reports neighborhood
elementary, middle, and high assignments for this school
year](https://www.apsva.us/registration/maps-boundaries/), and Arlington
County publishes [three matching boundary layers](https://arlgis.arlingtonva.us/arcgis/rest/services/Open_Data/od_School_Boundaries_Polygons/FeatureServer).
The same service also lists bus and walk eligibility polygons; those must
be excluded. The [live APS locator](https://arlgis.maps.arcgis.com/apps/instant/lookup/index.html?appid=fcbef7f2d50c4b2bae5558ae51cb5c14)
uses a separate public MapServer with 20 elementary, six middle, and three
high school areas. The checked download and build scripts are
`data-raw/stage_arlington_zone_source.py` and
`data-raw/build_arlington_zones.py`; hashes are in
`data-raw/zone_source_manifests/5100270.json`. The local file preserves the
year-field conflict in every popup. Local app level and selected-school checks
passed on 2026-10-25. Bead `.4` records the details; Campbell County is a
second source-type candidate from the earlier
statewide estimate.

## Session handoff record

At the end of each session, append to the active Beads task: the division IDs
worked, accepted or rejected source URLs, dates, download hashes, feature and
matched-school counts, tests, outstanding source questions, and the exact next
division. Refresh the inventory and report any uncommitted changes. Keep a
public release decision separate from local validation.

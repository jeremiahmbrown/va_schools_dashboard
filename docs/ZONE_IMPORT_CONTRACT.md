# Verified attendance-zone import contract

Work item: `va_schools_dashboard-nlf.5`. This contract extends the [statewide
plan](STATEWIDE_ZONE_PLAN_2026-10-24.md). The shared ArcGIS and direct GeoJSON
importer has one validated pilot, Arlington. Do not run
`data-raw/build_zones_division_sources.R`: it writes rings rather than the
GeoJSON structure the app reads.

## Inputs for one division

- A public source URL and official evidence URL, with the effective school
  year or a clear statement that it is unknown. Record source layer IDs and
  names. Do not use bus, walk, proposed, or school-district outline layers.
- One checked source manifest in `data-raw/zone_source_manifests/<id>.json`.
  It names every raw file, URL, feature count, and SHA-256. Raw files go under
  ignored `data-raw/input/zones/<id>/`. An ArcGIS download must request all
  object IDs, fetch every page, and verify that returned IDs equal the full
  server ID set. A first page is not a complete download.
- One explicit crosswalk in `data-raw/zone_school_crosswalks/<id>.csv`, with
  the columns `division_id`, `level`, `source_name`, `school_id`,
  `school_name`, `disposition`, and `note`. `disposition` is `mapped` or
  `no_neighborhood_zone`. A mapped row links one source polygon to one current
  `app/data/schools.csv` ID. An excluded row has no `source_name` and states
  why that school has no neighborhood polygon. Do not rely on a fuzzy name
  match during publication.

The [Arlington crosswalk](../data-raw/zone_school_crosswalks/5100270.csv) is
the first example: 29 mapped rows and six schools without a neighborhood
polygon. Yorktown High has an explicit note because the school snapshot calls
it `Combined` while APS publishes a high-school boundary.

## Checks before publication

1. Check the source date, adopted status, scope, reuse rights, every raw hash,
   all source object IDs, expected feature counts, and every source name.
2. Require a complete crosswalk: every source polygon is mapped once, every
   mapped school ID belongs to the division, and every current school has a
   mapped or excluded disposition. Reject duplicate zone IDs, school IDs, or
   unreviewed levels.
3. Require WGS84 geometry. ArcGIS queries request `outSR=4326`; reproject
   other source formats before staging. Reject empty or out-of-state geometry. Repair
   invalid rings with GDAL/GEOS and verify the feature and school IDs again.
   Report same-level overlap area and the gap between level unions. Stop on
   overlaps above a division-specific reviewed limit; never fill a gap by
   drawing a polygon.
4. Build a GeoJSON `FeatureCollection` with `geometry` and `properties` that
   include `zone_id`, `division_id`, `school_name`, `level`, `app_school_id`,
   `source`, `source_url`, `popup`, and `style`. The popup must show a known
   source-year conflict or unknown effective date.
5. Write to a temporary file. Replace the active division file only after all
   checks pass. Preserve any prior SABS file. Refresh the level cache only
   after the combined file is valid. If the cache fails, its hash must not
   match the new combined file, so the app uses the combined file directly.
6. Run `data-raw/audit_zones.py`, `tests/check_zones.R`,
   `tests/check_snapshot.R`, and `git diff --check`. Review the local map by
   level and selected school. Update the statewide inventory and Beads with
   source links, dates, counts, exclusions, checks, and the next action.

## Current implementation state

`data-raw/stage_zone_source.py <division_id>` reads a tracked spec from
`data-raw/zone_import_specs/`, requests all ArcGIS object IDs and checked
pages, or reads a direct GeoJSON source. Raw files use a content-hash folder;
the manifest changes only after every layer passes. The direct GeoJSON path
has not yet been tested against a real division source.

`data-raw/import_zone_source.py <division_id>` checks the manifest, raw
hashes, explicit school crosswalk, record counts, dates, and ID links. It
repairs geometry with GDAL, writes an ignored candidate and geometry report,
and leaves the active file untouched. `--publish` replaces the active file
only after validation, builds the checked level cache, and saves the tracked
report. [Arlington's report](../data-raw/zone_validation_reports/5100270.json)
sets a reviewed 5 m² same-level overlap limit; its largest measured sliver is
3.00 m². The 8.09 km² elementary gap is reported, not filled.

The shared path reproduced the reviewed Arlington GeoJSON exactly (SHA-256
`054e5dea550d73225910d7d4b7f6e62308a7a2c25c8c0b1272038a47124bb95b`).
`python3 tests/check_zone_importer.py` checks multi-page ArcGIS retrieval,
rejects a missing page ID, and stages a direct GeoJSON fixture. It also checks
that a bad source hash, bad crosswalk, or interrupted second-layer download
preserves the active file and prior manifest. The local app level and school selection check was done
with this same GeoJSON. A second source type and division remain to be
validated before treating the importer as fully general.

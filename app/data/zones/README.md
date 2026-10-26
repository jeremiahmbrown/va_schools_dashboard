# Attendance Zones (Optional Layer)

This folder is reserved for **optional** attendance-boundary polygons used by the Shiny app.

The app will work without any files here.

## Layout
- `zone_sources.csv`: registry for division-published sources (seeded for the top 15 divisions by enrollment).
- `division/`: division-published zone polygons, one JSON per `division_id`.
- `sabs/`: NCES SABS fallback polygons, one JSON per `division_id`.

## Build Scripts
- `data-raw/build_zone_sources.R`
- `data-raw/build_zones_division_sources.R`
- `data-raw/build_zones_sabs.R`
- `data-raw/build_roanoke_county_zones.py` (Roanoke County public GIS)
- `data-raw/build_roanoke_city_zones.py` (Roanoke City public locator layers; requires GDAL `ogr2ogr`)
- `data-raw/build_neighbor_zones.py` (run in WSL with GDAL `ogr2ogr`; public GIS
  layers for Bedford, Botetourt, Montgomery, and Salem; preserves named SABS
  fallback zones where needed)
- `data-raw/stage_arlington_zone_source.py` (stage the APS locator's public GIS
  polygons and tracked source manifest; raw files are ignored by Git)
- `data-raw/build_arlington_zones.py` (build 29 checked Arlington neighborhood
  zones from those staged files; preserves the source-year conflict in popups)
- `data-raw/stage_zone_source.py` and `data-raw/import_zone_source.py` (shared
  ArcGIS or GeoJSON staging, crosswalk, geometry checks, and atomic local import)
- `data-raw/build_fairfax_zone_crosswalk.py` (reviewed names and exclusions for
  188 FCPS 2026–27 standard attendance polygons)
- `data-raw/build_prince_william_zone_crosswalk.py` (reviewed names and two
  exclusions for 94 PWCS mixed-date standard attendance polygons)
- `data-raw/build_zone_level_cache.py` (checked per-level files for division sources)
- `data-raw/link_neighbor_sabs.py` (run after a SABS rebuild; link only checked
  historical school IDs for Franklin, Craig, and Floyd)

## Notes
- Use the level selector to view elementary, middle, or high zones. These can overlap.
- Boundaries may be incomplete or outdated. Verify with the school division.
- Division files are GeoJSON read by the app without system GIS dependencies.
  The City build script uses GDAL to repair one source polygon before saving.


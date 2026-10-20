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

## Notes
- Use the level selector to view elementary, middle, or high zones. These can overlap.
- Boundaries may be incomplete or outdated. Verify with the school division.
- Division files are GeoJSON read by the app without system GIS dependencies.
  The City build script uses GDAL to repair one source polygon before saving.


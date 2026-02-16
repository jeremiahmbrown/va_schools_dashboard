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

## Notes
- Boundaries may be incomplete or outdated. Verify with the school division.
- Files are stored as lightweight JSON polygon rings so the deployed app does not
  require system GIS dependencies.


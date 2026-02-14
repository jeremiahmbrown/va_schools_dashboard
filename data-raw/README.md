# data-raw

This folder is for **repeatable data build scripts** that generate the frozen snapshot
bundled with the Shiny app under `app/data/`.

## Current State
- `build_snapshot.R` is the intended entrypoint but is not implemented yet.
- The app currently ships with a small **synthetic example snapshot** for wiring/testing.

## Intended Inputs (Future)
Place authoritative raw downloads under `data-raw/input/`, for example:
- VDOE School Quality Profile exports (school-level performance + discipline + demographics)
- A stable school identifier crosswalk (state IDs + NCES IDs)
- Division boundaries (polygons) suitable for choropleth mapping

Then implement parsers in `build_snapshot.R` to produce the snapshot schema documented in `docs/DATA_SCHEMA.md`.


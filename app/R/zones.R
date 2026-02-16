# ==============================================================================
# Script: zones.R
# Purpose:
#   Load and render attendance-zone polygons (optional snapshot assets).
#
#   The app supports two sources:
#   - Division-published boundaries (when available)
#   - NCES SABS fallback boundaries
#
#   Files are stored under `app/data/zones/` as lightweight JSON polygon rings
#   (similar to `divisions_polygons.json`) to keep runtime dependencies minimal.
#
# Inputs:
#   - `app/data/zones/division/<division_id>.json` (optional)
#   - `app/data/zones/sabs/<division_id>.json` (optional)
#
# Outputs:
#   - GeoJSON FeatureCollection string for leaflet::addGeoJSON()
#
# Key Details / Debugging:
#   - These are optional assets; the app should work even if zones are absent.
# ==============================================================================

suppressPackageStartupMessages({
})

read_zone_geojson <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

pick_zone_file <- function(data_dir, division_id, mode = c("division", "sabs")) {
  mode <- match.arg(mode)
  div_path <- file.path(data_dir, "zones", "division", paste0(division_id, ".json"))
  sabs_path <- file.path(data_dir, "zones", "sabs", paste0(division_id, ".json"))

  if (mode == "division") {
    if (file.exists(div_path)) return(div_path)
    if (file.exists(sabs_path)) return(sabs_path)
    return(NULL)
  }

  if (file.exists(sabs_path)) return(sabs_path)
  NULL
}

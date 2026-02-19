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
  library(jsonlite)
})

read_zone_geojson <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

filter_zone_geojson_by_school_id <- function(geojson_text, school_id) {
  # Keep only features for the selected app school_id. Returns GeoJSON text.
  # If no features match, returns NULL.
  if (is.null(geojson_text) || !nzchar(geojson_text)) return(NULL)
  if (is.null(school_id) || !nzchar(school_id)) return(NULL)

  obj <- jsonlite::fromJSON(geojson_text, simplifyVector = FALSE)
  feats <- obj$features
  if (is.null(feats) || length(feats) == 0) return(NULL)

  keep <- vapply(
    feats,
    function(f) {
      props <- f$properties
      if (is.null(props)) return(FALSE)
      sid <- props$app_school_id
      if (is.null(sid)) return(FALSE)
      if (is.list(sid)) {
        if (length(sid) != 1) return(FALSE)
        sid <- sid[[1]]
      }
      sid <- as.character(sid)
      if (length(sid) != 1 || !nzchar(sid)) return(FALSE)
      sid == school_id
    },
    logical(1)
  )

  feats2 <- feats[keep]
  if (length(feats2) == 0) return(NULL)

  obj$features <- feats2
  jsonlite::toJSON(obj, auto_unbox = TRUE)
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

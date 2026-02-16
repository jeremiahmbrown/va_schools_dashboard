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
#   - `sp::SpatialPolygonsDataFrame` for leaflet::addPolygons()
#
# Key Details / Debugging:
#   - These are optional assets; the app should work even if zones are absent.
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(sp)
})

zones_json_to_sp <- function(json_path) {
  obj <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  features <- obj$features
  if (is.null(features) || length(features) == 0) {
    return(NULL)
  }

  polys <- vector("list", length(features))
  data_rows <- vector("list", length(features))

  for (i in seq_along(features)) {
    f <- features[[i]]
    rings <- f$rings
    if (is.null(rings) || length(rings) == 0) next

    ring_objs <- lapply(rings, function(ring) {
      coords <- do.call(rbind, lapply(ring, function(pt) c(pt[[1]], pt[[2]])))
      if (nrow(coords) < 3) return(NULL)
      if (coords[1, 1] != coords[nrow(coords), 1] || coords[1, 2] != coords[nrow(coords), 2]) {
        coords <- rbind(coords, coords[1, , drop = FALSE])
      }
      sp::Polygon(coords)
    })
    ring_objs <- Filter(Negate(is.null), ring_objs)
    if (length(ring_objs) == 0) next

    zone_id <- as.character(f$zone_id %||% paste0("zone_", i))
    poly <- sp::Polygons(ring_objs, ID = zone_id)
    polys[[i]] <- poly
    data_rows[[i]] <- data.frame(
      zone_id = zone_id,
      school_name = as.character(f$school_name %||% NA_character_),
      level = as.character(f$level %||% NA_character_),
      source = as.character(f$source %||% NA_character_),
      stringsAsFactors = FALSE
    )
  }

  keep <- !vapply(polys, is.null, logical(1))
  polys <- polys[keep]
  data_rows <- data_rows[keep]

  if (length(polys) == 0) return(NULL)

  sp_polys <- sp::SpatialPolygons(polys)
  sp::proj4string(sp_polys) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")

  df <- do.call(rbind, data_rows)
  rownames(df) <- df$zone_id

  sp::SpatialPolygonsDataFrame(sp_polys, data = df, match.ID = TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

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


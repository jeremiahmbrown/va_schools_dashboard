# ==============================================================================
# Script: build_zones_division_sources.R
# Purpose:
#   Build attendance-zone polygons from division-published sources listed in
#   `app/data/zones/zone_sources.csv` (curated for top divisions by enrollment).
#
#   This script is intentionally conservative: it only attempts downloads for
#   rows with `source_type != 'none'`.
#
# Inputs:
#   - `app/data/zones/zone_sources.csv` (filled in manually)
#
# Outputs:
#   - `app/data/zones/division/<division_id>.json`
#
# Key Details / Debugging:
#   - Source types supported (intended):
#     - arcgis_feature_layer: ArcGIS REST feature layer URL
#     - geojson: direct GeoJSON URL
#     - shapefile: direct zip URL to a shapefile
#   - Network access is required. If your environment blocks downloads,
#     populate files manually and rerun the app.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(jsonlite)
})

repo_root <- normalizePath(file.path(getwd(), "."), mustWork = TRUE)
registry_path <- file.path(repo_root, "app", "data", "zones", "zone_sources.csv")
out_dir <- file.path(repo_root, "app", "data", "zones", "division")

if (!file.exists(registry_path)) stop("Missing registry: ", registry_path)
reg <- read.csv(registry_path, stringsAsFactors = FALSE)

if (!all(c("division_id", "source_type", "source_url") %in% names(reg))) {
  stop("zone_sources.csv must contain at least: division_id, source_type, source_url")
}

reg <- reg %>%
  mutate(
    division_id = as.character(division_id),
    source_type = tolower(trimws(as.character(source_type))),
    source_url = trimws(as.character(source_url))
  ) %>%
  filter(!is.na(division_id) & division_id != "", !is.na(source_type) & source_type != "none", nzchar(source_url))

if (nrow(reg) == 0) {
  message("No division sources configured (source_type == 'none' for all rows). Nothing to do.")
  quit(status = 0)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sf_to_rings <- function(geom) {
  g <- sf::st_geometry(geom)
  g <- sf::st_cast(g, "MULTIPOLYGON", warn = FALSE)
  coords <- sf::st_coordinates(g)
  if (nrow(coords) == 0) return(list())

  rings <- split(as.data.frame(coords), interaction(coords[, "L1"], coords[, "L2"], drop = TRUE))
  out <- lapply(rings, function(r) {
    pts <- as.matrix(r[, c("X", "Y"), drop = FALSE])
    lapply(seq_len(nrow(pts)), function(i) list(pts[i, 1], pts[i, 2]))
  })
  out
}

download_to_temp <- function(url) {
  dest <- tempfile(fileext = ".dat")
  utils::download.file(url, destfile = dest, quiet = TRUE, mode = "wb")
  dest
}

read_source <- function(source_type, url) {
  if (source_type == "geojson") {
    path <- download_to_temp(url)
    return(sf::st_read(path, quiet = TRUE))
  }

  if (source_type == "shapefile") {
    zip_path <- download_to_temp(url)
    unzip_dir <- tempfile("shp_")
    dir.create(unzip_dir, recursive = TRUE)
    utils::unzip(zip_path, exdir = unzip_dir)
    shp <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(shp) == 0) stop("No .shp found in downloaded zip from: ", url)
    return(sf::st_read(shp[[1]], quiet = TRUE))
  }

  if (source_type == "arcgis_feature_layer") {
    # Minimal ArcGIS support: many ArcGIS layers can be read directly by sf via the URL.
    # If this fails, export GeoJSON/shapefile manually and point `source_url` at it.
    return(sf::st_read(url, quiet = TRUE))
  }

  stop("Unsupported source_type: ", source_type)
}

for (i in seq_len(nrow(reg))) {
  row <- reg[i, ]
  div_id <- row$division_id[[1]]
  stype <- row$source_type[[1]]
  url <- row$source_url[[1]]

  message("Processing division ", div_id, " (", stype, ") ...")
  sf_obj <- tryCatch(read_source(stype, url), error = function(e) e)
  if (inherits(sf_obj, "error")) {
    warning("Failed to read source for division ", div_id, ": ", conditionMessage(sf_obj))
    next
  }

  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj <- sf::st_transform(sf_obj, 4326)
  sf_obj <- sf::st_simplify(sf_obj, dTolerance = 0.0007, preserveTopology = TRUE)

  feats <- vector("list", nrow(sf_obj))
  for (j in seq_len(nrow(sf_obj))) {
    feats[[j]] <- list(
      zone_id = paste0(div_id, "_", j),
      school_name = NA_character_,
      level = NA_character_,
      source = "division",
      rings = sf_to_rings(sf_obj[j, ])
    )
  }

  jsonlite::write_json(list(division_id = div_id, features = feats), file.path(out_dir, paste0(div_id, ".json")), auto_unbox = TRUE)
}

message("Done.")


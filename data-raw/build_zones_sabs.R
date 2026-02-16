# ==============================================================================
# Script: build_zones_sabs.R
# Purpose:
#   Build attendance-zone polygons for Virginia from the NCES School Attendance
#   Boundary Survey (SABS) as a fallback layer when division-published boundaries
#   are unavailable.
#
#   This script is designed to be re-runnable and to emit lightweight JSON
#   polygon rings under `app/data/zones/sabs/` for fast Shiny runtime loading.
#
# Inputs:
#   - Manual download of NCES SABS boundary data placed under:
#       `data-raw/input/sabs/`
#     (Expected: one or more shapefiles / geodatabases containing school
#      attendance boundaries with NCES identifiers.)
#
# Outputs:
#   - `app/data/zones/sabs/<division_id>.json` (one file per LEAID/division_id)
#
# Key Details / Debugging:
#   - SABS schemas vary by release. This script is defensive:
#     - it searches for a plausible LEAID field (e.g., `leaid`, `LEAID`)
#     - it searches for a plausible school name field
#   - If no LEAID-like field exists, the script will stop (since we cannot split
#     per division).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(jsonlite)
})

repo_root <- normalizePath(file.path(getwd(), "."), mustWork = TRUE)
input_dir <- file.path(repo_root, "data-raw", "input", "sabs")
out_dir <- file.path(repo_root, "app", "data", "zones", "sabs")

if (!dir.exists(input_dir)) {
  stop(
    "Missing SABS input directory: ", input_dir, "\n",
    "Download the NCES SABS boundary data and place it under this folder."
  )
}

find_shapefiles <- function(dir_path) {
  list.files(dir_path, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
}

shps <- find_shapefiles(input_dir)
if (length(shps) == 0) {
  stop("No .shp files found under ", input_dir, ".")
}

message("Reading SABS geometries (this can take a moment)...")
layers <- lapply(shps, function(path) {
  suppressWarnings(sf::st_read(path, quiet = TRUE))
})
zones <- dplyr::bind_rows(layers)

if (!inherits(zones, "sf") || nrow(zones) == 0) stop("No SABS features read.")

names_lower <- tolower(names(zones))
pick_col <- function(candidates) {
  idx <- match(tolower(candidates), names_lower)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NULL)
  names(zones)[idx[[1]]]
}

leaid_col <- pick_col(c("leaid", "lea_id", "leaid10", "leaid20"))
if (is.null(leaid_col)) {
  stop("Could not find an LEAID-like field in SABS data. Columns: ", paste(names(zones), collapse = ", "))
}

school_name_col <- pick_col(c("school_name", "sch_name", "name", "school", "att_sch_nm"))
level_col <- pick_col(c("level", "grade_lvl", "sch_level"))

zones <- zones %>%
  mutate(
    division_id = as.character(.data[[leaid_col]]),
    school_name = if (!is.null(school_name_col)) as.character(.data[[school_name_col]]) else NA_character_,
    level = if (!is.null(level_col)) as.character(.data[[level_col]]) else NA_character_
  ) %>%
  filter(!is.na(division_id) & division_id != "")

zones <- sf::st_make_valid(zones)
zones <- sf::st_transform(zones, 4326)

# Simplify for interactive display (tune as needed).
zones <- sf::st_simplify(zones, dTolerance = 0.0007, preserveTopology = TRUE)

sf_to_rings <- function(geom) {
  # Convert an sf POLYGON/MULTIPOLYGON into a list of outer rings.
  g <- sf::st_geometry(geom)
  g <- sf::st_cast(g, "MULTIPOLYGON", warn = FALSE)
  coords <- sf::st_coordinates(g)
  if (nrow(coords) == 0) return(list())

  # st_coordinates returns rows with L1/L2/L3 indices for MULTIPOLYGON/POLYGON/ring.
  rings <- split(as.data.frame(coords), interaction(coords[, "L1"], coords[, "L2"], drop = TRUE))
  out <- lapply(rings, function(r) {
    pts <- as.matrix(r[, c("X", "Y"), drop = FALSE])
    lapply(seq_len(nrow(pts)), function(i) list(pts[i, 1], pts[i, 2]))
  })
  out
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Writing per-division JSON to: ", out_dir)
div_ids <- sort(unique(zones$division_id))
for (div_id in div_ids) {
  sub <- zones %>% filter(division_id == div_id)
  if (nrow(sub) == 0) next

  feats <- vector("list", nrow(sub))
  for (i in seq_len(nrow(sub))) {
    feats[[i]] <- list(
      zone_id = paste0(div_id, "_", i),
      school_name = sub$school_name[[i]],
      level = sub$level[[i]],
      source = "sabs",
      rings = sf_to_rings(sub[i, ])
    )
  }

  jsonlite::write_json(list(division_id = div_id, features = feats), file.path(out_dir, paste0(div_id, ".json")), auto_unbox = TRUE)
}

message("Done.")


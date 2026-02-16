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

`%||%` <- function(x, y) if (is.null(x)) y else x

zones_json_to_geojson <- function(json_path) {
  obj <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  features <- obj$features
  if (is.null(features) || length(features) == 0) {
    return(NULL)
  }

  # Legacy schema: `rings` (list of rings; each ring is list of [lon, lat]).
  # New schema (preferred): `multipolygon` in GeoJSON coordinate form.
  normalize_geom <- function(f) {
    if (!is.null(f$multipolygon)) {
      return(list(type = "MultiPolygon", coordinates = f$multipolygon))
    }

    rings <- f$rings
    if (is.null(rings) || length(rings) == 0) return(NULL)

    # Treat each ring as its own polygon part (one outer ring, no holes).
    coords <- lapply(rings, function(ring) {
      pts <- lapply(ring, function(pt) c(pt[[1]], pt[[2]]))
      list(pts)
    })
    list(type = "MultiPolygon", coordinates = coords)
  }

  to_grade <- function(x) {
    x <- toupper(trimws(as.character(x %||% "")))
    if (!nzchar(x)) return(NA_character_)
    if (x %in% c("PK", "PREK")) return("Pre-K")
    if (x %in% c("KG", "K")) return("K")
    # Keep 01..12 as integers.
    if (grepl("^[0-9]+$", x)) return(as.character(as.integer(x)))
    x
  }

  to_level <- function(x, lo = NA_character_, hi = NA_character_) {
    # Parent-facing, coarse bucket.
    x <- toupper(trimws(as.character(x %||% "")))
    if (x %in% c("PRIMARY", "ELEM", "ELEMENTARY")) return("Elementary")
    if (x %in% c("MIDDLE", "MS")) return("Middle")
    if (x %in% c("HIGH", "HS")) return("High")
    if (x %in% c("OTHER")) return("Other")

    # Infer from grade span if present.
    lo <- to_grade(lo)
    hi <- to_grade(hi)
    if (!is.na(lo) && !is.na(hi)) {
      if (lo %in% c("Pre-K", "K", "1", "2", "3", "4", "5") && hi %in% c("Pre-K", "K", "1", "2", "3", "4", "5")) return("Elementary")
      if (lo %in% c("6", "7", "8") && hi %in% c("6", "7", "8")) return("Middle")
      if (lo %in% c("9", "10", "11", "12") && hi %in% c("9", "10", "11", "12")) return("High")
    }

    NA_character_
  }

  mk_popup <- function(school_name, level_label, grade_span, source) {
    school_name <- as.character(school_name %||% "")
    source <- as.character(source %||% "sabs")

    bits <- c("<strong>Attendance zone (approximate)</strong>")
    if (nzchar(school_name)) bits <- c(bits, paste0("School: ", htmltools::htmlEscape(school_name)))
    if (!is.na(level_label) && nzchar(level_label)) bits <- c(bits, paste0("School level: ", htmltools::htmlEscape(level_label)))
    if (!is.na(grade_span) && nzchar(grade_span)) bits <- c(bits, paste0("Grades served: ", htmltools::htmlEscape(grade_span)))
    bits <- c(bits, paste0("Source: ", if (source == "sabs") "NCES SABS (2015\u201316)" else htmltools::htmlEscape(source)))
    bits <- c(bits, "<span class='muted'>Confirm boundaries on the school division website.</span>")
    paste(bits, collapse = "<br/>")
  }

  feats_out <- vector("list", 0)
  for (i in seq_along(features)) {
    f <- features[[i]]
    geom <- normalize_geom(f)
    if (is.null(geom)) next

    zone_id <- as.character(f$zone_id %||% paste0("zone_", i))
    school_name <- as.character(f$school_name %||% "")
    source <- as.character(f$source %||% "sabs")

    g_lo <- f$grades_lo %||% f$gslo
    g_hi <- f$grades_hi %||% f$gshi
    lo <- to_grade(g_lo)
    hi <- to_grade(g_hi)
    grade_span <- NA_character_
    if (!is.na(lo) && !is.na(hi)) grade_span <- if (lo == hi) lo else paste0(lo, "\u2013", hi)

    level_label <- to_level(f$level, lo = lo, hi = hi)

    level_cols <- c(
      Elementary = "#1F77B4",
      Middle = "#F58518",
      High = "#54A24B",
      Other = "#777777",
      Unknown = "#777777"
    )
    stroke <- level_cols[[if (!is.na(level_label) && nzchar(level_label)) level_label else "Unknown"]]

    style <- list(
      pane = "zonePolygonPane",
      weight = 1.0,
      opacity = 0.9,
      fillOpacity = 0.12,
      color = stroke,
      fillColor = stroke
    )

    feats_out[[length(feats_out) + 1]] <- list(
      type = "Feature",
      id = zone_id,
      properties = list(
        zone_id = zone_id,
        school_name = school_name,
        popup = mk_popup(school_name, level_label, grade_span, source),
        style = style
      ),
      geometry = geom
    )
  }

  if (length(feats_out) == 0) return(NULL)

  fc <- list(type = "FeatureCollection", features = feats_out)
  jsonlite::toJSON(fc, auto_unbox = TRUE)
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

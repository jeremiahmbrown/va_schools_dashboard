# ==============================================================================
# Script: snapshot.R
# Purpose:
#   Load the frozen snapshot dataset that is bundled with the deployed Shiny app.
#   This file centralizes snapshot I/O so future dataset updates only require
#   replacing `app/data/*` artifacts (and not editing app logic).
#
# Inputs:
#   - `app/data/snapshot_meta.json`
#   - `app/data/schools.csv`
#   - `app/data/metric_defs.csv`
#   - `app/data/school_metrics.csv`
#   - `app/data/division_metrics.csv`
#   - `app/data/divisions_polygons.json`
#
# Outputs:
#   - A list containing snapshot metadata, dimension tables, and an `sp`
#     SpatialPolygonsDataFrame for division polygons.
#
# Key Details / Debugging:
#   - Suppressed values are expected to be `value = NA` with `suppressed = TRUE`.
#   - Division polygons are stored as lon/lat rings in JSON and converted to `sp`.
#   - This implementation uses `sp` (not `sf`) to avoid system GIS deps on Ubuntu.
# ==============================================================================

load_snapshot <- function(data_dir = "data") {
  snapshot_meta_path <- file.path(data_dir, "snapshot_meta.json")
  schools_path <- file.path(data_dir, "schools.csv")
  metric_defs_path <- file.path(data_dir, "metric_defs.csv")
  school_metrics_path <- file.path(data_dir, "school_metrics.csv")
  division_metrics_path <- file.path(data_dir, "division_metrics.csv")
  school_subject_perf_path <- file.path(data_dir, "school_subject_perf.csv")
  division_subject_perf_path <- file.path(data_dir, "division_subject_perf.csv")
  divisions_polygons_path <- file.path(data_dir, "divisions_polygons.json")

  meta <- jsonlite::fromJSON(snapshot_meta_path)

  schools <- read.csv(
    schools_path,
    stringsAsFactors = FALSE,
    colClasses = c(
      school_id = "character",
      school_name = "character",
      division_id = "character",
      division_name = "character",
      lat = "numeric",
      lon = "numeric",
      school_level = "character",
      enrollment_latest = "integer"
    )
  )

  metric_defs <- read.csv(
    metric_defs_path,
    stringsAsFactors = FALSE,
    colClasses = c(
      metric_id = "character",
      category = "character",
      label_short = "character",
      label_long = "character",
      unit = "character",
      better_direction = "character",
      format = "character",
      palette = "character"
    )
  )

  school_metrics <- read.csv(
    school_metrics_path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    colClasses = c(
      school_id = "character",
      year = "integer",
      metric_id = "character",
      value = "numeric",
      unit = "character",
      suppressed = "logical",
      numerator = "numeric",
      denominator = "numeric",
      source = "character"
    )
  )

  division_metrics <- read.csv(
    division_metrics_path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    colClasses = c(
      division_id = "character",
      year = "integer",
      metric_id = "character",
      value = "numeric",
      suppressed = "logical",
      n_included = "integer",
      n_suppressed = "integer",
      agg_method = "character"
    )
  )

  divisions_sp <- divisions_polygons_json_to_sp(divisions_polygons_path)

  school_subject_perf <- NULL
  if (file.exists(school_subject_perf_path)) {
    school_subject_perf <- read.csv(
      school_subject_perf_path,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA"),
      colClasses = c(
        school_id = "character",
        year = "integer",
        subject = "character",
        subgroup = "character",
        acr_rate = "numeric",
        pass_rate = "numeric",
        acr_suppressed = "logical",
        pass_suppressed = "logical",
        denominator = "numeric",
        source = "character"
      )
    )
  }

  division_subject_perf <- NULL
  if (file.exists(division_subject_perf_path)) {
    division_subject_perf <- read.csv(
      division_subject_perf_path,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA"),
      colClasses = c(
        division_id = "character",
        year = "integer",
        subject = "character",
        subgroup = "character",
        acr_rate = "numeric",
        pass_rate = "numeric",
        acr_suppressed = "logical",
        pass_suppressed = "logical",
        weight_basis = "character",
        source = "character"
      )
    )
  }

  list(
    meta = meta,
    schools = schools,
    metric_defs = metric_defs,
    school_metrics = school_metrics,
    division_metrics = division_metrics,
    school_subject_perf = school_subject_perf,
    division_subject_perf = division_subject_perf,
    divisions_sp = divisions_sp
  )
}

divisions_polygons_json_to_sp <- function(json_path) {
  divisions <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

  polys <- vector("list", length(divisions))
  data_rows <- vector("list", length(divisions))

  for (i in seq_along(divisions)) {
    division <- divisions[[i]]
    division_id <- division$division_id
    division_name <- division$division_name

    # Support multipart divisions by loading all outer rings as one `Polygons`.
    ring_objs <- lapply(division$rings, function(ring) {
      coords <- do.call(rbind, lapply(ring, function(pt) c(pt[[1]], pt[[2]])))
      if (nrow(coords) < 3) return(NULL)
      # Ensure closed ring for sp::Polygon.
      if (nrow(coords) >= 3 && (coords[1, 1] != coords[nrow(coords), 1] || coords[1, 2] != coords[nrow(coords), 2])) {
        coords <- rbind(coords, coords[1, , drop = FALSE])
      }
      sp::Polygon(coords)
    })
    ring_objs <- Filter(Negate(is.null), ring_objs)
    if (length(ring_objs) == 0) next

    poly <- sp::Polygons(ring_objs, ID = division_id)
    polys[[i]] <- poly
    data_rows[[i]] <- data.frame(
      division_id = division_id,
      division_name = division_name,
      stringsAsFactors = FALSE
    )
  }

  keep <- !vapply(polys, is.null, logical(1))
  polys <- polys[keep]
  data_rows <- data_rows[keep]

  sp_polys <- sp::SpatialPolygons(polys)
  sp::proj4string(sp_polys) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")

  df <- do.call(rbind, data_rows)
  rownames(df) <- df$division_id

  spdf <- sp::SpatialPolygonsDataFrame(
    sp_polys,
    data = df,
    match.ID = TRUE
  )

  spdf
}

# ==============================================================================
# Script: build_zones_sabs.R
# Purpose:
#   Build attendance-zone polygons for Virginia from the NCES School Attendance
#   Boundary Survey (SABS) as a fallback layer when division-published boundaries
#   are unavailable.
#
#   This script is designed to be re-runnable and to emit lightweight GeoJSON
#   FeatureCollections under `app/data/zones/sabs/` for fast Shiny runtime
#   loading (no runtime re-serialization).
#
# Inputs:
#   - Manual download of NCES SABS boundary data placed under:
#       `data-raw/input/sabs/`
#     (Expected: one or more shapefiles / geodatabases containing school
#      attendance boundaries with NCES identifiers.)
#   - Recommended invocation (avoids project renv auto-loader):
#       `Rscript --vanilla data-raw/build_zones_sabs.R`
#
# Outputs:
#   - `app/data/zones/sabs/<division_id>.json` (GeoJSON FeatureCollection; one
#     file per LEAID/division_id)
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

# SABS geometries include some invalid self-intersections. sf's s2 backend can
# throw hard errors when ingesting invalid loops; we prefer GEOS here.
sf::sf_use_s2(FALSE)

repo_root <- normalizePath(file.path(getwd(), "."), mustWork = TRUE)
input_dir <- file.path(repo_root, "data-raw", "input", "sabs")
out_dir <- file.path(repo_root, "app", "data", "zones", "sabs")

schools_path <- file.path(repo_root, "app", "data", "schools.csv")
schools_app <- NULL
if (file.exists(schools_path)) {
  schools_app <- tryCatch(
    read.csv(schools_path, stringsAsFactors = FALSE) %>%
      transmute(
        school_id = as.character(school_id),
        school_name = as.character(school_name),
        division_id = as.character(division_id),
        school_level = as.character(school_level)
      ),
    error = function(e) NULL
  )
}

norm_school_name <- function(x) {
  x <- as.character(x)
  if (is.na(x) || !nzchar(x)) return("")
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

strip_level_tokens <- function(x) {
  x <- as.character(x)
  if (is.na(x) || !nzchar(x)) return("")
  x <- gsub("\\b(elementary|middle|high|school|academy|center|centre|primary|intermediate)\\b", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

match_app_school_id <- function(zone_school_name, level_label, division_schools) {
  # Attempt to match SABS zone features to this app's school_id so the Shiny app
  # can filter to a single zone when a school is selected.
  if (is.null(division_schools) || nrow(division_schools) == 0) return(NA_character_)

  z_norm <- norm_school_name(zone_school_name)
  if (!nzchar(z_norm)) return(NA_character_)
  z_nolevel <- strip_level_tokens(z_norm)

  cands <- division_schools
  if (!is.na(level_label) && level_label %in% c("Elementary", "Middle", "High")) {
    ok <- cands$school_level %in% c(level_label, "Combined")
    if (any(ok, na.rm = TRUE)) cands <- cands[ok, , drop = FALSE]
  }

  idx <- which(cands$name_norm == z_norm)
  if (length(idx) == 1) return(cands$school_id[[idx]])

  idx <- which(cands$name_norm_nolevel == z_nolevel)
  if (length(idx) == 1) return(cands$school_id[[idx]])

  # Conservative fuzzy match (within-division only). Prefer correctness over coverage.
  if (!nzchar(z_nolevel)) return(NA_character_)
  d <- suppressWarnings(utils::adist(z_nolevel, cands$name_norm_nolevel))
  if (length(d) == 0) return(NA_character_)
  d <- as.integer(d[1, ])
  if (all(!is.finite(d))) return(NA_character_)
  ord <- order(d, na.last = NA)
  if (length(ord) == 0) return(NA_character_)
  best <- ord[[1]]
  min_d <- d[[best]]
  second_d <- if (length(ord) >= 2) d[[ord[[2]]]] else Inf

  len <- nchar(z_nolevel)
  thr <- max(2L, floor(0.08 * len))
  if (!is.finite(min_d) || min_d > thr) return(NA_character_)
  if (is.finite(second_d) && second_d == min_d) return(NA_character_) # ambiguous

  cand_nm <- cands$name_norm_nolevel[[best]]
  if (nzchar(cand_nm)) {
    if (substr(z_nolevel, 1, 3) != substr(cand_nm, 1, 3) && substr(z_nolevel, 1, 2) != substr(cand_nm, 1, 2)) {
      return(NA_character_)
    }
  }

  cands$school_id[[best]]
}

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
  # Allow a simpler workflow: user drops the SABS zip(s) into the folder without
  # manually unzipping. We extract in-place, then re-scan for shapefiles.
  zips <- list.files(input_dir, pattern = "\\.zip$", full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
  if (length(zips) > 0) {
    message("No shapefiles found; extracting zip(s) under: ", input_dir)
    for (z in zips) {
      utils::unzip(z, exdir = input_dir, overwrite = TRUE)
    }
    shps <- find_shapefiles(input_dir)
  }
}
if (length(shps) == 0) {
  stop("No .shp files found under ", input_dir, ".")
}

# Prefer a pre-filtered Virginia-only shapefile if present, or create one using GDAL
# to avoid reading the full nationwide boundary file into R.
pick_va_filtered <- function(paths) {
  va <- paths[grepl("_VA\\.shp$|SABS_1516_VA\\.shp$", paths, ignore.case = TRUE)]
  if (length(va) > 0) return(va[[1]])
  NULL
}

va_shp <- pick_va_filtered(shps)
if (is.null(va_shp)) {
  full <- shps[grepl("SABS_1516\\.shp$", shps, ignore.case = TRUE)]
  if (length(full) > 0) {
    full <- full[[1]]
    out_dir_ogr <- file.path(input_dir, "SABS_1516_VA")
    out_shp <- file.path(out_dir_ogr, "SABS_1516_VA.shp")
    if (!file.exists(out_shp)) {
      dir.create(out_dir_ogr, recursive = TRUE, showWarnings = FALSE)
      message("Creating VA-only subset via ogr2ogr (stAbbrev = 'VA')...")
      # This is dramatically faster than reading the full national dataset into R.
      cmd <- sprintf(
        "ogr2ogr -overwrite -where %s %s %s",
        shQuote("stAbbrev = 'VA'"),
        shQuote(out_shp),
        shQuote(full)
      )
      status <- suppressWarnings(system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE))
      if (!file.exists(out_shp) || (!is.null(status) && is.numeric(status) && status != 0)) {
        stop(
          "ogr2ogr VA subset failed; expected output missing: ", out_shp, "\n",
          "Try running manually:\n",
          "  ogr2ogr -overwrite -where \"stAbbrev = 'VA'\" ", shQuote(out_shp), " ", shQuote(full)
        )
      }
    }
    shps <- c(out_shp)
  }
} else {
  shps <- c(va_shp)
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
gslo_col <- pick_col(c("gslo", "grade_lo", "grade_low"))
gshi_col <- pick_col(c("gshi", "grade_hi", "grade_high"))
ncessch_col <- pick_col(c("ncessch", "nces_sch", "school_id", "ncesid"))

# SABS 2015-16 uses `schnam` / `SrcName` for name; pick them if our generic
# candidates missed.
if (is.null(school_name_col)) {
  school_name_col <- pick_col(c("schnam", "srcname", "SrcName"))
}

zones <- zones %>%
  mutate(
    division_id = as.character(.data[[leaid_col]]),
    school_name = if (!is.null(school_name_col)) as.character(.data[[school_name_col]]) else NA_character_,
    level = if (!is.null(level_col)) as.character(.data[[level_col]]) else NA_character_,
    grades_lo = if (!is.null(gslo_col)) as.character(.data[[gslo_col]]) else NA_character_,
    grades_hi = if (!is.null(gshi_col)) as.character(.data[[gshi_col]]) else NA_character_,
    nces_school_id = if (!is.null(ncessch_col)) as.character(.data[[ncessch_col]]) else NA_character_
  ) %>%
  filter(!is.na(division_id) & division_id != "")

# Filter to Virginia divisions to keep runtime small and avoid unrelated
# invalid geometries outside the app's scope.
va_divisions <- tryCatch({
  div_path <- file.path(repo_root, "app", "data", "division_metrics.csv")
  if (file.exists(div_path)) {
    read.csv(div_path, stringsAsFactors = FALSE) %>% distinct(division_id) %>% pull(division_id) %>% as.character()
  } else {
    NULL
  }
}, error = function(e) NULL)

if (!is.null(va_divisions) && length(va_divisions) > 0) {
  zones <- zones %>% filter(division_id %in% va_divisions)
  message("Filtered to VA divisions: ", length(unique(zones$division_id)))
}

# Repair invalid loops defensively (GEOS path; s2 disabled above).
zones <- tryCatch(sf::st_make_valid(zones), error = function(e) {
  message("st_make_valid failed: ", conditionMessage(e), " ; attempting st_buffer(0) fallback")
  sf::st_buffer(zones, 0)
})
zones <- sf::st_make_valid(zones)

# Simplify in a projected CRS (meters) for interactive display, then transform to WGS84.
zones <- sf::st_transform(zones, 3857)
# Tolerance in meters. Higher values improve Leaflet performance at the cost of
# boundary fidelity (acceptable for a parent-facing "approximate" overlay).
zones <- sf::st_simplify(zones, dTolerance = 150, preserveTopology = TRUE)
zones <- sf::st_make_valid(zones)
zones <- sf::st_transform(zones, 4326)

close_ring <- function(mat) {
  if (nrow(mat) < 3) return(mat)
  if (mat[1, 1] != mat[nrow(mat), 1] || mat[1, 2] != mat[nrow(mat), 2]) {
    mat <- rbind(mat, mat[1, , drop = FALSE])
  }
  mat
}

sf_geom_to_multipolygon_coords <- function(geom_row) {
  g <- sf::st_geometry(geom_row)
  if (length(g) == 0) return(list())
  g <- sf::st_make_valid(g)
  g <- sf::st_cast(g, "MULTIPOLYGON", warn = FALSE)
  mp <- g[[1]]
  if (is.null(mp) || length(mp) == 0) return(list())

  # mp is list(poly -> list(ring -> matrix)).
  lapply(mp, function(poly) {
    lapply(poly, function(ring) {
      ring <- close_ring(ring)
      lapply(seq_len(nrow(ring)), function(i) list(ring[i, 1], ring[i, 2]))
    })
  })
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Writing per-division JSON to: ", out_dir)
div_ids <- sort(unique(zones$division_id))

to_grade <- function(x) {
  x <- toupper(trimws(as.character(x)))
  if (!nzchar(x) || is.na(x)) return(NA_character_)
  if (x %in% c("PK", "PREK")) return("Pre-K")
  if (x %in% c("KG", "K")) return("K")
  if (grepl("^[0-9]+$", x)) return(as.character(as.integer(x)))
  x
}

to_level_label <- function(level_raw, lo, hi) {
  x <- toupper(trimws(as.character(level_raw)))
  if (nzchar(x)) {
    if (x %in% c("PRIMARY", "ELEM", "ELEMENTARY")) return("Elementary")
    if (x %in% c("MIDDLE", "MS")) return("Middle")
    if (x %in% c("HIGH", "HS")) return("High")
    if (x %in% c("OTHER")) return("Other")
  }

  lo <- to_grade(lo)
  hi <- to_grade(hi)
  if (!is.na(lo) && !is.na(hi)) {
    if (lo %in% c("Pre-K", "K", "1", "2", "3", "4", "5") && hi %in% c("Pre-K", "K", "1", "2", "3", "4", "5")) return("Elementary")
    if (lo %in% c("6", "7", "8") && hi %in% c("6", "7", "8")) return("Middle")
    if (lo %in% c("9", "10", "11", "12") && hi %in% c("9", "10", "11", "12")) return("High")
  }

  "Unknown"
}

mk_grade_span <- function(lo, hi) {
  lo <- to_grade(lo)
  hi <- to_grade(hi)
  if (is.na(lo) || is.na(hi)) return(NA_character_)
  if (identical(lo, hi)) lo else paste0(lo, "\u2013", hi)
}

mk_popup <- function(school_name, level_label, grade_span) {
  school_name <- as.character(school_name)
  if (is.na(school_name)) school_name <- ""

  bits <- c("<strong>Attendance zone (approximate)</strong>")
  if (nzchar(school_name)) bits <- c(bits, paste0("School: ", school_name))
  if (!is.na(level_label) && nzchar(level_label) && level_label != "Unknown") bits <- c(bits, paste0("School level: ", level_label))
  if (!is.na(grade_span) && nzchar(grade_span)) bits <- c(bits, paste0("Grades served: ", grade_span))
  bits <- c(bits, "Source: NCES SABS (2015\u201316)")
  bits <- c(bits, "<span class='muted'>Confirm boundaries on the school division website.</span>")
  paste(bits, collapse = "<br/>")
}

level_cols <- c(
  Elementary = "#1F77B4",
  Middle = "#F58518",
  High = "#54A24B",
  Other = "#777777",
  Unknown = "#777777"
)

for (div_id in div_ids) {
  sub <- zones %>% filter(division_id == div_id)
  if (nrow(sub) == 0) next

  division_schools <- NULL
  if (!is.null(schools_app)) {
    division_schools <- schools_app %>%
      filter(division_id == div_id) %>%
      mutate(
        name_norm = vapply(school_name, norm_school_name, character(1)),
        name_norm_nolevel = vapply(vapply(school_name, norm_school_name, character(1)), strip_level_tokens, character(1))
      )
  }

  feats <- vector("list", nrow(sub))
  for (i in seq_len(nrow(sub))) {
    zone_id <- paste0(div_id, "_", i)
    grade_span <- mk_grade_span(sub$grades_lo[[i]], sub$grades_hi[[i]])
    level_label <- to_level_label(sub$level[[i]], lo = sub$grades_lo[[i]], hi = sub$grades_hi[[i]])
    stroke <- unname(level_cols[[level_label]])
    app_school_id <- match_app_school_id(sub$school_name[[i]], level_label, division_schools)
    if (is.na(app_school_id) || !nzchar(app_school_id)) app_school_id <- NULL

    props <- list(
      zone_id = zone_id,
      division_id = div_id,
      school_name = sub$school_name[[i]],
      level = level_label,
      grades_lo = sub$grades_lo[[i]],
      grades_hi = sub$grades_hi[[i]],
      grade_span = grade_span,
      nces_school_id = sub$nces_school_id[[i]],
      source = "sabs",
      popup = mk_popup(sub$school_name[[i]], level_label, grade_span),
      style = list(
        weight = 1.0,
        opacity = 0.9,
        fillOpacity = 0.12,
        color = stroke,
        fillColor = stroke
      )
    )
    if (!is.null(app_school_id)) props$app_school_id <- app_school_id

    feats[[i]] <- list(
      type = "Feature",
      id = zone_id,
      properties = props,
      geometry = list(
        type = "MultiPolygon",
        coordinates = sf_geom_to_multipolygon_coords(sub[i, ])
      )
    )
  }

  jsonlite::write_json(
    list(type = "FeatureCollection", features = feats),
    file.path(out_dir, paste0(div_id, ".json")),
    auto_unbox = TRUE
  )
}

message("Done.")

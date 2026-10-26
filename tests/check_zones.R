# Run from repository root: Rscript --vanilla tests/check_zones.R
source("app/R/zones.R")

path <- pick_zone_file("app/data", "5103330")
stopifnot(identical(path, file.path("app/data", "zones", "division", "5103330.json")))
data <- read_zone_geojson(path)
all_features <- jsonlite::fromJSON(data, simplifyVector = FALSE)$features
stopifnot(length(all_features) == 60)

for (level in c("Elementary", "Middle", "High")) {
  filtered <- filter_zone_geojson_by_level(data, level)
  features <- jsonlite::fromJSON(filtered, simplifyVector = FALSE)$features
  stopifnot(length(features) == 20)
  stopifnot(all(vapply(features, function(f) identical(f$properties$level, level), logical(1))))
}
stopifnot(is.null(filter_zone_geojson_by_level(data, "Invalid")))
old_sabs <- read_zone_geojson(pick_zone_file("app/data", "5100330"))
unknown <- filter_zone_geojson_by_level(old_sabs, "Unknown")
stopifnot(!is.null(unknown), length(jsonlite::fromJSON(unknown, simplifyVector = FALSE)$features) > 0)
stopifnot(is.null(filter_zone_geojson_by_school_id(data, "not-a-school")))

schools <- read.csv("app/data/schools.csv", colClasses = c(school_id = "character", division_id = "character"))
roanoke <- subset(schools, division_id == "5103330")
stopifnot(nrow(roanoke) == 26)
for (school_id in roanoke$school_id) {
  filtered <- filter_zone_geojson_by_school_id(data, school_id)
  stopifnot(!is.null(filtered))
  features <- jsonlite::fromJSON(filtered, simplifyVector = FALSE)$features
  stopifnot(length(features) >= 1)
  stopifnot(all(vapply(features, function(f) identical(f$properties$app_school_id, school_id), logical(1))))
}

city_path <- pick_zone_file("app/data", "5103300")
stopifnot(identical(city_path, file.path("app/data", "zones", "division", "5103300.json")))
city <- read_zone_geojson(city_path)
city_features <- jsonlite::fromJSON(city, simplifyVector = FALSE)$features
stopifnot(length(city_features) == 24L)
city_levels <- vapply(city_features, function(f) f$properties$level, character(1))
stopifnot(identical(as.integer(table(factor(city_levels, levels = c("Elementary", "Middle", "High")))), c(17L, 5L, 2L)))
city_schools <- subset(schools, division_id == "5103300")
stopifnot(nrow(city_schools) == 24L)
city_ids <- vapply(city_features, function(f) f$properties$app_school_id, character(1))
stopifnot(setequal(city_ids, city_schools$school_id), !anyDuplicated(city_ids))
for (level in c("Elementary", "Middle", "High")) {
  filtered <- filter_zone_geojson_by_level(city, level)
  features <- jsonlite::fromJSON(filtered, simplifyVector = FALSE)$features
  stopifnot(length(features) == c(Elementary = 17L, Middle = 5L, High = 2L)[[level]])
}
for (sid in city_schools$school_id) {
  filtered <- filter_zone_geojson_by_school_id(city, sid)
  stopifnot(!is.null(filtered), length(jsonlite::fromJSON(filtered, simplifyVector = FALSE)$features) == 1L)
}
city_sf <- sf::st_read(city_path, quiet = TRUE)
stopifnot(all(sf::st_is_valid(city_sf)))
bb <- sf::st_bbox(city_sf)
stopifnot(bb[["xmin"]] > -81, bb[["xmax"]] < -79, bb[["ymin"]] > 36.5, bb[["ymax"]] < 38)

# Neighboring divisions use their public GIS files where available. Only
# explicitly documented old SABS polygons remain in the combined files.
neighbor_expected <- list(
  `5100360` = c(Elementary = 13L, Middle = 3L, High = 3L),
  `5100420` = c(Elementary = 7L, Middle = 2L, High = 2L),
  `5102520` = c(Elementary = 11L, Middle = 10L, High = 10L),
  `5103460` = c(Elementary = 4L, Middle = 1L, High = 1L)
)
for (division_id in names(neighbor_expected)) {
  zone_path <- pick_zone_file("app/data", division_id)
  stopifnot(identical(zone_path, file.path("app/data", "zones", "division", paste0(division_id, ".json"))))
  features <- jsonlite::fromJSON(read_zone_geojson(zone_path), simplifyVector = FALSE)$features
  levels <- vapply(features, function(f) f$properties$level, character(1))
  counts <- as.integer(table(factor(levels, levels = c("Elementary", "Middle", "High"))))
  stopifnot(identical(counts, unname(as.integer(neighbor_expected[[division_id]]))))
  stopifnot(all(vapply(features, function(f) identical(f$properties$division_id, division_id), logical(1))))
  school_ids <- vapply(features, function(f) f$properties$app_school_id, character(1))
  expected_ids <- schools$school_id[schools$division_id == division_id]
  if (division_id == "5103460") expected_ids <- setdiff(expected_ids, subset(schools, division_id == "5103460" & school_level == "Combined")$school_id)
  stopifnot(setequal(school_ids, expected_ids))
  geometry <- sf::st_read(zone_path, quiet = TRUE)
  # GDAL makevalid uses GEOS; Leaflet draws the planar GeoJSON rings.
  old_s2 <- sf::sf_use_s2(FALSE)
  valid <- sf::st_is_valid(geometry)
  sf::sf_use_s2(old_s2)
  stopifnot(all(valid))
}
salem <- jsonlite::fromJSON(read_zone_geojson(pick_zone_file("app/data", "5103460")), simplifyVector = FALSE)$features
stopifnot(sum(vapply(salem, function(f) grepl("SABS", f$properties$popup, fixed = TRUE), logical(1))) == 2L)
bedford <- jsonlite::fromJSON(read_zone_geojson(pick_zone_file("app/data", "5100360")), simplifyVector = FALSE)$features
stopifnot(sum(vapply(bedford, function(f) grepl("SABS", f$properties$popup, fixed = TRUE), logical(1))) == 1L)

# APS links its locator for current assignments, but the GIS records still
# carry School_Year 2022-2023. Keep that caveat on every Arlington polygon.
arlington_path <- pick_zone_file("app/data", "5100270")
stopifnot(identical(arlington_path, file.path("app/data", "zones", "division", "5100270.json")))
arlington <- jsonlite::fromJSON(read_zone_geojson(arlington_path), simplifyVector = FALSE)$features
stopifnot(length(arlington) == 29L)
arlington_levels <- vapply(arlington, function(f) f$properties$level, character(1))
stopifnot(identical(as.integer(table(factor(arlington_levels, levels = c("Elementary", "Middle", "High")))), c(20L, 6L, 3L)))
arlington_ids <- vapply(arlington, function(f) f$properties$app_school_id, character(1))
stopifnot(!anyDuplicated(arlington_ids), length(unique(arlington_ids)) == 29L)
stopifnot(all(arlington_ids %in% subset(schools, division_id == "5100270")$school_id))
stopifnot("510027000115" %in% arlington_ids) # Yorktown High is Combined in the school snapshot.
stopifnot(all(vapply(arlington, function(f) grepl("2022-2023", f$properties$popup, fixed = TRUE), logical(1))))
stopifnot(length(jsonlite::fromJSON(filter_zone_geojson_by_school_id(read_zone_geojson(arlington_path), "510027000115"), simplifyVector = FALSE)$features) == 1L)
stopifnot(length(jsonlite::fromJSON(filter_zone_geojson_by_school_id(read_zone_geojson(arlington_path), "510027000081"), simplifyVector = FALSE)$features) == 1L)
stopifnot(is.null(filter_zone_geojson_by_school_id(read_zone_geojson(arlington_path), "510027000104")))
arlington_sf <- sf::st_read(arlington_path, quiet = TRUE)
stopifnot(all(sf::st_is_valid(arlington_sf)), !any(sf::st_is_empty(arlington_sf)))

# Three other neighbors retain old SABS geometry. Check the deliberate links
# and keep former schools and old middle campuses without a current school ID.
historical_expected <- c(`5101440` = 11L, `5101020` = 2L, `5101350` = 5L)
for (division_id in names(historical_expected)) {
  path <- pick_zone_file("app/data", division_id)
  stopifnot(identical(path, file.path("app/data", "zones", "sabs", paste0(division_id, ".json"))))
  features <- jsonlite::fromJSON(read_zone_geojson(path), simplifyVector = FALSE)$features
  linked <- unique(vapply(features, function(f) {
    sid <- f$properties$app_school_id
    if (is.null(sid)) "" else sid
  }, character(1)))
  linked <- setdiff(linked, "")
  stopifnot(length(linked) == historical_expected[[division_id]])
  stopifnot(all(linked %in% schools$school_id[schools$division_id == division_id]))
  stopifnot(all(vapply(features, function(f) identical(f$properties$source, "sabs"), logical(1))))
}

# A level cache must contain exactly the checked source features and must be
# ignored if the combined source file changes.
for (division_id in c(names(neighbor_expected), "5103300", "5103330")) {
  combined_path <- pick_zone_file("app/data", division_id)
  combined <- jsonlite::fromJSON(read_zone_geojson(combined_path), simplifyVector = FALSE)$features
  for (level in c("Elementary", "Middle", "High")) {
    cache_path <- pick_zone_level_cache("app/data", division_id, level, combined_path)
    stopifnot(!is.null(cache_path))
    cached <- jsonlite::fromJSON(read_zone_geojson(cache_path), simplifyVector = FALSE)$features
    expected <- Filter(function(f) identical(f$properties$level, level), combined)
    stopifnot(identical(cached, expected))
  }
  changed_source <- tempfile(fileext = ".json")
  writeLines("changed source", changed_source)
  stopifnot(is.null(pick_zone_level_cache("app/data", division_id, "Elementary", changed_source)))
  unlink(changed_source)
  crlf_source <- tempfile(fileext = ".json")
  source_bytes <- readBin(combined_path, what = "raw", n = file.info(combined_path)$size)
  writeBin(charToRaw(gsub("\n", "\r\n", rawToChar(source_bytes), fixed = TRUE)), crlf_source)
  stopifnot(!is.null(pick_zone_level_cache("app/data", division_id, "Elementary", crlf_source)))
  unlink(crlf_source)
}

cat("Division zone level, school match, geometry, and cache checks passed.\n")

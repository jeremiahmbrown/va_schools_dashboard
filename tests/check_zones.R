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

# A level cache must contain exactly the checked source features and must be
# ignored if the combined source file changes.
for (division_id in c("5103300", "5103330")) {
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

cat("County and City zone level, school match, geometry, and cache checks passed.\n")

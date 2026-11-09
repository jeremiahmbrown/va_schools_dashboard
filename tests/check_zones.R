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
for (division_id in c(names(neighbor_expected), "5103300", "5103330", "5102640")) {
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

beach_text <- read_zone_geojson(pick_zone_file("app/data", "5103840"))
beach_features <- jsonlite::fromJSON(beach_text, simplifyVector = FALSE)$features
tri <- Filter(function(f) identical(f$properties$official_school_name, "TRI-CAMPUS"), beach_features)
stopifnot(length(tri) == 1L)
tri_ids <- unlist(tri[[1]]$properties$app_school_ids, use.names = FALSE)
stopifnot(setequal(tri_ids, c("510384002770", "510384001704", "510384001725")))
for (sid in tri_ids) {
  selected <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(beach_text, sid), simplifyVector = FALSE)$features
  stopifnot(length(selected) == 1L, identical(selected[[1]]$properties$zone_id, tri[[1]]$properties$zone_id))
}
stopifnot(is.null(filter_zone_geojson_by_school_id(beach_text, "510384002957")))

henrico_text <- read_zone_geojson(pick_zone_file("app/data", "5101890"))
henrico_features <- jsonlite::fromJSON(henrico_text, simplifyVector = FALSE)$features
joint <- Filter(function(f) identical(f$properties$official_school_name, "MEHFOUD&VARINA"), henrico_features)
stopifnot(length(joint) == 1L)
for (sid in c("510189002124", "510189000834")) {
  selected <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(henrico_text, sid), simplifyVector = FALSE)$features
  stopifnot(length(selected) == 1L, identical(selected[[1]]$properties$zone_id, joint[[1]]$properties$zone_id))
}
kaechele <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(henrico_text, "510189002953"), simplifyVector = FALSE)$features
stopifnot(length(kaechele) == 2L)

stafford_text <- read_zone_geojson(pick_zone_file("app/data", "5103660"))
stafford_features <- jsonlite::fromJSON(stafford_text, simplifyVector = FALSE)$features
stopifnot(length(stafford_features) == 33L)
source_only <- Filter(function(f) f$properties$official_school_name %in% c("ES#18", "ES#19", "HS#6"), stafford_features)
stopifnot(length(source_only) == 3L,
          all(vapply(source_only, function(f) is.null(f$properties[["app_school_id", exact = TRUE]]), logical(1))))

wjcc_text <- read_zone_geojson(pick_zone_file("app/data", "5104020"))
wjcc_features <- jsonlite::fromJSON(wjcc_text, simplifyVector = FALSE)$features
stopifnot(length(wjcc_features) == 16L,
          length(unique(vapply(wjcc_features, function(f) f$properties$app_school_id, character(1)))) == 16L,
          setequal(vapply(wjcc_features, function(f) f$properties$level, character(1)),
                   c("Elementary", "Middle", "High")))

alex_text <- read_zone_geojson(pick_zone_file("app/data", "5100120"))
alex_features <- jsonlite::fromJSON(alex_text, simplifyVector = FALSE)$features
stopifnot(length(alex_features) == 20L,
          length(unique(vapply(alex_features, function(f) f$properties$app_school_id, character(1)))) == 17L)
jh <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(alex_text, "510012000044"), simplifyVector = FALSE)$features
ph <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(alex_text, "510012000052"), simplifyVector = FALSE)$features
stopifnot(length(jh) == 3L, length(ph) == 2L,
          setequal(vapply(jh, function(f) f$properties$level, character(1)), c("Elementary", "Middle")),
          setequal(vapply(ph, function(f) f$properties$level, character(1)), c("Elementary", "Middle")))

norfolk_text <- read_zone_geojson(pick_zone_file("app/data", "5102670"))
norfolk_features <- jsonlite::fromJSON(norfolk_text, simplifyVector = FALSE)$features
stopifnot(length(norfolk_features) == 41L,
          length(unique(vapply(norfolk_features, function(f) f$properties$app_school_id, character(1)))) == 37L)
lake_taylor <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(norfolk_text, "510267001105"), simplifyVector = FALSE)$features
azalea <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(norfolk_text, "510267001078"), simplifyVector = FALSE)$features
norview_high <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(norfolk_text, "510267001122"), simplifyVector = FALSE)$features
stopifnot(length(lake_taylor) == 2L,
          setequal(vapply(lake_taylor, function(f) f$properties$level, character(1)), c("Elementary", "Middle")),
          length(azalea) == 2L, length(norview_high) == 2L)

chesapeake_text <- read_zone_geojson(pick_zone_file("app/data", "5100810"))
chesapeake_features <- jsonlite::fromJSON(chesapeake_text, simplifyVector = FALSE)$features
stopifnot(length(chesapeake_features) == 38L)
for (sid in c("510081003157", "510081003158")) {
  selected <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(chesapeake_text, sid),
                                  simplifyVector = FALSE)$features
  stopifnot(length(selected) == 1L, identical(selected[[1]]$properties$level, "Elementary"))
}
sparrow <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(chesapeake_text, "510081000309"),
                              simplifyVector = FALSE)$features
stopifnot(length(sparrow) == 2L)

loudoun_text <- read_zone_geojson(pick_zone_file("app/data", "5102250"))
loudoun_features <- jsonlite::fromJSON(loudoun_text, simplifyVector = FALSE)$features
stopifnot(length(loudoun_features) == 93L,
          length(unique(vapply(loudoun_features, function(f) f$properties$app_school_id,
                               character(1)))) == 93L)
for (sid in c("510225002412", "510225002450")) {
  selected <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(loudoun_text, sid),
                                  simplifyVector = FALSE)$features
  stopifnot(length(selected) == 1L)
}

# Newport News City GIS keeps Newsome grade areas and repeated high-school pieces.
news_path <- pick_zone_file("app/data", "5102640")
stopifnot(identical(news_path, file.path("app/data", "zones", "division", "5102640.json")))
news_text <- read_zone_geojson(news_path)
news <- jsonlite::fromJSON(news_text, simplifyVector = FALSE)$features
stopifnot(length(news) == 37L)
news_levels <- vapply(news, function(f) f$properties$level, character(1))
stopifnot(identical(as.integer(table(factor(news_levels, levels = c("Elementary", "Middle", "High")))), c(23L, 6L, 8L)))
news_ids <- unique(vapply(news, function(f) f$properties$app_school_id, character(1)))
stopifnot(length(news_ids) == 33L, all(news_ids %in% schools$school_id[schools$division_id == "5102640"]))
newsome <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(news_text, "510264001065"), simplifyVector = FALSE)$features
stopifnot(length(newsome) == 2L,
          setequal(vapply(newsome, function(f) f$properties$official_school_name, character(1)),
                   c("Newsome Park K-2", "Newsome Park K-5")))
for (pair in list(c("Menchville High", 3L), c("Warwick High", 2L))) {
  sid <- schools$school_id[which(schools$division_id == "5102640" & trimws(schools$school_name) == pair[[1]])]
  selected <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(news_text, sid), simplifyVector = FALSE)$features
  stopifnot(length(sid) == 1L, length(selected) == as.integer(pair[[2]]))
}
for (name in c("David A. Dutrow Elementary", "Deer Park Elementary", "John Marshall Early Childhood Center",
               "B.T. Washington Middle", "Achievable Dream Middle/High")) {
  sid <- schools$school_id[which(schools$division_id == "5102640" & trimws(schools$school_name) == name)]
  stopifnot(length(sid) == 1L, is.null(filter_zone_geojson_by_school_id(news_text, sid)))
}
news_sf <- sf::st_read(news_path, quiet = TRUE)
stopifnot(all(sf::st_is_valid(news_sf)), !any(sf::st_is_empty(news_sf)))

# Campbell County 2026-27 locator has 27 level areas for all 13 schools.
campbell_path <- pick_zone_file("app/data", "5100600")
campbell <- read_zone_geojson(campbell_path)
campbell_features <- jsonlite::fromJSON(campbell, simplifyVector = FALSE)$features
campbell_schools <- subset(schools, division_id == "5100600")
stopifnot(length(campbell_features) == 27L, nrow(campbell_schools) == 13L)
levels <- vapply(campbell_features, function(f) f$properties$level, character(1))
stopifnot(identical(as.integer(table(factor(levels, levels = c("Elementary", "Middle", "High")))),
                    c(9L, 9L, 9L)))
stopifnot(setequal(vapply(campbell_features, function(f) f$properties$app_school_id, character(1)),
                   campbell_schools$school_id))
stopifnot(length(jsonlite::fromJSON(filter_zone_geojson_by_school_id(
  campbell, "510060000234"), simplifyVector = FALSE)$features) == 2L)
altavista <- jsonlite::fromJSON(filter_zone_geojson_by_school_id(
  campbell, "510060000220"), simplifyVector = FALSE)$features
stopifnot(setequal(vapply(altavista, function(f) f$properties$level, character(1)), c("Middle", "High")))

stopifnot(all(vapply(campbell_features, function(f) grepl("CCPS links", f$properties$popup, fixed = TRUE), logical(1))))

# Charlottesville has six linked City elementary areas and one historical high area.
cv_path <- pick_zone_file("app/data", "5100780")
cv_text <- read_zone_geojson(cv_path)
cv <- jsonlite::fromJSON(cv_text, simplifyVector = FALSE)$features
stopifnot(length(cv) == 7L)
cv_elem <- Filter(function(f) identical(f$properties$level, "Elementary"), cv)
cv_high <- Filter(function(f) identical(f$properties$level, "High"), cv)
stopifnot(length(cv_elem) == 6L, length(cv_high) == 1L,
  !is.null(pick_zone_level_cache("app/data", "5100780", "Elementary", cv_path)),
  !is.null(pick_zone_level_cache("app/data", "5100780", "High", cv_path)),
  grepl("2015–16", cv_high[[1]]$properties$popup, fixed = TRUE),
  all(vapply(cv_elem, function(f) grepl("rezoning is phased from 2027", f$properties$popup, fixed = TRUE), logical(1))))
stopifnot(all(vapply(cv_elem, function(f) !is.null(f$properties$app_school_id), logical(1))),
          length(jsonlite::fromJSON(read_zone_geojson(pick_zone_level_cache("app/data", "5100780", "Middle", cv_path)), simplifyVector = FALSE)$features) == 0L)
for (f in cv) if (!is.null(f$properties$app_school_id)) stopifnot(!is.null(filter_zone_geojson_by_school_id(cv_text, f$properties$app_school_id)))

cat("Division zone level, school match, geometry, and cache checks passed.\n")

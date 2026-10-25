# Run from the repository root: Rscript tests/check_division_polygons.R
source("app/R/snapshot.R")

path <- "app/data/divisions_polygons.json"
polygons <- jsonlite::fromJSON(path, simplifyVector = FALSE)
ids <- vapply(polygons, function(x) x$division_id, character(1))
stopifnot(length(polygons) == 136L, !anyDuplicated(ids))
stopifnot(sum(vapply(polygons, function(x) length(x$holes), integer(1))) == 17L)
stopifnot(sum(ids == "5102220") == 1L)

divisions_sp <- divisions_polygons_json_to_sp(path)
schools <- read.csv("app/data/schools.csv", colClasses = c(school_id = "character", division_id = "character"))
nearby <- schools[schools$division_id %in% c("5102220", "5103370"), ]
points <- sp::SpatialPoints(cbind(nearby$lon, nearby$lat),
                            proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
mapped <- sp::over(points, divisions_sp)$division_id
stopifnot(all(mapped[nearby$division_id == "5102220"] == "5102220"))
# Two Rockbridge schools are physically inside Lexington City. Their school
# division remains Rockbridge, so the app must use direct IDs for its metrics.
stopifnot(all(mapped[nearby$division_id == "5103370" & mapped != "5102220"] == "5103370"),
          sum(mapped[nearby$division_id == "5103370"] == "5102220") == 2L)

lexington <- divisions_sp[divisions_sp@data$division_id == "5102220", ]
rockbridge <- divisions_sp[divisions_sp@data$division_id == "5103370", ]
stopifnot(length(lexington) == 1L, length(rockbridge) == 1L)
stopifnot(all(is.na(sp::over(points[nearby$division_id == "5102220", ],
                            rockbridge)$division_id)))
cat("Division polygons: 136 unique IDs, 17 preserved holes, two Lexington schools within its outline.\n")

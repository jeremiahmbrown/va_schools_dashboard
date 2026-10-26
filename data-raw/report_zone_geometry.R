# Usage: Rscript --vanilla data-raw/report_zone_geometry.R candidate.json report.json max_overlap_m2
suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Need candidate, report, and max overlap area")
candidate <- args[[1]]
report_path <- args[[2]]
max_overlap <- as.numeric(args[[3]])
if (!is.finite(max_overlap) || max_overlap < 0) stop("Invalid overlap limit")

old_s2 <- sf_use_s2(FALSE)
zones <- st_read(candidate, quiet = TRUE)
if (!nrow(zones) || !all(c("division_id", "level", "app_school_id") %in% names(zones))) {
  stop("Candidate has no zones or required properties")
}
if (length(unique(zones$division_id)) != 1L || anyNA(zones$level)) stop("Mixed division or missing level")
valid <- st_is_valid(zones)
empty <- st_is_empty(zones)
if (anyNA(valid) || any(!valid) || any(empty)) stop("Candidate has invalid or empty geometry")

projected <- st_transform(zones, 5070) # US equal-area projection, in metres.
levels <- sort(unique(zones$level))
unions <- setNames(lapply(levels, function(level) st_union(projected[projected$level == level, ])), levels)
areas <- vapply(unions, function(geom) as.numeric(st_area(geom)), numeric(1))
overlap <- setNames(vapply(levels, function(level) {
  group <- projected[projected$level == level, ]
  max(0, sum(as.numeric(st_area(group))) - areas[[level]])
}, numeric(1)), levels)
reference_level <- names(which.max(areas))
gaps <- setNames(vapply(levels, function(level) {
  if (level == reference_level) return(0)
  difference <- st_difference(unions[[reference_level]], unions[[level]])
  if (length(difference) == 0L || all(st_is_empty(difference))) return(0)
  sum(as.numeric(st_area(difference)))
}, numeric(1)), levels)
box <- st_bbox(zones)
report <- list(
  division_id = unique(zones$division_id),
  feature_count = nrow(zones),
  level_counts = as.list(table(zones$level)),
  matched_school_count = length(unique(zones$app_school_id)),
  valid_geometry_count = sum(valid),
  empty_geometry_count = sum(empty),
  bounds_wgs84 = as.list(as.numeric(box)),
  area_m2_by_level = as.list(areas),
  same_level_overlap_m2 = as.list(overlap),
  gap_reference_level = reference_level,
  gap_from_reference_m2 = as.list(gaps),
  max_allowed_same_level_overlap_m2 = max_overlap
)
names(report$bounds_wgs84) <- names(box)
dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
write_json(report, report_path, pretty = TRUE, auto_unbox = TRUE)
sf_use_s2(old_s2)
if (any(overlap > max_overlap)) {
  stop("Same-level overlap exceeds reviewed limit; see ", report_path)
}
cat("Geometry checked:", report$feature_count, "features; report:", report_path, "\n")

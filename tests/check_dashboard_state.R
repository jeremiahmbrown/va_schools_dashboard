# Run from repository root: Rscript --vanilla tests/check_dashboard_state.R
# The state decision only needs the path contract. Keep this test independent
# of mapping and JSON packages.
pick_zone_file <- function(data_dir, division_id, mode) {
  path <- file.path(data_dir, "zones", "division", paste0(division_id, ".json"))
  if (file.exists(path)) path else NULL
}
source("app/R/dashboard_state.R")

target <- function(enabled, division = NULL, school = NULL, level = "Elementary", zoom = 10) {
  zone_overlay_target(enabled, division, school, level, zoom, "app/data")
}

stopifnot(identical(target(FALSE)$phase, "off"))
stopifnot(identical(target(TRUE)$phase, "no_selection"))
stopifnot(identical(target(TRUE, "5103330", zoom = 8)$phase, "zoom_low"))
stopifnot(identical(target(TRUE, "5103330", zoom = NA_real_)$phase, "zoom_low"))
stopifnot(identical(target(TRUE, "0000000")$phase, "missing_division"))

division <- target(TRUE, "5103330")
school <- target(TRUE, "5103330", "510333000001")
middle <- target(TRUE, "5103330", level = "Middle")
stopifnot(identical(division$phase, "ready"), identical(division$mode, "division"))
stopifnot(identical(school$phase, "ready"), identical(school$mode, "school"))
stopifnot(identical(division$path, school$path))
stopifnot(!identical(division$key, school$key), !identical(division$key, middle$key))
stopifnot(identical(division$key, target(TRUE, "5103330", zoom = 12)$key))

for (phase in c("no_selection", "zoom_low", "missing_division", "loading",
                "shown", "missing_school", "missing_level", "error")) {
  stopifnot(is.character(zone_overlay_status(phase, "division", "Elementary")))
}
stopifnot(is.null(zone_overlay_status("off")))
stopifnot(grepl("school", zone_overlay_status("shown", "school", "Elementary"), fixed = TRUE))
cat("Dashboard state checks passed.\n")

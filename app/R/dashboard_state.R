# Pure state decisions for the attendance-zone overlay. Keep map commands in
# app.R; this function only decides what the current inputs mean.
zone_overlay_target <- function(enabled, division_id, school_id, level, zoom, data_dir) {
  if (!isTRUE(enabled)) return(list(phase = "off", key = "off"))
  if (is.null(division_id) || !nzchar(division_id)) {
    return(list(phase = "no_selection", key = "enabled::noselection"))
  }
  if (length(zoom) != 1L || !is.finite(zoom) || zoom < 9) {
    return(list(phase = "zoom_low", key = paste0("enabled::zoomlow::", division_id)))
  }
  path <- pick_zone_file(data_dir, division_id, mode = "division")
  if (is.null(path) || !file.exists(path)) {
    return(list(phase = "missing_division", key = paste0("enabled::missing::", division_id)))
  }
  if (!is.null(school_id) && nzchar(school_id)) {
    return(list(phase = "ready", key = paste0("enabled::show_school::", division_id, "::", school_id),
                mode = "school", path = path))
  }
  list(phase = "ready", key = paste0("enabled::show_division::", division_id, "::", level),
       mode = "division", path = path)
}

zone_overlay_status <- function(phase, mode = NULL, level = NULL) {
  switch(phase,
    off = NULL,
    no_selection = "Select a school or division to show attendance zones.",
    zoom_low = "Zoom in further to show attendance zones.",
    missing_division = "No attendance zones are available for this division in this snapshot.",
    loading = "Loading attendance zones…",
    shown = if (identical(mode, "school")) "Showing the selected school's zone." else
      paste0("Showing ", if (identical(level, "Unknown")) "unclassified" else tolower(level), " attendance zones."),
    missing_school = "No attendance zone is available for this school in this snapshot.",
    missing_level = paste0("No ", if (identical(level, "Unknown")) "unclassified" else tolower(level),
                           " attendance zones are available for this division."),
    error = "Attendance zones could not be displayed. Try another division or level.",
    NULL
  )
}

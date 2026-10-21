# ==============================================================================
# Script: map_helpers.R
# Purpose:
#   Leaflet-specific helpers: palettes, legend titles, and safe color handling
#   for suppressed/missing values.
#
# Inputs:
#   - Metric definitions
#   - Numeric domains (vector of values)
#
# Outputs:
#   - Leaflet palette functions and legend titles
#
# Key Details / Debugging:
#   - Leaflet palette functions error if domain has no finite values; we guard.
# ==============================================================================

make_ramp <- function(metric_def_row) {
  # One calm color scale keeps the map legible over the gray basemap.
  # Reverse it for measures where a lower value is the better result.
  ramp <- grDevices::colorRampPalette(c("#e4f0f2", "#a8d0d7", "#4d9bab", "#155b6d"))(256)
  if (!is.null(metric_def_row$better_direction[[1]]) && metric_def_row$better_direction[[1]] == "lower_better") {
    ramp <- rev(ramp)
  }
  ramp
}

make_palette <- function(metric_def_row, domain_values) {
  domain_values <- domain_values[is.finite(domain_values)]

  # Anchor percent/index metrics explicitly to 0..100 for stable comparisons.
  if (!is.null(metric_def_row$unit[[1]]) && metric_def_row$unit[[1]] %in% c("pct", "index")) {
    domain_values <- c(domain_values, 0, 100)
  }

  # Fallback domain to avoid leaflet palette errors.
  if (length(domain_values) == 0) {
    domain_values <- c(0, 1)
  }

  leaflet::colorNumeric(palette = make_ramp(metric_def_row), domain = domain_values, na.color = "#9e9e9e")
}

legend_title <- function(metric_def_row) {
  metric_def_row$label_short[[1]]
}

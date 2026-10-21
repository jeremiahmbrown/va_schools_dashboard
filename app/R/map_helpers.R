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
  direction <- metric_def_row$better_direction[[1]]
  # Rust and teal show direction without the visual noise of bright red/green.
  # Neutral measures use indigo instead of rust to avoid implying a judgment.
  low_color <- if (identical(direction, "neutral")) "#625b8c" else "#a44f3f"
  ramp <- grDevices::colorRampPalette(c(low_color, "#e6b39d", "#f5f2ec", "#a3ccd1", "#145c6e"))(256)
  if (identical(direction, "neutral")) {
    ramp <- grDevices::colorRampPalette(c("#625b8c", "#b6b2cf", "#f4f2f3", "#a3ccd1", "#145c6e"))(256)
  }
  if (identical(direction, "lower_better")) {
    ramp <- rev(ramp)
  }
  ramp
}

map_scale_limits <- function(school_values, division_values) {
  divisions <- division_values[is.finite(division_values)]
  schools <- school_values[is.finite(school_values)]
  reference <- if (length(unique(divisions)) >= 3L) divisions else schools
  if (length(reference) == 0) return(c(0, 1))

  limits <- as.numeric(stats::quantile(reference, c(0.05, 0.95), names = FALSE))
  if (limits[[1]] == limits[[2]]) limits <- range(reference)
  if (limits[[1]] == limits[[2]]) limits <- limits + c(-0.5, 0.5)
  limits
}

make_palette <- function(metric_def_row, domain_values) {
  limits <- domain_values[is.finite(domain_values)]
  if (length(limits) == 0) limits <- c(0, 1)
  limits <- range(limits)
  if (limits[[1]] == limits[[2]]) limits <- limits + c(-0.5, 0.5)

  scale <- leaflet::colorNumeric(palette = make_ramp(metric_def_row),
                                 domain = limits, na.color = "#9e9e9e")
  function(values) {
    # Values beyond the display limits still have data. Give them the end
    # colors instead of Leaflet's missing-data gray.
    scale(pmin(pmax(values, limits[[1]]), limits[[2]]))
  }
}

legend_title <- function(metric_def_row) {
  metric_def_row$label_short[[1]]
}

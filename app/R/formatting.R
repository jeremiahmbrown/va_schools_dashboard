# ==============================================================================
# Script: formatting.R
# Purpose:
#   Consistent numeric formatting and labeling for map tooltips, popups, and
#   charts based on `metric_defs`.
#
# Inputs:
#   - Metric definition rows (from `metric_defs.csv`)
#   - Raw numeric values
#
# Outputs:
#   - Human-readable strings (e.g., "83.8", "18.0%")
#
# Key Details / Debugging:
#   - Suppressed/missing values should be passed as `NA_real_` and will render
#     as "No data".
# ==============================================================================

fmt_value <- function(value, format = "num_1", unit = NULL) {
  if (is.na(value)) {
    return("No data")
  }

  out <- switch(
    format,
    pct_1 = sprintf("%.1f%%", value),
    pct_0 = sprintf("%.0f%%", value),
    num_1 = sprintf("%.1f", value),
    num_0 = sprintf("%.0f", value),
    int = sprintf("%d", as.integer(round(value))),
    sprintf("%.2f", value)
  )

  if (!is.null(unit) && nzchar(unit) && !(grepl("%$", out) && unit == "pct")) {
    if (unit == "per_100") {
      out <- paste0(out, " per 100")
    } else if (unit == "index") {
      out <- paste0(out, " (index)")
    }
  }

  out
}

metric_label <- function(metric_defs, metric_id) {
  row <- metric_defs[metric_defs$metric_id == metric_id, , drop = FALSE]
  if (nrow(row) == 0) return(metric_id)
  row$label_long[[1]]
}


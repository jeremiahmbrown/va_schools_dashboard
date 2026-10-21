plot_metric_trend <- function(df, metric_label, entity_label, compact = FALSE, unit = NULL) {
  if (is.null(df) || nrow(df) == 0 || !any(is.finite(df$value))) {
    plot.new()
    text(0.5, 0.5, "No reportable trend for this measure.", col = "#617686", cex = if (compact) 0.9 else 1)
    return(invisible(NULL))
  }

  df <- df[order(df$year), , drop = FALSE]
  df <- df[is.finite(df$value) & !(df$suppressed %in% TRUE), , drop = FALSE]
  if (nrow(df) == 0) {
    plot.new()
    text(0.5, 0.5, "No reportable trend for this measure.", col = "#617686", cex = 0.9)
    return(invisible(NULL))
  }

  # A missing year breaks the line; it must not imply a continuous series.
  df$segment <- cumsum(c(0L, as.integer(diff(df$year) > 1L)))
  segment_sizes <- table(df$segment)
  line_segments <- as.integer(names(segment_sizes[segment_sizes >= 2L]))
  line_df <- df[df$segment %in% line_segments, , drop = FALSE]
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_line(data = line_df, ggplot2::aes(group = segment), linewidth = 1, color = "#287f91", na.rm = TRUE) +
    ggplot2::geom_point(size = if (compact) 2.5 else 3, color = "#287f91", na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$year))) +
    ggplot2::labs(x = NULL, y = NULL, title = if (compact) NULL else metric_label, subtitle = if (compact) NULL else entity_label) +
    ggplot2::theme_minimal(base_size = if (compact) 9 else 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e2e9ed"),
      axis.text = ggplot2::element_text(color = "#617686"),
      plot.title = ggplot2::element_text(face = "bold", color = "#10283a"),
      plot.subtitle = ggplot2::element_text(color = "#617686"),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
  if (!is.null(unit) && unit %in% c("pct", "index")) {
    axis_labels <- if (unit == "pct") function(x) paste0(x, "%") else ggplot2::waiver()
    p <- p + ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100), labels = axis_labels)
  }
  if (compact) p <- p + ggplot2::theme(axis.title = ggplot2::element_blank())
  p
}

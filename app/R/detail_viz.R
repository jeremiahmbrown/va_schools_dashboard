# ==============================================================================
# Script: detail_viz.R
# Purpose:
#   Rendering helpers for the "details" section below the map:
#   - Gauge-style charts for Performance Index and Suspensions per 100
#   - 100% stacked bar chart for race/ethnicity demographics
#
# Inputs:
#   - Metric definition row (1-row data.frame)
#   - Numeric values (typically 0..100)
#   - Demographics data.frame with `label`, `abbr`, `pct`
#
# Outputs:
#   - ggplot2 plot objects
#
# Key Details / Debugging:
#   - We avoid extra widget dependencies (plotly, shinyWidgets) to keep deploys
#     simple on Ubuntu + shinyapps.io.
#   - Demographics labels handle extreme skew by:
#     - labeling large segments inside the bar
#     - labeling small segments outside with a simple non-overlap "spreader"
# ==============================================================================

spread_positions <- function(y, min_sep, lower = -Inf, upper = Inf) {
  # Greedy spacing to reduce label overlap for small stacked segments.
  y2 <- y
  if (length(y2) <= 1) return(y2)

  for (i in 2:length(y2)) {
    if ((y2[[i]] - y2[[i - 1]]) < min_sep) {
      y2[[i]] <- y2[[i - 1]] + min_sep
    }
  }

  for (i in (length(y2) - 1):1) {
    if ((y2[[i + 1]] - y2[[i]]) < min_sep) {
      y2[[i]] <- y2[[i + 1]] - min_sep
    }
  }

  pmax(lower, pmin(upper, y2))
}

make_bar_gauge_plot <- function(value,
                                metric_def_row,
                                goal,
                                min_value = 0,
                                max_value = 100,
                                title = NULL) {
  if (!is.finite(value)) {
    p <- ggplot() +
      theme_void() +
      annotate("text", x = 0, y = 0, label = "No data", size = 4)
    if (!is.null(title)) p <- p + labs(title = title)
    return(p)
  }

  value <- max(min_value, min(max_value, value))

  ramp <- make_ramp(metric_def_row)

  grad_df <- data.frame(
    x = seq(min_value, max_value, length.out = 256),
    y = 1
  )

  metric_label <- metric_def_row$label_short[[1]]
  fmt <- metric_def_row$format[[1]]

  midpoint <- (min_value + max_value) / 2
  offset <- (max_value - min_value) * 0.035
  if (!is.finite(offset) || offset <= 0) offset <- 2.5
  label_x <- if (value < midpoint) value + offset else value - offset
  label_x <- max(min_value, min(max_value, label_x))
  hjust <- if (value < midpoint) 0 else 1

  ggplot(grad_df, aes(x = x, y = y, fill = x)) +
    geom_tile(height = 0.35) +
    scale_fill_gradientn(colours = ramp, limits = c(min_value, max_value), guide = "none") +
    annotate("rect", xmin = min_value, xmax = max_value, ymin = 0.825, ymax = 1.175, fill = NA, color = "#333333", linewidth = 0.4) +
    geom_vline(xintercept = value, color = "#111111", linewidth = 1.2) +
    annotate(
      "text",
      x = label_x,
      y = 1.32,
      label = fmt_value(value, fmt, unit = NULL),
      fontface = "bold",
      size = 5,
      vjust = 0,
      hjust = hjust
    ) +
    scale_x_continuous(
      limits = c(min_value, max_value),
      breaks = c(min_value, 50, max_value),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(0.6, 1.45), clip = "off") +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(size = 11, face = "bold", margin = margin(b = 4)),
      plot.margin = margin(t = 6, r = 18, b = 4, l = 18)
    ) +
    labs(title = if (is.null(title)) metric_label else title)
}

make_demographics_donut <- function(df, title = "Demographics") {
  # Expected columns: label, abbr, pct
  if (is.null(df) || nrow(df) == 0 || all(!is.finite(df$pct))) {
    return(
      ggplot() +
        theme_void() +
        annotate("text", x = 0, y = 0, label = "No demographics data", size = 4) +
        labs(title = title)
    )
  }

  d <- df %>%
    mutate(pct = ifelse(is.finite(pct), pct, 0)) %>%
    mutate(pct = pmax(0, pct)) %>%
    mutate(pct = pct / sum(pct) * 100)

  # Stable order across schools/divisions.
  order_labels <- c("% White", "% Black", "% Hispanic", "% Asian", "% Two+", "% Am. Indian", "% Nat. Hawaiian")
  d$label <- factor(d$label, levels = order_labels)
  d <- d %>% arrange(label)

  d <- d %>%
    mutate(
      ymin = dplyr::lag(cumsum(pct), default = 0),
      ymax = cumsum(pct),
      y_mid = (ymin + ymax) / 2
    )

  # Qualitative palette (7 categories) tuned for donut readability.
  demo_cols <- c(
    "% White" = "#4C78A8",
    "% Black" = "#E45756",
    "% Hispanic" = "#F58518",
    "% Asian" = "#72B7B2",
    "% Two+" = "#54A24B",
    "% Am. Indian" = "#B279A2",
    "% Nat. Hawaiian" = "#9D755D"
  )

  label_clean <- function(x) sub("^%\\s*", "", as.character(x))
  d <- d %>%
    mutate(
      legend_label = paste0(label_clean(label), " ", sprintf("%.1f%%", pct)),
      legend_label = factor(legend_label, levels = legend_label)
    )
  legend_cols <- setNames(demo_cols[as.character(d$label)], as.character(d$legend_label))

  ggplot(d, aes(x = 2, y = pct, fill = legend_label)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = legend_cols, drop = FALSE) +
    xlim(0.6, 2.55) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, face = "bold", margin = margin(b = 4)),
      plot.margin = margin(t = 4, r = 10, b = 4, l = 6),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.key.size = grid::unit(0.9, "lines")
    ) +
    labs(title = title)

}

make_race_stacked_bar <- function(df, title = "Student composition", top_n = 6) {
  # Horizontal 100% stacked bar intended to handle tiny segments cleanly:
  # - no in-bar labels
  # - legend carries values ("White (62.6%)")
  if (is.null(df) || nrow(df) == 0 || all(!is.finite(df$pct))) {
    return(
      ggplot() +
        theme_void() +
        annotate("text", x = 0, y = 0, label = "No demographics data", size = 4) +
        labs(title = title)
    )
  }

  d <- df %>%
    transmute(label = as.character(label), pct = as.numeric(pct)) %>%
    mutate(pct = ifelse(is.finite(pct), pct, 0)) %>%
    mutate(pct = pmax(0, pct))

  if (sum(d$pct) <= 0) {
    return(
      ggplot() +
        theme_void() +
        annotate("text", x = 0, y = 0, label = "No demographics data", size = 4) +
        labs(title = title)
    )
  }

  # Normalize to exactly 100 for display.
  d <- d %>% mutate(pct = pct / sum(pct) * 100)

  # Top N + Other for very small segments.
  d <- d %>% arrange(desc(pct))
  if (nrow(d) > top_n) {
    other_pct <- sum(d$pct[(top_n + 1):nrow(d)])
    d <- bind_rows(d[seq_len(top_n), ], data.frame(label = "Other", pct = other_pct, stringsAsFactors = FALSE))
  }

  order_labels <- c("% White", "% Black", "% Hispanic", "% Asian", "% Two+", "% Am. Indian", "% Nat. Hawaiian", "Other")
  d$label <- factor(d$label, levels = order_labels)
  d <- d %>% arrange(label)

  label_clean <- function(x) sub("^%\\s*", "", as.character(x))
  d <- d %>%
    mutate(
      legend_label = paste0(label_clean(label), " (", sprintf("%.1f%%", pct), ")"),
      legend_label = factor(legend_label, levels = legend_label)
    )

  demo_cols <- c(
    "% White" = "#4C78A8",
    "% Black" = "#E45756",
    "% Hispanic" = "#F58518",
    "% Asian" = "#72B7B2",
    "% Two+" = "#54A24B",
    "% Am. Indian" = "#B279A2",
    "% Nat. Hawaiian" = "#9D755D",
    "Other" = "#9e9e9e"
  )
  fill_cols <- setNames(demo_cols[as.character(d$label)], as.character(d$legend_label))

  p <- ggplot(d, aes(x = "Race/ethnicity", y = pct, fill = legend_label)) +
    geom_col(width = 0.55, color = "white", linewidth = 0.6) +
    coord_flip() +
    scale_fill_manual(values = fill_cols, drop = FALSE) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 12, face = "bold", margin = margin(b = 4)),
      plot.margin = margin(t = 6, r = 10, b = 6, l = 6),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      legend.key.size = grid::unit(0.9, "lines")
    ) +
    labs(title = title)

  # Add in-bar % labels for larger segments only (keeps tiny segments uncluttered).
  d_lab <- d %>% filter(is.finite(pct) & pct >= 5)
  if (nrow(d_lab) > 0) {
    p <- p + geom_text(
      data = d_lab,
      aes(label = sprintf("%.1f%%", pct)),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 3.4,
      show.legend = FALSE
    )
  }

  p
}

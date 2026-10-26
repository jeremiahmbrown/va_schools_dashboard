# ==============================================================================
# Script: app.R
# Purpose:
#   Shiny dashboard for exploring Virginia public schools via an interactive map.
#   The app is designed for shinyapps.io deployment using a frozen snapshot under
#   `app/data/` that can be swapped out later without changing app logic.
#
# Inputs:
#   - Frozen snapshot files in `app/data/` (see `docs/DATA_SCHEMA.md`)
#
# Outputs:
#   - Interactive Leaflet map (schools + division choropleth)
#   - Details panel with a 5-year trend for the selected metric
#
# Key Details / Debugging:
#   - We use `sp` polygons to avoid system GIS dependencies on Ubuntu.
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(jsonlite)
  library(sp)
})

has_leaflet <- requireNamespace("leaflet", quietly = TRUE)
if (has_leaflet) {
  suppressPackageStartupMessages(library(leaflet))
}

# Support two execution modes:
# - `shiny::runApp('.../va_schools_dashboard/app')` (working dir = app/)
# - shinyapps.io deploy from repo root with `appPrimaryDoc = 'app/app.R'` (working dir = repo root)
app_dir <- if (file.exists(file.path("app", "R", "snapshot.R"))) "app" else "."

source(file.path(app_dir, "R", "snapshot.R"))
source(file.path(app_dir, "R", "formatting.R"))
source(file.path(app_dir, "R", "map_helpers.R"))
source(file.path(app_dir, "R", "detail_viz.R"))
source(file.path(app_dir, "R", "percentiles.R"))
source(file.path(app_dir, "R", "zones.R"))
source(file.path(app_dir, "R", "dashboard_details.R"))
source(file.path(app_dir, "R", "dashboard_panel_server.R"))
source(file.path(app_dir, "R", "metric_availability.R"))

snapshot <- load_snapshot(file.path(app_dir, "data"))

schools <- snapshot$schools
metric_defs <- snapshot$metric_defs
school_metrics <- snapshot$school_metrics
enrollment_by_year <- snapshot$enrollment_by_year
division_metrics <- snapshot$division_metrics
school_subject_perf <- snapshot$school_subject_perf
division_subject_perf <- snapshot$division_subject_perf
divisions_sp <- snapshot$divisions_sp

metric_years <- reportable_metric_years(school_metrics, division_metrics)
default_metric <- "test_sol_annual_overall"
available_years <- sort(unique(metric_years$year[metric_years$metric_id == default_metric]))
default_year <- max(available_years)

metric_defs_by_id <- metric_defs %>% distinct(metric_id, .keep_all = TRUE)

# Precompute percentiles once so the UI can render quickly.
school_percentiles <- compute_school_percentiles(school_metrics, schools, metric_defs_by_id)
division_percentiles_state <- compute_division_percentiles(division_metrics, metric_defs_by_id)

# Approximate polygon area in lon/lat degrees for draw-order control.
# We draw larger divisions first so smaller city divisions sit on top.
polygon_area_deg2 <- function(spdf) {
  vapply(slot(spdf, "polygons"), function(poly_obj) {
    ring_areas <- vapply(slot(poly_obj, "Polygons"), function(ring_obj) {
      coords <- slot(ring_obj, "coords")
      if (nrow(coords) < 3) return(0)

      # Remove duplicate closing vertex for shoelace area.
      if (coords[1, 1] == coords[nrow(coords), 1] && coords[1, 2] == coords[nrow(coords), 2]) {
        coords <- coords[-nrow(coords), , drop = FALSE]
      }
      if (nrow(coords) < 3) return(0)

      x <- coords[, 1]
      y <- coords[, 2]
      abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1]))) / 2
    }, numeric(1))

    sum(ring_areas)
  }, numeric(1))
}

division_polygon_draw_order <- order(polygon_area_deg2(divisions_sp), decreasing = TRUE)

build_polygon_division_map <- function(schools_df, divisions_spdf) {
  base <- divisions_spdf@data %>%
    transmute(
      polygon_division_id = division_id,
      polygon_division_name = division_name
    )

  pts_df <- schools_df %>% filter(is.finite(lat), is.finite(lon))
  if (nrow(pts_df) == 0) {
    return(base %>% mutate(mapped_division_id = polygon_division_id, mapped_division_name = polygon_division_name, n_points = 0L))
  }

  pts <- sp::SpatialPoints(
    cbind(pts_df$lon, pts_df$lat),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
  )
  hit <- sp::over(pts, divisions_spdf)

  dominant <- pts_df %>%
    transmute(
      division_id = division_id,
      division_name = division_name,
      polygon_division_id = hit$division_id
    ) %>%
    filter(!is.na(polygon_division_id)) %>%
    count(polygon_division_id, division_id, division_name, name = "n_points", sort = TRUE) %>%
    group_by(polygon_division_id) %>%
    slice_max(n_points, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      polygon_division_id = polygon_division_id,
      mapped_division_id = division_id,
      mapped_division_name = division_name,
      n_points = as.integer(n_points)
    )

  direct <- schools_df %>%
    distinct(division_id, division_name) %>%
    transmute(polygon_division_id = division_id,
              direct_division_name = division_name)

  base %>%
    left_join(dominant, by = "polygon_division_id") %>%
    left_join(direct, by = "polygon_division_id") %>%
    mutate(
      mapped_division_id = if_else(!is.na(direct_division_name),
                                   polygon_division_id,
                                   dplyr::coalesce(mapped_division_id, polygon_division_id)),
      mapped_division_name = dplyr::coalesce(direct_division_name,
                                             mapped_division_name,
                                             polygon_division_name),
      n_points = dplyr::coalesce(n_points, 0L)
    ) %>%
    select(-direct_division_name)
}

polygon_division_map <- build_polygon_division_map(schools, divisions_sp)
division_name_lookup <- schools %>% distinct(division_id, division_name)

division_search_choices <- polygon_division_map %>%
  distinct(mapped_division_id, mapped_division_name) %>%
  arrange(mapped_division_name) %>%
  mutate(
    value = paste0("division::", mapped_division_id),
    label = paste0("Division: ", mapped_division_name)
  )

polygon_search_choices <- polygon_division_map %>%
  distinct(polygon_division_id, polygon_division_name, mapped_division_id, mapped_division_name) %>%
  arrange(polygon_division_name) %>%
  mutate(
    value = paste0("polygon::", polygon_division_id),
    label = paste0("Boundary: ", polygon_division_name)
  )

school_search_choices <- schools %>%
  arrange(school_name, division_name) %>%
  mutate(
    value = paste0("school::", school_id),
    label = paste0("School: ", school_name, " (", division_name, ")")
  )

map_widget <- if (has_leaflet) {
  leafletOutput("map", height = "100%")
} else {
  tagList(
    tags$div(
      class = "alert alert-warning",
      tags$strong("Leaflet not installed."),
      " Showing a fallback plot-based map. Install the `leaflet` package for the full interactive basemap."
    ),
    plotOutput("map_plot", height = "100%", click = "map_plot_click")
  )
}

source(file.path(app_dir, "R", "dashboard_ui.R"))
ui <- dashboard_ui(map_widget, available_years, default_year, schools, snapshot, app_dir)

server <- function(input, output, session) {
  # School points are only shown once the user zooms in past this level.
  division_rollup_zoom_max <- 7

  selected_school_id <- reactiveVal(NULL)
  selected_division_id <- reactiveVal(NULL)
  search_ready <- reactiveVal(FALSE)
  zones_warned_key <- reactiveVal(NULL)

  output$zone_source_note <- renderUI({
    division_id <- selected_division_id()
    if (is.null(division_id) || !nzchar(division_id)) return(NULL)
    note <- if (identical(division_id, "5103300")) {
      "Roanoke City: 2026–27 elementary Phase 1 zones; City middle and high GIS. The source has a small overlap between Preston Park and Round Hill elementary areas. Confirm an address with RCPS."
    } else if (identical(division_id, "5103330")) {
      "Roanoke County: public county GIS, retrieved 2026-10-20. The service does not state a boundary effective date."
    } else if (identical(division_id, "5100360")) {
      "Bedford County: public county GIS for elementary, middle, and high zones; Bedford Primary uses an older NCES SABS 2015–16 polygon. The county is reviewing boundary changes. Confirm an address with BCPS."
    } else if (identical(division_id, "5100420")) {
      "Botetourt County: public county GIS elementary, middle, and high zones. The GIS layers do not state a school-year effective date. Confirm an address with BCPS."
    } else if (identical(division_id, "5102520")) {
      "Montgomery County: public county GIS elementary, middle, and high assignments. Christiansburg Primary shares the Christiansburg Elementary area, as listed by MCPS. The GIS layer does not state a school-year effective date."
    } else if (identical(division_id, "5103460")) {
      "Salem City: public city GIS elementary zones. Middle and high zones use older NCES SABS 2015–16 polygons. The GIS layer does not state a school-year effective date. Confirm an address with Salem City Schools."
    } else if (identical(division_id, "5100270")) {
      "Arlington County: public APS locator and County GIS neighborhood zones. APS links 2026–27 maps, but the GIS School_Year field says 2022–2023. Six schools have no neighborhood polygon. Confirm an address with APS."
    } else if (identical(division_id, "5101260")) {
      "Fairfax County: FCPS and County GIS standard attendance areas for 2026–27. Ten schools have no separate polygon in these layers. Some high-school edges overlap; confirm an address with the FCPS boundary locator."
    } else if (identical(division_id, "5103130")) {
      "Prince William County: PWCS GIS elementary areas are labeled 2025–26; middle and high areas are labeled current without a separate year. The high layer extends near Quantico beyond elementary and middle coverage. The 2026–27 elementary layer has Woodbridge, absent from this frozen snapshot. Confirm an address with PWCS or the base school liaison."
    } else if (identical(division_id, "5101440")) {
      "Franklin County: NCES SABS 2015–16 survey zones. This file includes former schools and may not match present assignments. Confirm an address with Franklin County Public Schools."
    } else if (identical(division_id, "5101020") || identical(division_id, "5101350")) {
      "NCES SABS 2015–16 survey zones. Some combined-grade schools appear under Unclassified (older source). Confirm an address with the school division."
    } else if (file.exists(file.path(app_dir, "data", "zones", "sabs", paste0(division_id, ".json")))) {
      "NCES SABS 2015–16 survey zones. These may not match current attendance rules."
    } else {
      "No attendance-zone file is available for this division in the current snapshot."
    }
    tags$p(class = "meta-note", note)
  })
  output$zone_selection_action <- renderUI({
    if (!isTRUE(input$zones_enabled) || is.null(selected_school_id())) return(NULL)
    actionButton("show_division_zones", "Show all zones in this division")
  })
  observeEvent(input$show_division_zones, {
    selected_school_id(NULL)
  }, ignoreInit = TRUE)

  perf_metric_id <- reactive({
    metric <- if (is.null(input$perf_metric) || !nzchar(input$perf_metric)) "sol" else input$perf_metric
    view <- if (is.null(input$perf_view) || !nzchar(input$perf_view)) "overall" else input$perf_view
    subj <- if (is.null(input$perf_subject) || !nzchar(input$perf_subject)) "ela" else input$perf_subject

    if (view == "overall") {
      return(dplyr::case_when(
        metric == "sol" ~ "test_sol_annual_overall",
        metric == "pass" ~ "test_overall_pass_index",
        TRUE ~ "test_overall_perf_index"
      ))
    }

    suffix <- subj
    if (metric == "sol") return(paste0("test_sol_annual_", suffix))
    if (metric == "pass") return(paste0("test_pass_", suffix))
    paste0("test_acr_", suffix)
  })

  metric_id_selected <- reactive({
    if (!is.null(input$category) && input$category == "test") {
      perf_metric_id()
    } else {
      metric <- input$metric_id
      valid <- metric_defs_by_id$metric_id[metric_defs_by_id$category == input$category]
      if (length(metric) != 1 || !(metric %in% valid)) return(NULL)
      metric
    }
  })

  dashboard_panel_server(
    input, output, session, selected_school_id, selected_division_id,
    metric_id_selected, schools, division_name_lookup, divisions_sp,
    metric_defs_by_id, school_metrics, division_metrics, enrollment_by_year
  )

  metric_available_years <- reactive({
    metric <- metric_id_selected()
    req(metric)
    sort(unique(metric_years$year[metric_years$metric_id == metric]))
  })

  observe({
    years <- metric_available_years()
    current_year <- isolate(input$year)
    selected <- if (length(years) == 0) {
      character(0)
    } else if (length(current_year) == 1 && suppressWarnings(as.integer(current_year)) %in% years) {
      current_year
    } else {
      max(years)
    }
    updateSelectInput(session, "year", choices = data_year_choices(years), selected = selected)
  })

  output$metric_availability <- renderUI({
    years <- metric_available_years()
    metric <- metric_id_selected()
    row <- metric_defs_by_id %>% filter(metric_id == metric) %>% slice(1)
    if (length(years) == 0) {
      return(tags$p(class = "meta-note", "No reportable school or division values for this measure."))
    }
    selected <- suppressWarnings(as.integer(input$year))
    message <- if (length(selected) == 1 && is.finite(selected) && !selected %in% years) {
      paste0("No values for ", selected, "; latest available: ", max(years), ".")
    } else {
      paste0("Available through ", max(years), ".")
    }
    if (metric == "test_overall_perf_index") {
      message <- paste(message, "App-defined English and Math ACR mean.")
    } else if (metric %in% c("test_overall_pass_index", "test_sol_annual_overall")) {
      message <- paste(message, "App-defined mean across available subjects.")
    }
    tags$div(class = "meta-note", tags$strong(message),
             tags$details(tags$summary("Metric definition"), row$label_long[[1]]))
  })

  observeEvent(input$perf_metric, {
    family <- switch(input$perf_metric, sol = "test_sol_annual_", acr = "test_acr_", pass = "test_pass_")
    if (is.null(family)) return()
    subjects <- c("English (ELA)" = "ela", "Math" = "math", "Science" = "science")
    choices <- subjects[paste0(family, unname(subjects)) %in% metric_years$metric_id]
    current <- isolate(input$perf_subject)
    selected <- if (length(choices) == 0) character(0) else if (length(current) == 1 && current %in% choices) current else unname(choices[[1]])
    updateSelectInput(session, "perf_subject", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  perf_metric_ids <- metric_defs_by_id %>%
    filter(category == "test") %>%
    pull(metric_id)

  # Avoid reacting to selectize initialization/update churn on page load.
  session$onFlushed(function() {
    search_ready(TRUE)
  }, once = TRUE)

  observeEvent(TRUE, {
    updateSelectizeInput(
      session,
      "entity_search",
      choices = list(
        Divisions = stats::setNames(division_search_choices$value, division_search_choices$label),
        Boundaries = stats::setNames(polygon_search_choices$value, polygon_search_choices$label),
        Schools = stats::setNames(school_search_choices$value, school_search_choices$label)
      ),
      selected = character(0),
      server = TRUE
    )
  }, once = TRUE)

  school_threshold_base <- school_metrics %>%
    filter(metric_id %in% c(perf_metric_ids, "behavior_sbar_oss_actions_per_100")) %>%
    select(year, school_id, metric_id, value) %>%
    tidyr::pivot_wider(names_from = metric_id, values_from = value)

  division_threshold_base <- division_metrics %>%
    filter(metric_id %in% c(perf_metric_ids, "behavior_sbar_oss_actions_per_100")) %>%
    select(year, division_id, metric_id, value) %>%
    tidyr::pivot_wider(names_from = metric_id, values_from = value)

  # Debounce slider updates to avoid expensive redraws on every drag step.
  threshold_inputs <- reactive({
    list(
      enabled = isTRUE(input$threshold_enabled),
      scope = input$threshold_scope,
      min_perf = input$min_perf_threshold,
      max_susp = input$max_susp_threshold
    )
  }) %>% debounce(250)

  output$metric_selector <- renderUI({
    if (input$category == "test") {
      return(selectInput(
        "perf_metric", "Measure",
        choices = c("Annual SOL pass rate" = "sol",
                    "Accreditation Combined Rate (ACR)" = "acr",
                    "SQP Percent Passing" = "pass"),
        selected = if (default_year >= 2025) "sol" else "acr", selectize = FALSE
      ))
    }

    defs <- metric_defs_by_id %>%
      filter(category == input$category, metric_id %in% metric_years$metric_id)
    if (identical(input$category, "behavior")) {
      defs <- defs %>%
        arrange(desc(metric_id == "behavior_sbar_oss_actions_per_100"))
    }
    choices <- setNames(defs$metric_id, defs$label_short)
    selected <- if (nrow(defs) == 0) character(0) else defs$metric_id[[1]]
    selectInput("metric_id", "Metric", choices = choices, selected = selected, selectize = FALSE)
  })

  current_map_data <- reactive({
    req(metric_id_selected(), input$year)

    year_selected <- as.integer(input$year)
    th <- threshold_inputs()

    metric_def_row <- metric_defs_by_id %>%
      filter(metric_id == metric_id_selected()) %>%
      slice(1)

    # Apply filters for the school layer.
    schools_filtered <- schools
    if (!is.null(input$school_level) && input$school_level != "All") {
      schools_filtered <- schools_filtered %>% filter(school_level == input$school_level)
    }
    if (!is.null(input$division_filter) && input$division_filter != "All") {
      schools_filtered <- schools_filtered %>% filter(division_name == input$division_filter)
    }

    school_vals <- school_metrics %>%
      filter(year == year_selected, metric_id == metric_id_selected()) %>%
      select(school_id, value, suppressed, unit)

    schools_joined <- schools_filtered %>%
      left_join(school_vals, by = "school_id")

    threshold_school_ids <- schools$school_id
    threshold_division_ids <- divisions_sp@data$division_id
    if (isTRUE(th$enabled)) {
      perf_col <- perf_metric_id()
      if (!perf_col %in% names(school_threshold_base)) perf_col <- "test_overall_perf_index"

      threshold_school_ids <- school_threshold_base %>%
        filter(year == year_selected) %>%
        filter(
          is.finite(.data[[perf_col]]),
          is.finite(behavior_sbar_oss_actions_per_100),
          .data[[perf_col]] >= th$min_perf,
          behavior_sbar_oss_actions_per_100 <= th$max_susp
        ) %>%
        pull(school_id)

      threshold_division_ids <- division_threshold_base %>%
        filter(year == year_selected) %>%
        filter(
          is.finite(.data[[perf_col]]),
          is.finite(behavior_sbar_oss_actions_per_100),
          .data[[perf_col]] >= th$min_perf,
          behavior_sbar_oss_actions_per_100 <= th$max_susp
        ) %>%
        pull(division_id)

      if (th$scope %in% c("schools", "both")) {
        schools_joined <- schools_joined %>% filter(school_id %in% threshold_school_ids)
      }
    }

    div_vals_primary <- division_metrics %>%
      filter(year == year_selected, metric_id == metric_id_selected()) %>%
      select(division_id, value, suppressed, n_included, n_suppressed, agg_method)

    # Backfill division values for any polygon division not present in
    # pre-aggregated division_metrics (rare in source snapshots).
    div_vals_fallback <- schools %>%
      left_join(school_vals, by = "school_id") %>%
      mutate(weight = dplyr::if_else(is.finite(enrollment_latest) & enrollment_latest > 0, enrollment_latest, 1)) %>%
      group_by(division_id) %>%
      summarise(
        value = if (all(!is.finite(value))) NA_real_ else stats::weighted.mean(value[is.finite(value)], w = weight[is.finite(value)], na.rm = TRUE),
        suppressed = all(is.na(suppressed) | suppressed),
        n_included = sum(is.finite(value) & !dplyr::coalesce(suppressed, FALSE)),
        n_suppressed = sum(dplyr::coalesce(suppressed, FALSE)),
        agg_method = "enrollment_weighted_fallback",
        .groups = "drop"
      )
    # The 2021–22 SOL division file reports rates directly. Do not replace a
    # missing official division rate with a mean of school percentages.
    if (year_selected == 2022 &&
        startsWith(metric_id_selected(), "test_sol_annual_")) {
      div_vals_fallback <- div_vals_fallback %>%
        mutate(value = NA_real_, suppressed = TRUE)
    }

    # Prefer precomputed division metrics; if missing/suppressed at source,
    # backfill from school-level values when available.
    div_vals <- full_join(
      div_vals_primary,
      div_vals_fallback,
      by = "division_id",
      suffix = c("_primary", "_fallback")
    ) %>%
      transmute(
        division_id = division_id,
        value = dplyr::case_when(
          is.finite(value_primary) ~ value_primary,
          is.finite(value_fallback) ~ value_fallback,
          TRUE ~ NA_real_
        ),
        suppressed = dplyr::case_when(
          is.finite(value_primary) ~ dplyr::coalesce(suppressed_primary, FALSE),
          is.finite(value_fallback) ~ dplyr::coalesce(suppressed_fallback, FALSE),
          !is.na(suppressed_primary) ~ suppressed_primary,
          !is.na(suppressed_fallback) ~ suppressed_fallback,
          TRUE ~ TRUE
        ),
        n_included = dplyr::case_when(
          is.finite(value_primary) ~ dplyr::coalesce(n_included_primary, 0L),
          is.finite(value_fallback) ~ dplyr::coalesce(n_included_fallback, 0L),
          TRUE ~ dplyr::coalesce(n_included_primary, n_included_fallback, 0L)
        ),
        n_suppressed = dplyr::case_when(
          is.finite(value_primary) ~ dplyr::coalesce(n_suppressed_primary, 0L),
          is.finite(value_fallback) ~ dplyr::coalesce(n_suppressed_fallback, 0L),
          TRUE ~ dplyr::coalesce(n_suppressed_primary, n_suppressed_fallback, 0L)
        ),
        agg_method = dplyr::case_when(
          is.finite(value_primary) ~ agg_method_primary,
          is.finite(value_fallback) ~ agg_method_fallback,
          TRUE ~ dplyr::coalesce(agg_method_primary, agg_method_fallback, "unknown")
        )
      )

    if (isTRUE(th$enabled) && th$scope %in% c("divisions", "both")) {
      div_vals <- div_vals %>% filter(division_id %in% threshold_division_ids)
    }

    polygon_vals <- polygon_division_map %>%
      left_join(
        div_vals %>%
          transmute(
            mapped_division_id = division_id,
            value = value,
            suppressed = suppressed,
            n_included = n_included,
            n_suppressed = n_suppressed,
            agg_method = agg_method
          ),
        by = "mapped_division_id"
      ) %>%
      transmute(
        division_id = polygon_division_id,
        display_division_name = mapped_division_name,
        mapped_division_id = mapped_division_id,
        value = value,
        suppressed = suppressed,
        n_included = n_included,
        n_suppressed = n_suppressed,
        agg_method = agg_method
      )

    if (isTRUE(th$enabled) && th$scope %in% c("divisions", "both")) {
      polygon_vals <- polygon_vals %>% filter(is.finite(value))
    }

    list(
      metric_def_row = metric_def_row,
      schools_joined = schools_joined,
      div_vals = div_vals,
      polygon_vals = polygon_vals,
      threshold_enabled = isTRUE(th$enabled),
      threshold_scope = th$scope
    )
  })

  output$map_legend <- renderUI({
    data <- current_map_data()
    def <- data$metric_def_row
    limits <- map_scale_limits(data$schools_joined$value,
                               if ("divisions" %in% input$layers) data$polygon_vals$value else numeric(0))
    colors <- make_ramp(def)[c(1, 90, 170, 256)]
    legend_number <- function(value) {
      if (def$unit[[1]] == "pct") return(sprintf("%.0f%%", value))
      if (def$unit[[1]] == "index") return(sprintf("%.0f", value))
      sprintf("%.1f", value)
    }
    scale_group <- if ("divisions" %in% input$layers) "division" else "school"
    tags$div(
      title = paste0("Colors use the middle 90% of ", scale_group,
                     " values for this measure and year. Values outside that range use the end colors."),
      tags$span(class = "legend-caption", switch(def$better_direction[[1]],
        higher_better = "Higher is better", lower_better = "Lower is better", "Value scale")),
      tags$div(class = "legend-ramp",
               style = paste0("background: linear-gradient(90deg, ", paste(colors, collapse = ", "), ");")),
      tags$div(class = "legend-labels",
               tags$span(paste0("≤ ", legend_number(limits[[1]]))),
               tags$span(paste0("≥ ", legend_number(limits[[2]]))))
    )
  })

  if (has_leaflet) {
    focus_division_from_map <- function(polygon_id) {
      div_sp <- divisions_sp[divisions_sp@data$division_id == polygon_id, ]
      if (length(div_sp) == 1) {
        bb <- sp::bbox(div_sp)
        leafletProxy("map") %>%
          fitBounds(lng1 = bb[1, 1], lat1 = bb[2, 1], lng2 = bb[1, 2], lat2 = bb[2, 2])
      }
    }

    # Base map.
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 6, preferCanvas = TRUE)) %>%
        addMapPane(name = "divisionPolygonPane", zIndex = 380) %>%
        addMapPane(name = "zonePolygonPane", zIndex = 410) %>%
        addMapPane(name = "schoolPointPane", zIndex = 440) %>%
        # Light Gray Canvas keeps roads faint without place labels outside Virginia.
        addTiles(
          "https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
          attribution = "Tiles &copy; Esri, HERE, Garmin, &copy; OpenStreetMap contributors, and the GIS user community"
        ) %>%
        setView(lng = -78.7, lat = 37.5, zoom = 7) %>%
        htmlwidgets::onRender("function(el, x) {
          var map = this;
          if (!map || typeof map.on !== 'function') return;
          var hoverReady = true;
          el.addEventListener('pointermove', function() { hoverReady = true; }, { passive: true });
          if (window.Shiny) {
            Shiny.addCustomMessageHandler('dashboard-close-popup', function(message) {
              map.closePopup();
            });
          }
          map.on('popupopen', function() {
            hoverReady = false;
            map.eachLayer(function(layer) {
              if (typeof layer.closeTooltip === 'function') layer.closeTooltip();
            });
          });
          map.on('tooltipopen', function(event) {
            if (!hoverReady && event.tooltip) map.closeTooltip(event.tooltip);
          });
        }")
    })

    # Update selection based on map clicks.
  observeEvent(input$map_marker_click, {
    marker_id <- input$map_marker_click$id
    if (is.null(marker_id)) return()
    if (startsWith(marker_id, "school::") || startsWith(marker_id, "school-tap::")) {
      school_id <- sub("^school(-tap)?::", "", marker_id)
      school_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
      division_id <- if (nrow(school_row) == 1) school_row$division_id[[1]] else NULL

      selected_school_id(school_id)
      if (!is.null(division_id) && nzchar(division_id)) selected_division_id(division_id)
      if (nrow(school_row) == 1 && is.finite(school_row$lat[[1]]) && is.finite(school_row$lon[[1]])) {
        # Clicking a school should not zoom *out* if the user is already zoomed in.
        # (Leaflet provides input$map_zoom for the current zoom level.)
        current_zoom <- suppressWarnings(as.numeric(input$map_zoom))
        if (length(current_zoom) != 1 || !is.finite(current_zoom)) current_zoom <- NA_real_
        target_zoom <- if (is.finite(current_zoom)) max(12, current_zoom) else 12
        leafletProxy("map") %>%
          flyTo(lng = school_row$lon[[1]], lat = school_row$lat[[1]], zoom = target_zoom)
      }
      return()
    }

    if (startsWith(marker_id, "division::")) {
      polygon_id <- sub("^division::", "", marker_id)
      mapped <- polygon_division_map %>%
        filter(polygon_division_id == polygon_id) %>%
        slice(1)
      division_id <- if (nrow(mapped) == 1) mapped$mapped_division_id[[1]] else polygon_id

      selected_division_id(division_id)
      selected_school_id(NULL)

      focus_division_from_map(polygon_id)
    }
  })

  observeEvent(input$map_shape_click, {
    polygon_id <- input$map_shape_click$id
    if (is.null(polygon_id) || !nzchar(polygon_id)) return()
    mapped <- polygon_division_map %>%
      filter(polygon_division_id == polygon_id) %>%
      slice(1)
    division_id <- if (nrow(mapped) == 1) mapped$mapped_division_id[[1]] else polygon_id

    selected_division_id(division_id)
    selected_school_id(NULL)

    focus_division_from_map(polygon_id)
  })

  } else {
    observeEvent(input$map_plot_click, {
      click <- input$map_plot_click
      if (is.null(click$x) || is.null(click$y)) return()

      data <- current_map_data()
      schools_joined <- data$schools_joined

      # Pick nearest school point if reasonably close; else select division by polygon.
      dx <- schools_joined$lon - click$x
      dy <- schools_joined$lat - click$y
      dist2 <- dx * dx + dy * dy
      idx <- which.min(dist2)

      # Rough threshold in degrees (works OK for VA-scale clicking).
      if (length(idx) == 1 && is.finite(dist2[[idx]]) && dist2[[idx]] < (0.06 * 0.06)) {
        selected_school_id(schools_joined$school_id[[idx]])
        selected_division_id(schools_joined$division_id[[idx]])
        return()
      }

      pt <- sp::SpatialPoints(matrix(c(click$x, click$y), ncol = 2), proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
      hit <- sp::over(pt, divisions_sp)
      if (!is.null(hit) && nrow(hit) == 1 && !is.na(hit$division_id[[1]])) {
        selected_division_id(hit$division_id[[1]])
        selected_school_id(NULL)
      }
    })
  }

  observeEvent(input$entity_search, {
    if (!isTRUE(search_ready())) return()

    val <- input$entity_search
    if (is.null(val) || !nzchar(val)) return()
    if (has_leaflet) session$sendCustomMessage("dashboard-close-popup", list())

    if (startsWith(val, "school::")) {
      school_id <- sub("^school::", "", val)
      selected_school_id(school_id)

      school_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
      if (nrow(school_row) == 1 && nzchar(school_row$division_id[[1]])) {
        selected_division_id(school_row$division_id[[1]])
      }
      if (nrow(school_row) == 1 && is.finite(school_row$lat[[1]]) && is.finite(school_row$lon[[1]])) {
        leafletProxy("map") %>%
          flyTo(lng = school_row$lon[[1]], lat = school_row$lat[[1]], zoom = 12)
      }
      updateSelectizeInput(session, "entity_search", selected = character(0))
      return()
    }

    if (startsWith(val, "division::")) {
      division_id <- sub("^division::", "", val)
      selected_division_id(division_id)
      selected_school_id(NULL)

      polygon_ids <- polygon_division_map %>%
        filter(mapped_division_id == division_id) %>%
        pull(polygon_division_id)

      div_sp <- divisions_sp[divisions_sp@data$division_id %in% polygon_ids, ]
      if (length(div_sp) >= 1) {
        bb <- sp::bbox(div_sp)
        leafletProxy("map") %>%
          fitBounds(lng1 = bb[1, 1], lat1 = bb[2, 1], lng2 = bb[1, 2], lat2 = bb[2, 2])
      }
      updateSelectizeInput(session, "entity_search", selected = character(0))
      return()
    }

    if (startsWith(val, "polygon::")) {
      polygon_id <- sub("^polygon::", "", val)
      mapped <- polygon_division_map %>%
        filter(polygon_division_id == polygon_id) %>%
        slice(1)
      if (nrow(mapped) == 1) {
        selected_division_id(mapped$mapped_division_id[[1]])
      } else {
        selected_division_id(polygon_id)
      }
      selected_school_id(NULL)

      div_sp <- divisions_sp[divisions_sp@data$division_id == polygon_id, ]
      if (length(div_sp) == 1) {
        bb <- sp::bbox(div_sp)
        leafletProxy("map") %>%
          fitBounds(lng1 = bb[1, 1], lat1 = bb[2, 1], lng2 = bb[1, 2], lat2 = bb[2, 2])
      }
      updateSelectizeInput(session, "entity_search", selected = character(0))
    }
  }, ignoreInit = TRUE)

  # Main map updater.
  observe({
    data <- current_map_data()
    mobile_map <- isTRUE(input$dashboard_mobile_map)
    metric_def_row <- data$metric_def_row
    schools_joined <- data$schools_joined
    div_vals <- data$div_vals
    polygon_vals <- data$polygon_vals
    threshold_enabled <- data$threshold_enabled
    threshold_scope <- data$threshold_scope
    schools_for_map <- schools_joined %>% filter(is.finite(lat), is.finite(lon))

    domain_values <- map_scale_limits(schools_for_map$value,
                                      if ("divisions" %in% input$layers) polygon_vals$value else numeric(0))

    if (has_leaflet) {
      pal <- make_palette(metric_def_row, domain_values)
      popup_text <- function(names, values, suppressed, kind) {
        formatted <- vapply(values, fmt_value, character(1),
                            format = metric_def_row$format[[1]], unit = metric_def_row$unit[[1]])
        status <- ifelse(is.finite(values), formatted,
                         ifelse(!is.na(suppressed) & suppressed, "Suppressed", "No data"))
        sprintf(
          "<div class='map-selection-popup'><span class='map-selection-kind'>%s</span><strong>%s</strong><span class='map-selection-value'>%s: %s</span></div>",
          kind,
          htmltools::htmlEscape(names),
          htmltools::htmlEscape(metric_def_row$label_short[[1]]),
          htmltools::htmlEscape(status)
        )
      }
      proxy <- leafletProxy("map")
      proxy %>%
        clearGroup("schools") %>%
        clearGroup("divisions") %>%
        clearControls()

      if ("divisions" %in% input$layers) {
        div_sp <- divisions_sp[division_polygon_draw_order, ]
        if (isTRUE(threshold_enabled) && threshold_scope %in% c("divisions", "both")) {
          div_sp <- div_sp[div_sp@data$division_id %in% polygon_vals$division_id, ]
        }
        div_df <- div_sp@data %>%
          left_join(polygon_vals, by = "division_id")
        div_sp@data <- div_df

        div_popups <- popup_text(div_sp@data$display_division_name, div_sp@data$value,
                                 div_sp@data$suppressed, "Division")

        if (nrow(div_sp@data) > 0) {
          proxy %>%
            addPolygons(
              data = div_sp,
              group = "divisions",
              layerId = ~division_id,
              fillColor = ~pal(value),
              fillOpacity = 0.76,
              color = "#415866",
              weight = 1.2,
              opacity = 0.8,
              popup = lapply(div_popups, htmltools::HTML),
              label = if (mobile_map) NULL else lapply(div_popups, htmltools::HTML),
              options = pathOptions(pane = "divisionPolygonPane", bubblingMouseEvents = FALSE),
              labelOptions = labelOptions(direction = "top", textsize = "11px", sticky = TRUE,
                                          className = "map-hover-label"),
              highlightOptions = highlightOptions(weight = 2, color = "#153746", fillOpacity = 0.9, bringToFront = FALSE)
            )
        }
      }

      if ("schools" %in% input$layers) {
        colors <- ifelse(is.na(schools_for_map$value), "#9e9e9e", pal(schools_for_map$value))
        school_popups <- popup_text(schools_for_map$school_name, schools_for_map$value,
                                    schools_for_map$suppressed, "School")

        if (nrow(schools_for_map) > 0) {
          if (mobile_map) {
            # Transparent circles make nearby school points easier to tap above division polygons.
            proxy %>% addCircleMarkers(
              data = schools_for_map,
              group = "schools",
              layerId = ~paste0("school-tap::", school_id),
              lng = ~lon,
              lat = ~lat,
              radius = 16,
              stroke = FALSE,
              fillColor = "#10283a",
              fillOpacity = 0.001,
              popup = lapply(school_popups, htmltools::HTML),
              options = pathOptions(pane = "schoolPointPane", bubblingMouseEvents = FALSE)
            )
          }
          proxy %>%
            addCircleMarkers(
              data = schools_for_map,
              group = "schools",
              layerId = ~paste0("school::", school_id),
              lng = ~lon,
              lat = ~lat,
              radius = if (mobile_map) 6.5 else 5,
              stroke = TRUE,
              weight = if (mobile_map) 1.3 else 1,
              color = "#222222",
              fillColor = colors,
              fillOpacity = 0.9,
              popup = lapply(school_popups, htmltools::HTML),
              label = if (mobile_map) NULL else lapply(school_popups, htmltools::HTML),
              options = pathOptions(pane = "schoolPointPane", bubblingMouseEvents = FALSE),
              labelOptions = labelOptions(direction = "top", textsize = "11px", sticky = FALSE,
                                          className = "map-hover-label")
            )
        }
      }

    }
  })

  if (has_leaflet) {
    # Optional attendance zone overlay (renders for the selected division; if a
    # specific school is selected, we filter to that school's zone only).
    zones_zoom_debounced <- reactive(input$map_zoom) %>% debounce(250)
    zones_render_key <- reactiveVal("off")
    zones_pending_token <- reactiveVal(0L)
    zones_pending <- reactiveVal(NULL)
    zones_debug <- isTRUE(getOption("va_zones_debug")) || Sys.getenv("VA_ZONES_DEBUG") == "1"

    observeEvent(
      list(input$zones_enabled, input$zone_level, selected_division_id(), selected_school_id(), zones_zoom_debounced()),
      {
        enabled <- isTRUE(input$zones_enabled)
        division_id <- selected_division_id()
        school_id <- selected_school_id()
        zone_level <- input$zone_level
        if (is.null(zone_level) || !zone_level %in% c("Elementary", "Middle", "High", "Unknown")) zone_level <- "Elementary"
        zoom <- suppressWarnings(as.numeric(zones_zoom_debounced()))
        if (length(zoom) != 1 || !is.finite(zoom)) zoom <- NA_real_

        proxy <- leafletProxy("map")

        # Resolve division_id from selected school if needed.
        if ((is.null(division_id) || !nzchar(division_id)) && !is.null(school_id) && nzchar(school_id)) {
          div_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
          if (nrow(div_row) == 1) division_id <- div_row$division_id[[1]]
        }

        # Decide what should be shown. We deliberately do NOT re-add zones on every zoom
        # tick/animation; only when state changes (enabled/disabled, division changes,
        # or crossing the zoom threshold).
        desired_key <- "off"
        zone_mode <- "division"
        if (enabled) {
          if (is.null(division_id) || !nzchar(division_id)) {
            desired_key <- "enabled::noselection"
          } else if (!is.finite(zoom) || zoom < 9) {
            desired_key <- paste0("enabled::zoomlow::", division_id)
          } else {
            data_dir <- file.path(app_dir, "data")
            zone_path <- pick_zone_file(data_dir, division_id, mode = "division")
            if (is.null(zone_path) || !file.exists(zone_path)) {
              desired_key <- paste0("enabled::missing::", division_id)
            } else {
              if (!is.null(school_id) && nzchar(school_id)) zone_mode <- "school"
              if (identical(zone_mode, "school")) {
                desired_key <- paste0("enabled::show_school::", division_id, "::", school_id)
              } else {
                desired_key <- paste0("enabled::show_division::", division_id, "::", zone_level)
              }
            }
          }
        }

        previous_key <- zones_render_key()
        if (identical(previous_key, desired_key)) {
          return()
        }
        same_division_level_change <-
          startsWith(previous_key, paste0("enabled::show_division::", division_id, "::")) &&
          startsWith(desired_key, paste0("enabled::show_division::", division_id, "::"))
        zones_render_key(desired_key)
        if (zones_debug) message("zones observer: desired_key=", desired_key, " zoom=", if (is.finite(zoom)) zoom else "NA")

        # Always clear when the state changes (avoids stacking layers).
        proxy %>% removeGeoJSON("zones")
        zones_pending(NULL)

        if (!enabled) return()
        if (startsWith(desired_key, "enabled::noselection")) return()

        if (startsWith(desired_key, "enabled::zoomlow::")) {
          key <- paste0("zoneszoom::", division_id)
          if (!identical(zones_warned_key(), key)) {
            zones_warned_key(key)
            showNotification("Zoom in further to display attendance zones.", type = "message", duration = 4)
          }
          return()
        }

        if (startsWith(desired_key, "enabled::missing::")) {
          key <- paste0("nozones::", division_id)
          if (!identical(zones_warned_key(), key)) {
            zones_warned_key(key)
            showNotification(
              paste0("No attendance zone boundaries found for this division (", division_id, ") in the current snapshot."),
              type = "warning",
              duration = 6
            )
          }
          return()
        }

        # enabled::show_*::<division_id>[::<school_id>]
        token <- zones_pending_token() + 1L
        zones_pending_token(token)
        zone_path <- pick_zone_file(file.path(app_dir, "data"), division_id, mode = "division")

        # Delay rendering slightly so we don't compete with fitBounds/flyTo zoom animations.
        zones_pending(list(
          token = token,
          desired_key = desired_key,
          division_id = division_id,
          zone_mode = zone_mode,
          school_id = if (identical(zone_mode, "school")) school_id else NULL,
          zone_level = zone_level,
          zone_path = zone_path,
          delay_s = if (same_division_level_change) 0 else 0.6,
          scheduled_at = Sys.time()
        ))
      },
      ignoreInit = TRUE
    )

    observe({
      pend <- zones_pending()
      if (is.null(pend)) return()

      shiny::invalidateLater(200, session)

      if (is.null(pend$scheduled_at) || as.numeric(difftime(Sys.time(), pend$scheduled_at, units = "secs")) < pend$delay_s) {
        return()
      }

      # Confirm state didn't change while waiting.
      if (!identical(zones_pending_token(), pend$token)) return()
      if (!identical(zones_render_key(), pend$desired_key)) return()
      if (!isTRUE(input$zones_enabled)) return()
      if (is.null(pend$zone_path) || !file.exists(pend$zone_path)) return()

      level_cache_path <- if (identical(pend$zone_mode, "division")) {
        pick_zone_level_cache(file.path(app_dir, "data"), pend$division_id,
                              pend$zone_level, pend$zone_path)
      } else NULL
      zones_geojson <- read_zone_geojson(if (is.null(level_cache_path)) pend$zone_path else level_cache_path)
      if (is.null(zones_geojson) || !nzchar(zones_geojson)) {
        zones_pending(NULL)
        return()
      }

      if (identical(pend$zone_mode, "school") && !is.null(pend$school_id) && nzchar(pend$school_id)) {
        filtered <- filter_zone_geojson_by_school_id(zones_geojson, pend$school_id)
        if (!is.null(filtered) && nzchar(filtered)) {
          zones_geojson <- filtered
        } else {
          key <- paste0("nozones_school::", pend$division_id, "::", pend$school_id)
          if (!identical(zones_warned_key(), key)) {
            zones_warned_key(key)
            showNotification(
              "No attendance zone boundary found for this school in the current snapshot.",
              type = "warning",
              duration = 6
            )
          }
          zones_pending(NULL)
          return()
        }
      } else {
        filtered <- if (is.null(level_cache_path)) {
          filter_zone_geojson_by_level(zones_geojson, pend$zone_level)
        } else zones_geojson
        if (is.null(filtered) || !nzchar(filtered)) {
          key <- paste0("nozones_level::", pend$division_id, "::", pend$zone_level)
          if (!identical(zones_warned_key(), key)) {
            zones_warned_key(key)
            showNotification(
              paste0("No ", if (pend$zone_level == "Unknown") "unclassified" else tolower(pend$zone_level), " attendance zones found for this division in the current snapshot."),
              type = "warning", duration = 6
            )
          }
          zones_pending(NULL)
          return()
        }
        zones_geojson <- filtered
      }

      t0 <- proc.time()[[3]]
      tryCatch({
        leafletProxy("map") %>%
          removeGeoJSON("zones") %>%
          addGeoJSON(
            zones_geojson,
            layerId = "zones",
            group = "zones",
            options = pathOptions(pane = "zonePolygonPane")
          )
      }, error = function(e) {
        if (zones_debug) message("zones observer: addGeoJSON error: ", conditionMessage(e))
        NULL
      })
      if (zones_debug) {
        dt <- proc.time()[[3]] - t0
        message("zones observer: rendered zones for division ", pend$division_id, " in ", sprintf("%.3fs", dt))
      }

      zones_pending(NULL)
    })

    observe({
      proxy <- leafletProxy("map")
      zoom <- input$map_zoom
      if (is.null(zoom)) zoom <- 7

      if ("divisions" %in% input$layers) {
        proxy %>% showGroup("divisions")
      } else {
        proxy %>% hideGroup("divisions")
      }

      if ("schools" %in% input$layers) {
        if (zoom <= division_rollup_zoom_max) {
          proxy %>% hideGroup("schools")
        } else {
          proxy %>% showGroup("schools")
        }
      } else {
        proxy %>% hideGroup("schools")
      }
    })
  }

  if (!has_leaflet) {
    output$map_plot <- renderPlot({
      data <- current_map_data()
      metric_def_row <- data$metric_def_row
      schools_joined <- data$schools_joined
      polygon_vals <- data$polygon_vals

      div_sp <- divisions_sp
      if (isTRUE(data$threshold_enabled) && data$threshold_scope %in% c("divisions", "both")) {
        div_sp <- div_sp[div_sp@data$division_id %in% polygon_vals$division_id, ]
      }
      div_sp@data <- div_sp@data %>% left_join(polygon_vals, by = "division_id")

      limits <- map_scale_limits(schools_joined$value,
                                 if ("divisions" %in% input$layers) div_sp@data$value else numeric(0))
      vmin <- limits[[1]]
      vmax <- limits[[2]]
      colors <- make_ramp(metric_def_row)

      colorize <- function(vals) {
        out <- rep("#9e9e9e", length(vals))
        ok <- is.finite(vals)
        if (!any(ok)) return(out)
        idx <- floor((vals[ok] - vmin) / (vmax - vmin) * 255) + 1
        idx <- pmax(1, pmin(256, idx))
        out[ok] <- colors[idx]
        out
      }

      div_fill <- grDevices::adjustcolor(colorize(div_sp@data$value), alpha.f = 0.76)
      plot(div_sp, col = div_fill, border = "#415866", lwd = 1)

      if ("schools" %in% input$layers) {
        pts_col <- colorize(schools_joined$value)
        points(schools_joined$lon, schools_joined$lat, pch = 21, bg = pts_col, col = "#222222", cex = 1.2)
      }

      title(main = paste0(metric_def_row$label_short[[1]], " — ", input$year))
      mtext("Click near a school point or inside a division polygon for details.", side = 3, line = 0.3, cex = 0.85)

      # Simple legend with endpoints.
      legend(
        "bottomright",
        title = legend_title(metric_def_row),
        legend = c(fmt_value(vmin, metric_def_row$format[[1]], metric_def_row$unit[[1]]), fmt_value(vmax, metric_def_row$format[[1]], metric_def_row$unit[[1]]), "No data"),
        fill = c(colors[1], colors[256], "#9e9e9e"),
        border = NA,
        bty = "n"
      )
    })
  }

  output$behavior_attendance_panel <- renderUI({
    req(input$year)
    year <- as.integer(input$year)

    school_id <- selected_school_id()
    division_id <- selected_division_id()

    if (is.null(school_id) && is.null(division_id)) {
      return(tags$div(
        class = "detail-panel",
        tags$h4("Behavior & Attendance"),
        tags$div(class = "muted", "Select a school or division to see behavior and attendance metrics.")
      ))
    }

    metric_order <- c(
      "attendance_chronic_absent_pct",
      "behavior_sbar_oss_actions_per_100",
      "behavior_sbar_iss_actions_per_100",
      "behavior_susp_per_100",
      "behavior_susp_short_per_100",
      "behavior_susp_long_per_100",
      "behavior_susp_incidents_per_100",
      "behavior_expulsions_per_100",
      "behavior_expulsion_incidents_per_100"
    )

    metric_ids <- metric_defs_by_id %>%
      filter(category %in% c("behavior", "attendance")) %>%
      pull(metric_id)
    metric_ids <- intersect(metric_order, metric_ids)
    if (length(metric_ids) == 0) metric_ids <- metric_defs_by_id %>% filter(category %in% c("behavior", "attendance")) %>% pull(metric_id)

    base_defs <- metric_defs_by_id %>%
      filter(metric_id %in% metric_ids) %>%
      select(metric_id, label_short, unit, format, better_direction)

    if (!is.null(school_id)) {
      vals <- school_metrics %>%
        filter(year == !!year, school_id == !!school_id, metric_id %in% metric_ids) %>%
        select(school_id, year, metric_id, value)

      df <- base_defs %>%
        left_join(vals, by = "metric_id") %>%
        mutate(school_id = school_id, year = year) %>%
        left_join(school_percentiles$state, by = c("school_id", "year", "metric_id")) %>%
        left_join(school_percentiles$division, by = c("school_id", "year", "metric_id"))
    } else {
      vals <- division_metrics %>%
        filter(year == !!year, division_id == !!division_id, metric_id %in% metric_ids) %>%
        select(division_id, year, metric_id, value)

      df <- base_defs %>%
        left_join(vals, by = "metric_id") %>%
        mutate(division_id = division_id, year = year) %>%
        left_join(division_percentiles_state, by = c("division_id", "year", "metric_id")) %>%
        mutate(pct_division = NA_real_)
    }

    df <- df %>%
      mutate(metric_id = factor(metric_id, levels = metric_ids)) %>%
      arrange(metric_id) %>%
      mutate(
        value_str = dplyr::if_else(
          is.finite(value),
          mapply(
            function(v, f, u) fmt_value(v, format = f, unit = u),
            value,
            as.character(format),
            as.character(unit),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
          ),
          "\u2014"
        ),
        pct_state_str = ifelse(is.finite(pct_state), sprintf("%d", as.integer(round(pct_state))), "\u2014"),
        pct_division_str = ifelse(is.finite(pct_division), sprintf("%d", as.integer(round(pct_division))), "\u2014")
      )

    tags$div(
      class = "detail-panel",
      tags$h4("Behavior & Attendance"),
      tags$table(
        class = "metric-table",
        tags$thead(tags$tr(
          tags$th("Metric"),
          tags$th(class = "num", paste0("Value (", year, ")")),
          tags$th(class = "num", "Division pctile"),
          tags$th(class = "num", "State pctile")
        )),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(df$label_short[[i]]),
              tags$td(class = "num", df$value_str[[i]]),
              tags$td(class = "num", df$pct_division_str[[i]]),
              tags$td(class = "num", df$pct_state_str[[i]])
            )
          })
        )
      ),
      tags$div(
        class = "meta-note",
        "Higher percentiles mean better performance or lower absence/behavior rates. For meals, they show position only. An em dash means data is missing, suppressed, or unavailable for that year."
      )
    )
  })

  output$demographics_panel <- renderUI({
    req(input$year)
    year <- as.integer(input$year)
    school_id <- selected_school_id()
    division_id <- selected_division_id()
    if (is.null(school_id) && is.null(division_id)) {
      return(tags$div(class = "detail-panel",
                      tags$h4("Student demographics"),
                      tags$div(class = "muted", "Select a school or division to see student demographics.")))
    }

    demo_order <- c("demo_white_pct", "demo_black_pct", "demo_hispanic_pct",
                    "demo_asian_pct", "demo_two_plus_pct", "demo_american_indian_pct",
                    "demo_native_hawaiian_pct")
    defs <- metric_defs_by_id %>%
      filter(category == "demographics") %>%
      select(metric_id, label_short, format, unit) %>%
      mutate(sort_order = match(metric_id, demo_order)) %>%
      arrange(sort_order, label_short) %>%
      select(-sort_order)
    demo_metric_ids <- as.character(defs$metric_id)

    if (!is.null(school_id)) {
      vals <- school_metrics %>%
        filter(year == !!year, school_id == !!school_id, metric_id %in% demo_metric_ids) %>%
        select(metric_id, value, suppressed)
    } else {
      vals <- division_metrics %>%
        filter(year == !!year, division_id == !!division_id, metric_id %in% demo_metric_ids) %>%
        select(metric_id, value, suppressed)
    }

    df <- defs %>% left_join(vals, by = "metric_id")
    tags$div(class = "detail-panel",
      tags$h4("Student demographics"),
      tags$table(class = "metric-table",
        tags$thead(tags$tr(tags$th("Metric"), tags$th(class = "num", paste0("Value (", year, ")")))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          value <- df$value[[i]]
          reported <- is.finite(value) && !isTRUE(df$suppressed[[i]])
          tags$tr(tags$td(df$label_short[[i]]),
                  tags$td(class = "num", if (reported) fmt_value(value, df$format[[i]], df$unit[[i]]) else "\u2014"))
        }))
      ),
      tags$p(class = "meta-note", "These are reported student shares, not a rating. A dash means data is missing, suppressed, or unavailable for this year.")
    )
  })

  output$needs_panel <- renderUI({
    req(input$year)
    year_selected <- as.integer(input$year)

    needs_ids <- metric_defs_by_id %>%
      filter(category == "needs") %>%
      pull(metric_id)

    if (length(needs_ids) == 0) {
      return(tags$div(
        class = "detail-panel",
        tags$h4("Student needs & programs"),
        tags$div(class = "muted", "No student needs/program metrics are included in the current snapshot.")
      ))
    }

    school_id <- selected_school_id()
    division_id <- selected_division_id()

    if (is.null(school_id) && is.null(division_id)) {
      return(tags$div(
        class = "detail-panel",
        tags$h4("Student needs & programs"),
        tags$div(class = "muted", "Select a school or division to see student needs/program metrics.")
      ))
    }

    # Needs/program metrics may not be available for all years. If the selected year has
    # no reportable values, fall back to the latest year with data (and make it explicit).
    avail_years <- sort(unique(school_metrics$year[school_metrics$metric_id %in% needs_ids]))
    year <- year_selected
    if (length(avail_years) > 0 && !(year_selected %in% avail_years)) {
      year <- max(avail_years, na.rm = TRUE)
    }
    year_note <- if (year != year_selected) paste0(" (showing ", year, "; no needs/program data for ", year_selected, ")") else ""

    if (!is.null(school_id)) {
      base_defs <- metric_defs_by_id %>%
        filter(metric_id %in% needs_ids) %>%
        select(metric_id, label_short, unit, format, better_direction)

      vals <- school_metrics %>%
        filter(year == !!year, school_id == !!school_id, metric_id %in% needs_ids) %>%
        select(school_id, year, metric_id, value)

      df <- base_defs %>%
        left_join(vals, by = "metric_id") %>%
        mutate(school_id = school_id, year = year) %>%
        left_join(school_percentiles$state, by = c("school_id", "year", "metric_id")) %>%
        left_join(school_percentiles$division, by = c("school_id", "year", "metric_id"))
    } else {
      base_defs <- metric_defs_by_id %>%
        filter(metric_id %in% needs_ids) %>%
        select(metric_id, label_short, unit, format, better_direction)

      vals <- division_metrics %>%
        filter(year == !!year, division_id == !!division_id, metric_id %in% needs_ids) %>%
        select(division_id, year, metric_id, value)

      df <- base_defs %>%
        left_join(vals, by = "metric_id") %>%
        mutate(division_id = division_id, year = year) %>%
        left_join(division_percentiles_state, by = c("division_id", "year", "metric_id")) %>%
        mutate(pct_division = NA_real_)
    }

    df <- df %>%
      mutate(metric_id = factor(metric_id, levels = needs_ids)) %>%
      arrange(metric_id) %>%
      mutate(
        value_str = dplyr::if_else(
          is.finite(value),
          mapply(
            function(v, f, u) fmt_value(v, format = f, unit = u),
            value,
            as.character(format),
            as.character(unit),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
          ),
          "\u2014"
        ),
        pct_state_str = ifelse(is.finite(pct_state), sprintf("%d", as.integer(round(pct_state))), "\u2014"),
        pct_division_str = ifelse(is.finite(pct_division), sprintf("%d", as.integer(round(pct_division))), "\u2014")
      )

    tags$div(
      class = "detail-panel",
      tags$h4(paste0("Student needs & programs", year_note)),
      tags$table(
        class = "metric-table",
        tags$thead(tags$tr(
          tags$th("Metric"),
          tags$th(class = "num", paste0("Value (", year, ")")),
          tags$th(class = "num", "Division pctile"),
          tags$th(class = "num", "State pctile")
        )),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(df$label_short[[i]]),
              tags$td(class = "num", df$value_str[[i]]),
              tags$td(class = "num", df$pct_division_str[[i]]),
              tags$td(class = "num", df$pct_state_str[[i]])
            )
          })
        )
      )
    )
  })
}

shinyApp(ui, server)

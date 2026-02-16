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

snapshot <- load_snapshot(file.path(app_dir, "data"))

schools <- snapshot$schools
metric_defs <- snapshot$metric_defs
school_metrics <- snapshot$school_metrics
division_metrics <- snapshot$division_metrics
school_subject_perf <- snapshot$school_subject_perf
division_subject_perf <- snapshot$division_subject_perf
divisions_sp <- snapshot$divisions_sp

available_years <- sort(unique(school_metrics$year))
default_year <- max(available_years, na.rm = TRUE)
default_metric <- "test_overall_perf_index"

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

  base %>%
    left_join(dominant, by = "polygon_division_id") %>%
    mutate(
      mapped_division_id = dplyr::coalesce(mapped_division_id, polygon_division_id),
      mapped_division_name = dplyr::coalesce(mapped_division_name, polygon_division_name),
      n_points = dplyr::coalesce(n_points, 0L)
    )
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
  leafletOutput("map", height = 600)
} else {
  tagList(
    tags$div(
      class = "alert alert-warning",
      tags$strong("Leaflet not installed."),
      " Showing a fallback plot-based map. Install the `leaflet` package for the full interactive basemap."
    ),
    plotOutput("map_plot", height = 600, click = "map_plot_click")
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      ".side-controls .form-group { margin-bottom: 10px; }
       .side-controls .control-label { margin-bottom: 3px; }
       .meta-note { font-size: 12px; color: #444; margin-top: 10px; }
       .detail-panel { margin: 10px 0 14px 0; padding: 10px 12px; border: 1px solid #e3e3e3; border-radius: 6px; background: #fbfbfb; }
       .detail-panel h4 { margin-top: 0; margin-bottom: 8px; font-size: 16px; }
       .metric-table { width: 100%; font-size: 13px; }
       .metric-table th { font-weight: 700; color: #333; padding: 6px 6px; border-bottom: 1px solid #ddd; }
       .metric-table td { padding: 6px 6px; border-bottom: 1px solid #eee; vertical-align: top; }
       .metric-table td.num { text-align: right; white-space: nowrap; font-variant-numeric: tabular-nums; }
       .metric-table td.muted { color: #666; }"
    ))
  ),
  titlePanel("Virginia Public Schools Dashboard"),
  sidebarLayout(
    sidebarPanel(
      div(
        class = "side-controls",
        selectInput(
          "year",
          "Year",
          choices = available_years,
          selected = default_year
        ),
        checkboxInput("threshold_enabled", "Enable threshold filter", value = FALSE),
        conditionalPanel(
          condition = "input.threshold_enabled == true",
          radioButtons(
            "threshold_scope",
            "Threshold applies to",
            choices = c("Schools and divisions" = "both", "Schools only" = "schools", "Divisions only" = "divisions"),
            selected = "both"
          ),
          sliderInput(
            "min_perf_threshold",
            "Minimum performance",
            min = 0,
            max = 100,
            value = 90,
            step = 0.5
          ),
          sliderInput(
            "max_susp_threshold",
            "Maximum suspensions per 100",
            min = 0,
            max = 50,
            value = 5,
            step = 0.1
          )
        ),
        selectInput(
          "category",
          "Metric category",
          choices = c("Performance" = "test", "Behavior" = "behavior", "Demographics" = "demographics"),
          selected = "test"
        ),
        uiOutput("metric_selector"),
        checkboxGroupInput(
          "layers",
          "Layers",
          choices = c("Schools (points)" = "schools", "Divisions (choropleth)" = "divisions"),
          selected = c("schools", "divisions")
        ),
        checkboxInput("zones_enabled", "Show attendance zones (when available)", value = FALSE),
        selectInput(
          "school_level",
          "School level",
          choices = c("All", sort(unique(stats::na.omit(schools$school_level)))),
          selected = "All"
        ),
        selectInput(
          "division_filter",
          "Division filter",
          choices = c("All", sort(unique(schools$division_name))),
          selected = "All"
        ),
        selectizeInput(
          "entity_search",
          "Search",
          choices = character(0),
          selected = character(0),
          options = list(
            placeholder = "Search schools or divisions...",
            # Prevent auto-selecting the first available option on load/refresh.
            onInitialize = I("function() { this.clear(true); }")
          )
        ),
        tags$div(
          class = "meta-note",
          tags$strong("Performance metrics"),
          tags$p(
            "This app supports two subject-level performance measures from VDOE School Quality Profiles:",
            tags$br(),
            tags$strong("Accreditation Combined Rate (ACR)"),
            "and",
            tags$strong("SOL Pass Rate (Percent Passing)"),
            "for English (ELA), Math, and Science (All Students)."
          ),
          tags$p(
            tags$strong("Overall"),
            "values shown in the app are app-defined composites computed as the mean across available subjects",
            "for the selected metric (ACR or Pass Rate). They are provided for convenience and are not an",
            "official VDOE accreditation label."
          ),
          tags$p(
            "Division values for subjects are aggregated as enrollment-weighted averages across schools. Division",
            "overall values are computed as the mean across the division's subject-level values."
          ),
          tags$p(
            "ACR provides equal credit for students who passed SOL tests and students who did not pass but met",
            "growth/progress benchmarks (see the VDOE glossary definition of 'Combined Rate')."
          )
        ),
        tags$div(
          class = "meta-note",
          tags$strong("Sources"),
          tags$ul(
            tags$li(tags$a("VDOE School Quality Profiles — Download Data", href = "https://schoolquality.virginia.gov/download-data", target = "_blank")),
            tags$li(tags$a("VDOE School Accreditation (Standards of Accreditation)", href = "https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/accreditation-federal-reports/soa-school-accreditation", target = "_blank")),
            tags$li(tags$a("VDOE / School Quality Profiles glossary", href = "https://schoolquality.virginia.gov/glossary", target = "_blank")),
            tags$li(tags$a("NCES EDGE Public School/LEA Geocodes", href = "https://nces.ed.gov/programs/edge/", target = "_blank")),
            tags$li(tags$a("U.S. Census TIGER/Line School District Boundaries", href = "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html", target = "_blank"))
          )
        ),
        tags$div(
          class = "meta-note",
          tags$strong("Snapshot date: "),
          if (!is.null(snapshot$meta$snapshot_date)) snapshot$meta$snapshot_date else "Unknown"
        ),
        tags$div(
          class = "meta-note",
          tags$a(
            "GitHub repo",
            href = "https://github.com/jeremiahmbrown/va_schools_dashboard",
            target = "_blank"
          )
        ),
        tags$div(
          class = "meta-note",
          tags$strong("Attendance zones: "),
          "Turn on the zones layer, then select a school or division to load that division's zones.",
          tags$br(),
          "Boundaries may be incomplete or outdated; verify with the school division."
        )
      ),
      width = 3
    ),
    mainPanel(
      width = 9,
      map_widget,
      tags$hr(),
      uiOutput("details_header"),
      uiOutput("behavior_attendance_panel"),
      fluidRow(
        column(width = 6, plotOutput("demo_plot", height = 180)),
        column(width = 6, uiOutput("needs_panel"))
      ),
      tags$hr(),
      plotOutput("trend_plot", height = 375)
    )
  )
)

server <- function(input, output, session) {
  # School points are only shown once the user zooms in past this level.
  division_rollup_zoom_max <- 7

  selected_school_id <- reactiveVal(NULL)
  selected_division_id <- reactiveVal(NULL)
  search_ready <- reactiveVal(FALSE)
  zones_warned_key <- reactiveVal(NULL)

  perf_metric_id <- reactive({
    # Default to the legacy overall ACR composite.
    metric <- if (is.null(input$perf_metric) || !nzchar(input$perf_metric)) "acr" else input$perf_metric
    view <- if (is.null(input$perf_view) || !nzchar(input$perf_view)) "overall" else input$perf_view
    subj <- if (is.null(input$perf_subject) || !nzchar(input$perf_subject)) "ela" else input$perf_subject

    if (view == "overall") {
      return(if (metric == "pass") "test_overall_pass_index" else "test_overall_perf_index")
    }

    suffix <- subj
    if (metric == "pass") paste0("test_pass_", suffix) else paste0("test_acr_", suffix)
  })

  metric_id_selected <- reactive({
    if (!is.null(input$category) && input$category == "test") {
      perf_metric_id()
    } else {
      input$metric_id
    }
  })

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
    filter(metric_id %in% c(perf_metric_ids, "behavior_susp_per_100")) %>%
    select(year, school_id, metric_id, value) %>%
    tidyr::pivot_wider(names_from = metric_id, values_from = value)

  division_threshold_base <- division_metrics %>%
    filter(metric_id %in% c(perf_metric_ids, "behavior_susp_per_100")) %>%
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
      return(tagList(
        radioButtons(
          "perf_metric",
          "Performance metric",
          choices = c("Accreditation Combined Rate (ACR)" = "acr", "SOL Pass Rate" = "pass"),
          selected = "acr"
        ),
        radioButtons(
          "perf_view",
          "View",
          choices = c("Overall (mean across subjects)" = "overall", "By subject" = "subject"),
          selected = "overall"
        ),
        conditionalPanel(
          condition = "input.perf_view == 'subject'",
          selectInput(
            "perf_subject",
            "Subject",
            choices = c("English (ELA)" = "ela", "Math" = "math", "Science" = "science"),
            selected = "ela"
          )
        )
      ))
    }

    defs <- metric_defs_by_id %>% filter(category == input$category)
    choices <- setNames(defs$metric_id, defs$label_short)
    selected <- if (default_metric %in% defs$metric_id) default_metric else defs$metric_id[[1]]
    selectInput("metric_id", "Metric", choices = choices, selected = selected)
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
          is.finite(behavior_susp_per_100),
          .data[[perf_col]] >= th$min_perf,
          behavior_susp_per_100 <= th$max_susp
        ) %>%
        pull(school_id)

      threshold_division_ids <- division_threshold_base %>%
        filter(year == year_selected) %>%
        filter(
          is.finite(.data[[perf_col]]),
          is.finite(behavior_susp_per_100),
          .data[[perf_col]] >= th$min_perf,
          behavior_susp_per_100 <= th$max_susp
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

  if (has_leaflet) {
    # Base map.
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 6)) %>%
        addMapPane(name = "divisionPolygonPane", zIndex = 380) %>%
        addMapPane(name = "zonePolygonPane", zIndex = 410) %>%
        addMapPane(name = "schoolPointPane", zIndex = 440) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -78.7, lat = 37.5, zoom = 7)
    })

    # Update selection based on map clicks.
  observeEvent(input$map_marker_click, {
    marker_id <- input$map_marker_click$id
    if (is.null(marker_id)) return()

    if (startsWith(marker_id, "school::")) {
      school_id <- sub("^school::", "", marker_id)
      selected_school_id(school_id)
      selected_division_id(NULL)

      school_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
      if (nrow(school_row) == 1 && is.finite(school_row$lat[[1]]) && is.finite(school_row$lon[[1]])) {
        # Clicking a school should not zoom *out* if the user is already zoomed in.
        # (Leaflet provides input$map_zoom for the current zoom level.)
        current_zoom <- suppressWarnings(as.numeric(input$map_zoom))
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

      div_sp <- divisions_sp[divisions_sp@data$division_id == polygon_id, ]
      if (length(div_sp) == 1) {
        bb <- sp::bbox(div_sp)
        leafletProxy("map") %>%
          fitBounds(lng1 = bb[1, 1], lat1 = bb[2, 1], lng2 = bb[1, 2], lat2 = bb[2, 2])
      }
    }
  })

  observeEvent(input$map_shape_click, {
    polygon_id <- input$map_shape_click$id
    mapped <- polygon_division_map %>%
      filter(polygon_division_id == polygon_id) %>%
      slice(1)
    division_id <- if (nrow(mapped) == 1) mapped$mapped_division_id[[1]] else polygon_id

    selected_division_id(division_id)
    selected_school_id(NULL)

    div_sp <- divisions_sp[divisions_sp@data$division_id == polygon_id, ]
    if (length(div_sp) == 1) {
      bb <- sp::bbox(div_sp)
      leafletProxy("map") %>%
        fitBounds(lng1 = bb[1, 1], lat1 = bb[2, 1], lng2 = bb[1, 2], lat2 = bb[2, 2])
    }
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
        selected_division_id(NULL)
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

    if (startsWith(val, "school::")) {
      school_id <- sub("^school::", "", val)
      selected_school_id(school_id)
      selected_division_id(NULL)

      school_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
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
    metric_def_row <- data$metric_def_row
    schools_joined <- data$schools_joined
    div_vals <- data$div_vals
    polygon_vals <- data$polygon_vals
    threshold_enabled <- data$threshold_enabled
    threshold_scope <- data$threshold_scope
    schools_for_map <- schools_joined %>% filter(is.finite(lat), is.finite(lon))

    domain_values <- c(schools_for_map$value, polygon_vals$value)

    if (has_leaflet) {
      pal <- make_palette(metric_def_row, domain_values)
      legend_values <- domain_values[is.finite(domain_values)]
      if (length(legend_values) == 0) legend_values <- c(0, 1)

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

        div_labels <- sprintf(
          "<strong>%s</strong><br/>%s: %s%s",
          div_sp@data$display_division_name,
          metric_def_row$label_short[[1]],
          vapply(div_sp@data$value, fmt_value, character(1), format = metric_def_row$format[[1]], unit = metric_def_row$unit[[1]]),
          ifelse(!is.na(div_sp@data$suppressed) & div_sp@data$suppressed, "<br/><em>Suppressed / not reported</em>", "")
        )

        if (nrow(div_sp@data) > 0) {
          proxy %>%
            addPolygons(
              data = div_sp,
              group = "divisions",
              layerId = ~division_id,
              fillColor = ~pal(value),
              fillOpacity = 0.28,
              color = "#2f2f2f",
              weight = 1.5,
              opacity = 0.95,
              # Keep polygons behind points and avoid hover text competing with school tooltips.
              label = lapply(div_labels, htmltools::HTML),
              popup = div_labels,
              options = pathOptions(pane = "divisionPolygonPane", bubblingMouseEvents = FALSE),
              highlightOptions = highlightOptions(weight = 2, color = "#222222", fillOpacity = 0.55, bringToFront = FALSE)
            )
        }
      }

      if ("schools" %in% input$layers) {
        enrollment_labels <- ifelse(
          is.na(schools_for_map$enrollment_latest),
          "No data",
          format(schools_for_map$enrollment_latest, big.mark = ",", scientific = FALSE, trim = TRUE)
        )

        popups <- sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s<br/>Enrollment (latest): %s",
          schools_for_map$school_name,
          schools_for_map$division_name,
          metric_def_row$label_short[[1]],
          vapply(schools_for_map$value, fmt_value, character(1), format = metric_def_row$format[[1]], unit = metric_def_row$unit[[1]]),
          enrollment_labels
        )

        colors <- ifelse(is.na(schools_for_map$value), "#9e9e9e", pal(schools_for_map$value))
        school_labels <- sprintf(
          "<strong>%s</strong><br/>%s<br/>%s: %s<br/>Enrollment: %s",
          schools_for_map$school_name,
          schools_for_map$division_name,
          metric_def_row$label_short[[1]],
          vapply(schools_for_map$value, fmt_value, character(1), format = metric_def_row$format[[1]], unit = metric_def_row$unit[[1]]),
          enrollment_labels
        )

        if (nrow(schools_for_map) > 0) {
          proxy %>%
            addCircleMarkers(
              data = schools_for_map,
              group = "schools",
              layerId = ~paste0("school::", school_id),
              lng = ~lon,
              lat = ~lat,
              radius = 5,
              stroke = TRUE,
              weight = 1,
              color = "#222222",
              fillColor = colors,
              fillOpacity = 0.9,
              label = lapply(school_labels, htmltools::HTML),
              popup = popups,
              options = pathOptions(pane = "schoolPointPane", bubblingMouseEvents = FALSE),
              labelOptions = labelOptions(direction = "top", textsize = "12px")
            )
        }
      }

      proxy %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = legend_values,
          title = legend_title(metric_def_row),
          opacity = 1
        )
    }
  })

  if (has_leaflet) {
    # Clear active selection when user zooms back out to statewide view.
    observeEvent(input$map_zoom, {
      zoom <- input$map_zoom
      if (is.null(zoom)) return()

      if (zoom <= division_rollup_zoom_max) {
        selected_school_id(NULL)
        selected_division_id(NULL)
      }
    }, ignoreInit = TRUE)

    # Optional attendance zone overlay (renders only when an entity is selected).
    observe({
      enabled <- isTRUE(input$zones_enabled)
      division_id <- selected_division_id()
      school_id <- selected_school_id()
      proxy <- leafletProxy("map")

      proxy %>% clearGroup("zones")

      if (!enabled) {
        return()
      }

      if ((is.null(division_id) || !nzchar(division_id)) && !is.null(school_id) && nzchar(school_id)) {
        div_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
        if (nrow(div_row) == 1) division_id <- div_row$division_id[[1]]
      }

      if (is.null(division_id) || !nzchar(division_id)) return()

      data_dir <- file.path(app_dir, "data")
      # Prefer division sources when present; otherwise fall back to SABS if present.
      zone_path <- pick_zone_file(data_dir, division_id, mode = "division")
      if (is.null(zone_path) || !file.exists(zone_path)) {
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

      zones_sp <- zones_json_to_sp(zone_path)
      if (is.null(zones_sp) || length(zones_sp) == 0) return()

      level_cols <- c(
        ES = "#1F77B4",
        MS = "#F58518",
        HS = "#54A24B",
        UNK = "#777777"
      )
      level_key <- function(x) {
        x <- toupper(trimws(as.character(x)))
        if (x %in% c("E", "ES", "ELEMENTARY")) return("ES")
        if (x %in% c("M", "MS", "MIDDLE")) return("MS")
        if (x %in% c("H", "HS", "HIGH")) return("HS")
        "UNK"
      }
      zones_sp@data$level_key <- vapply(zones_sp@data$level, level_key, character(1))
      zones_sp@data$stroke_col <- unname(level_cols[zones_sp@data$level_key])
      zones_sp@data$fill_col <- zones_sp@data$stroke_col

      zone_labels <- sprintf(
        "<strong>%s</strong><br/>Level: %s<br/>Source: %s",
        ifelse(is.na(zones_sp@data$school_name) | zones_sp@data$school_name == "", "Attendance zone", zones_sp@data$school_name),
        zones_sp@data$level_key,
        ifelse(is.na(zones_sp@data$source) | zones_sp@data$source == "", "unknown", zones_sp@data$source)
      )

      proxy %>%
        addPolygons(
          data = zones_sp,
          group = "zones",
          layerId = ~paste0("zone::", zone_id),
          color = ~stroke_col,
          weight = 1.0,
          opacity = 0.9,
          fillColor = ~fill_col,
          fillOpacity = 0.12,
          label = lapply(zone_labels, htmltools::HTML),
          popup = zone_labels,
          options = pathOptions(pane = "zonePolygonPane", bubblingMouseEvents = FALSE),
          highlightOptions = highlightOptions(weight = 2.0, color = "#111111", fillOpacity = 0.18, bringToFront = FALSE)
        )
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

      domain_values <- c(schools_joined$value, div_sp@data$value)
      finite <- domain_values[is.finite(domain_values)]
      if (length(finite) == 0) finite <- c(0, 1)
      vmin <- min(finite)
      vmax <- max(finite)
      if (!is.finite(vmin) || !is.finite(vmax) || vmin == vmax) {
        vmin <- 0
        vmax <- 1
      }

      colors <- switch(
        metric_def_row$palette[[1]],
        viridis = grDevices::colorRampPalette(c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725"))(256),
        magma = grDevices::colorRampPalette(c("#000004", "#3b0f70", "#8c2981", "#de4968", "#fe9f6d", "#fcfdbf"))(256),
        grDevices::colorRampPalette(c("#f7fbff", "#08306b"))(256)
      )

      colorize <- function(vals) {
        out <- rep("#9e9e9e", length(vals))
        ok <- is.finite(vals)
        if (!any(ok)) return(out)
        idx <- floor((vals[ok] - vmin) / (vmax - vmin) * 255) + 1
        idx <- pmax(1, pmin(256, idx))
        out[ok] <- colors[idx]
        out
      }

      div_fill <- grDevices::adjustcolor(colorize(div_sp@data$value), alpha.f = 0.35)
      plot(div_sp, col = div_fill, border = "#444444", lwd = 1)

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

  output$details_header <- renderUI({
    school_id <- selected_school_id()
    division_id <- selected_division_id()
    year <- input$year

    if (!is.null(school_id)) {
      school_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
      return(tags$h4(paste0("School: ", school_row$school_name[[1]], " — ", year)))
    }

    if (!is.null(division_id)) {
      div_name <- division_name_lookup %>% filter(division_id == !!division_id) %>% slice(1) %>% pull(division_name)
      if (length(div_name) == 0) {
        div_name <- divisions_sp@data %>% filter(division_id == !!division_id) %>% slice(1) %>% pull(division_name)
      }
      if (length(div_name) == 0) div_name <- division_id
      return(tags$h4(paste0("Division: ", div_name[[1]], " — ", year)))
    }

    tags$h4(paste0("Click a school point or division polygon for details — ", year))
  })

  output$trend_plot <- renderPlot({
    req(input$year)
    school_id <- selected_school_id()
    division_id <- selected_division_id()

    if (is.null(school_id) && is.null(division_id)) {
      plot.new()
      text(0.5, 0.5, "Select a school or division to see a trend.", cex = 1.1)
      return()
    }

    metric_key <- if (!is.null(input$perf_metric) && input$perf_metric == "pass") "pass" else "acr"
    metric_label <- if (metric_key == "pass") "SOL Pass Rate" else "Accreditation Combined Rate (ACR)"

    # Stable ordering (and legend order) across years.
    series_levels <- c("English (ELA)", "Math", "Science", "Overall")
    series_cols <- c(
      "English (ELA)" = "#1F77B4",
      "Math" = "#F58518",
      "Science" = "#54A24B",
      "Overall" = "#111111"
    )

    if (!is.null(school_id)) {
      if (is.null(school_subject_perf)) {
        plot.new()
        text(0.5, 0.5, "Subject-level performance data is not available.", cex = 1.1)
        return()
      }

      df_subj <- school_subject_perf %>%
        filter(school_id == !!school_id, subgroup == "All Students") %>%
        select(year, subject, acr_rate, pass_rate)

      school_row <- schools %>% filter(school_id == !!school_id) %>% slice(1)
      obj_label <- paste0("School: ", school_row$school_name[[1]])
    } else {
      if (is.null(division_subject_perf)) {
        plot.new()
        text(0.5, 0.5, "Subject-level performance data is not available.", cex = 1.1)
        return()
      }

      df_subj <- division_subject_perf %>%
        filter(division_id == !!division_id, subgroup == "All Students") %>%
        select(year, subject, acr_rate, pass_rate)

      div_name <- division_name_lookup %>% filter(division_id == !!division_id) %>% slice(1) %>% pull(division_name)
      if (length(div_name) == 0) {
        div_name <- divisions_sp@data %>% filter(division_id == !!division_id) %>% slice(1) %>% pull(division_name)
      }
      if (length(div_name) == 0) div_name <- division_id
      obj_label <- paste0("Division: ", div_name[[1]])
    }

    df_subj <- df_subj %>%
      filter(subject %in% series_levels[series_levels != "Overall"]) %>%
      mutate(value = if (metric_key == "pass") pass_rate else acr_rate) %>%
      transmute(year = as.integer(year), series_name = subject, value = value)

    overall <- df_subj %>%
      group_by(year) %>%
      summarise(
        series_name = "Overall",
        value = if (all(!is.finite(value))) NA_real_ else mean(value, na.rm = TRUE),
        .groups = "drop"
      )

    df_long <- bind_rows(df_subj, overall) %>%
      mutate(series_name = factor(series_name, levels = series_levels)) %>%
      arrange(series_name, year)

    if (nrow(df_long) == 0 || all(!is.finite(df_long$value))) {
      plot.new()
      text(0.5, 0.5, "No time-series data available for this metric.", cex = 1.1)
      return()
    }

    df_overall <- df_long %>%
      filter(series_name == "Overall", is.finite(value)) %>%
      mutate(
        label = sprintf("%.0f", value),
        label_y = pmin(100, value + 3)
      )

    ggplot() +
      geom_line(
        data = df_long %>% filter(series_name != "Overall"),
        aes(x = year, y = value, color = series_name, group = series_name),
        linewidth = 0.9,
        alpha = 0.9,
        na.rm = TRUE
      ) +
      geom_point(
        data = df_long %>% filter(series_name != "Overall"),
        aes(x = year, y = value, color = series_name),
        size = 1.9,
        alpha = 0.9,
        na.rm = TRUE
      ) +
      geom_line(
        data = df_long %>% filter(series_name == "Overall"),
        aes(x = year, y = value, color = series_name, group = series_name),
        linewidth = 0.9,
        alpha = 1,
        linetype = "dashed",
        na.rm = TRUE
      ) +
      geom_point(
        data = df_long %>% filter(series_name == "Overall"),
        aes(x = year, y = value, color = series_name),
        size = 2.3,
        alpha = 1,
        na.rm = TRUE
      ) +
      geom_label(
        data = df_overall,
        aes(x = year, y = label_y, label = label),
        inherit.aes = FALSE,
        color = "#111111",
        fill = grDevices::adjustcolor("white", alpha.f = 0.75),
        label.size = 0,
        size = 3.1,
        fontface = "bold",
        na.rm = TRUE
      ) +
      scale_color_manual(values = series_cols, drop = FALSE) +
      scale_x_continuous(breaks = available_years) +
      scale_y_continuous(
        limits = c(0, 100),
        expand = expansion(mult = c(0.05, 0.12)),
        breaks = c(0, 50, 100),
        labels = c("0", "50", "100")
      ) +
      labs(
        x = NULL,
        y = NULL,
        title = paste0(metric_label, " Trend"),
        subtitle = obj_label,
        color = NULL
      ) +
      theme_classic(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 9, color = "#333333"),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = "#333333", linewidth = 0.6),
        axis.ticks.x = element_line(color = "#333333", linewidth = 0.6),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "#444444", margin = margin(b = 4)),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 9),
        legend.key.width = grid::unit(1.3, "lines"),
        plot.margin = margin(t = 6, r = 8, b = 6, l = 6)
      ) +
      guides(color = guide_legend(order = 1, nrow = 1, byrow = TRUE)) +
      coord_cartesian(clip = "off")
  })

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
        "Percentiles are empirical ranks among peers for the same year. For metrics where lower values are better, percentiles are inverted so higher = better."
      )
    )
  })

  output$demo_plot <- renderPlot({
    req(input$year)
    year <- as.integer(input$year)

    demo_metric_ids <- metric_defs_by_id %>%
      filter(category == "demographics") %>%
      pull(metric_id)

    school_id <- selected_school_id()
    division_id <- selected_division_id()

    df <- NULL
    if (!is.null(school_id)) {
      df <- school_metrics %>%
        filter(year == !!year, school_id == !!school_id, metric_id %in% demo_metric_ids) %>%
        left_join(metric_defs_by_id %>% select(metric_id, label_short), by = "metric_id") %>%
        transmute(label = label_short, pct = value)
    } else if (!is.null(division_id)) {
      df <- division_metrics %>%
        filter(year == !!year, division_id == !!division_id, metric_id %in% demo_metric_ids) %>%
        left_join(metric_defs_by_id %>% select(metric_id, label_short), by = "metric_id") %>%
        transmute(label = label_short, pct = value)
    } else {
      df <- data.frame(label = character(0), pct = numeric(0))
    }

    make_race_stacked_bar(df, title = "Student composition", top_n = 7)
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

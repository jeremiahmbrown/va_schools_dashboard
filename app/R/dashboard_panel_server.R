# Register detail-panel outputs and observers for one Shiny session.
dashboard_panel_server <- function(
  input, output, session, selected_school_id, selected_division_id,
  metric_id_selected, schools, division_name_lookup, divisions_sp,
  metric_defs_by_id, school_metrics, division_metrics
) {

entity_context <- reactive({
  school_id <- selected_school_id()
  division_id <- selected_division_id()

  if (!is.null(school_id)) {
    row <- schools[schools$school_id == school_id, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    level <- if (is.na(row$school_level[[1]])) "School" else paste(row$school_level[[1]], "school")
    return(list(
      kind = "school", id = school_id, name = row$school_name[[1]],
      subtitle = paste(level, row$division_name[[1]], sep = " · ")
    ))
  }

  if (is.null(division_id)) return(NULL)
  row <- division_name_lookup[division_name_lookup$division_id == division_id, , drop = FALSE]
  if (nrow(row) == 0) {
    row <- divisions_sp@data[divisions_sp@data$division_id == division_id, , drop = FALSE]
  }
  name <- if (nrow(row) == 0) division_id else row$division_name[[1]]
  list(kind = "division", id = division_id, name = name, subtitle = "School division")
})

metric_def_selected <- reactive({
  metric_id <- metric_id_selected()
  req(metric_id)
  row <- metric_defs_by_id[metric_defs_by_id$metric_id == metric_id, , drop = FALSE]
  req(nrow(row) > 0)
  row[1, , drop = FALSE]
})

metric_value_for <- function(entity, metric_id, year) {
  rows <- if (entity$kind == "school") school_metrics else division_metrics
  id_column <- if (entity$kind == "school") "school_id" else "division_id"
  match <- rows[rows[[id_column]] == entity$id & rows$metric_id == metric_id &
                  rows$year == year, , drop = FALSE]
  if (nrow(match) == 0) return(list(value = NA_real_, status = "Not available"))
  if (isTRUE(match$suppressed[[1]])) return(list(value = NA_real_, status = "Suppressed"))
  value <- match$value[[1]]
  if (!is.finite(value)) return(list(value = NA_real_, status = "Not available"))
  list(value = value, status = "Reportable")
}

display_metric_value <- function(metric_id, value) {
  if (!is.finite(value)) return("—")
  def <- metric_defs_by_id[metric_defs_by_id$metric_id == metric_id, , drop = FALSE]
  if (nrow(def) == 0) return(as.character(value))
  fmt_value(value, format = def$format[[1]], unit = def$unit[[1]])
}

output$map_caption <- renderUI({
  def <- metric_def_selected()
  year <- suppressWarnings(as.integer(input$year))
  entity <- entity_context()
  caption <- if (length(year) == 1 && is.finite(year)) {
    paste0("Data year ", year, " · ",
           if (is.null(entity)) "select a map feature" else entity$name)
  } else {
    "No reportable year"
  }
  tags$div(
    tags$strong(def$label_short[[1]]),
    tags$span(caption)
  )
})

output$details_header <- renderUI({
  entity <- entity_context()
  if (is.null(entity)) {
    return(tagList(
      tags$span(class = "eyebrow", "Explore Virginia"),
      tags$h2("Choose a school or division"),
      tags$span(class = "entity-subtitle", "Select a map feature or use search.")
    ))
  }
  tagList(
    tags$span(class = "eyebrow", if (entity$kind == "school") "School profile" else "Division profile"),
    tags$h2(entity$name),
    tags$span(class = "entity-subtitle", entity$subtitle)
  )
})

output$clear_selection_control <- renderUI({
  if (is.null(entity_context())) return(NULL)
  actionButton("clear_selection", "Clear", class = "clear-selection")
})

observeEvent(input$clear_selection, {
  session$sendCustomMessage("dashboard-close-popup", list())
  selected_school_id(NULL)
  selected_division_id(NULL)
  updateSelectizeInput(session, "entity_search", selected = character(0))
}, ignoreInit = TRUE)

observeEvent(list(selected_school_id(), selected_division_id()), {
  session$sendCustomMessage("dashboard-selection-changed", list(
    selected = !is.null(selected_school_id()) || !is.null(selected_division_id())
  ))
}, ignoreInit = TRUE)

selected_metric_value <- reactive({
  entity <- entity_context()
  if (is.null(entity)) return(NULL)
  year <- suppressWarnings(as.integer(input$year))
  if (length(year) != 1 || !is.finite(year)) return(NULL)
  metric_value_for(entity, metric_id_selected(), year)
})

output$selection_summary <- renderUI({
  entity <- entity_context()
  if (is.null(entity)) {
    return(tags$p(class = "intro-copy",
                  "Select a school or division to see its value and trend while the map stays in view."))
  }
  def <- metric_def_selected()
  result <- selected_metric_value()
  req(result)
  if (entity$kind == "school") {
    enrollment <- schools$enrollment_latest[schools$school_id == entity$id]
    enrollment <- if (length(enrollment) == 1 && is.finite(enrollment)) enrollment else NA_integer_
    enrollment_label <- "Total enrollment"
    enrollment_caption <- "Latest reported count"
  } else {
    division_schools <- schools[schools$division_id == entity$id &
                                  is.finite(schools$enrollment_latest), , drop = FALSE]
    enrollment <- if (nrow(division_schools)) sum(division_schools$enrollment_latest) else NA_integer_
    enrollment_label <- "Mapped-school enrollment"
    enrollment_caption <- "Sum of latest school counts"
  }
  enrollment_display <- if (is.finite(enrollment)) format(enrollment, big.mark = ",", trim = TRUE) else "—"
  tags$div(class = "metric-hero",
    tags$div(class = "metric-hero-grid",
      tags$div(class = "metric-hero-item",
        tags$span(class = "metric-label", enrollment_label),
        tags$strong(class = "metric-value", enrollment_display),
        tags$span(class = "metric-context", enrollment_caption)
      ),
      tags$div(class = "metric-hero-item metric-hero-selected",
        tags$span(class = "metric-label", def$label_short[[1]]),
        tags$strong(class = "metric-value", display_metric_value(def$metric_id[[1]], result$value)),
        tags$span(class = "metric-state", paste0(input$year, " · ", result$status))
      )
    ),
    tags$p(class = "metric-hero-note", "Enrollment gives context. A rate may use a different denominator or year.")
  )
})

output$welcome_overview <- renderUI({
  if (!is.null(entity_context())) return(NULL)
  school_count <- sum(is.finite(schools$lat) & is.finite(schools$lon))
  division_count <- length(unique(schools$division_id))
  years <- range(school_metrics$year, na.rm = TRUE)
  tags$div(class = "welcome-overview",
    tags$h3("In this snapshot"),
    tags$div(class = "welcome-grid",
      tags$div(class = "welcome-stat", tags$strong(format(school_count, big.mark = ",")), tags$span("Mapped schools")),
      tags$div(class = "welcome-stat", tags$strong(format(division_count, big.mark = ",")), tags$span("School divisions")),
      tags$div(class = "welcome-stat welcome-years", tags$strong(paste(years, collapse = "–")), tags$span("Data years; availability varies by measure"))
    ),
    tags$p("Use the search box for a known school. Select a division boundary to explore an area.")
  )
})

output$peek_value <- renderUI({
  entity <- entity_context()
  if (is.null(entity)) return(tags$span("Select a place to see its data"))
  result <- selected_metric_value()
  def <- metric_def_selected()
  req(result)
  tags$span(class = "peek-value",
            paste(display_metric_value(def$metric_id[[1]], result$value),
                  def$label_short[[1]], sep = " · "))
})

output$quick_metrics <- renderUI({
  entity <- entity_context()
  if (is.null(entity)) return(NULL)
  year <- suppressWarnings(as.integer(input$year))
  if (length(year) != 1 || !is.finite(year)) return(NULL)
  metric_ids <- c("attendance_chronic_absent_pct", "needs_meal_eligibility_pct",
                  "behavior_susp_per_100")
  metric_ids <- metric_ids[metric_ids %in% metric_defs_by_id$metric_id]
  if (length(metric_ids) == 0) return(NULL)

  cards <- lapply(metric_ids, function(metric_id) {
    def <- metric_defs_by_id[metric_defs_by_id$metric_id == metric_id, , drop = FALSE]
    result <- metric_value_for(entity, metric_id, year)
    tags$div(class = "quick-card",
      tags$span(def$label_short[[1]]),
      tags$strong(display_metric_value(metric_id, result$value))
    )
  })
  tags$div(tags$h3(class = "quick-heading", "At a glance"),
           tags$div(class = "quick-grid", cards))
})

selected_trend_data <- reactive({
  entity <- entity_context()
  if (is.null(entity)) return(NULL)
  rows <- if (entity$kind == "school") school_metrics else division_metrics
  id_column <- if (entity$kind == "school") "school_id" else "division_id"
  rows[rows[[id_column]] == entity$id & rows$metric_id == metric_id_selected(),
       c("year", "value", "suppressed"), drop = FALSE]
})

output$profile_trend <- renderUI({
  if (is.null(entity_context())) return(NULL)
  tags$div(class = "trend-preview",
    tags$h3("Trend over available years"),
    plotOutput("trend_preview", height = 190)
  )
})

output$trend_preview <- renderPlot({
  entity <- entity_context()
  req(entity)
  def <- metric_def_selected()
  plot_metric_trend(selected_trend_data(), def$label_short[[1]], entity$name,
                    compact = TRUE, unit = def$unit[[1]], value_format = def$format[[1]])
})
}

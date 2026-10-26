data_year_choices <- function(years) {
  year_labels <- paste0(years, " · ", years - 1L, "–", substr(years, 3, 4))
  stats::setNames(years, year_labels)
}

dashboard_ui <- function(map_widget, available_years, default_year, schools, snapshot, app_dir) {
  year_choices <- data_year_choices(available_years)
  asset_url <- function(filename) {
    hash <- unname(tools::md5sum(file.path(app_dir, "www", filename)))
    paste0("dashboard-assets/", filename, "?v=", hash)
  }

  fluidPage(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "stylesheet", type = "text/css", href = asset_url("dashboard.css")),
      tags$script(src = asset_url("dashboard.js"))
    ),
    tags$div(
      class = "dashboard-shell",
      tags$header(
        class = "dashboard-header",
        tags$div(
          class = "brand",
          tags$span(class = "brand-mark", "VA", `aria-hidden` = "true"),
          tags$div(
            tags$h1("Virginia Schools Explorer"),
            tags$span(class = "brand-subtitle", "Public data dashboard")
          )
        ),
        tags$nav(
          class = "header-links", `aria-label` = "Dashboard links",
          tags$span(paste0("Snapshot · ", if (!is.null(snapshot$meta$snapshot_date)) snapshot$meta$snapshot_date else "date unknown")),
          tags$a(tags$span(class = "desktop-link-label", "Methods & sources"),
                 tags$span(class = "mobile-link-label", "Sources"),
                 href = "https://github.com/jeremiahmbrown/va_schools_dashboard/blob/master/docs/METHODOLOGY.md",
                 target = "_blank", rel = "noopener noreferrer")
        )
      ),
      tags$div(
        class = "toolbar", role = "search", `aria-label` = "Explore schools",
        tags$div(
          class = "toolbar-field search-field",
          selectizeInput(
            "entity_search", "Find a place", choices = character(0),
            selected = character(0),
            options = list(
              placeholder = "Search school, division, or boundary",
              onInitialize = I("function() { this.clear(true); }")
            )
          )
        ),
        tags$div(
          class = "toolbar-field category-field",
          selectInput(
            "category", "Category",
            choices = c("Performance" = "test", "Behavior" = "behavior", "Attendance" = "attendance", "School meals" = "needs", "Demographics" = "demographics"),
            selected = "test", selectize = FALSE
          )
        ),
        tags$div(class = "toolbar-field measure-field", uiOutput("metric_selector")),
        tags$div(class = "toolbar-field year-field", selectInput("year", "Data year", choices = year_choices, selected = default_year, selectize = FALSE)),
        tags$details(
          class = "filter-menu",
          tags$summary("Filters", `aria-label` = "Open filters and map layers"),
          tags$div(
            class = "filter-content",
            tags$div(
              class = "filter-content-header",
              tags$h2("Filters"),
              tags$button(type = "button", class = "filter-close", id = "filter-close",
                          `aria-label` = "Close filters and return to map", "Close filters")
            ),
            tags$h2("Map layers"),
            checkboxGroupInput(
              "layers", "Show", choices = c("Schools (points)" = "schools", "Divisions (choropleth)" = "divisions"),
              selected = c("schools", "divisions")
            ),
            checkboxInput("zones_enabled", "Show attendance zones", value = FALSE),
            conditionalPanel(
              condition = "input.zones_enabled == true",
              selectInput("zone_level", "Zone level", choices = c("Elementary", "Middle", "High", "Unclassified (older source)" = "Unknown"), selected = "Elementary", selectize = FALSE),
              tags$p(class = "help-note", "Select a division for all its zones, or a school for one zone. Confirm an address with the division."),
              uiOutput("zone_selection_action"),
              uiOutput("zone_source_note")
            ),
            tags$hr(),
            conditionalPanel(
              condition = "input.category == 'test'",
              tags$h2("Performance view"),
              radioButtons("perf_view", "Show", choices = c("Overall mean" = "overall", "By subject" = "subject"), selected = "overall", inline = TRUE),
              conditionalPanel(
                condition = "input.perf_view == 'subject'",
                selectInput("perf_subject", "Subject", choices = c("English (ELA)" = "ela", "Math" = "math", "Science" = "science"), selected = "ela", selectize = FALSE)
              )
            ),
            tags$hr(),
            tags$h2("School and division filters"),
            selectInput("school_level", "School level (points)", choices = c("All", sort(unique(stats::na.omit(schools$school_level)))), selected = "All", selectize = FALSE),
            selectInput("division_filter", "Division filter (school points)", choices = c("All", sort(unique(schools$division_name))), selected = "All", selectize = FALSE),
            checkboxInput("threshold_enabled", "Enable threshold filter", value = FALSE),
            conditionalPanel(
              condition = "input.threshold_enabled == true",
              radioButtons("threshold_scope", "Apply to", choices = c("Schools and divisions" = "both", "Schools only" = "schools", "Divisions only" = "divisions"), selected = "both"),
              sliderInput("min_perf_threshold", "Minimum performance", min = 0, max = 100, value = 90, step = 0.5),
              sliderInput("max_susp_threshold", "Maximum out-of-school actions per 100 (SBAR)", min = 0, max = 50, value = 5, step = 0.1)
            ),
            tags$hr(),
            tags$details(
              class = "filter-help",
              tags$summary("About these measures"),
              tags$p("Annual SOL pass rates (2022–2026) are separate from historical Accreditation Combined Rate and School Quality Profiles Percent Passing (through 2024)."),
              tags$p("Overall values are app-defined means across available subjects. They are not official VDOE ratings. Science ACR is blank in the current export, so the ACR mean uses English and Math."),
              tags$p("A dash means a value is missing, suppressed, or unavailable for the selected year. Attendance-zone dates and coverage vary; confirm current address rules with the division.")
            )
          )
        )
      ),
      tags$div(class = "availability-bar", uiOutput("metric_availability")),
      tags$main(
        class = "dashboard-workspace",
        tags$section(
          class = "map-stage", `aria-label` = "Virginia school map",
          map_widget,
          tags$div(class = "map-caption", uiOutput("map_caption")),
          tags$div(class = "map-legend", uiOutput("map_legend"))
        ),
        tags$aside(
          class = "detail-stage", id = "detail-stage", `aria-label` = "Selected school or division details",
          tags$div(class = "sheet-handle", `aria-hidden` = "true"),
          tags$div(
            class = "detail-heading",
            uiOutput("details_header"),
            uiOutput("clear_selection_control")
          ),
          tags$div(class = "mobile-peek", uiOutput("peek_value")),
          tags$button(type = "button", class = "sheet-toggle", id = "sheet-toggle", `aria-expanded` = "false", `aria-controls` = "detail-content", "View details"),
          tags$div(
            id = "detail-content", class = "detail-content",
            tabsetPanel(
              id = "detail_tab", type = "tabs",
              tabPanel("Profile", value = "profile", uiOutput("selection_summary"), uiOutput("welcome_overview"), uiOutput("quick_metrics"), uiOutput("profile_trend")),
              tabPanel(
                "Detailed measures", value = "measures",
                uiOutput("behavior_attendance_panel"),
                uiOutput("needs_panel"),
                uiOutput("demographics_panel")
              )
            ),
            tags$p(class = "detail-footnote", "Values use the selected data year unless a panel states another year. Missing and suppressed values are not zero.")
          )
        )
      )
    )
  )
}

# Dashboard state map

This page lists the dashboard's interactive state flows. Use it when changing
an input, a map event, or a detail panel. Most state is held by Shiny inputs;
the phone controls and Leaflet also keep browser state.

| Flow | States | Events and guards | Owner |
| --- | --- | --- | --- |
| Place selection | None, division, school | Map click, search, Clear, or Show all zones changes the selected IDs. A school also sets its division. | `app/app.R`, `app/R/dashboard_panel_server.R` |
| Search setup | Initializing, ready | The server ignores search changes until the first Shiny flush. Search selection then resets the search box. | `app/app.R` |
| Measure choice | Performance family with overall or subject, or a metric from another category | Category changes the measure control. Performance family changes available subjects. Measure changes available years. | `app/app.R`, `app/R/dashboard_ui.R` |
| Data year | Available, temporarily unavailable during an input update, or no reportable year | A measure change retains the year when possible; otherwise it selects the latest available year. | `app/app.R` |
| Reported value | Reportable, suppressed, or unavailable | Profile cards and map popups show a number only for a finite, reportable value. Trend lines break at years without a value. | `app/R/dashboard_panel_server.R`, `app/R/dashboard_details.R` |
| Needs panel year | Selected year or latest year with needs data | When this measure group lacks the selected year, the panel labels the year it uses. | `app/app.R` |
| Map points | Hidden, visible | School layer must be on and zoom must exceed 7. Division layer has its own switch. Mobile layout adds larger school tap targets. | `app/app.R`, `app/www/dashboard.js` |
| Map engine | Leaflet or static plot | The app picks Leaflet when its package is installed; otherwise it uses a plot with click selection. | `app/app.R` |
| Threshold filter | Off or on for schools, divisions, or both | The checkbox enables it. Sliders are debounced before a map redraw. | `app/app.R` |
| Attendance zones | Off, no selection, zoom too low, missing division, loading, shown, missing school, missing level, error | Zone switch, place, level, or zoom can change the target. A delayed draw uses a token and key to discard stale work. The filter panel shows the current state. | `app/R/dashboard_state.R`, `app/app.R` |
| Phone details | Closed or open | The button toggles it. A place selection closes it so the map remains visible. | `app/www/dashboard.js` |
| Filter menu | Closed or open | The summary toggles it. Close button, outside click, and Escape close it. | `app/www/dashboard.js` |
| Filter help and metric definition | Closed or open | Native details controls show or hide explanation text. | `app/R/dashboard_ui.R`, `app/app.R` |
| Map popup hover | Hover allowed or blocked | Opening a popup closes tooltips and blocks new ones until a pointer move. Search and Clear close the popup. | `app/app.R` Leaflet script |
| Detail tab | Profile or Detailed measures | Shiny tab control switches the visible panel. | `app/R/dashboard_ui.R` |

## Zone overlay contract

`zone_overlay_target()` returns a state and a content key without drawing on
the map. The key changes only when the drawn content must change. The map
observer clears the prior boundary, waits for map movement when needed, and
draws the target. The pending token and content key reject a delayed draw
after a new selection, level, or switch event. A missing school or level keeps
the overlay empty and gives a lasting status message in Filters.

Check state decisions with `Rscript --vanilla tests/check_dashboard_state.R`.

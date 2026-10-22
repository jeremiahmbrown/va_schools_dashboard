# Virginia Schools Explorer

An interactive R Shiny and Leaflet dashboard for exploring public data about Virginia schools and school divisions. It brings school locations, division boundaries, performance, attendance, school meals, discipline, and student demographics into one map and profile.

**[Open the live dashboard](https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/)**

## Explore the data

- Search for a school or division, or select it on the map. Its profile stays beside the map on a desktop screen. On a phone, open the profile from the panel at the bottom of the map.
- Change the measure and data year to see where values are available. View a trend for the selected place and measure.
- Use filters to show school points, division boundaries, and available attendance zones.

## Data and interpretation

The app uses a checked snapshot of public data saved in [`app/data/`](app/data/). It does not download live records when a visitor opens the dashboard.

- **Years:** The control shows the ending year of a school year. For example, 2026 means 2025–26.
- **Performance:** Annual Standards of Learning (SOL) pass rates for 2022–2026 are separate from the historical Accreditation Combined Rate (ACR) and School Quality Profiles Percent Passing series. “Overall” values are means calculated by this app across available subjects. They are **not official Virginia Department of Education ratings**.
- **Missing values:** A dash means a value is missing, suppressed, or unavailable for that year. It does not mean zero.
- **Demographics:** Student race and ethnicity shares use Virginia Fall Membership counts for 2022–2026. Source checks leave some school values missing.
- **Attendance zones:** Coverage is partial, and boundaries come from different years. Confirm a current school assignment with the division before making an address decision.

For definitions, coverage, and known limits, see the [methodology](docs/METHODOLOGY.md), [performance coverage](docs/PERFORMANCE_COVERAGE_2026-10-20.md), and [zone coverage audit](docs/ZONES_COVERAGE.md).

Primary sources: [Virginia Department of Education School Quality Profiles](https://schoolquality.virginia.gov/download-data), [VDOE annual SOL results](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results), [NCES EDGE](https://nces.ed.gov/programs/edge/), [U.S. Census TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html), and [NCES School Attendance Boundary Survey](https://nces.ed.gov/surveys/sabs/).

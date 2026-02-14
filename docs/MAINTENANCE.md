# Maintenance (Deploy + Snapshot Updates)

This document is intended for maintainers/agents working on the dashboard.

## Deploy (shinyapps.io)
Deploy from the repo root so `renv.lock` is included:

```r
setwd("/path/to/va_schools_dashboard")
rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "app/app.R",
  account = "jeremiahmbrown",
  appName = "va_schools_dashboard"
)
```

Notes:
- `.rscignore` excludes `.Rprofile` and `renv/` from the deploy bundle so renv does not attempt to activate at runtime on shinyapps.io.
- `renv.lock` is included so rsconnect can determine package dependencies at deploy time.

## Updating The Frozen Snapshot
This repo ships with a frozen snapshot under `app/data/`. To update:

1. Run the snapshot build script (downloads source files + regenerates `app/data/*`):
```bash
Rscript data-raw/build_snapshot.R
```

2. Validate locally:
```bash
R -e "shiny::runApp('app', port = 3838, launch.browser = FALSE)"
```

3. Commit the updated `app/data/*`, push to GitHub, and redeploy to shinyapps.io.


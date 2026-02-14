# Setup (Ubuntu + Fresh R Install)

## R Packages
This project expects CRAN package installs for:
- `shiny`, `leaflet`, `dplyr`, `tidyr`, `ggplot2`, `jsonlite`, `sp`, `rsconnect`, `renv`

Bootstrap command:
```bash
cd va_schools_dashboard
Rscript scripts/bootstrap.R
```

If you see errors like `Could not resolve host: cloud.r-project.org`, fix DNS/network first and rerun.

WSL note (common cause):
- Check `/etc/resolv.conf` and confirm the `nameserver` is reachable from WSL.
- If it is not, update WSL networking / DNS configuration (editing `/etc/resolv.conf` directly is often temporary because it can be regenerated).

## Leaflet System Dependencies (Ubuntu)
The app can run without `leaflet` (fallback plot-based map), but for the full basemap you may need build deps for geospatial packages pulled in by CRAN (commonly GDAL).

If `leaflet` install fails due to `terra`/GDAL, install system deps and retry:
```bash
sudo apt-get update
sudo apt-get install -y cmake gdal-bin libgdal-dev libudunits2-dev libssl-dev libcurl4-openssl-dev
```

Then try:
```bash
VA_DASH_INSTALL_LEAFLET=1 Rscript scripts/bootstrap.R
```

## Local Run
```bash
R -e "shiny::runApp('va_schools_dashboard/app', port = 3838, launch.browser = FALSE)"
```

## Deploy (shinyapps.io)
```bash
cd va_schools_dashboard
R -e "rsconnect::deployApp(appDir = '.', appPrimaryDoc = 'app/app.R')"
```

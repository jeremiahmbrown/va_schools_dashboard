# ==============================================================================
# Script: bootstrap.R
# Purpose:
#   Bootstrap R package dependencies for local development and shinyapps.io
#   deployment. This is intended for a fresh R install on Ubuntu.
#
# Inputs:
#   - Internet access to CRAN (required to install packages)
#
# Outputs:
#   - Installs required R packages into the active library.
#   - Optionally initializes / snapshots `renv`.
#
# Key Details / Debugging:
#   - If installs fail behind a proxy/firewall, set `options(repos=...)` and
#     verify network access to CRAN.
#   - If DNS/network is unavailable (e.g., cannot resolve `cloud.r-project.org`),
#     package installation cannot succeed; fix network first, then rerun.
#   - Full `leaflet` installs can pull geospatial dependencies (e.g., GDAL via
#     `terra`). If you don't have system build deps yet, use the app's fallback
#     plot-based map and install leaflet later.
# ==============================================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))

install_leaflet <- identical(Sys.getenv("VA_DASH_INSTALL_LEAFLET", "0"), "1")
use_renv <- identical(Sys.getenv("VA_DASH_USE_RENV", "1"), "1")

core_required <- c(
  "shiny",
  "dplyr",
  "tidyr",
  "ggplot2",
  "jsonlite",
  "sp",
  "rsconnect",
  "renv"
)

optional <- character(0)
if (install_leaflet) {
  optional <- c(optional, "leaflet")
}

install_some <- function(pkgs, label) {
  pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(pkgs) == 0) {
    message(label, ": already installed")
    return(invisible(TRUE))
  }
  message(label, ": installing ", paste(pkgs, collapse = ", "))
  ok <- TRUE
  for (p in pkgs) {
    tryCatch(
      {
        install.packages(p)
      },
      error = function(e) {
        ok <<- FALSE
        message("Failed installing ", p, ": ", conditionMessage(e))
      }
    )
  }
  invisible(ok)
}

install_some(core_required, "Core packages")

if (length(optional) > 0) {
  message("Optional packages enabled via VA_DASH_INSTALL_LEAFLET=1")
  install_some(optional, "Optional packages")
} else {
  message("Skipping optional packages (set VA_DASH_INSTALL_LEAFLET=1 to try installing `leaflet`).")
}

if (use_renv && requireNamespace("renv", quietly = TRUE)) {
  message("Initializing renv (if not already initialized)...")
  if (!file.exists("renv.lock")) {
    renv::init(bare = TRUE)
  }
  message("Snapshotting renv.lock...")
  renv::snapshot(prompt = FALSE)
} else {
  message("Skipping renv (set VA_DASH_USE_RENV=1 to enable).")
}

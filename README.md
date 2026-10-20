# Virginia Public Schools Dashboard

This R Shiny dashboard maps Virginia public school divisions and schools. It uses a frozen public-data snapshot in `app/data/`; opening the app does not download data. The [public app](https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/) keeps its existing name and URL.

## What you can explore

- Division polygons and school points, with metric values and detail charts.
- Historical Accreditation Combined Rate (ACR) and SOL pass rate from VDOE School Quality Profiles accreditation exports. The latest available year for these exports is 2024. The app's "Overall" values are simple subject means, not official VDOE ratings. Science ACR is blank in these files, so the ACR mean uses English and Math. The pass-rate mean can use all three subjects.
- Discipline measures through 2023–24, and chronic absence and meal measures through 2025–26 where values are reportable.
- Student race and ethnicity shares based on available VDOE subgroup data.

The year control uses the **ending year** for school-year data: `2026` means 2025–26. Accreditation exports use their stated year. The app shows the latest available year for the chosen metric and says when a selected year has no values. Suppressed values appear as missing, not zero. For measures with a clear better direction, a higher percentile means a better result among reportable peers in the same year.

The optional attendance-zone overlay can be incomplete or old. Check a division's current boundaries before using it for an address decision.

## Current source limits

The new VDOE School Performance and Support Framework (SPSF) is a different accountability system from historical ACR. This dashboard does not join them into one trend. The official 2025–26 SOL files were listed but could not be retrieved directly from this environment, so this snapshot does not include them. See the [source and year matrix](docs/SOURCE_MATRIX_2026-10-19.md), [methodology](docs/METHODOLOGY.md), and [refresh QA](docs/REFRESH_QA_2026-10-20.md) for exact years and checks.

School coordinates use NCES 2024–25 geocodes and division polygons use Census 2024 boundaries. The current match is 1,810 of 1,828 VDOE school names; the 18 unmatched schools are listed in `app/data/snapshot_meta.json`.

## Run it in Ubuntu on Windows

The checkout is at `/home/jeremiah/code/va_schools_dashboard` in WSL Ubuntu. Open an Ubuntu terminal and run:

```bash
cd /home/jeremiah/code/va_schools_dashboard
Rscript tests/check_snapshot.R
Rscript -e 'shiny::runApp("app", port = 3838, launch.browser = FALSE)'
```

Open `http://localhost:3838` in a Windows browser. Press `Ctrl+C` in Ubuntu to stop the app. The [WSL runbook](docs/WSL_RUNBOOK.md) explains the Windows path, R package lockfile, Beads (`bd`), and data refresh commands.

## Maintain or release it

The [improvement plan](docs/IMPROVEMENT_ROADMAP.md) lists each work phase and its Beads issue. The [maintenance guide](docs/MAINTENANCE.md) gives the data build, verification, same-name shinyapps.io deployment, and rollback steps. Local code or data changes do not update the public URL until deployed.

Source agencies: [VDOE School Quality Profiles](https://schoolquality.virginia.gov/download-data), [NCES EDGE](https://nces.ed.gov/programs/edge/), [Census TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html), and [NCES SABS](https://nces.ed.gov/surveys/sabs/). See [data schema](docs/DATA_SCHEMA.md), [data inventory](docs/DATA_INVENTORY.md), and [map specification](docs/MAP_SPEC.md) for the existing data model.

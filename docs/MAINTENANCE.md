# Maintain and release the frozen Shiny snapshot

Work from the Ubuntu checkout at `/home/jeremiah/code/va_schools_dashboard`. The Windows and WSL setup is in [WSL_RUNBOOK.md](WSL_RUNBOOK.md). Check the [source matrix](SOURCE_MATRIX_2026-10-19.md) and [QA record](REFRESH_QA_2026-10-20.md) before you call a year "current."

## Refresh and check data

The normal build uses cached public raw files. To request new VDOE files, set `VA_DASH_REFRESH_VDOE=1`. See the WSL runbook for the verified temporary CA bundle needed when the VDOE server omits its TLS intermediate. Do not use `curl -k`.

```bash
cd /home/jeremiah/code/va_schools_dashboard
Rscript data-raw/build_snapshot.R
Rscript tests/check_snapshot.R
Rscript -e 'renv::status()'
git diff --check
git diff --stat
```

The builder rejects missing or changed source columns, HTML or short files, bad year coverage, duplicate output keys, and published numeric values with suppression flags. It writes to a staging directory first, then replaces the frozen `app/data/` snapshot. If a refresh or build fails, inspect the error and keep the last good snapshot. Source SHA-256 values and match counts are in `app/data/snapshot_meta.json`. Compare year-by-metric coverage and values against the prior Git revision before a release. The test script checks keys, bounds, flags, percentile behavior, preserved zones, and selected division aggregate calculations.

## Check the app locally

```bash
Rscript -e 'shiny::runApp("app", port = 3838, launch.browser = FALSE)'
```

From Windows, open `http://localhost:3838`. Check a desktop and a phone-size view. Select an attendance metric to see 2026; select historical ACR to see 2024. Check school search, a division click, map attribution, "No data" behavior, and the attendance-zone warning. These observations are separate from the automated data checks.

## Deploy to the existing public URL

Review the code and snapshot diff first. Keep the prior committed snapshot and deployment available for rollback. From the repo root in Ubuntu, use the existing shinyapps.io account and app name:

```r
setwd("/home/jeremiah/code/va_schools_dashboard")
rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "app/app.R",
  account = "jeremiahmbrown",
  appName = "va_schools_dashboard"
)
```

The target is `https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/`. Verify the deployment result and load that public URL. Check the same map, a 2026 attendance metric, a historical ACR metric, search, and details there. A successful local run does not prove that the public deployment works.

If the release fails, redeploy the last known good Git commit or restore the prior shinyapps.io deployment, then check the public URL again. Do not relabel an old public snapshot as the new one. `.rscignore` keeps raw downloads, Beads, tests, docs, and local `renv/` out of the app bundle; `renv.lock` is included for package resolution.

# Work on this R Shiny app from Windows and WSL

The Git checkout is in Ubuntu at `/home/jeremiah/code/va_schools_dashboard`.
Windows Explorer can open it at
`\\wsl.localhost\Ubuntu\home\jeremiah\code\va_schools_dashboard`.
Use the Linux path in the Ubuntu terminal. Keep this checkout on the WSL file
system; R, Git, `bd`, and the data build run there. Windows can show and edit
the same files, and a Windows browser can open the WSL app at `localhost`.

## Open the project

Open the Ubuntu terminal, then run:

```bash
cd /home/jeremiah/code/va_schools_dashboard
git status --short
bd ready
```

`bd` is Beads, the repo's local task tracker. A *bead* is an issue with a
description, acceptance condition, status, and optional dependencies. The
main plan is epic `va_schools_dashboard-osa`:

```bash
bd show va_schools_dashboard-osa
bd list --status=open
bd show va_schools_dashboard-osa.2.3
```

Beads 1.3.1 is installed in `~/.local/bin`. A line in `~/.bashrc` puts it on
the interactive Bash `PATH`. Open a new Ubuntu terminal if `bd` is not found.
Anonymous Beads usage metrics are on at the owner's request. `bd metrics status`
shows that setting.

Beads keeps its live database in `.beads/` on this WSL machine. The tracked
`.beads/issues.jsonl` is a portable issue export, so a new clone can run
`bd init --non-interactive --skip-agents --skip-hooks` and then
`bd import .beads/issues.jsonl`. After changing issues, run
`bd export --output .beads/issues.jsonl` to refresh that copy before a Git
release. The export includes issue statuses and dependencies; it is not a
full database backup. Do not hand-edit the JSONL.

## Run the current snapshot and app

```bash
cd /home/jeremiah/code/va_schools_dashboard
Rscript tests/check_snapshot.R
Rscript -e 'shiny::runApp("app", port = 3838, launch.browser = FALSE)'
```

Open `http://localhost:3838` in a Windows browser. WSL forwards this local
port to Windows. Press `Ctrl+C` in Ubuntu to stop the app.

The app reads a frozen set of files under `app/data/`. Starting it does not
fetch new data. `renv.lock` records the R packages; `Rscript -e
'renv::status()'` checks for drift.

## Rebuild from cached public data

```bash
cd /home/jeremiah/code/va_schools_dashboard
Rscript data-raw/build_snapshot.R
Rscript tests/check_snapshot.R
git diff --stat
```

The build reads the existing raw cache unless asked to refresh it. It writes
to a staging directory and publishes the new snapshot only after the build's
key and suppression checks pass. The current committed Git snapshot is also
the rollback copy.

## Download the latest VDOE files

The VDOE School Quality Profiles server omitted a TLS intermediate certificate
when checked on 2026-10-19. If `curl` reports certificate error 60, get the
missing intermediate from DigiCert over HTTPS, verify it against Ubuntu's root
store, and use a temporary CA bundle:

```bash
curl --fail --location --silent --show-error \
  https://cacerts.digicert.com/DigiCertGlobalG2TLSRSASHA2562020CA1-1.crt \
  -o /tmp/va-digicert-intermediate.crt
openssl x509 -inform DER -in /tmp/va-digicert-intermediate.crt \
  -out /tmp/va-digicert-intermediate.pem
openssl verify -CAfile /etc/ssl/certs/ca-certificates.crt \
  /tmp/va-digicert-intermediate.pem
cat /etc/ssl/certs/ca-certificates.crt \
  /tmp/va-digicert-intermediate.pem > /tmp/va-dashboard-ca-bundle.pem
VA_DASH_REFRESH_VDOE=1 \
VA_DASH_CURL_CA_BUNDLE=/tmp/va-dashboard-ca-bundle.pem \
  Rscript data-raw/build_snapshot.R
Rscript tests/check_snapshot.R
```

The `openssl verify` command must report `OK`. If the certificate or issuer
changes, inspect the new chain before using this specific intermediate. The
builder does not use `curl -k`. A failed download does not replace a cached
file. The previous `app/data/` directory remains in place if a build fails.

Current VDOE downloads use a rolling three-year window. The five compressed
files in `data-raw/historical/` preserve baseline public rows for years that
the current download has dropped. The builder uses them only for absent years.
Do not label an archived row as a fresh VDOE download.

## Prepare a release

Check `docs/REFRESH_QA_2026-10-20.md`, review `git diff`, and test the map,
search, filters, and details locally. `docs/MAINTENANCE.md` gives the existing
shinyapps.io deploy call and rollback steps. A local build does not update the
public site. The app URL remains
`https://jeremiahmbrown.shinyapps.io/va_schools_dashboard/` only if deployed
with its current account and `appName`.

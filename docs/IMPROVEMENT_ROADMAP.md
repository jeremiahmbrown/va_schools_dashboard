# Virginia Public Schools Dashboard: Incremental Improvement Plan

Date: 2026-10-19 (America/New_York)

Beads epic: `va_schools_dashboard-osa`

Branch: `improve-data-refresh-and-ui`

This plan keeps the existing R Shiny app, frozen `app/data/` snapshot, and
`jeremiahmbrown.shinyapps.io/va_schools_dashboard/` app identity. Beads is the
task record. Use `bd show <id>`, `bd ready`, and `bd list` to see live status.

## Current baseline

- The clean WSL checkout matched public commit `8f86732` before this work.
- Ubuntu has R 4.3.3, `curl`, `unzip`, `ogr2ogr`, Shiny, Leaflet and rsconnect.
- The deployed app opened at its current URL. It displayed 2024 as the latest
  year and a 2026-02-15 snapshot date. Its CARTO background tiles displayed
  `API KEY REQUIRED` on 2026-10-19.
- `renv::status()` found packages used or installed but absent from `renv.lock`.
- The builder retains nonempty cached raw CSVs and selects years from its
  accreditation export. The public source and bundled data must be checked
  before a new year can be claimed.

## A. Establish a safe baseline (`...osa.1`)

1. **A1 `...osa.1.1`:** Record Git, R, package, snapshot and live-app state.
   Evidence: exact commands and a dated baseline.
2. **A2 `...osa.1.2`:** Start the existing app in Ubuntu and count source and
   snapshot rows, distinct keys and missing values. Evidence: startup result
   and inventory, with failures clearly marked.
3. **A3 `...osa.1.3`:** Record a backup of the current frozen snapshot and the
   same-name shinyapps.io deployment/rollback procedure. Gate: no production
   write during this phase.

## B. Identify and refresh authoritative data (`...osa.2`)

1. **B1 `...osa.2.1`:** Make a source matrix with metric, official URL, file,
   school year, update date, field names, grain, and availability. Confirmed
   candidates include VDOE 2025–2026 SOL and fall membership; VDOE
   school-level chronic absenteeism is confirmed through 2024–2025.
2. **B2 `...osa.2.2`:** Check whether the old Accreditation Combined Rate
   continues after Virginia's School Performance and Support Framework (SPSF)
   change. Keep SOL pass rate, old ACR, app-defined overall means and official
   SPSF ratings separate. Gate: no false continuous trend.
3. **B3 `...osa.2.3`:** Adapt only the necessary source readers and rebuild the
   snapshot. Record source file hashes, retrieval dates, per-metric latest
   years, and transform version. Gate: a newer year appears only where its
   source and definition pass checks.
4. **B4 `...osa.2.4`:** Review NCES ID joins, the 80% minimum match rule,
   excluded schools, coordinates and 2024 division/attendance-zone geometry.
   Gate: unmatched entities and boundary age are visible to maintainers.

## C. Add data quality gates (`...osa.3`)

1. **C1 `...osa.3.1`:** Reject HTTP errors, HTML, partial CSVs, changed
   columns, duplicate keys, invalid years and missing source files. Keep the
   prior snapshot if a download or build fails.
2. **C2 `...osa.3.2`:** Test suppression and `NA` handling, valid 0–100 rates,
   positive denominators, percentage totals, percentile direction and division
   aggregation with small examples. Do not turn missing values into zero.
3. **C3 `...osa.3.3`:** Compare old/new row counts, school and division coverage,
   year-by-metric missingness, joins and large value changes. Explain each
   material shift before release.

## D. Repair and simplify R code (`...osa.4`)

1. **D1 `...osa.4.1`:** Add an explicit raw-data refresh option, verified
   downloads, secure TLS, and safe replacement of cached files.
2. **D2 `...osa.4.2`:** Fix vector suppression handling and align percentile
   calculations with the app's higher-is-better wording; test ties and missing
   values.
3. **D3 `...osa.4.3`:** Remove only year/metric assumptions that stop the new
   snapshot from loading. Keep the existing Shiny entry point and data model.

## E. Improve the existing interface (`...osa.5`)

1. **E1 `...osa.5.1`:** Replace the CARTO tile call with a supported background
   and preserve required map attribution. Check polygons and points over it.
2. **E2 `...osa.5.2`:** Use clear school-year labels and show a metric's latest
   available year/source. Mark app-defined measures and missing data clearly.
3. **E3 `...osa.5.3`:** Shorten dense sidebar text, improve mobile layout and
   keyboard labels, and check color use without changing the product layout.

## F. Verify and prepare the same-URL release (`...osa.6`)

1. **F1 `...osa.6.1`:** Run targeted R tests, parse checks, snapshot validator,
   and desktop/phone smoke checks for map, search, filtering and details.
2. **F2 `...osa.6.2`:** Update source/methodology, maintenance and WSL runbook
   docs with exact commands and known limits.
3. **F3 `...osa.6.3`:** Review the diff, data comparison and deployment bundle.
   Deploy only through the existing shinyapps.io account and `appName` after
   the release evidence is reviewable. Check the live URL and keep the prior
   snapshot available for rollback.

## Source and release boundaries

- Use public aggregate data only. Do not add student-level records or PII.
- A source's publication year is not proof that every metric exists for that
  year. The app must not use one overall year label to imply otherwise.
- Preserve suppression and note when a division value uses school enrollment
  weights rather than the official test denominator.
- The existing attendance-zone overlay is optional and can be incomplete;
  do not treat its polygons as current address assignment advice.
- Keep deployment separate from local tests. A successful local build is not
  proof that the public app has been updated.

## Beads quick start

From Ubuntu in the repository:

```bash
~/.local/bin/bd ready
~/.local/bin/bd show va_schools_dashboard-osa
~/.local/bin/bd list --status=open
```

Claim a ready bead with `bd update <id> --claim`; close it with
`bd close <id> --reason="what was verified"`. `bd show <id>` displays its
description, acceptance condition and dependencies.

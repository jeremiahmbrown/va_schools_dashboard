# Annual SOL school subject import: QA record

Checked 2026-10-20 in Ubuntu on `improve-data-refresh-and-ui`. This import
keeps the existing R Shiny app and frozen `app/data/` snapshot. This was a
pre-release QA checkpoint; the public app was released later that day. See the
[release record](RELEASE_2026-10-20.md).

## Source and extraction

Source: [VDOE SOL Test Pass Rates & Other Results](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results),
**School Subject-Area** workbooks. Direct requests from WSL returned HTTP 403;
the user placed the two public aggregate workbooks in ignored
`data-raw/input/manual/`. Their source SHA-256 values are:

| Workbook | Source rows | SHA-256 |
| --- | ---: | --- |
| `SY 2024-2025 Subject Area by School.xlsx` | 80,648 | `4867721258e57a80d346b46b5756c4a975312d6a222732f68437bf62de086c7b` |
| `SY 2025-26 School - Subject.xlsx` | 77,451 | `ae33df46fcde70d1533af19643f8e91a377ebd6a91375903e611e4a8395569e2` |

`Rscript data-raw/extract_annual_sol.R` keeps **All Students** rows for
English Reading, Mathematics, and Science. It takes 2022–23 from the first
workbook and 2023–24 through 2025–26 from the newer workbook. The checked
extract has 21,588 school-subject-year rows, 5,397 per ending year. It is
saved as `data-raw/annual_sol_school.csv.gz`; original file hashes and the
extract hash are in `data-raw/annual_sol_sources.json`.

The extractor rejects changed headers, unexpected text markers, duplicate
school-year-subject keys, impossible counts or rates, and a reported rate more
than 1.1 percentage points from `100 × numerator / denominator`. The maximum
observed difference was 0.5 points, consistent with integer rounding.
Suppressed (`<`) and unavailable cells keep `NA` for value and counts. The
builder checks this again and refuses to publish a snapshot if the extract is
invalid or the school match is below 95%.

| Ending year | Reportable source subject rows | Mapped school overall values |
| --- | ---: | ---: |
| 2023 | 5,346 | 1,764 |
| 2024 | 5,341 | 1,766 |
| 2025 | 5,317 | 1,771 |
| 2026 | 5,324 | 1,769 |

## Comparability and joins

The workbooks repeat earlier years. Of 5,373 matching school-subject keys,
the 2023–24 rate differed on 11 and the 2024–25 rate differed on 12 between
the two releases; 35 of the latter had changed numerator or denominator. The
newer workbook takes precedence for the overlapping years. The old dashboard's
2024 School Quality Profiles `Percent Passing` matched the newer workbook's
2023–24 annual rate exactly for only 685 of 5,213 reportable mapped
school-subject pairs (mean absolute difference 3.64 points). It is retained
as **SQP Percent Passing**, separate from **Annual SOL Pass Rate**. ACR is also
separate and ends at its last verified year, 2024.

Names were joined within normalized division and school names to the NCES
school IDs used by the map. Across the four-year source union, 1,777 of 1,821
source school codes matched (97.6%). In each of 2025 and 2026, 1,774 of
1,812 source schools matched; the rest include schools absent from the 2024–25
NCES geocode snapshot and older or changed school names. Unmatched source
schools are excluded from map points and derived division rates. The metadata
records the match counts. These exclusions need review when the map's school
geocodes are refreshed.

School subject values and test counts come from VDOE. The school `Overall`
measure is an app-defined mean of available English Reading, Mathematics, and
Science rates, without a single denominator. Division subject rates are
`100 × sum(numerators) / sum(denominators)` over reportable **matched**
schools. All 130 division choropleth records have reportable annual SOL
subjects in 2025 and 2026. Division `Overall` is a mean of division subject
rates. These are derived dashboard values, not official VDOE division results.

## Checks and local display

- `Rscript data-raw/build_snapshot.R` completed and atomically replaced the
  frozen snapshot after its validation gates.
- `Rscript tests/check_snapshot.R` passed, including duplicate keys,
  suppression, 2025/2026 annual values, and recomputed 2026 division English
  rates from school test counts.
- `Rscript tests/check_zones.R` passed after the rebuild; Roanoke City and
  County files remain in the snapshot.
- `renv::status()` reported no issues after adding the workbook reader and
  its dependencies to `renv.lock`.
- The local app loaded at `http://localhost:3839` with 2026 Annual SOL as the
  default, the gray Esri map and attribution, and school/division overlays.
  Selecting 2025 kept that year; moving from 2024 ACR to 2025 selected Annual
  SOL. The Roanoke City annual trend rendered for 2023–2026.

## Remaining limits

- The 2024–25 old-system accreditation rating workbook was checked later on
  2026-10-20. It has no numeric ACR field, so there is no verified 2025 ACR
  point. SPSF is a different framework and is not appended to ACR. See the
  [performance coverage check](PERFORMANCE_COVERAGE_2026-10-20.md).
- The 2025–26 manual XLSX files are ignored by Git. The tracked extract and
  source hashes make the current build repeatable, but a new annual release
  requires the original workbooks and a new extraction and QA pass.
- This is a local code and snapshot change. The public URL still has its
  previous deployment until a reviewed same-name release.

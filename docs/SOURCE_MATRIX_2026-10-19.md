# VDOE source and year matrix

School Quality Profiles checked on 2026-10-19; annual SOL files added on 2026-10-20 from user-supplied workbooks. School Quality Profiles CSVs were obtained
from `https://schoolquality.virginia.gov/download-data` with `level=schools`,
`divisions[]=any`, `schools[]=any`, and `year=all`. The export service did not
provide a last-updated date in the CSV. Retrieval date is not publication date.

| Dashboard measure | Export request | Years in current response | Grain and fields | Decision |
| --- | --- | --- | --- | --- |
| English, math, science Accreditation Combined Rate and SQP Percent Passing | `type=accreditation`, `indicators[]=1`, `3`, `5` | 2022, 2023, 2024 | School, year, subject, subgroup, level; `Accreditation Combined Rate`, `Percent Passing` | Historical only. Do not extend to 2025 or 2026 from this endpoint. Science ACR is missing in every available year in the stored snapshot. |
| Chronic absence | `type=learningClimate`, `indicators[]=absenteeism` | 2023–24, 2024–25, 2025–26 | School, year, subgroup; `Percent above 10`, counts above and below 10 | Candidate through 2025–26, subject to complete-year and suppression checks. |
| Fall membership enrollment | `type=enrollment`, `indicators[]=fallMembership` | 2023–24, 2024–25, 2025–26 | School, year, one total row plus grade rows; `Count` | Use the reported school total once, after checking it equals the grade sum. |
| Fall membership race/ethnicity | VDOE Fall Membership Build-A-Table, manual public CSV | 2024–25 and 2025–26 | School, year, race category; full-time, part-time, total counts | Use 2025–26 school shares. Reconcile each school-year to the separate gender/grade total file. Seven blank category counts stay missing. See [race QA](FALL_MEMBERSHIP_RACE_QA_2026-10-20.md). |
| Free/reduced meal eligibility and breakfast/lunch participation | `type=learningClimate`, `indicators[]=frEligibility`, `frBreakfast`, `frLunch` | 2023–24, 2024–25, 2025–26 | School, year, level; `Percent` | Candidate through 2025–26; check completeness and definition for each measure. |
| Short/long suspension and expulsion counts | `type=learningClimate`, `indicators[]=shortTermSuspensions`, `longTermSuspensions`, `expulsions` | 2022–23, 2023–24 | School, year, subgroup; student and incident counts | Historical only. Do not infer newer zeroes. |

The [VDOE SOL results page](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results)
lists 2025–26 subject and test files at school, division, and state levels,
with numerators and denominators. These are a separate source and definition
from the School Quality Profiles accreditation export. Direct XLS requests returned HTTP 403 to this environment. The user supplied the 2024–25 and 2025–26 school workbooks manually on 2026-10-20. Their All Students English Reading, Mathematics, and Science rows now supply a separate 2023–2026 Annual SOL Pass Rate series after schema, count, suppression, and match checks. See [annual SOL QA](ANNUAL_SOL_IMPORT_QA_2026-10-20.md).

The [VDOE SPSF overview](https://www.doe.virginia.gov/state-board-data-funding/accreditation-accountability/school-performance-and-support-framework/spsf-overview)
and [2024–25 SPSF release](https://www.doe.virginia.gov/teaching-learning-assessment/road-to-readiness-school-performance-and-support-framework-resource-hub/2024-25-spsf-accountability-data-release)
describe a new school accountability framework. Its categories and composite
score differ from the dashboard's simple mean of historical accreditation
rates. They must have separate labels and series if added.

The current builder fixes [NCES 2024–25 school geocodes](https://nces.ed.gov/programs/edge/Geographic/SchoolLocations)
and Census 2024 school district boundaries. The year is the source vintage,
not a guarantee that all current schools or boundaries match.

The current School Quality Profiles response uses a rolling three-year window
for enrollment, absence, and meal measures. The baseline repo's public raw
files include 2022–23 rows that the current response no longer returns. Five
compressed copies in `data-raw/historical/` keep those older rows. The builder
uses archived rows only when the whole year is absent from the live response.

## Source checks before release

- Save the source URL, request fields, retrieval time, SHA-256, row count,
  columns, and available years for each download.
- Check that each source has the expected school-year coverage, unique grain,
  valid numeric range, and suppression markers before using a new year.
- Compare school coverage and missing values with the current snapshot.
- Keep historical ACR and app-defined overall mean separate from SOL pass rate
  and official SPSF results.

The 2026-10-20 ACR source check, official SOL download steps, and completed manual file handoff are in [ACR_SOL_STATUS_2026-10-20.md](ACR_SOL_STATUS_2026-10-20.md). The old-system 2024–25 accreditation rating workbook needs its underlying performance year checked before it can supply any 2025 ACR value.

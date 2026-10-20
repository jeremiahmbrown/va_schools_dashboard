# ACR and SOL source status

Checked 2026-10-20. Historical ACR and annual SOL pass rates are different measures. Do not join them into one trend or substitute one for the other.

## Accreditation Combined Rate (ACR)

The dashboard's downloaded [School Quality Profiles](https://schoolquality.virginia.gov/download-data) `type=accreditation` exports contain `Year` 2022, 2023, and 2024. They do not contain 2025 or 2026 rows. The current app therefore shows ACR only through 2024.

The [VDOE SOL and accreditation reports page](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results) lists an old-system **2024–2025 accreditation rating workbook**. A rating year is not the same as the school year of its underlying performance figures. The VDOE's 2024–25 recognition release says its award data are based on performance through **2023–24**. Before adding a 2025 ACR point, inspect the workbook's exact field, performance year, school grain, and method. No 2025 ACR point has passed that check yet.

For 2025–26, VDOE reports school performance through the [School Performance and Support Framework (SPSF)](https://www.doe.virginia.gov/state-board-data-funding/accreditation-accountability/school-performance-and-support-framework/spsf-overview). The older [combined rate](https://schoolquality.virginia.gov/vdoe_glossaryterm) gives credit for passing and for students who meet growth benchmarks despite not passing. SPSF instead has separately defined Mastery, Growth, Readiness, and Graduation components with school-level weights. Both concern student outcomes, but the ACR and SPSF scores are **not numerically continuous**. ACR should end at its last verified year; SPSF should start a separate series with a visible method break. The Board also [changed the 2025–26 SPSF calculation](https://www.doe.virginia.gov/Home/Components/News/News/557/227), including annual rather than cohort-based Mastery, so compare SPSF years only after checking their formulas.

Annual SOL pass rate is a different, narrower construct: the share passing a given test. It can be shown as its own year-by-year series when the source columns, tests, student groups, and suppression rules match. Do not extend the ACR line with pass rates, Mastery, or SPSF overall points.

## Annual SOL pass rates

The [VDOE SOL results page](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results) lists school-level **School Subject-Area (XLS)** files under both **2024–2025 Test Administration** and **2025–2026 Test Administration**. It also lists school test-level and participation files. The direct document requests returned HTTP 403 from WSL. The user placed both school workbooks in the manual input folder on 2026-10-20. They were checked and imported as a separate annual SOL series; see [annual SOL QA](ANNUAL_SOL_IMPORT_QA_2026-10-20.md).

### Manual source files and future refresh

The files are `SY 2024-2025 Subject Area by School.xlsx` and `SY 2025-26 School - Subject.xlsx`. The original workbooks stay in ignored `data-raw/input/manual/`; the checked public aggregate extract and source hashes are tracked under `data-raw/`. For a later release, put newer original files in the same folder, run `Rscript data-raw/extract_annual_sol.R`, review its QA, then run the snapshot builder.

The optional **2024–2025** old-system accreditation rating workbook has not been supplied or checked. Obtain it from the same VDOE page only if the possible 2025 ACR point is still needed. Its rating year must be checked against the underlying performance year before use.

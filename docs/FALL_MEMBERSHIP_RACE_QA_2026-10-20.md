# Fall Membership race and enrollment QA — 2026-10-20

## Files and grain

The user supplied three public CSVs from the [VDOE Fall Membership Build-A-Table](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics). The originals stay in ignored `data-raw/input/manual/`. Their names and SHA-256 hashes are in `data-raw/fall_membership_race_sources.json`.

| File | Grain | Use |
| --- | --- | --- |
| `fall_membership_statistics.csv` | School year × division number × school number × race | Source of race counts; 21,978 rows |
| `fall_membership_statistics (1).csv` | School year × division number × school number × gender; grade count columns | Independent school enrollment total check |
| `fall_membership_statistics (2).csv` | Same as the first file | Byte-for-byte duplicate; not added a second time |

The race file covers 1,862 schools in 2024–25 and 1,864 in 2025–26. There are no duplicate school-year-race keys and only the expected seven race/ethnicity categories. The sum of reported race counts equals the independent gender/grade school total for each of the 3,726 school-years. Statewide sums in these files are 1,260,956 for 2024–25 and 1,246,285 for 2025–26. These are source totals, not dashboard map totals.

Seven race rows have a blank count. They remain missing in the dashboard. When a category row is absent and the other race counts equal the independent school total, the extractor records zero. It does not convert an explicit blank count to zero. The checked, expanded extract has 26,082 school-year-category rows and seven suppressed values. The source CSVs use commas in large numbers; the extractor validates and removes those separators.

## Map join and measures

The race extract joins to existing NCES-backed dashboard schools by normalized division and school names. It matches 1,795 of 1,862 source schools in 2024–25 (96.4%) and 1,810 of 1,864 in 2025–26 (97.1%). The 2026 match includes all 1,810 schools currently on the map. Other source schools lack a matching map school and are excluded from dashboard school and division measures. The build stops if either year's source-school match falls below 95%.

The dashboard now has 12,565 school race-measure rows in 2025 and 12,670 in 2026, with three and four suppressed values, respectively. All 130 map divisions have a reportable 2025 and 2026 Black student share. For 3,598 fully reportable school-years, the seven shares sum to 100% within floating-point precision. The snapshot stores exact race counts as numerators and the same-source school total as the denominator. Division shares aggregate those counts over matched, reportable schools. They are not official division totals.

The 2023–24 demographic values still come from the School Quality Profiles short-term suspension export's subgroup percentages. The 2025 source change is explicit in `source` and metric descriptions; avoid interpreting an apparent 2024-to-2025 step as an exact like-for-like change.

## Enrollment correction

The separate School Quality Profiles enrollment export contains a school total row plus grade rows. The current three-year response has 5,414 school-years, and each total equals its grade sum. The archived source has the same structure. The old builder added both sets, doubling `schools.enrollment_latest` and the denominator of enrollment-based rates. The builder now requires one total row per school-year, checks that it equals the grade sum, and uses it once. For example, Accawmacke Elementary's latest enrollment is 483 rather than 966. Where the underlying discipline numerator is unchanged, the corrected per-100 rate is twice the previously built rate. The builder stops if any total or grade count is invalid or does not reconcile.

## Repeat and verification

From the repository root in WSL:

```bash
Rscript data-raw/extract_membership_race.R
Rscript data-raw/build_snapshot.R
Rscript tests/check_snapshot.R
```

The first command writes the tracked, checked `data-raw/fall_membership_race_school.csv.gz` and source manifest. The second command builds a staged frozen snapshot and publishes it only if its validation passes. The final snapshot check passed on 2026-10-20. `git diff --check` also passed. The public app was released later that day; see the [release record](RELEASE_2026-10-20.md).

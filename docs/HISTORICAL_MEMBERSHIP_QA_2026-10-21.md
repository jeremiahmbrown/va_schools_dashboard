# Historical Fall Membership import — QA record

The user supplied school-level [VDOE Fall Membership Build-A-Table](https://p1pe.doe.virginia.gov/buildatable/fallmembership)
race and gender CSVs for 2021–22 through 2023–24. The 2021–22 standalone
files exactly match the 2021–22 rows of the combined files. Originals remain
in the ignored `data-raw/input/manual/` folder; filenames, byte sizes, and
SHA-256 hashes are in `data-raw/fall_membership_historical_sources.json`.
The checked race and 2021–22 enrollment extracts are tracked gzip files.

## Grain and totals

The gender file has one row per **school, year, and gender**, with a `Total
Count`. It need not have one row per grade. We sum the gender rows once to
check the school total. Every nonblank row also has `FT Count + PT Count =
Total Count`; there are no duplicate school-year-gender or
school-year-race keys. Explicit blank counts stay missing. A 2023–24 school
with zero total is excluded from shares and rates.

| Ending year | Extracted schools | Race versus gender total | Mapped schools | Dashboard treatment |
| --- | ---: | --- | ---: | --- |
| 2022 | 1,862 | Equal for every school | 1,757 (94.4%) | Seven race shares and same-year enrollment for the SBAR action rate. |
| 2023 | 1,870 | Race exceeds gender at 327 schools because this gender export has no `Other` rows | 1,777 (95.0%) | Use race shares for 1,755 map schools whose race sum equals the separate SQP school total. Exclude the 22 mismatches. |
| 2024 | 1,868 | Equal for every school | 1,789 (95.8%) | Seven race shares; one zero-enrollment source school is excluded. |

The 2022–23 race sums match School Quality Profiles enrollment at 1,755 of
1,777 joined map schools. The other 22 do not enter 2023 race measures.
Some of the separately reported SQP totals also differ from the 2023–24
Fall Membership totals; 2024 uses the internally reconciled race and gender
files for the race-share denominator. The 2022 name-match gate is 94%, lower
than the recent-file 95% gate because this dashboard uses 2024–25 NCES map
schools. Unmatched historical schools stay out of school and division values.

For each school-year, the seven race categories are expanded from the source.
An absent category becomes zero only where the other categories exhaust the
checked school total. An explicit blank remains suppressed. The 2022–24
extract has 26 blank race cells in the original files; some belong to the
zero-enrollment school or excluded 2023 schools, so the published counts vary
by year and measure. The older approximate shares based on SQP suspension
subgroups have been replaced. The 2025–26 race extract remains unchanged.

The 2021–22 SBAR denominator uses the checked Fall Membership school total.
The resulting action rates cover 1,673 mapped schools and 128 divisions in
2022. Accawmacke Elementary has 24 out-of-school actions and 408 enrolled
students, giving `100 × 24 / 408 = 5.9` after display rounding.
The Profile now reads `enrollment_by_year.csv` so the selected 2022 rate
appears beside the 2022 enrollment count, rather than the latest school count.

`data-raw/audit_historical_membership.py` reproduces the source-grain and
cross-source checks. `data-raw/extract_historical_membership.R` validates the
original CSVs and writes the two checked extracts. The rebuilt snapshot
passed `tests/check_snapshot.R`, `tests/check_metric_availability.R`, and
`tests/check_source_validation.R` locally. Public-app verification is a
separate release check.

# Methodology and data limits

## School year and source availability

For school-year exports, `2026` means 2025–26. Historical accreditation exports use their stated year. Each metric has its own available years; the year selector is not evidence that every metric exists in that year. The [source matrix](SOURCE_MATRIX_2026-10-19.md) records the exact VDOE exports and years. `app/data/snapshot_meta.json` records input hashes, the transform version, and the NCES match result. File retrieval time is not the agency's publication date.

The current source response has a rolling three-year window for enrollment, absence, and meal measures. Five compressed public inputs in `data-raw/historical/` retain the 2022–23 rows from the original repo. The builder uses those rows only if the whole year is absent from the current response; current data win an overlap.

The Profile panel shows each school's latest reported total enrollment from the School Quality Profiles enrollment export. A division's displayed enrollment is the sum of those latest counts for schools matched to this map, not an official division total. These counts can be from a different year than the selected measure. They give scale but are not necessarily the denominator of a percentage or rate; annual SOL pass rates, for example, use tested students.

## Performance

The accreditation export provides school-year and subject values for All Students. Its `Accreditation Combined Rate` credits passing and qualifying growth or progress. Its `Percent Passing` is labeled School Quality Profiles Percent Passing. It differs from VDOE annual test-administration pass rates and stays in a separate series. The app-defined `Overall` value is the arithmetic mean of available subject values for that school or division and year. It is not an official VDOE rating. Science ACR is blank for every available year in this export; the ACR mean therefore uses English and Math. Science Percent Passing values can contribute to the older SQP mean.

The accreditation export ends at 2024. VDOE annual School Subject-Area workbooks provide the separate Annual SOL Pass Rate series for test years ending 2023–2026. The newer 2025–26 workbook supplies overlapping 2023–24 and 2024–25 values; the older 2024–25 workbook supplies 2022–23. School rates and test counts are taken from the source. Small counts or unavailable tests remain missing. The new School Performance and Support Framework (SPSF) has different categories and scores. No ACR, Percent Passing, annual SOL, or SPSF series is extended by copying another series into it. See the [annual SOL QA](ANNUAL_SOL_IMPORT_QA_2026-10-20.md).

## Discipline and attendance

Suspensions per 100 students use `100 × (short-term student count + long-term student count) / enrollment`. Other discipline rates use the relevant student or incident count and the same enrollment denominator. The Fall Membership export has one school total row and separate grade rows; the builder checks that they agree and uses the total once. Earlier snapshots added both sets of rows and thus doubled enrollment denominators and halved derived per-100 discipline rates. These discipline source exports end at 2023–24. A missing later year means unavailable, not zero incidents. Chronic absenteeism is the reported percentage of students missing at least 10% of school days; its current export reaches 2025–26.

If a needed component is suppressed, the derived total is missing. The builder also removes the underlying numeric value and numerator from a suppressed record. A suppressed or absent value is shown as "No data" and excluded from the map color scale and percentile peer set.

## Division rates and comparisons

Where numerators and denominators are present, a division rate is `100 × sum(numerators) / sum(denominators)` over reportable schools. For rates without public denominators, the builder uses an enrollment-weighted mean over reportable schools. ACR and School Quality Profiles Percent Passing division subjects use enrollment-weighted school means. Annual SOL division subjects use `100 × sum(reported school numerators) / sum(reported school denominators)` for matched dashboard schools. Each division `Overall` value is a simple mean of available division subjects. These derived division values are not official VDOE division rates; annual SOL values exclude source schools without a matched map school ID.

Percentiles use reportable peers for the same year, metric, and school or division comparison group. Ties have the same rank. Missing peers are excluded. For lower-is-better measures, the ranking direction is reversed so a higher percentile means a lower adverse outcome. Neutral meal and demographic measures have no better/worse percentile interpretation.

## Race, geography, and attendance zones

For 2025 and 2026, race and ethnicity shares use school-level VDOE Fall Membership counts divided by each school's total in the same export. The race counts were checked against an independent gender/grade export for every school-year. A race category absent from the source is zero when the other categories equal that independently checked total. Seven explicit blank count cells remain missing. For 2023–2024, the dashboard uses subgroup percentages from the VDOE short-term suspension export, normalized to sum to 100 within each school-year. The source changes in 2025, so changes across that boundary can reflect source methods. Division race shares use the sums of reportable school counts and school totals among matched map schools; they are not official division totals. See the [Fall Membership QA](FALL_MEMBERSHIP_RACE_QA_2026-10-20.md).

School points come from NCES 2024–25 geocodes. The name join matched 1,810 of 1,828 VDOE schools in this build. Census 2024 unified school district polygons show most division outlines; Lexington City has no direct polygon in this layer. Attendance areas are separate: 118 divisions use NCES SABS 2015–16 polygons, Roanoke County and Roanoke City use local GIS polygons, and 10 divisions have no zone file. Only 603 of 1,810 schools have a matched zone. View elementary, middle, and high zones separately because they overlap. These layers are not address assignment advice; see [zone coverage](ZONES_COVERAGE.md).

The [initial refresh QA](REFRESH_QA_2026-10-20.md) and [annual SOL QA](ANNUAL_SOL_IMPORT_QA_2026-10-20.md) give build counts, suppression checks, source comparison, and remaining limits.

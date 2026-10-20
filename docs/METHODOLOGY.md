# Methodology and data limits

## School year and source availability

For school-year exports, `2026` means 2025–26. Historical accreditation exports use their stated year. Each metric has its own available years; the year selector is not evidence that every metric exists in that year. The [source matrix](SOURCE_MATRIX_2026-10-19.md) records the exact VDOE exports and years. `app/data/snapshot_meta.json` records input hashes, the transform version, and the NCES match result. File retrieval time is not the agency's publication date.

The current source response has a rolling three-year window for enrollment, absence, and meal measures. Five compressed public inputs in `data-raw/historical/` retain the 2022–23 rows from the original repo. The builder uses those rows only if the whole year is absent from the current response; current data win an overlap.

## Performance

The accreditation export provides school-year and subject values for All Students. Its `Accreditation Combined Rate` credits passing and qualifying growth or progress. Its `Percent Passing` is the app's historical SOL pass rate. They are different measures. The app-defined `Overall` value is the arithmetic mean of available subject values for that school or division and year. It is not an official VDOE rating. Science ACR is blank for every available year in this export; the ACR mean therefore uses English and Math. Science pass rates can contribute to the pass-rate mean.

The accreditation export ends at 2024. The 2025–26 SOL results listed by VDOE are a separate data product and were not imported. The new School Performance and Support Framework (SPSF) has different categories and scores. No ACR, pass-rate, or SPSF series is extended by copying another series into it.

## Discipline and attendance

Suspensions per 100 students use `100 × (short-term student count + long-term student count) / enrollment`. Other discipline rates use the relevant student or incident count and the same enrollment denominator. These source exports end at 2023–24. A missing later year means unavailable, not zero incidents. Chronic absenteeism is the reported percentage of students missing at least 10% of school days; its current export reaches 2025–26.

If a needed component is suppressed, the derived total is missing. The builder also removes the underlying numeric value and numerator from a suppressed record. A suppressed or absent value is shown as "No data" and excluded from the map color scale and percentile peer set.

## Division rates and comparisons

Where numerators and denominators are present, a division rate is `100 × sum(numerators) / sum(denominators)` over reportable schools. For rates without public denominators, the builder uses an enrollment-weighted mean over reportable schools. The division performance subject values are enrollment-weighted school means; the division `Overall` value is a simple mean of available division subjects. These derived division values are not official VDOE division rates.

Percentiles use reportable peers for the same year, metric, and school or division comparison group. Ties have the same rank. Missing peers are excluded. For lower-is-better measures, the ranking direction is reversed so a higher percentile means a lower adverse outcome. Neutral meal and demographic measures have no better/worse percentile interpretation.

## Race, geography, and attendance zones

Race and ethnicity shares use available VDOE subgroup percentages. The displayed categories are normalized to total 100% within each school-year; this can hide missing source categories, so use the source for an official count. School points come from NCES 2024–25 geocodes. The name join matched 1,810 of 1,828 VDOE schools in this build. Census 2024 polygons show school divisions. Optional NCES SABS or curated attendance zones can be incomplete or older than the selected metric year; they are not address assignment advice.

The [QA record](REFRESH_QA_2026-10-20.md) gives the build counts, suppression change, local checks, and outstanding source limits.

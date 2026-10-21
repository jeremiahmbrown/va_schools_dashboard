# SBAR sanction action import — QA record

The [VDOE Student Behavior and Administrative Response (SBAR) Sanction
Report](https://p1pe.doe.virginia.gov/buildatable/sbar) was downloaded at
School level, all divisions, all schools, and all sanction types, one school
year at a time. The five original CSVs are in the ignored
`data-raw/input/manual/` folder. Their names, sizes, and SHA-256 hashes are in
`data-raw/sbar_school_sanctions_sources.json`. The checked, compact extract is
`data-raw/sbar_school_sanctions.csv.gz`; it is tracked so the dashboard can
rebuild without a fresh download.

## Definition check

The export has distinct columns for `Number of Sanctions` and `Number of
Students`. The new dashboard measures use **Number of Sanctions**. We sum the
rows with sanction code `OSS` or `ISS` per school and year, then divide by the
same-year checked enrollment and multiply by 100. The 2021–22 denominator is
the separate Fall Membership school total; later years use the checked School
Quality Profiles enrollment.
The resulting unit is **actions per 100 enrolled students**, not a percentage
of students receiving a sanction. It can exceed 100. We do not add the
`Number of Students` values across OSS rows because one student may occur in
more than one row. The School Quality Profiles display for Accawmacke
Elementary in 2025–26 shows 49 out-of-school actions; the matching SBAR school
rows also add to 49. Its checked enrollment is 483, so the new rate is
`100 × 49 / 483 = 10.1` after display rounding.

The source omits a sanction code when its count is zero. The extract assigns
zero to a missing ISS or OSS code only for a school with some other reported
sanction row in that year. A school absent from the whole export gets no
invented zero. Explicit blank source counts fail extraction. The source has
42,136 sanction rows for 8,887 school-years; the extract checks unique school
codes, year labels, nonnegative integer counts, and distinct-student counts
not greater than actions.

## Coverage and limits

| Ending year | Source schools | Matched map schools | Reportable schools with same-year enrollment | Reportable divisions |
| --- | ---: | ---: | ---: | ---: |
| 2022 | 1,737 | 1,673 | 1,673 | 128 |
| 2023 | 1,781 | 1,723 | 1,723 | 130 |
| 2024 | 1,777 | 1,733 | 1,722 | 129 |
| 2025 | 1,793 | 1,769 | 1,754 | 130 |
| 2026 | 1,799 | 1,776 | 1,761 | 130 |

The 2022 rate was added after the independent Fall Membership race and gender
totals agreed for all 1,862 source schools. The 2024 export has no Buchanan
County school rows. In other
years, some SBAR schools do not match the fixed map or lack a same-year
enrollment. A division value uses `100 × sum(actions) / sum(enrollment)` over
its matched, reportable schools. It is a dashboard aggregate, not an official
VDOE division figure. The [coverage audit](DATA_COVERAGE_AUDIT_2026-10-21.md)
shows all metrics and years.

The historical SQP short-term and long-term suspension measures count
students and stop in 2023–24. They remain separate. Their higher levels of
suppression are not filled with SBAR action counts.

## Checks run

`Rscript --vanilla data-raw/extract_sbar_sanctions.R` validated and recorded
the source CSVs. `Rscript --vanilla data-raw/build_snapshot.R` matched schools,
joined denominators, and rebuilt the frozen snapshot. `tests/check_snapshot.R`
checked source labels, positive denominators, Accawmacke's 2022 and 2026 values, and
the recomputed 2026 division values. The other repository checks for year
availability, source validation, and map zones passed. A local Shiny run
showed the new measure and 2026 year in the Behavior controls; the 2026
Accawmacke Profile showed 483 enrollment and 10.1 actions per 100. The
older suspension measure offered only 2023 and 2024.
After adding year-specific Profile enrollment, a local Shiny check showed
Accawmacke's 2022 enrollment as 408 beside its 5.9 out-of-school actions
per 100. The Demographics category also offered 2022 and displayed a
reportable value for that school.

# Data coverage audit — 21 October 2026

This audit counts reportable values in the frozen dashboard snapshot, by
measure and school year. `2026` means 2025–26. The map has 1,810 matched
schools and 130 divisions. A count below 1,810 can mean that the source lacks
a school, its value is suppressed, the school lacks a checked denominator, or
the school did not match the map. These causes must not be changed to zero.
The script `python data-raw/audit_metric_coverage.py --markdown` reproduces
the full table at the end of this document.

## Results and decisions

1. **Recent discipline:** The older School Quality Profiles (SQP) short-term,
   long-term, expulsion, and incident series have only 2022–23 and 2023–24
   source rows. I downloaded five school-level SBAR Sanction Report CSVs,
   checked their fields and counts, and added separate in-school and
   out-of-school **sanction actions per 100 enrolled students** for 2022–23
   through 2025–26. The 2025–26 measure has 1,761 reportable map schools
   and 130 divisions. The new series is not a continuation of the old one:
   it counts actions, while the older suspension measure counts students.
   Multiple OSS rows for one school can include the same student. The 2021–22
   SBAR source is retained but its rate cannot be calculated without checked
   2021–22 enrollment. Buchanan County has no school rows in the 2023–24 SBAR
   export, so its 2024 division rate is missing.
2. **Performance:** Annual SOL pass rates cover 2022–23 through 2025–26.
   The 2021–22 school workbook could add a 2022 point after schema and
   suppression checks. ACR and SQP Percent Passing cover 2021–22 through
   2023–24 only. Science ACR has zero reportable values in all three years;
   the SQP source field is blank. Current accreditation levels and SPSF scores
   cannot be treated as those missing ACR percentages.
3. **Attendance and meals:** Chronic absence, meal eligibility, breakfast
   participation, and lunch participation cover 2022–23 through 2025–26.
   No checked like-for-like 2021–22 export is in this snapshot. Apart from one
   missing breakfast value in each of 2023 and 2024, their source rows are
   reportable. The source may omit a school entirely; the table shows counts
   against the full 1,810-school map for context.
4. **Demographics:** 2024–25 and 2025–26 use checked Fall Membership race
   counts. Seven explicit blank category counts across those two years remain
   missing. The older 2022–23 and 2023–24 values come from normalized SQP
   subgroup shares and are sparse for small categories: Native Hawaiian has
   792 reportable map schools in 2023 and 813 in 2024. A historical Fall
   Membership race export could improve this and add 2021–22. The source
   change means an apparent 2024-to-2025 step is not necessarily a real
   demographic change.

## Manual files that would add coverage

Save files in `data-raw/input/manual/` and keep the original file names when
possible. These files are ignored by Git. The checked extracts and source
hashes are tracked. Do not edit a blank or suppressed source cell.

| Priority | Source and exact selection | Save as | What it could add |
| --- | --- | --- | --- |
| 1 | On the [VDOE SOL results page](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results), open **2021–2022 Test Administration → School Subject-Area (XLS)**. The [direct workbook link](https://www.doe.virginia.gov/home/showpublisheddocument/32602/638047216292570000) may also work in your browser. | `2021-2022 School Subject-Area.xls` or its original download name | A possible 2022 annual SOL pass-rate point, subject to field and suppression checks. Direct file requests in this environment returned HTTP 403. |
| 2 | On the [VDOE Fall Membership Build-A-Table](https://p1pe.doe.virginia.gov/buildatable/fallmembership), choose **2021–22**, **School** level, all divisions and schools, all grades and genders, and download the CSV with the school total count. Also download the same year with race categories and total counts. If the tool has separate report layouts, select the school gender/grade layout for the total and the school race layout for the race rows. | `fall_membership_2022_gender_grade.csv` and `fall_membership_2022_race.csv` | A checked denominator for 2022 SBAR action rates and 2022 demographic shares. The two totals will be reconciled before import. |
| 3 | In the same Fall Membership tool, repeat the school race and school gender/grade exports for **2022–23** and **2023–24** (all divisions and schools). A combined multi-year CSV is also useful if the tool offers one. | Add the school year to each file name, or keep one combined original | Replace the sparse older SQP subgroup approximation with checked race counts. We will compare each year's category sum with the separate school total before changing the dashboard. |

The five SBAR CSVs for 2021–22 through 2025–26 are **already downloaded** and
need no action from you. The 2024–25 and 2025–26 SOL workbooks and Fall
Membership race extracts are also already in checked, tracked extracts.
If the Fall Membership site asks you to pass its human check, complete it in
your browser and continue to the report form. Share the CSV files without
changing their cell values. The historical files above are requests for
additional source data, not a claim that all gaps can be filled.

## Full snapshot coverage

Each table cell is **reportable schools / reportable divisions**. A dash means
the metric has no snapshot rows for that year; `0/0` means rows exist but no
reportable value exists. These are map counts, not official state totals.

| Measure | 2022 | 2023 | 2024 | 2025 | 2026 |
| --- | ---: | ---: | ---: | ---: | ---: |
| ACR — Overall | 1765/130 | 1777/130 | 1781/130 | — | — |
| SQP Percent Passing — Overall | 1765/130 | 1773/130 | 1776/130 | — | — |
| ACR — English (ELA) | 1767/130 | 1777/130 | 1781/130 | — | — |
| ACR — Math | 1765/130 | 1777/130 | 1782/130 | — | — |
| ACR — Science | 0/0 | 0/0 | 0/0 | — | — |
| SQP Percent Passing — English (ELA) | 1767/130 | 1777/130 | 1781/130 | — | — |
| SQP Percent Passing — Math | 1765/130 | 1777/130 | 1782/130 | — | — |
| SQP Percent Passing — Science | 1721/130 | 1729/130 | 1734/130 | — | — |
| Annual SOL Pass Rate — Overall | — | 1764/130 | 1766/130 | 1771/130 | 1769/130 |
| Annual SOL Pass Rate — English (ELA) | — | 1762/130 | 1765/130 | 1770/130 | 1769/130 |
| Annual SOL Pass Rate — Math | — | 1764/130 | 1766/130 | 1771/130 | 1769/130 |
| Annual SOL Pass Rate — Science | — | 1722/130 | 1723/130 | 1725/130 | 1731/130 |
| Suspensions per 100 | — | 470/88 | 515/97 | — | — |
| Short-term suspensions per 100 | — | 1646/128 | 1669/127 | — | — |
| Long-term suspensions per 100 | — | 470/88 | 516/97 | — | — |
| Suspendable incidents per 100 | — | 470/88 | 515/97 | — | — |
| Short-term incidents per 100 | — | 1646/128 | 1669/127 | — | — |
| Long-term incidents per 100 | — | 470/88 | 516/97 | — | — |
| Expulsions per 100 | — | 86/36 | 177/75 | — | — |
| Expellable incidents per 100 | — | 86/36 | 177/75 | — | — |
| Out-of-school actions per 100 (SBAR) | — | 1723/130 | 1722/129 | 1754/130 | 1761/130 |
| In-school actions per 100 (SBAR) | — | 1723/130 | 1722/129 | 1754/130 | 1761/130 |
| Chronic absenteeism | — | 1794/130 | 1783/130 | 1793/130 | 1794/130 |
| Meal eligibility | — | 1780/130 | 1769/130 | 1776/130 | 1780/130 |
| Breakfast participation | — | 1779/130 | 1768/130 | 1776/130 | 1780/130 |
| Lunch participation | — | 1780/130 | 1769/130 | 1776/130 | 1780/130 |
| % Am. Indian | — | 1123/116 | 1116/123 | 1794/130 | 1809/130 |
| % Black | — | 1747/129 | 1758/129 | 1795/130 | 1810/130 |
| % Hispanic | — | 1768/129 | 1776/130 | 1795/130 | 1810/130 |
| % White | — | 1776/130 | 1783/130 | 1795/130 | 1810/130 |
| % Asian | — | 1587/128 | 1588/129 | 1795/130 | 1808/130 |
| % Nat. Hawaiian | — | 792/97 | 813/100 | 1793/130 | 1809/130 |
| % Two+ | — | 1771/130 | 1777/130 | 1795/130 | 1810/130 |

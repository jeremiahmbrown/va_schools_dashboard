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
   out-of-school **sanction actions per 100 enrolled students** for 2021–22
   through 2025–26. The 2021–22 rate now uses checked same-year Fall
   Membership totals and covers 1,673 mapped schools and 128 divisions. The
   2025–26 measure covers 1,761 schools and 130 divisions. The new series is
   not a continuation of the old one:
   it counts actions, while the older suspension measure counts students.
   Multiple OSS rows for one school can include the same student. Buchanan
   County has no school rows in the 2023–24 SBAR
   export, so its 2024 division rate is missing.
2. **Performance:** Annual SOL pass rates cover 2022–23 through 2025–26.
   A 2021–22 school workbook found in Downloads has pass rates but lacks the
   tested-student numerators and denominators available in the newer files;
   its 2022 school and division import needs separate method checks. ACR and
   SQP Percent Passing cover 2021–22 through
   2023–24 only. Science ACR has zero reportable values in all three years;
   the SQP source field is blank. Current accreditation levels and SPSF scores
   cannot be treated as those missing ACR percentages.
3. **Attendance and meals:** Chronic absence, meal eligibility, breakfast
   participation, and lunch participation cover 2022–23 through 2025–26.
   No checked like-for-like 2021–22 export is in this snapshot. Apart from one
   missing breakfast value in each of 2023 and 2024, their source rows are
   reportable. The source may omit a school entirely; the table shows counts
   against the full 1,810-school map for context.
4. **Demographics:** All five years now use VDOE Fall Membership race counts.
   The 2021–22 and 2023–24 race totals match independent gender totals for
   every school. The 2022–23 gender export omits the `Other` category at 327
   schools. For that year, the importer uses race totals only where they match
   the separate SQP school enrollment; mismatches remain missing. Explicit
   blank race counts remain suppressed. For example, Native Hawaiian coverage
   rose from 792 to more than 1,700 mapped schools in 2023 after this change.

## Manual files that would add coverage

Save files in `data-raw/input/manual/` and keep the original file names when
possible. These files are ignored by Git. The checked extracts and source
hashes are tracked. Do not edit a blank or suppressed source cell.

| Priority | Source and exact selection | Save as | What it could add |
| --- | --- | --- | --- |
| 1 | The [VDOE SOL results page](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results) lists **2021–2022 Test Administration → Division Subject-Area (XLS)**. Download that division workbook if a verified 2022 division pass-rate map is wanted. The school workbook already found in Downloads has only pass rates, so it cannot support the newer tested-student weighted division calculation by itself. | Keep the original download name | A separate, clearly sourced 2022 division annual SOL value after schema and definition checks. |

The five SBAR CSVs and the Fall Membership school race and gender exports are
already checked and imported; no further Fall Membership download is needed.
The user-supplied 2021–22 school-level file in Downloads has been inspected,
but its rate-only schema needs a separate method decision before release. A
file named `2020-2021-school-subject-area.xlsx` in the older WSL input folder
is for a different school year and was not substituted for 2021–22.

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
| Out-of-school actions per 100 (SBAR) | 1673/128 | 1723/130 | 1722/129 | 1754/130 | 1761/130 |
| In-school actions per 100 (SBAR) | 1673/128 | 1723/130 | 1722/129 | 1754/130 | 1761/130 |
| Chronic absenteeism | — | 1794/130 | 1783/130 | 1793/130 | 1794/130 |
| Meal eligibility | — | 1780/130 | 1769/130 | 1776/130 | 1780/130 |
| Breakfast participation | — | 1779/130 | 1768/130 | 1776/130 | 1780/130 |
| Lunch participation | — | 1780/130 | 1769/130 | 1776/130 | 1780/130 |
| % Am. Indian | 1754/129 | 1754/130 | 1787/130 | 1794/130 | 1809/130 |
| % Black | 1756/129 | 1755/130 | 1789/130 | 1795/130 | 1810/130 |
| % Hispanic | 1757/129 | 1755/130 | 1789/130 | 1795/130 | 1810/130 |
| % White | 1757/129 | 1755/130 | 1789/130 | 1795/130 | 1810/130 |
| % Asian | 1757/129 | 1755/130 | 1789/130 | 1795/130 | 1808/130 |
| % Nat. Hawaiian | 1754/129 | 1749/130 | 1785/130 | 1793/130 | 1809/130 |
| % Two+ | 1756/129 | 1755/130 | 1789/130 | 1795/130 | 1810/130 |

# Methodology Notes

## Overall Performance Index
The default “Overall Performance Index” shown in the dashboard is a **computed composite** built from VDOE School Quality Profiles accreditation exports.

Key points:
- The index is computed as the **mean** of `Accreditation Combined Rate` across:
  - Academic Achievement - English
  - Academic Achievement - Math
  - Academic Achievement - Science
  for **All Students**, at the **school level**, for each year.
- If any of those subject components are suppressed/missing for a school-year, the index is treated as missing/suppressed.

This is designed to be transparent and reproducible from public downloads, even if VDOE’s internal methodologies or labels change over time.

## Race/Ethnicity Percentages
Race/ethnicity percentages are sourced from VDOE’s “Short Term Suspensions” download export field `Percent of the Student Population` for each race subgroup.

To ensure the race category is internally consistent in the UI, these values are **normalized to sum to 100** within each school-year across the available race subgroups (handles rounding and partial missingness).

## Division Aggregation
Division choropleth values are derived from school-level data using transparent aggregation rules:
- When numerator/denominator are available:
  - Use totals-based rates (e.g., `100 * sum(numerator) / sum(denominator)`).
- When only school-level rates are available:
  - Use an enrollment-weighted mean.

The app should surface basic aggregation metadata in tooltips (`agg_method`, included vs suppressed counts).

## Suppression / Missingness
Some metrics may be suppressed (“too small to report”) or missing for other reasons.
- Suppressed values are stored as `value = NA` with `suppressed = TRUE`.
- Suppressed/missing values are rendered as “No data” and excluded from numeric color scaling.

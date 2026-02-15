# Methodology Notes

## Performance Metrics (ACR + SOL Pass Rate)
The dashboard supports two **subject-level** performance measures from VDOE School Quality Profiles accreditation exports (All Students):
- **Accreditation Combined Rate (ACR)** (credits both passing and growth/progress)
- **SOL Pass Rate** (Percent Passing; proficiency-only)

Key points:
- Subject-level rows are kept at `school × year × subject × subgroup` for:
  - English (ELA)
  - Math
  - Science
- The “Overall” values shown in the app are **app-defined composites** computed as the **mean across subjects** for the selected metric (ACR or Pass Rate).
- If subject components are suppressed/missing for a school-year, the overall value is computed over the available subjects, and the app tracks how many subjects were present.

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

For performance specifically:
- Division subject-level values are computed as **enrollment-weighted averages across schools**.
- Division overall values are computed as the **mean across the division’s subject-level values** (not a direct aggregation of school-level overall scores).

## Suppression / Missingness
Some metrics may be suppressed (“too small to report”) or missing for other reasons.
- Suppressed values are stored as `value = NA` with `suppressed = TRUE`.
- Suppressed/missing values are rendered as “No data” and excluded from numeric color scaling.

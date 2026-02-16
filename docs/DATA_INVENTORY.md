# Data Inventory (Snapshot + Cached Raw Exports)

- Generated: 2026-02-15 18:57:04 EST
- Snapshot dir: `/home/jeremiah/code/va_schools_dashboard/app/data`
- Raw cache dir: `/home/jeremiah/code/va_schools_dashboard/data-raw/input`

## Metric Categories Present

- attendance, behavior, demographics, needs, test

## Behavior / Attendance Metrics Present (Snapshot)

| metric_id | label | unit | better_direction |
| --- | --- | --- | --- |
| behavior_susp_per_100 | Suspensions per 100 | per_100 | lower_better |
| behavior_susp_short_per_100 | Short-term suspensions per 100 | per_100 | lower_better |
| behavior_susp_long_per_100 | Long-term suspensions per 100 | per_100 | lower_better |
| behavior_susp_incidents_per_100 | Suspendable incidents per 100 | per_100 | lower_better |
| behavior_susp_incidents_short_per_100 | Short-term incidents per 100 | per_100 | lower_better |
| behavior_susp_incidents_long_per_100 | Long-term incidents per 100 | per_100 | lower_better |
| behavior_expulsions_per_100 | Expulsions per 100 | per_100 | lower_better |
| behavior_expulsion_incidents_per_100 | Expellable incidents per 100 | per_100 | lower_better |
| attendance_chronic_absent_pct | Chronic absenteeism | pct | lower_better |

## Demographics Metrics Present (Snapshot)

| metric_id | label | unit | better_direction |
| --- | --- | --- | --- |
| demo_american_indian_pct | % Am. Indian | pct | neutral |
| demo_black_pct | % Black | pct | neutral |
| demo_hispanic_pct | % Hispanic | pct | neutral |
| demo_white_pct | % White | pct | neutral |
| demo_asian_pct | % Asian | pct | neutral |
| demo_native_hawaiian_pct | % Nat. Hawaiian | pct | neutral |
| demo_two_plus_pct | % Two+ | pct | neutral |

## Student Needs / Programs Metrics Present (Snapshot)

| metric_id | label | unit | better_direction |
| --- | --- | --- | --- |
| needs_meal_eligibility_pct | Meal eligibility | pct | neutral |
| needs_breakfast_participation_pct | Breakfast participation | pct | neutral |
| needs_lunch_participation_pct | Lunch participation | pct | neutral |

## Metric Units Present (school_metrics)

| metric_id | unit |
| --- | --- |
| attendance_chronic_absent_pct | pct |
| behavior_expulsion_incidents_per_100 | per_100 |
| behavior_expulsions_per_100 | per_100 |
| behavior_susp_incidents_long_per_100 | per_100 |
| behavior_susp_incidents_per_100 | per_100 |
| behavior_susp_incidents_short_per_100 | per_100 |
| behavior_susp_long_per_100 | per_100 |
| behavior_susp_per_100 | per_100 |
| behavior_susp_short_per_100 | per_100 |
| demo_american_indian_pct | pct |
| demo_asian_pct | pct |
| demo_black_pct | pct |
| demo_hispanic_pct | pct |
| demo_native_hawaiian_pct | pct |
| demo_two_plus_pct | pct |
| demo_white_pct | pct |
| needs_breakfast_participation_pct | pct |
| needs_lunch_participation_pct | pct |
| needs_meal_eligibility_pct | pct |
| test_acr_ela | index |
| test_acr_math | index |
| test_acr_science | index |
| test_overall_pass_index | index |
| test_overall_perf_index | index |
| test_pass_ela | index |
| test_pass_math | index |
| test_pass_science | index |

## Cached Raw Export Headers (if present)

| file | n_cols | columns |
| --- | --- | --- |
| vdoe_short_term_suspensions.csv |  9 | Year, School, Division, Subgroup, Level, "Percent of the Student Population", "Number Suspended Short Term", "Number of Short Term Suspendable Incidents", "Percent of Short Term Suspensions" |
| vdoe_long_term_suspensions.csv |  9 | Year, School, Division, Subgroup, Level, "Percent of the Student Population", "Number Suspended Long Term", "Number of Long Term Suspendable Incidents", "Percent of Long Term Suspensions" |
| vdoe_enrollment.csv |  6 | Year, Level, Division, School, Grade, Count |
| vdoe_accreditation_english.csv | 12 | Year, Division, School, Indicator, Subgroup, "Level Code", "Percent Passing", "Percent Passing With Recovery", "Percent Showing Growth", "Percent Showing EL Progress or Proficiency", "Accreditation Combined Rate", "No Proficiency or Growth" |
| vdoe_accreditation_math.csv | 12 | Year, Division, School, Indicator, Subgroup, "Level Code", "Percent Passing", "Percent Passing With Recovery", "Percent Showing Growth", "Percent Showing EL Progress or Proficiency", "Accreditation Combined Rate", "No Proficiency or Growth" |
| vdoe_accreditation_science.csv | 12 | Year, Division, School, Indicator, Subgroup, "Level Code", "Percent Passing", "Percent Passing With Recovery", "Percent Showing Growth", "Percent Showing EL Progress or Proficiency", "Accreditation Combined Rate", "No Proficiency or Growth" |
| vdoe_absenteeism.csv |  9 | Year, Level, Division, School, Subgroup, "Count Below 10", "Count Above 10", "Percent Below 10", "Percent above 10" |
| vdoe_expulsions.csv |  9 | Year, School, Division, Subgroup, Level, "Percent of the Student Population", "Number Expelled", "Number of Expellable Incidents", "Percent of Expelled" |
| vdoe_fr_eligibility.csv |  5 | Year, Division, School, Level, Percent |
| vdoe_fr_breakfast.csv |  5 | Year, Division, School, Level, Percent |
| vdoe_fr_lunch.csv |  5 | Year, Division, School, Level, Percent |

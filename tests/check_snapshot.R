# Run from the repository root: Rscript tests/check_snapshot.R
source("app/R/percentiles.R")

stopifnot(isTRUE(all.equal(percentile_scores(c(10, 20, NA, 20), rep("lower_better", 4)),
                           c(100, 100 * 2 / 3, NA_real_, 100 * 2 / 3))))
stopifnot(isTRUE(all.equal(percentile_scores(c(10, 20, NA), rep("higher_better", 3)),
                           c(50, 100, NA_real_))))

schools <- read.csv("app/data/schools.csv", colClasses = c(school_id = "character", division_id = "character"))
school <- read.csv("app/data/school_metrics.csv", colClasses = c(school_id = "character"))
enrollment_by_year <- read.csv("app/data/enrollment_by_year.csv",
                               colClasses = c(school_id = "character"))
division <- read.csv("app/data/division_metrics.csv", colClasses = c(division_id = "character"))
defs <- read.csv("app/data/metric_defs.csv")

stopifnot(nrow(school) > 100000, nrow(schools) > 1700)
stopifnot(!anyDuplicated(schools$school_id))
stopifnot(!anyDuplicated(school[c("school_id", "year", "metric_id")]))
stopifnot(!anyDuplicated(enrollment_by_year[c("school_id", "year")]))
stopifnot(all(enrollment_by_year$school_id %in% schools$school_id),
          all(is.finite(enrollment_by_year$enrollment) &
                enrollment_by_year$enrollment > 0))
stopifnot(!anyDuplicated(division[c("division_id", "year", "metric_id")]))
stopifnot(all(school$school_id %in% schools$school_id))
stopifnot(all(school$metric_id %in% defs$metric_id))
stopifnot(all(is.finite(school$value) | school$suppressed %in% TRUE))
stopifnot(!any(is.finite(school$value) & school$suppressed %in% TRUE))
stopifnot(all(is.finite(division$value) | division$suppressed %in% TRUE))
stopifnot(!any(is.finite(division$value) & division$suppressed %in% TRUE))

pct <- school$unit %in% c("pct", "index") & is.finite(school$value)
stopifnot(all(school$value[pct] >= 0 & school$value[pct] <= 100))
stopifnot(all(school$year >= 2022 & school$year <= 2026))
stopifnot(any(school$year == 2026 & school$metric_id == "attendance_chronic_absent_pct" & is.finite(school$value)))
stopifnot(any(school$year == 2026 & school$metric_id == "needs_meal_eligibility_pct" & is.finite(school$value)))
stopifnot(!any(school$year > 2024 & school$metric_id %in% c(
  "test_overall_perf_index", "test_overall_pass_index",
  paste0("test_acr_", c("ela", "math", "science")),
  paste0("test_pass_", c("ela", "math", "science"))
)))
stopifnot(any(school$year == 2025 & school$metric_id == "test_sol_annual_overall" &
              is.finite(school$value)))
stopifnot(any(school$year == 2026 & school$metric_id == "test_sol_annual_overall" &
              is.finite(school$value)))
race_ids <- defs$metric_id[defs$category == 'demographics']
stopifnot(length(race_ids) == 7)
for (target_year in 2022:2024) {
  historical_race <- subset(school, year == target_year & metric_id %in% race_ids)
  stopifnot(nrow(historical_race) > 12000,
            all(historical_race$source == 'vdoe_fall_membership_race_school_historical'),
            sum(is.finite(historical_race$value)) > 12000)
}
for (target_year in 2025:2026) {
  recent_race <- subset(school, year == target_year & metric_id %in% race_ids)
  stopifnot(nrow(recent_race) > 12000,
            all(recent_race$source == 'vdoe_fall_membership_race_school'),
            sum(is.finite(recent_race$value)) > 12000)
}
stopifnot(!any(school$year <= 2024 &
                 school$source == 'vdoe_fall_membership_race_school'))
accawmacke <- subset(schools, school_name == 'Accawmacke Elementary' &
                       division_name == 'Accomack County Public Schools')
stopifnot(nrow(accawmacke) == 1, accawmacke$enrollment_latest == 483)
stopifnot(!any(school$year > 2024 &
                 (startsWith(school$metric_id, "behavior_susp") |
                  startsWith(school$metric_id, "behavior_exp"))))
sbar_ids <- c('behavior_sbar_oss_actions_per_100',
              'behavior_sbar_iss_actions_per_100')
stopifnot(defs$format[match('test_sol_annual_overall', defs$metric_id)] == 'pct_1',
          defs$format[match('behavior_susp_per_100', defs$metric_id)] == 'num_1',
          defs$format[match(sbar_ids[[1]], defs$metric_id)] == 'num_1')
for (target_year in 2022:2026) {
  sbar_rows <- subset(school, year == target_year & metric_id %in% sbar_ids)
  stopifnot(nrow(sbar_rows) > 3300,
            all(sbar_rows$source == 'vdoe_sbar_school_sanctions+vdoe_enrollment'),
            all(is.finite(sbar_rows$value)),
            all(sbar_rows$numerator >= 0 & sbar_rows$denominator > 0))
}
accawmacke_sbar_2022 <- subset(school, school_id == accawmacke$school_id &
  year == 2022 & metric_id == 'behavior_sbar_oss_actions_per_100')
stopifnot(nrow(accawmacke_sbar_2022) == 1,
          accawmacke_sbar_2022$numerator == 24,
          accawmacke_sbar_2022$denominator == 408,
          abs(accawmacke_sbar_2022$value - 100 * 24 / 408) < 1e-8)
stopifnot(enrollment_by_year$enrollment[
  enrollment_by_year$school_id == accawmacke$school_id &
    enrollment_by_year$year == 2022] == 408)
accawmacke_sbar <- subset(school, school_id == accawmacke$school_id &
                            year == 2026 &
                            metric_id == 'behavior_sbar_oss_actions_per_100')
stopifnot(nrow(accawmacke_sbar) == 1,
          accawmacke_sbar$numerator == 49,
          accawmacke_sbar$denominator == 483,
          abs(accawmacke_sbar$value - 100 * 49 / 483) < 1e-8)
stopifnot(file.exists("app/data/zones/zone_sources.csv"))
stopifnot(length(list.files("app/data/zones/sabs", pattern = "[.]json$")) > 100)

# Recompute published division rates from the unsuppressed school numerators.
school_division <- merge(school, schools[c("school_id", "division_id")], by = "school_id")
for (metric in c("attendance_chronic_absent_pct", "needs_meal_eligibility_pct", "test_sol_annual_ela", "demo_black_pct", "behavior_sbar_oss_actions_per_100")) {
  rows <- subset(school_division, metric_id == metric & year == 2026 &
                   is.finite(numerator) & is.finite(denominator) & denominator > 0)
  totals <- aggregate(cbind(numerator, denominator) ~ division_id + year,
                      data = rows, FUN = sum)
  totals$expected <- 100 * totals$numerator / totals$denominator
  actual <- subset(division, metric_id == metric & year == 2026 & is.finite(value))
  joined <- merge(totals, actual, by = c("division_id", "year"))
  stopifnot(nrow(joined) > 100, all(abs(joined$expected - joined$value) < 1e-6))
}

cat("Snapshot and percentile checks passed.\n")

# Run from the repository root: Rscript tests/check_snapshot.R
source("app/R/percentiles.R")

stopifnot(isTRUE(all.equal(percentile_scores(c(10, 20, NA, 20), rep("lower_better", 4)),
                           c(100, 100 * 2 / 3, NA_real_, 100 * 2 / 3))))
stopifnot(isTRUE(all.equal(percentile_scores(c(10, 20, NA), rep("higher_better", 3)),
                           c(50, 100, NA_real_))))

schools <- read.csv("app/data/schools.csv", colClasses = c(school_id = "character", division_id = "character"))
school <- read.csv("app/data/school_metrics.csv", colClasses = c(school_id = "character"))
division <- read.csv("app/data/division_metrics.csv", colClasses = c(division_id = "character"))
defs <- read.csv("app/data/metric_defs.csv")

stopifnot(nrow(school) > 100000, nrow(schools) > 1700)
stopifnot(!anyDuplicated(schools$school_id))
stopifnot(!anyDuplicated(school[c("school_id", "year", "metric_id")]))
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
stopifnot(!any(school$year > 2024 & startsWith(school$metric_id, "test_")))
stopifnot(!any(school$year > 2024 & startsWith(school$metric_id, "behavior_")))
stopifnot(file.exists("app/data/zones/zone_sources.csv"))
stopifnot(length(list.files("app/data/zones/sabs", pattern = "[.]json$")) > 100)

# Recompute published division rates from the unsuppressed school numerators.
school_division <- merge(school, schools[c("school_id", "division_id")], by = "school_id")
for (metric in c("attendance_chronic_absent_pct", "needs_meal_eligibility_pct")) {
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

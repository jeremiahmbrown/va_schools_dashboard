# Percentiles compare finite peer values within the same metric and year.
# A high score means a better outcome for directional metrics.
suppressPackageStartupMessages(library(dplyr))

percentile_scores <- function(value, better_direction) {
  out <- rep(NA_real_, length(value))
  valid <- is.finite(value)
  if (!any(valid)) return(out)
  adjusted <- ifelse(better_direction[valid] == "lower_better", -value[valid], value[valid])
  out[valid] <- 100 * dplyr::cume_dist(adjusted)
  out
}

compute_school_percentiles <- function(school_metrics, schools, metric_defs) {
  defs <- metric_defs %>% distinct(metric_id, better_direction)
  base <- school_metrics %>%
    left_join(schools %>% select(school_id, division_id), by = "school_id") %>%
    left_join(defs, by = "metric_id")

  pct_state <- base %>%
    group_by(year, metric_id) %>%
    mutate(pct_state = percentile_scores(value, better_direction)) %>%
    ungroup() %>%
    select(school_id, year, metric_id, pct_state)

  pct_division <- base %>%
    group_by(division_id, year, metric_id) %>%
    mutate(pct_division = percentile_scores(value, better_direction)) %>%
    ungroup() %>%
    select(school_id, year, metric_id, pct_division)

  list(state = pct_state, division = pct_division)
}

compute_division_percentiles <- function(division_metrics, metric_defs) {
  defs <- metric_defs %>% distinct(metric_id, better_direction)
  division_metrics %>%
    left_join(defs, by = "metric_id") %>%
    group_by(year, metric_id) %>%
    mutate(pct_state = percentile_scores(value, better_direction)) %>%
    ungroup() %>%
    select(division_id, year, metric_id, pct_state)
}

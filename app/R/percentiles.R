# ==============================================================================
# Script: percentiles.R
# Purpose:
#   Percentile helpers used by the detail panels.
#
#   We compute empirical percentiles for each metric/year distribution and
#   display them in the UI.
#
# Inputs:
#   - `metric_defs` (for `better_direction`)
#   - Metrics fact tables (school_metrics, division_metrics)
#   - `schools` lookup (to map schools -> division_id for within-division ranks)
#
# Outputs:
#   - Data frames with percentile columns keyed by entity/year/metric_id.
#
# Key Details / Debugging:
#   - We use `dplyr::cume_dist` (ECDF) so ties are stable and interpret as:
#       percentile = % of peers with value <= this value
#   - We do not invert "lower is better" metrics; percentiles are purely
#     distributional (higher percentile = higher observed value).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

compute_school_percentiles <- function(school_metrics, schools, metric_defs) {
  defs <- metric_defs %>% distinct(metric_id, better_direction)

  base <- school_metrics %>%
    left_join(schools %>% select(school_id, division_id), by = "school_id") %>%
    left_join(defs, by = "metric_id")

  pct_state <- base %>%
    group_by(year, metric_id) %>%
    mutate(pct_state_raw = ifelse(is.finite(value), 100 * dplyr::cume_dist(value), NA_real_)) %>%
    ungroup() %>%
    mutate(pct_state = pct_state_raw) %>%
    select(school_id, year, metric_id, pct_state)

  pct_division <- base %>%
    group_by(division_id, year, metric_id) %>%
    mutate(pct_division_raw = ifelse(is.finite(value), 100 * dplyr::cume_dist(value), NA_real_)) %>%
    ungroup() %>%
    mutate(pct_division = pct_division_raw) %>%
    select(school_id, year, metric_id, pct_division)

  list(state = pct_state, division = pct_division)
}

compute_division_percentiles <- function(division_metrics, metric_defs) {
  defs <- metric_defs %>% distinct(metric_id, better_direction)
  base <- division_metrics %>%
    left_join(defs, by = "metric_id")

  pct_state <- base %>%
    group_by(year, metric_id) %>%
    mutate(pct_state_raw = ifelse(is.finite(value), 100 * dplyr::cume_dist(value), NA_real_)) %>%
    ungroup() %>%
    mutate(pct_state = pct_state_raw) %>%
    select(division_id, year, metric_id, pct_state)

  pct_state
}

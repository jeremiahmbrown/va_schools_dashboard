# A year is offered only when at least one school or division has a
# reportable value for the selected measure.
reportable_metric_years <- function(school_metrics, division_metrics) {
  reportable <- function(rows) {
    rows[is.finite(rows$value) & !(rows$suppressed %in% TRUE),
         c("metric_id", "year"), drop = FALSE]
  }
  unique(rbind(reportable(school_metrics), reportable(division_metrics)))
}

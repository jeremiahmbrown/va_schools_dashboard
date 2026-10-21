# Build a separate SBAR action-rate series. The legacy SQP short/long-term
# suspension and incident measures keep their original definitions and years.
build_sbar_sanction_metrics <- function(archive, schools_out, enroll_keep,
                                        matched, years_keep, norm_key,
                                        historical_enrollment_archive = NULL) {
  if (!file.exists(archive)) stop('Missing checked SBAR archive: ', archive)
  raw <- read.csv(gzfile(archive), stringsAsFactors = FALSE,
                  colClasses = c(division_code = 'character',
                                 school_code = 'character'))
  expected <- c('year', 'division_code', 'division_name', 'school_code',
                'school_name', 'source_file', 'metric_id', 'actions')
  if (!identical(names(raw), expected) ||
      !identical(sort(unique(raw$year)), 2022:2026) ||
      anyDuplicated(raw[c('year', 'division_code', 'school_code', 'metric_id')]) ||
      any(!raw$metric_id %in% c('behavior_sbar_oss_actions_per_100',
                                'behavior_sbar_iss_actions_per_100')) ||
      any(!is.finite(raw$actions) | raw$actions < 0)) {
    stop('Invalid checked SBAR archive.')
  }
  school_lookup <- schools_out %>%
    dplyr::transmute(school_id,
      division_key = norm_key(sub(' Public Schools$', '', division_name,
                                  ignore.case = TRUE)),
      school_key = norm_key(school_name))
  if (anyDuplicated(school_lookup[c('division_key', 'school_key')])) {
    stop('Ambiguous dashboard school names in SBAR join.')
  }
  joined <- raw %>%
    dplyr::mutate(division_key = norm_key(division_name),
                  school_key = norm_key(school_name)) %>%
    dplyr::left_join(school_lookup, by = c('division_key', 'school_key'))
  coverage <- joined %>%
    dplyr::distinct(year, division_code, school_code, school_id) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(source_schools = dplyr::n(),
                     matched_schools = sum(!is.na(school_id)),
                     .groups = 'drop')
  for (i in seq_len(nrow(coverage))) {
    message(sprintf('SBAR school match %d: %d / %d (%.1f%%)',
      coverage$year[[i]], coverage$matched_schools[[i]],
      coverage$source_schools[[i]],
      100 * coverage$matched_schools[[i]] / coverage$source_schools[[i]]))
  }
  if (any(coverage$matched_schools / coverage$source_schools < 0.95)) {
    stop('SBAR school match is below the 95% build gate.')
  }
  enrollment <- enroll_keep %>%
    dplyr::left_join(matched %>%
      dplyr::select(division_name, school_name, school_id),
      by = c('division_name', 'school_name')) %>%
    dplyr::filter(!is.na(school_id), is.finite(enrollment), enrollment > 0) %>%
    dplyr::select(school_id, year, enrollment)
  if (anyDuplicated(enrollment[c('school_id', 'year')])) {
    stop('Duplicate school-year enrollment in SBAR denominator join.')
  }
  with_denominator <- joined %>%
    dplyr::left_join(enrollment, by = c('school_id', 'year'))
  if (!is.null(historical_enrollment_archive)) {
    historical <- read.csv(gzfile(historical_enrollment_archive),
      stringsAsFactors = FALSE,
      colClasses = c(division_code = 'character', school_code = 'character'))
    if (!identical(names(historical),
                   c('year', 'division_code', 'school_code', 'enrollment')) ||
        !identical(unique(historical$year), 2022L) ||
        anyDuplicated(historical[c('year', 'division_code', 'school_code')]) ||
        any(!is.finite(historical$enrollment) | historical$enrollment <= 0)) {
      stop('Invalid checked 2021–22 Fall Membership school totals.')
    }
    with_denominator <- with_denominator %>%
      dplyr::left_join(historical %>%
        dplyr::rename(historical_enrollment = enrollment),
        by = c('year', 'division_code', 'school_code')) %>%
      dplyr::mutate(enrollment = dplyr::coalesce(enrollment,
                                                historical_enrollment)) %>%
      dplyr::select(-historical_enrollment)
  }
  metrics <- with_denominator %>%
    dplyr::filter(!is.na(school_id), year %in% years_keep,
                  is.finite(enrollment), enrollment > 0) %>%
    dplyr::transmute(school_id, year, metric_id,
      value = 100 * actions / enrollment,
      unit = 'per_100', suppressed = FALSE,
      numerator = as.numeric(actions), denominator = as.numeric(enrollment),
      source = 'vdoe_sbar_school_sanctions+vdoe_enrollment')
  if (anyDuplicated(metrics[c('school_id', 'year', 'metric_id')])) {
    stop('Duplicate SBAR dashboard metric keys.')
  }
  list(metrics = metrics, coverage = coverage,
       unmatched = joined %>% dplyr::filter(is.na(school_id)) %>%
         dplyr::distinct(year, division_code, division_name,
                         school_code, school_name),
       missing_denominator = with_denominator %>%
         dplyr::filter(!is.na(school_id), !is.finite(enrollment)) %>%
         dplyr::distinct(year, school_id))
}

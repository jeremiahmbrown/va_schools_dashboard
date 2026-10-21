# Build dashboard race shares from the checked Fall Membership count extract.
build_membership_race_metrics <- function(archive, schools_out, years_keep,
                                          norm_key,
                                          expected_years = 2025:2026,
                                          source_id = 'vdoe_fall_membership_race_school',
                                          min_match = 0.95,
                                          enroll_keep = NULL,
                                          matched = NULL) {
  if (!file.exists(archive)) stop('Missing Fall Membership race extract: ', archive)
  raw <- read.csv(gzfile(archive), stringsAsFactors = FALSE,
                  na.strings = c('', 'NA'),
                  colClasses = c(division_code = 'character',
                                 school_code = 'character'))
  expected <- c('year', 'division_code', 'division_name', 'school_code',
                'school_name', 'metric_id', 'count', 'denominator',
                'suppressed', 'source_file')
  if (!identical(names(raw), expected) ||
      !identical(sort(unique(raw$year)), expected_years) ||
      anyDuplicated(raw[c('year', 'division_code', 'school_code', 'metric_id')])) {
    stop('Invalid Fall Membership race extract schema, years, or keys.')
  }
  if (!all(raw$metric_id %in% c('demo_american_indian_pct', 'demo_asian_pct',
      'demo_black_pct', 'demo_hispanic_pct', 'demo_native_hawaiian_pct',
      'demo_two_plus_pct', 'demo_white_pct')) ||
      any(!is.finite(raw$denominator) | raw$denominator <= 0) ||
      any(!raw$suppressed & (!is.finite(raw$count) | raw$count < 0 |
                             raw$count > raw$denominator)) ||
      any(raw$suppressed & is.finite(raw$count))) {
    stop('Invalid Fall Membership race category or count.')
  }
  school_lookup <- schools_out %>%
    dplyr::transmute(
      school_id,
      division_key = norm_key(sub(' Public Schools$', '', division_name,
                                  ignore.case = TRUE)),
      school_key = norm_key(school_name)
    )
  if (anyDuplicated(school_lookup[c('division_key', 'school_key')])) {
    stop('Ambiguous dashboard school names in Fall Membership join.')
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
    message(sprintf('Fall Membership race match %d: %d / %d (%.1f%%)',
      coverage$year[[i]], coverage$matched_schools[[i]],
      coverage$source_schools[[i]],
      100 * coverage$matched_schools[[i]] / coverage$source_schools[[i]]))
  }
  if (any(coverage$matched_schools / coverage$source_schools < min_match)) {
    stop('Fall Membership race school match is below the build gate.')
  }
  eligible <- joined %>%
    dplyr::filter(!is.na(school_id), year %in% years_keep)
  if (!is.null(enroll_keep) && !is.null(matched)) {
    # The 2022–23 gender file omits Other. Require the race sum to agree with
    # the separately checked SQP school total before using that year's shares.
    sqp_totals <- enroll_keep %>%
      dplyr::left_join(matched %>%
        dplyr::select(division_name, school_name, school_id),
        by = c('division_name', 'school_name')) %>%
      dplyr::filter(year == 2023, !is.na(school_id)) %>%
      dplyr::select(school_id, year, sqp_enrollment = enrollment)
    if (anyDuplicated(sqp_totals[c('school_id', 'year')])) {
      stop('Duplicate SQP enrollment in historical race check.')
    }
    eligible <- eligible %>%
      dplyr::left_join(sqp_totals, by = c('school_id', 'year')) %>%
      dplyr::filter(year != 2023 |
        (is.finite(sqp_enrollment) & denominator == sqp_enrollment)) %>%
      dplyr::select(-sqp_enrollment)
  }
  metrics <- eligible %>%
    dplyr::transmute(school_id, year, metric_id,
      value = ifelse(!suppressed, 100 * count / denominator, NA_real_),
      unit = 'pct', suppressed,
      numerator = ifelse(!suppressed, as.numeric(count), NA_real_),
      denominator = ifelse(!suppressed, as.numeric(denominator), NA_real_),
      source = source_id)
  if (anyDuplicated(metrics[c('school_id', 'year', 'metric_id')])) {
    stop('Duplicate Fall Membership dashboard metric keys.')
  }
  list(metrics = metrics, coverage = coverage,
       unmatched = joined %>% dplyr::filter(is.na(school_id)) %>%
         dplyr::distinct(year, division_code, division_name,
                         school_code, school_name))
}

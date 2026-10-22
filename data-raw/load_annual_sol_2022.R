# Add reported 2021–22 school and division rates from the older VDOE exports.
# The source has no test counts, so division values come from its division file.
build_annual_sol_2022_metrics <- function(school_archive, division_archive,
                                          schools_out, years_keep, norm_key) {
  if (!file.exists(school_archive) || !file.exists(division_archive)) {
    stop('Missing checked 2021–22 SOL extracts.')
  }
  read_checked <- function(path, level) {
    classes <- if (level == 'school') {
      c(division_code = 'character', school_code = 'character')
    } else c(division_code = 'character')
    x <- read.csv(gzfile(path), stringsAsFactors = FALSE,
                  na.strings = c('', 'NA'), colClasses = classes)
    expected <- if (level == 'school') {
      c('division_code', 'division_name', 'school_code', 'school_name',
        'subject', 'year', 'pass_rate', 'suppressed', 'source_file')
    } else {
      c('division_code', 'division_name', 'subject', 'year',
        'pass_rate', 'suppressed', 'source_file')
    }
    keys <- if (level == 'school') {
      c('division_code', 'school_code', 'subject')
    } else c('division_code', 'subject')
    if (!identical(names(x), expected) || !identical(unique(x$year), 2022L) ||
        anyDuplicated(x[keys]) || anyNA(x$suppressed) ||
        !all(x$subject %in% c('English: Reading', 'Mathematics', 'Science')) ||
        anyNA(x[keys]) ||
        any(!x$suppressed & (!is.finite(x$pass_rate) |
                             x$pass_rate < 0 | x$pass_rate > 100)) ||
        any(x$suppressed & is.finite(x$pass_rate))) {
      stop('Invalid checked 2021–22 SOL ', level, ' extract.')
    }
    x
  }
  school <- read_checked(school_archive, 'school')
  division <- read_checked(division_archive, 'division')
  subject_id <- c('English: Reading' = 'test_sol_annual_ela',
                  'Mathematics' = 'test_sol_annual_math',
                  'Science' = 'test_sol_annual_science')
  school_lookup <- schools_out %>%
    dplyr::transmute(school_id,
      division_key = norm_key(sub(' Public Schools$', '', division_name,
                                  ignore.case = TRUE)),
      school_key = norm_key(school_name))
  if (anyDuplicated(school_lookup[c('division_key', 'school_key')])) {
    stop('Ambiguous dashboard school names in 2021–22 SOL join.')
  }
  school_joined <- school %>%
    dplyr::mutate(division_key = norm_key(division_name),
                  school_key = norm_key(school_name)) %>%
    dplyr::left_join(school_lookup, by = c('division_key', 'school_key'))
  school_source_count <- dplyr::n_distinct(paste(school$division_code, school$school_code))
  school_matched_count <- dplyr::n_distinct(paste(
    school_joined$division_code[!is.na(school_joined$school_id)],
    school_joined$school_code[!is.na(school_joined$school_id)]))
  message(sprintf('2021–22 SOL school match: %d / %d (%.1f%%)',
    school_matched_count, school_source_count,
    100 * school_matched_count / school_source_count))
  if (school_matched_count / school_source_count < 0.95) {
    stop('2021–22 SOL school match is below the 95% build gate.')
  }
  school_subject <- school_joined %>%
    dplyr::filter(!is.na(school_id), year %in% years_keep) %>%
    dplyr::transmute(school_id, year,
      metric_id = unname(subject_id[subject]), value = pass_rate,
      unit = 'pct', suppressed, numerator = NA_real_, denominator = NA_real_,
      source = 'vdoe_sol_school_subject_2022_reported_rate')
  school_overall <- school_subject %>%
    dplyr::group_by(school_id, year) %>%
    dplyr::summarise(metric_id = 'test_sol_annual_overall',
      value = if (all(!is.finite(value))) NA_real_ else mean(value, na.rm = TRUE),
      unit = 'pct', suppressed = all(!is.finite(value)),
      numerator = NA_real_, denominator = NA_real_,
      source = 'vdoe_sol_school_subject_2022_app_mean', .groups = 'drop')

  division_lookup <- schools_out %>%
    dplyr::distinct(division_id, division_name) %>%
    dplyr::mutate(division_key = norm_key(sub(' Public Schools$', '', division_name,
                                             ignore.case = TRUE))) %>%
    dplyr::select(division_id, division_key)
  if (anyDuplicated(division_lookup$division_key)) {
    stop('Ambiguous dashboard division names in 2021–22 SOL join.')
  }
  division_joined <- division %>%
    dplyr::mutate(division_key = norm_key(division_name)) %>%
    dplyr::left_join(division_lookup, by = 'division_key')
  division_source_count <- dplyr::n_distinct(division$division_code)
  division_matched_count <- dplyr::n_distinct(
    division_joined$division_code[!is.na(division_joined$division_id)])
  message(sprintf('2021–22 SOL division match: %d / %d (%.1f%%)',
    division_matched_count, division_source_count,
    100 * division_matched_count / division_source_count))
  if (division_matched_count / division_source_count < 0.95) {
    stop('2021–22 SOL division match is below the 95% build gate.')
  }
  division_subject <- division_joined %>%
    dplyr::filter(!is.na(division_id), year %in% years_keep) %>%
    dplyr::transmute(division_id, year,
      metric_id = unname(subject_id[subject]), value = pass_rate,
      suppressed, n_included = NA_integer_, n_suppressed = NA_integer_,
      agg_method = 'reported_division_rate')
  school_metrics <- dplyr::bind_rows(school_subject, school_overall)
  division_metrics <- division_subject
  if (anyDuplicated(school_metrics[c('school_id', 'year', 'metric_id')]) ||
      anyDuplicated(division_metrics[c('division_id', 'year', 'metric_id')])) {
    stop('Duplicate 2021–22 SOL dashboard metric keys.')
  }
  list(school_metrics = school_metrics, division_metrics = division_metrics,
       school_source_count = school_source_count,
       school_matched_count = school_matched_count,
       division_source_count = division_source_count,
       division_matched_count = division_matched_count)
}

# Read the checked, public VDOE annual school subject-area extract. The
# workbook extractor records source hashes and keeps this snapshot build free
# of Excel dependencies.
build_annual_sol_metrics <- function(archive, schools_out, years_keep, norm_key) {
  if (!file.exists(archive)) stop('Missing annual SOL extract: ', archive)
  raw <- read.csv(gzfile(archive), stringsAsFactors = FALSE,
                  na.strings = c('', 'NA'),
                  colClasses = c(division_code = 'character',
                                 school_code = 'character'))
  expected <- c('division_code', 'division_name', 'school_code', 'school_name',
                'subject', 'year', 'numerator', 'denominator', 'pass_rate',
                'suppressed', 'source_file')
  if (!identical(names(raw), expected)) stop('Unexpected annual SOL extract schema.')
  if (!identical(sort(unique(raw$year)), 2023:2026)) {
    stop('Annual SOL extract does not contain the expected 2023–2026 years.')
  }
  if (anyDuplicated(raw[c('division_code', 'school_code', 'year', 'subject')])) {
    stop('Duplicate annual SOL school-year-subject keys.')
  }
  if (!all(raw$subject %in% c('English: Reading', 'Mathematics', 'Science'))) {
    stop('Unexpected annual SOL subject.')
  }
  if (any(!raw$suppressed & (!is.finite(raw$pass_rate) |
      !is.finite(raw$numerator) | !is.finite(raw$denominator) |
      raw$denominator <= 0 | raw$numerator < 0 |
      raw$numerator > raw$denominator | raw$pass_rate < 0 |
      raw$pass_rate > 100))) {
    stop('Invalid annual SOL rate or count in extract.')
  }
  if (any(raw$suppressed & (is.finite(raw$pass_rate) |
      is.finite(raw$numerator) | is.finite(raw$denominator)))) {
    stop('Suppressed annual SOL row still contains a numeric value.')
  }

  school_lookup <- schools_out %>%
    dplyr::transmute(
      school_id,
      division_key = norm_key(sub(' Public Schools$', '', division_name,
                                  ignore.case = TRUE)),
      school_key = norm_key(school_name)
    )
  if (anyDuplicated(school_lookup[c('division_key', 'school_key')])) {
    stop('Ambiguous dashboard school names in annual SOL join.')
  }
  joined <- raw %>%
    dplyr::mutate(division_key = norm_key(division_name),
                  school_key = norm_key(school_name)) %>%
    dplyr::left_join(school_lookup, by = c('division_key', 'school_key'))
  source_schools <- dplyr::n_distinct(paste(raw$division_code, raw$school_code))
  matched_schools <- dplyr::n_distinct(paste(joined$division_code[!is.na(joined$school_id)],
                                           joined$school_code[!is.na(joined$school_id)]))
  match_rate <- matched_schools / source_schools
  message(sprintf('Annual SOL school match: %d / %d (%.1f%%)',
                  matched_schools, source_schools, 100 * match_rate))
  if (!is.finite(match_rate) || match_rate < 0.95) {
    stop('Annual SOL school match is below the 95% build gate.')
  }

  subject_metrics <- joined %>%
    dplyr::filter(!is.na(school_id), year %in% years_keep) %>%
    dplyr::mutate(metric_id = dplyr::case_when(
      subject == 'English: Reading' ~ 'test_sol_annual_ela',
      subject == 'Mathematics' ~ 'test_sol_annual_math',
      subject == 'Science' ~ 'test_sol_annual_science'
    )) %>%
    dplyr::transmute(
      school_id, year, metric_id, value = pass_rate, unit = 'pct',
      suppressed = suppressed, numerator, denominator,
      source = 'vdoe_sol_school_subject_annual'
    )
  overall_metrics <- subject_metrics %>%
    dplyr::group_by(school_id, year) %>%
    dplyr::summarise(
      metric_id = 'test_sol_annual_overall',
      value = if (all(!is.finite(value))) NA_real_ else mean(value, na.rm = TRUE),
      unit = 'pct',
      suppressed = all(!is.finite(value)),
      numerator = NA_real_, denominator = NA_real_,
      source = 'vdoe_sol_school_subject_annual_app_mean',
      .groups = 'drop'
    )
  metrics <- dplyr::bind_rows(subject_metrics, overall_metrics)
  if (anyDuplicated(metrics[c('school_id', 'year', 'metric_id')])) {
    stop('Duplicate annual SOL dashboard metric keys.')
  }
  list(metrics = metrics,
       unmatched = joined %>% dplyr::filter(is.na(school_id)) %>%
         dplyr::distinct(division_code, division_name, school_code, school_name),
       source_schools = source_schools, matched_schools = matched_schools)
}

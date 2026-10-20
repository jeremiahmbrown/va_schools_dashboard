# Run from the repository root after saving the two official Build-A-Table CSVs
# in data-raw/input/manual/. The race file is the source of dashboard values;
# the gender/grade file is an independent total-count check.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

manual_dir <- file.path('data-raw', 'input', 'manual')
race_path <- file.path(manual_dir, 'fall_membership_statistics.csv')
check_path <- file.path(manual_dir, 'fall_membership_statistics (1).csv')
duplicate_path <- file.path(manual_dir, 'fall_membership_statistics (2).csv')
if (!all(file.exists(c(race_path, check_path)))) {
  stop('Expected the race and gender/grade Fall Membership downloads.')
}
hash <- function(path) {
  line <- system2('sha256sum', shQuote(path), stdout = TRUE)
  strsplit(line, ' ', fixed = TRUE)[[1]][[1]]
}
if (file.exists(duplicate_path) && hash(race_path) != hash(duplicate_path)) {
  stop('The third Fall Membership download is no longer a duplicate.')
}
source_info <- function(path) list(file = basename(path),
  bytes = unname(file.info(path)$size), sha256 = hash(path))

race <- read.csv(race_path, check.names = FALSE, stringsAsFactors = FALSE,
                 colClasses = 'character')
gender <- read.csv(check_path, check.names = FALSE, stringsAsFactors = FALSE,
                   colClasses = 'character')
expected_race <- c('School Year', 'Division Number', 'Division Name',
  'School Number', 'School Name', 'Race', 'FT Count', 'PT Count', 'Total Count')
if (!identical(names(race), expected_race) ||
    !all(c(expected_race[1:5], 'Gender', 'Total Count') %in% names(gender))) {
  stop('Unexpected Fall Membership export schema.')
}
race_map <- c(
  'American Indian or Alaska Native' = 'demo_american_indian_pct',
  'Asian' = 'demo_asian_pct',
  'Black, not of Hispanic origin' = 'demo_black_pct',
  'Hispanic' = 'demo_hispanic_pct',
  'Native Hawaiian or Pacific Islander' = 'demo_native_hawaiian_pct',
  'Non-Hispanic, two or more races' = 'demo_two_plus_pct',
  'White, not of Hispanic origin' = 'demo_white_pct'
)
normal_space <- function(x) trimws(gsub('[[:space:]]+', ' ', x))
parse_count <- function(x) {
  x <- trimws(x)
  bad <- !is.na(x) & nzchar(x) &
    !grepl('^([0-9]+|[0-9]{1,3}(,[0-9]{3})+)$', x)
  if (any(bad)) stop('Unexpected count token: ', paste(unique(x[bad]), collapse = ', '))
  suppressWarnings(as.integer(ifelse(is.na(x) | !nzchar(x), NA_character_,
                                     gsub(',', '', x, fixed = TRUE))))
}
keys <- c('year', 'division_code', 'school_code')
race <- race %>% transmute(
  year = as.integer(sub('^.*-', '', `School Year`)),
  division_code = trimws(`Division Number`),
  division_name = normal_space(`Division Name`),
  school_code = trimws(`School Number`),
  school_name = normal_space(`School Name`),
  race = normal_space(Race),
  count = parse_count(`Total Count`),
  ft_count = parse_count(`FT Count`),
  pt_count = parse_count(`PT Count`)
)
gender <- gender %>% transmute(
  year = as.integer(sub('^.*-', '', `School Year`)),
  division_code = trimws(`Division Number`),
  school_code = trimws(`School Number`),
  gender = normal_space(Gender),
  count = parse_count(`Total Count`)
)
if (!identical(sort(unique(race$year)), 2025:2026) ||
    !identical(sort(unique(gender$year)), 2025:2026)) {
  stop('Expected both 2024-25 and 2025-26 Fall Membership years.')
}
if (any(!race$race %in% names(race_map)) ||
    anyDuplicated(race[c(keys, 'race')]) ||
    anyDuplicated(gender[c(keys, 'gender')]) || anyNA(gender$count)) {
  stop('Unexpected race/gender categories, duplicate keys, or missing gender total.')
}
if (any(!is.na(race$count) &
        race$count != rowSums(cbind(replace(race$ft_count, is.na(race$ft_count), 0L),
                                         replace(race$pt_count, is.na(race$pt_count), 0L))))) {
  stop('Race total differs from full-time plus part-time counts.')
}
race_totals <- race %>% group_by(across(all_of(keys))) %>%
  summarise(race_total = sum(count, na.rm = TRUE), .groups = 'drop')
gender_totals <- gender %>% group_by(across(all_of(keys))) %>%
  summarise(denominator = sum(count), .groups = 'drop')
checked <- full_join(race_totals, gender_totals, by = keys)
if (anyNA(checked) || any(checked$race_total != checked$denominator)) {
  stop('Race counts do not reconcile to gender/grade school totals.')
}
schools <- race %>% distinct(across(all_of(keys)), division_name, school_name) %>%
  left_join(gender_totals, by = keys)
if (anyDuplicated(schools[keys]) || anyNA(schools$denominator) ||
    any(schools$denominator <= 0)) stop('Invalid school-year totals.')

# The seven categories exhaust each independently checked school total.
# A category absent from the export is therefore zero. A category with an
# explicit blank count remains suppressed, even when the other counts add up.
out <- tidyr::crossing(schools, metric_id = unname(race_map)) %>%
  left_join(race %>% transmute(across(all_of(keys)),
    metric_id = unname(race_map[race]), count, source_row = TRUE),
    by = c(keys, 'metric_id')) %>%
  mutate(count = ifelse(is.na(source_row), 0, count),
         suppressed = is.na(count),
         source_file = basename(race_path)) %>%
  select(year, division_code, division_name, school_code, school_name,
         metric_id, count, denominator, suppressed, source_file) %>%
  arrange(year, division_code, school_code, metric_id)
if (anyDuplicated(out[c(keys, 'metric_id')]) ||
    any(out$count[!out$suppressed] > out$denominator[!out$suppressed])) {
  stop('Invalid extracted race rows.')
}
archive <- file.path('data-raw', 'fall_membership_race_school.csv.gz')
con <- gzfile(archive, open = 'wt')
write.csv(out, con, row.names = FALSE, na = '')
close(con)
manifest <- list(
  source_page = 'https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics',
  selection = 'Fall Membership Build-A-Table; school, year, race; all genders and grades; full-time plus part-time',
  source_files = lapply(c(race_path, check_path,
                          if (file.exists(duplicate_path)) duplicate_path),
                        source_info),
  race_rows = nrow(race), blank_race_counts = sum(is.na(race$count)),
  school_years = nrow(schools), school_year_totals = as.list(tapply(schools$denominator, schools$year, sum)),
  archive_file = basename(archive), archive_sha256 = hash(archive)
)
jsonlite::write_json(manifest, file.path('data-raw', 'fall_membership_race_sources.json'),
                     pretty = TRUE, auto_unbox = TRUE)
print(out %>% group_by(year) %>% summarise(
  schools = n_distinct(paste(division_code, school_code)),
  metric_rows = n(), suppressed = sum(suppressed), .groups = 'drop'))

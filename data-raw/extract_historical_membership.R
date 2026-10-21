# Check 2021–22 through 2023–24 school race counts and the 2021–22 total.
# Original Build-A-Table CSVs stay in the ignored manual input folder.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

manual <- file.path('data-raw', 'input', 'manual')
race_path <- file.path(manual, 'fall_membership_multiyear_race.csv')
gender_path <- file.path(manual, 'fall_membership__multiyear_gender_grade.csv')
race_2022_path <- file.path(manual, 'fall_membership_2022_race.csv')
gender_2022_path <- file.path(manual, 'fall_membership_2022_gender_grade.csv')
paths <- c(race_path, gender_path, race_2022_path, gender_2022_path)
if (!all(file.exists(paths))) stop('Missing historical Fall Membership CSVs.')
expected_base <- c('School Year', 'Division Number', 'Division Name',
                   'School Number', 'School Name')
normal_space <- function(x) trimws(gsub('[[:space:]]+', ' ', x))
parse_count <- function(x) {
  x <- trimws(x)
  bad <- !is.na(x) & nzchar(x) &
    !grepl('^([0-9]+|[0-9]{1,3}(,[0-9]{3})+)$', x)
  if (any(bad)) stop('Invalid Fall Membership count token.')
  suppressWarnings(as.integer(ifelse(is.na(x) | !nzchar(x), NA_character_,
    gsub(',', '', x, fixed = TRUE))))
}
read_source <- function(path, group) {
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                colClasses = 'character', fileEncoding = 'UTF-8-BOM')
  expected <- c(expected_base, group, 'FT Count', 'PT Count', 'Total Count')
  if (!identical(names(x), expected)) stop('Unexpected columns: ', basename(path))
  x
}
race_raw <- read_source(race_path, 'Race')
gender_raw <- read_source(gender_path, 'Gender')
if (!identical(subset(race_raw, `School Year` == '2021-2022'),
               read_source(race_2022_path, 'Race')) ||
    !identical(subset(gender_raw, `School Year` == '2021-2022'),
               read_source(gender_2022_path, 'Gender'))) {
  stop('Standalone 2021–22 files differ from the three-year exports.')
}
normalise <- function(x, group) {
  out <- x %>% transmute(
    year = as.integer(sub('^.*-', '', `School Year`)),
    division_code = trimws(`Division Number`),
    division_name = normal_space(`Division Name`),
    school_code = trimws(`School Number`),
    school_name = normal_space(`School Name`),
    category = normal_space(.data[[group]]),
    count = parse_count(`Total Count`),
    ft = parse_count(`FT Count`), pt = parse_count(`PT Count`)
  )
  if (anyNA(out[c('year', 'division_code', 'school_code', 'category')]) ||
      any(out$year < 2022 | out$year > 2024) ||
      anyDuplicated(out[c('year', 'division_code', 'school_code', 'category')]) ||
      any(!is.na(out$count) & out$count != coalesce(out$ft, 0L) + coalesce(out$pt, 0L))) {
    stop('Invalid historical Fall Membership rows.')
  }
  out
}
race <- normalise(race_raw, 'Race')
gender <- normalise(gender_raw, 'Gender')
if (!identical(sort(unique(race$year)), 2022:2024) ||
    !identical(sort(unique(gender$year)), 2022:2024) ||
    !setequal(gender$category, c('Female', 'Male', 'Other'))) {
  stop('Unexpected historical years or gender categories.')
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
if (any(!race$category %in% names(race_map))) stop('Unexpected race category.')
keys <- c('year', 'division_code', 'school_code')
race_totals <- race %>% group_by(across(all_of(keys))) %>%
  summarise(race_total = sum(count, na.rm = TRUE), .groups = 'drop')
gender_totals <- gender %>% group_by(across(all_of(keys))) %>%
  summarise(gender_total = sum(count, na.rm = TRUE), .groups = 'drop')
checked <- full_join(race_totals, gender_totals, by = keys)
message('Historical total checks: missing=', sum(!is.finite(checked$race_total) |
  !is.finite(checked$gender_total)), ', nonpositive=',
  sum(checked$race_total <= 0 | checked$gender_total <= 0, na.rm = TRUE),
  ', 2022/24 differences=', sum(checked$race_total[checked$year %in% c(2022, 2024)] !=
    checked$gender_total[checked$year %in% c(2022, 2024)], na.rm = TRUE),
  ', 2023 race below gender=', sum(checked$race_total[checked$year == 2023] <
    checked$gender_total[checked$year == 2023], na.rm = TRUE))
message('Zero-enrollment source school-years excluded: ',
  sum(checked$race_total == 0 | checked$gender_total == 0))
if (anyNA(checked) || any(checked$race_total < 0 | checked$gender_total < 0) ||
    any(checked$race_total[checked$year %in% c(2022, 2024)] !=
        checked$gender_total[checked$year %in% c(2022, 2024)]) ||
    any(checked$race_total[checked$year == 2023] <
        checked$gender_total[checked$year == 2023])) {
  stop('Historical race and gender totals do not follow checked year pattern.')
}
# The 2022–23 gender export omits Other. Its race count sum is the complete
# same-source total; the archived SQP enrollment is checked at map load time.
schools <- race %>% distinct(across(all_of(keys)), division_name, school_name) %>%
  left_join(checked, by = keys) %>%
  filter(race_total > 0, gender_total > 0) %>%
  mutate(denominator = ifelse(year == 2023, race_total, gender_total))
if (anyDuplicated(schools[keys]) || anyNA(schools$denominator)) {
  stop('Ambiguous historical school totals.')
}
out <- crossing(schools, metric_id = unname(race_map)) %>%
  left_join(race %>% transmute(across(all_of(keys)),
    metric_id = unname(race_map[category]), count, source_row = TRUE),
    by = c(keys, 'metric_id')) %>%
  mutate(count = ifelse(is.na(source_row), 0, count),
         suppressed = is.na(count), source_file = basename(race_path)) %>%
  select(year, division_code, division_name, school_code, school_name,
         metric_id, count, denominator, suppressed, source_file) %>%
  arrange(year, division_code, school_code, metric_id)
if (anyDuplicated(out[c(keys, 'metric_id')]) ||
    any(out$count[!out$suppressed] > out$denominator[!out$suppressed])) {
  stop('Invalid historical race output.')
}
hash <- function(path) strsplit(system2('sha256sum', shQuote(path),
  stdout = TRUE), ' ', fixed = TRUE)[[1]][[1]]
source_info <- function(path) list(file = basename(path),
  bytes = unname(file.info(path)$size), sha256 = hash(path))
race_archive <- file.path('data-raw', 'fall_membership_historical_race_school.csv.gz')
con <- gzfile(race_archive, open = 'wt')
write.csv(out, con, row.names = FALSE, na = '')
close(con)
enrollment_archive <- file.path('data-raw', 'fall_membership_2022_enrollment.csv.gz')
enrollment <- schools %>% filter(year == 2022) %>%
  transmute(year, division_code, school_code,
            enrollment = as.integer(denominator)) %>%
  arrange(division_code, school_code)
con <- gzfile(enrollment_archive, open = 'wt')
write.csv(enrollment, con, row.names = FALSE)
close(con)
manifest <- list(
  source_page = 'https://p1pe.doe.virginia.gov/buildatable/fallmembership',
  selection = 'School-level Fall Membership race and gender totals, 2021–22 through 2023–24; all divisions and schools',
  source_files = lapply(paths, source_info),
  school_years = nrow(schools),
  gender_missing_other_2023_school_years = sum(checked$year == 2023 &
    checked$race_total > checked$gender_total),
  blank_race_counts = sum(is.na(race$count)),
  race_archive_sha256 = hash(race_archive),
  enrollment_archive_sha256 = hash(enrollment_archive)
)
write_json(manifest, file.path('data-raw', 'fall_membership_historical_sources.json'),
           pretty = TRUE, auto_unbox = TRUE)
print(out %>% group_by(year) %>% summarise(schools = n_distinct(paste(division_code, school_code)),
  rows = n(), suppressed = sum(suppressed), .groups = 'drop'))

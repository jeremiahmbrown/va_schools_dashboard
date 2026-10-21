# Run from the repository root after saving the five school-level SBAR CSVs.
# Keep the original downloads ignored; track this checked, compact extract.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

manual_dir <- file.path('data-raw', 'input', 'manual')
paths <- file.path(manual_dir, paste0('sbar_school_sanctions_', 2022:2026, '.csv'))
if (!all(file.exists(paths))) stop('Missing school-level SBAR sanction CSV: ',
                                  paste(basename(paths[!file.exists(paths)]), collapse = ', '))
expected <- c('School Year', 'Division Number', 'Division Name', 'School Number',
              'School Name', 'Sanction Code', 'Sanction Description',
              'Number of Sanctions', 'Number of Students')
normal_space <- function(x) trimws(gsub('[[:space:]]+', ' ', x))
parse_count <- function(x) {
  x <- trimws(x)
  if (any(!grepl('^([0-9]+|[0-9]{1,3}(,[0-9]{3})+)$', x))) {
    stop('Invalid SBAR count token: ', paste(unique(x[!grepl('^([0-9]+|[0-9]{1,3}(,[0-9]{3})+)$', x)]), collapse = ', '))
  }
  as.integer(gsub(',', '', x, fixed = TRUE))
}
hash <- function(path) {
  line <- system2('sha256sum', shQuote(path), stdout = TRUE)
  strsplit(line, ' ', fixed = TRUE)[[1]][[1]]
}
source_info <- function(path) list(file = basename(path),
  bytes = unname(file.info(path)$size), sha256 = hash(path))

read_one <- function(path, expected_year) {
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                colClasses = 'character', fileEncoding = 'UTF-8-BOM')
  if (!identical(names(x), expected)) stop('Unexpected SBAR export columns: ', basename(path))
  out <- x %>% transmute(
    year = as.integer(sub('^.*-', '', `School Year`)),
    division_code = trimws(`Division Number`),
    division_name = normal_space(`Division Name`),
    school_code = trimws(`School Number`),
    school_name = normal_space(`School Name`),
    sanction_code = trimws(`Sanction Code`),
    sanction_description = normal_space(`Sanction Description`),
    actions = parse_count(`Number of Sanctions`),
    students = parse_count(`Number of Students`),
    source_file = basename(path)
  )
  if (anyNA(out) || !identical(unique(out$year), expected_year) ||
      any(!nzchar(out$division_code) | !nzchar(out$school_code) |
          !nzchar(out$sanction_code)) ||
      any(out$students > out$actions) ||
      anyDuplicated(out[c('year', 'division_code', 'school_code',
                          'sanction_code', 'sanction_description')])) {
    stop('Invalid SBAR rows, year, or duplicate keys: ', basename(path))
  }
  out
}
raw <- bind_rows(Map(read_one, paths, 2022:2026))
school_years <- raw %>%
  distinct(year, division_code, division_name, school_code, school_name, source_file)
if (anyDuplicated(school_years[c('year', 'division_code', 'school_code')])) {
  stop('A SBAR school-year has conflicting names.')
}
metric_map <- c(OSS = 'behavior_sbar_oss_actions_per_100',
                ISS = 'behavior_sbar_iss_actions_per_100')
selected <- raw %>%
  filter(sanction_code %in% names(metric_map)) %>%
  mutate(metric_id = unname(metric_map[sanction_code])) %>%
  group_by(year, division_code, school_code, metric_id) %>%
  summarise(actions = sum(actions), .groups = 'drop')

# The export omits zero-count sanction codes. A school with another reported
# sanction row is known to be present; an absent OSS or ISS code is zero.
# Schools absent from the entire export are not given invented zeroes.
out <- tidyr::crossing(school_years, metric_id = unname(metric_map)) %>%
  left_join(selected, by = c('year', 'division_code', 'school_code', 'metric_id')) %>%
  mutate(actions = coalesce(actions, 0L)) %>%
  arrange(year, division_code, school_code, metric_id)
if (anyDuplicated(out[c('year', 'division_code', 'school_code', 'metric_id')])) {
  stop('Duplicate checked SBAR metric keys.')
}
archive <- file.path('data-raw', 'sbar_school_sanctions.csv.gz')
con <- gzfile(archive, open = 'wt')
write.csv(out, con, row.names = FALSE)
close(con)
manifest <- list(
  source_page = 'https://p1pe.doe.virginia.gov/buildatable/sbar',
  selection = 'Sanction Report; school; all divisions and schools; all sanction types; one CSV per year',
  definition = 'Number of Sanctions, not Number of Students; OSS and ISS codes; absent code is zero only for a school with another reported sanction row',
  source_files = lapply(paths, source_info),
  source_rows = nrow(raw),
  school_years = nrow(school_years),
  archive_file = basename(archive),
  archive_sha256 = hash(archive)
)
jsonlite::write_json(manifest, file.path('data-raw', 'sbar_school_sanctions_sources.json'),
                     pretty = TRUE, auto_unbox = TRUE)
print(out %>% group_by(year, metric_id) %>%
  summarise(schools = n(), actions = sum(actions), .groups = 'drop'))

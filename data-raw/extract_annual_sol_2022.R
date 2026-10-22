# Check the 2021–22 School and Division Subject-Area workbooks. These older
# exports contain pass rates but no tested-student numerators or denominators.
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(jsonlite)
})

manual <- file.path('data-raw', 'input', 'manual')
school_path <- file.path(manual, 'school-by-subject-2022.xlsx')
division_path <- file.path(manual, 'division-by-subject-2022.xlsx')
if (!file.exists(school_path) || !file.exists(division_path)) {
  stop('Missing 2021–22 SOL School or Division Subject-Area workbook.')
}
subjects <- c('English: Reading', 'Mathematics', 'Science')
normal_headers <- function(x) trimws(gsub('[[:space:]]+', ' ', x))
read_source <- function(path, level) {
  x <- readxl::read_excel(path, col_types = 'text')
  names(x) <- normal_headers(names(x))
  expected <- if (level == 'Sch') {
    c('Level', 'Div Num', 'Div Name', 'Sch Num', 'Sch Name',
      'Sch Type', 'Low Grade', 'High Grade', 'Subject', 'Subgroup',
      '2019-2020 Pass Rate', '2020-2021 Pass Rate', '2021-2022 Pass Rate')
  } else {
    c('Level', 'Div Num', 'Div Name', 'Subject', 'Subgroup',
      '2019-2020 Pass Rate', '2020-2021 Pass Rate', '2021-2022 Pass Rate')
  }
  if (!identical(names(x), expected) || !all(x$Level == level)) {
    stop('Unexpected 2021–22 SOL schema or level: ', basename(path))
  }
  x <- x %>%
    filter(Subgroup == 'All Students', Subject %in% subjects)
  keys <- if (level == 'Sch') c('Div Num', 'Sch Num', 'Subject') else c('Div Num', 'Subject')
  if (anyNA(x[keys]) || anyDuplicated(x[keys])) {
    stop('Missing or duplicate 2021–22 SOL source keys: ', basename(path))
  }
  rate_raw <- trimws(x[['2021-2022 Pass Rate']])
  bad <- !is.na(rate_raw) & nzchar(rate_raw) &
    !grepl('^[0-9]+([.][0-9]+)?$', rate_raw) &
    !rate_raw %in% c('<', 'NA')
  if (any(bad)) stop('Unexpected 2021–22 SOL pass-rate marker: ',
                     paste(unique(rate_raw[bad]), collapse = ', '))
  rate <- suppressWarnings(as.numeric(rate_raw))
  if (any(is.finite(rate) & (rate < 0 | rate > 100))) {
    stop('2021–22 SOL pass rate outside 0–100: ', basename(path))
  }
  if (level == 'Sch') {
    out <- x %>% transmute(
      division_code = `Div Num`, division_name = `Div Name`,
      school_code = `Sch Num`, school_name = `Sch Name`,
      subject = Subject, year = 2022L,
      pass_rate = rate, suppressed = !is.finite(rate),
      source_file = basename(path))
    return(arrange(out, division_code, school_code, subject))
  }
  out <- x %>% transmute(
    division_code = `Div Num`, division_name = `Div Name`,
    subject = Subject, year = 2022L,
    pass_rate = rate, suppressed = !is.finite(rate),
    source_file = basename(path))
  arrange(out, division_code, subject)
}

school <- read_source(school_path, 'Sch')
division <- read_source(division_path, 'Div')
if (nrow(school) < 5000 || nrow(division) < 390 ||
    dplyr::n_distinct(school$division_code) != dplyr::n_distinct(division$division_code)) {
  stop('Incomplete 2021–22 SOL School or Division Subject-Area export.')
}
write_gzip_csv <- function(x, path) {
  con <- gzfile(path, open = 'wt')
  on.exit(close(con))
  write.csv(x, con, row.names = FALSE, na = '')
}
school_archive <- file.path('data-raw', 'annual_sol_2022_school.csv.gz')
division_archive <- file.path('data-raw', 'annual_sol_2022_division.csv.gz')
write_gzip_csv(school, school_archive)
write_gzip_csv(division, division_archive)
hash <- function(path) {
  line <- system2('sha256sum', shQuote(path), stdout = TRUE)
  strsplit(line, ' ', fixed = TRUE)[[1]][[1]]
}
source_info <- function(path) list(file = basename(path),
  bytes = unname(file.info(path)$size), sha256 = hash(path))
manifest <- list(
  source_page = 'https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results',
  selection = '2021–22 School and Division Subject-Area; All Students; English: Reading, Mathematics, Science; reported pass rates only',
  source_files = lapply(c(school_path, division_path), source_info),
  school_rows = nrow(school), school_reportable = sum(!school$suppressed),
  division_rows = nrow(division), division_reportable = sum(!division$suppressed),
  school_archive_sha256 = hash(school_archive),
  division_archive_sha256 = hash(division_archive)
)
jsonlite::write_json(manifest, file.path('data-raw', 'annual_sol_2022_sources.json'),
                     pretty = TRUE, auto_unbox = TRUE)
cat(sprintf('2021–22 SOL: %d/%d school subject rates and %d/%d division subject rates reportable.\n',
            sum(!school$suppressed), nrow(school),
            sum(!division$suppressed), nrow(division)))

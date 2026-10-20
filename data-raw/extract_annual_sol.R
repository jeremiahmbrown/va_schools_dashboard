# Run from the repository root after placing the two official school subject
# workbooks in data-raw/input/manual/. The output is a small, public-data cache
# that makes snapshot rebuilds possible without Excel files or readxl at runtime.
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(tidyr)
  library(jsonlite)
})

manual_dir <- file.path('data-raw', 'input', 'manual')
sources <- c(
  `2024-2025` = 'SY 2024-2025 Subject Area by School.xlsx',
  `2025-2026` = 'SY 2025-26 School - Subject.xlsx'
)
paths <- file.path(manual_dir, unname(sources))
if (!all(file.exists(paths))) stop('Missing manual VDOE school subject workbook(s).')

normal_headers <- function(x) trimws(gsub('[[:space:]]+', ' ', x))
read_source <- function(path, keep_years) {
  x <- readxl::read_excel(path, col_types = 'text')
  names(x) <- normal_headers(names(x))
  base_cols <- c('Level', 'Div Num', 'Div Name', 'Sch Num', 'Sch Name',
                 'Subject', 'Student Group')
  year_cols <- unlist(lapply(keep_years, function(y) {
    paste(y, c('Numerator', 'Denominator', 'Pass Rate'))
  }))
  if (!all(c(base_cols, year_cols) %in% names(x))) {
    stop('Unexpected VDOE workbook schema: ', basename(path))
  }
  x <- x %>%
    filter(Level == 'Sch', `Student Group` == 'All Students',
           Subject %in% c('English: Reading', 'Mathematics', 'Science'))
  if (anyDuplicated(x[c('Div Num', 'Sch Num', 'Subject')])) {
    stop('Duplicate school-subject rows in ', basename(path))
  }
  out <- lapply(keep_years, function(y) {
    x %>% transmute(
      division_code = `Div Num`, division_name = `Div Name`,
      school_code = `Sch Num`, school_name = `Sch Name`,
      subject = Subject, year = as.integer(sub('^.*-', '', y)),
      numerator_raw = .data[[paste(y, 'Numerator')]],
      denominator_raw = .data[[paste(y, 'Denominator')]],
      pass_rate_raw = .data[[paste(y, 'Pass Rate')]],
      source_file = basename(path)
    )
  })
  bind_rows(out)
}

# Use the newer workbook for overlapping years. The old workbook adds 2022-23.
raw <- bind_rows(
  read_source(paths[[1]], '2022-2023'),
  read_source(paths[[2]], c('2023-2024', '2024-2025', '2025-2026'))
) %>% arrange(year, division_code, school_code, subject)

if (!identical(sort(unique(raw$year)), 2023:2026)) stop('Unexpected SOL years.')
if (anyDuplicated(raw[c('division_code', 'school_code', 'subject', 'year')])) {
  stop('Duplicate school-year-subject keys in extracted SOL rows.')
}
allowed <- c('', '<', 'NA')
for (column in c('numerator_raw', 'denominator_raw', 'pass_rate_raw')) {
  values <- raw[[column]]
  bad <- unique(values[!is.na(values) &
                         !grepl('^[0-9]+([.][0-9]+)?$', values) &
                         !values %in% allowed])
  if (length(bad)) stop('Unexpected ', column, ' tokens: ', paste(bad, collapse = ', '))
}

raw <- raw %>% mutate(
  numerator = suppressWarnings(as.numeric(numerator_raw)),
  denominator = suppressWarnings(as.numeric(denominator_raw)),
  pass_rate = suppressWarnings(as.numeric(pass_rate_raw)),
  suppressed = !is.finite(numerator) | !is.finite(denominator) |
    !is.finite(pass_rate)
)
reportable <- raw %>% filter(!suppressed)
if (any(reportable$denominator <= 0 | reportable$numerator < 0 |
        reportable$numerator > reportable$denominator |
        reportable$pass_rate < 0 | reportable$pass_rate > 100)) {
  stop('Invalid reportable SOL numerator, denominator, or pass rate.')
}
rate_diff <- abs(reportable$pass_rate - 100 * reportable$numerator /
                   reportable$denominator)
if (any(rate_diff > 1.1)) {
  stop('Reported SOL rate differs from numerator/denominator by over 1.1 points.')
}
raw <- raw %>% mutate(
  numerator = ifelse(suppressed, NA_real_, numerator),
  denominator = ifelse(suppressed, NA_real_, denominator),
  pass_rate = ifelse(suppressed, NA_real_, pass_rate)
) %>% select(division_code, division_name, school_code, school_name,
             subject, year, numerator, denominator, pass_rate, suppressed,
             source_file)

archive <- file.path('data-raw', 'annual_sol_school.csv.gz')
con <- gzfile(archive, open = 'wt')
write.csv(raw, con, row.names = FALSE, na = '')
close(con)

hash <- function(path) {
  line <- system2('sha256sum', shQuote(path), stdout = TRUE)
  strsplit(line, ' ', fixed = TRUE)[[1]][[1]]
}
manifest <- list(
  source_page = 'https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results',
  selection = 'School Subject-Area; All Students; English: Reading, Mathematics, Science; 2022-23 from 2024-25 file and 2023-24 through 2025-26 from 2025-26 file',
  source_files = lapply(paths, function(p) list(file = basename(p),
    bytes = unname(file.info(p)$size), sha256 = hash(p))),
  extracted_rows = nrow(raw),
  year_rows = as.list(table(raw$year)),
  archive_file = basename(archive), archive_sha256 = hash(archive)
)
jsonlite::write_json(manifest, file.path('data-raw', 'annual_sol_sources.json'),
                     pretty = TRUE, auto_unbox = TRUE)
cat('Extracted', nrow(raw), 'school-subject-year rows.\n')
print(raw %>% group_by(year) %>% summarise(rows = n(),
  reportable = sum(!suppressed), schools = n_distinct(paste(division_code, school_code)),
  .groups = 'drop'))
cat('Maximum rate/count difference:', max(rate_diff), '\n')

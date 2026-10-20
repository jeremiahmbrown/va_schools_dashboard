# Run from repo root: Rscript tests/check_source_validation.R
# Load the exact builder functions without running its download/build side effects.
code <- parse("data-raw/build_snapshot.R")
wanted <- c("parse_end_year", "detect_csv_header_line", "read_vdoe_csv", "validate_not_html")
for (expression in code) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.symbol(expression[[2]]) && as.character(expression[[2]]) %in% wanted) {
    eval(expression, envir = .GlobalEnv)
  }
}
stopifnot(all(vapply(wanted, exists, logical(1), envir = .GlobalEnv, inherits = FALSE)))

input <- "data-raw/input/vdoe_fr_eligibility.csv"
stopifnot(file.exists(input), validate_not_html(input))

fixture_dir <- tempfile("vdoe-source-fixtures-")
dir.create(fixture_dir)
on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)
path <- file.path(fixture_dir, "vdoe_fr_eligibility.csv")

writeLines(c("<!doctype html>", "<html><body>Error</body></html>"), path)
stopifnot(!validate_not_html(path))

source_rows <- read_vdoe_csv(input)
write.csv(head(source_rows, 100), path, row.names = FALSE, na = "")
stopifnot(!validate_not_html(path))

invalid_year <- source_rows
invalid_year$Year[[1]] <- "invalid"
write.csv(invalid_year, path, row.names = FALSE, na = "")
stopifnot(!validate_not_html(path))

future_year <- source_rows
future_year$Year[[1]] <- "2999"
write.csv(future_year, path, row.names = FALSE, na = "")
stopifnot(!validate_not_html(path))

bad_schema <- source_rows
names(bad_schema)[names(bad_schema) == "Percent"] <- "NoPercent"
write.csv(bad_schema, path, row.names = FALSE, na = "")
stopifnot(!validate_not_html(path))

cat("Source validation fixtures passed.\n")

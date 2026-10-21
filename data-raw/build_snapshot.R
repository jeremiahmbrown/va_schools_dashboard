# ==============================================================================
# Script: build_snapshot.R
# Purpose:
#   Build a frozen snapshot under `app/data/` by downloading authoritative public
#   datasets and transforming them into the app’s snapshot schema.
#
#   Current implementation pulls:
#   - VDOE School Quality Profiles "Download Data" CSV exports (via POST)
#     for accreditation rates (used to compute an overall performance index),
#     and suspensions (used for behavior + race/ethnicity % of students).
#   - NCES EDGE public school geocodes (for lat/lon).
#   - US Census TIGER/Line unified school district boundaries for VA (for
#     division choropleth polygons).
#
# Inputs:
#   - Network access (downloads).
#   - System tools: `curl`, `unzip`, `ogr2ogr` (for TIGER/Line -> GeoJSON).
#
# Outputs:
#   - Writes snapshot files under `app/data/`:
#     `snapshot_meta.json`, `schools.csv`, `metric_defs.csv`,
#     `school_metrics.csv`, `division_metrics.csv`, `divisions_polygons.json`
#
# Key Details / Debugging:
#   - VDOE downloads are fetched from `schoolquality.virginia.gov/download-data`.
#     Set VA_DASH_CURL_CA_BUNDLE to a verified CA bundle if VDOE omits the
#     intermediate certificate. TLS verification is never disabled.
#   - VDOE files include 2 header lines + blank line; we auto-detect the CSV
#     header row.
#   - The "Overall Performance Index" is computed (not VDOE-published) as:
#       mean(available Accreditation Combined Rate) across subjects
#     for "All Students" at the school level.
#   - School geocoding relies on name-based matching between VDOE and NCES.
#     The script reports match rates and will stop if matching is too poor.
# ==============================================================================

required <- c("jsonlite", "dplyr", "tidyr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop(
  "Missing required packages: ", paste(missing, collapse = ", "),
  ". Run `Rscript scripts/bootstrap.R` first."
)

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
})

input_dir <- file.path("data-raw", "input")
if (!dir.exists(input_dir)) {
  dir.create(input_dir, recursive = TRUE)
}

snapshot_dir <- file.path("app", "data")
if (!dir.exists(snapshot_dir)) stop("Existing snapshot is missing: ", snapshot_dir)
snapshot_stage <- tempfile("snapshot-stage-", tmpdir = "app")
if (!dir.create(snapshot_stage)) stop("Could not create snapshot staging directory")
# Zone overlays are independent of metric refreshes; retain them in the bundle.
if (dir.exists(file.path(snapshot_dir, "zones"))) {
  copied <- file.copy(file.path(snapshot_dir, "zones"), snapshot_stage, recursive = TRUE)
  if (!all(copied)) stop("Could not preserve attendance-zone files")
}
refresh_vdoe <- identical(Sys.getenv("VA_DASH_REFRESH_VDOE", "0"), "1")
curl_ca_bundle <- Sys.getenv("VA_DASH_CURL_CA_BUNDLE", "")
curl_ca_args <- if (nzchar(curl_ca_bundle)) c("--cacert", curl_ca_bundle) else character(0)
if (nzchar(curl_ca_bundle) && !file.exists(curl_ca_bundle)) {
  stop("VA_DASH_CURL_CA_BUNDLE does not exist: ", curl_ca_bundle)
}

need_cmd <- function(cmd) {
  ok <- nzchar(Sys.which(cmd))
  if (!ok) stop("Missing required system command: `", cmd, "`")
}

need_cmd("curl")
need_cmd("unzip")
need_cmd("ogr2ogr")

message("Raw download cache: ", normalizePath(input_dir))
message("Snapshot output dir: ", normalizePath(snapshot_dir))

norm_key <- function(x) {
  x <- toupper(x)
  x <- gsub("&", " AND ", x, fixed = FALSE)
  x <- gsub("[^A-Z0-9 ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

parse_end_year <- function(x) {
  # Handles "2023 - 2024" and "2024".
  x <- trimws(gsub("\"", "", x, fixed = TRUE))
  if (grepl("-", x, fixed = TRUE)) {
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    as.integer(trimws(parts[[length(parts)]]))
  } else {
    suppressWarnings(as.integer(x))
  }
}

detect_csv_header_line <- function(lines) {
  # Find first line that looks like a header.
  candidates <- which(grepl("^Year,", lines) | grepl("^\"?Year\"?,", lines))
  if (length(candidates) == 0) stop("Could not detect header row in VDOE CSV.")
  candidates[[1]] - 1
}

read_vdoe_csv <- function(path) {
  lines <- readLines(path, n = 50, warn = FALSE)
  skip <- detect_csv_header_line(lines)
  read.csv(path, skip = skip, stringsAsFactors = FALSE, check.names = FALSE)
}

curl_post_download <- function(dest_path, url, form_fields) {
  args_fields <- unlist(lapply(names(form_fields), function(k) {
    unlist(lapply(form_fields[[k]], function(one) {
      c("--data-urlencode", paste0(k, "=", one))
    }))
  }), use.names = FALSE)
  args <- c("-sS", "-L", "--fail", "--max-time", "120", curl_ca_args,
            "-X", "POST", args_fields, "-o", dest_path, url)
  code <- system2("curl", args = args)
  if (is.na(code) || code != 0) {
    stop("Verified curl POST failed for ", url, " (exit code ", code, ").")
  }
}

curl_get_download <- function(dest_path, url) {
  args <- c("-sS", "-L", "--fail", "--max-time", "240", curl_ca_args,
            "-o", dest_path, url)
  code <- system2("curl", args = args)
  if (!is.na(code) && code == 0) return(invisible(TRUE))
  stop("Verified curl GET failed for ", url, " (exit code ", code, ").")
}

vdoe_url <- "https://schoolquality.virginia.gov/download-data"

download_cached <- function(filename, downloader, validator = NULL) {
  dest <- file.path(input_dir, filename)
  is_vdoe <- startsWith(filename, "vdoe_")
  has_cache <- file.exists(dest) && file.info(dest)$size > 0
  if (has_cache && !(is_vdoe && refresh_vdoe)) {
    if (is.null(validator) || isTRUE(validator(dest))) {
      message("Using cached: ", filename)
      return(dest)
    }
    message("Cached file failed validation; fetching replacement: ", filename)
  }
  tmp <- tempfile(paste0(filename, "-"), tmpdir = input_dir)
  on.exit(unlink(tmp), add = TRUE)
  message("Downloading: ", filename)
  downloader(tmp)
  if (!file.exists(tmp) || file.info(tmp)$size == 0 ||
      (!is.null(validator) && !isTRUE(validator(tmp)))) {
    stop("Downloaded file failed validation: ", filename)
  }
  old <- tempfile(paste0(filename, "-old-"), tmpdir = input_dir)
  if (file.exists(dest) && !file.rename(dest, old)) stop("Cannot back up cached file: ", dest)
  if (!file.rename(tmp, dest)) {
    if (file.exists(old)) file.rename(old, dest)
    stop("Cannot publish cached file: ", dest)
  }
  if (file.exists(old)) unlink(old)
  dest
}

validate_not_html <- function(path) {
  lines <- tryCatch(readLines(path, n = 50, warn = FALSE),
                    error = function(e) character(0))
  if (length(lines) < 3 || any(grepl("^\\s*<(!doctype|html)", lines, ignore.case = TRUE))) return(FALSE)
  df <- tryCatch(read_vdoe_csv(path), error = function(e) NULL)
  if (is.null(df) || !all(c("Year", "Division", "School") %in% names(df))) return(FALSE)
  parsed_years <- vapply(df$Year, parse_end_year, integer(1))
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (anyNA(parsed_years) || any(parsed_years < 2000L | parsed_years > current_year + 1L)) return(FALSE)

  name <- basename(path)
  if (startsWith(name, "vdoe_enrollment")) {
    required <- c("Level", "Grade", "Count")
    minimum_per_year <- 10000L
  } else if (startsWith(name, "vdoe_absenteeism")) {
    required <- c("Level", "Subgroup", "Count Below 10", "Count Above 10", "Percent above 10")
    minimum_per_year <- 20000L
  } else if (startsWith(name, "vdoe_fr_")) {
    required <- c("Level", "Percent")
    minimum_per_year <- 1600L
  } else if (startsWith(name, "vdoe_accreditation_")) {
    required <- c("Indicator", "Subgroup", "Level Code", "Accreditation Combined Rate", "Percent Passing")
    minimum_per_year <- 1600L
  } else if (startsWith(name, "vdoe_short_term_suspensions")) {
    required <- c("Level", "Subgroup", "Number Suspended Short Term", "Number of Short Term Suspendable Incidents")
    minimum_per_year <- 9000L
  } else if (startsWith(name, "vdoe_long_term_suspensions")) {
    required <- c("Level", "Subgroup", "Number Suspended Long Term", "Number of Long Term Suspendable Incidents")
    minimum_per_year <- 9000L
  } else if (startsWith(name, "vdoe_expulsions")) {
    required <- c("Level", "Subgroup", "Number Expelled", "Number of Expellable Incidents")
    minimum_per_year <- 9000L
  } else {
    stop("No VDOE schema rule for ", name)
  }
  all(required %in% names(df)) && length(unique(df$Year)) >= 1 &&
    all(table(df$Year) >= minimum_per_year)
}

vdoe_short_path <- download_cached("vdoe_short_term_suspensions.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "learningClimate",
    "indicators[]" = "shortTermSuspensions",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_long_path <- download_cached("vdoe_long_term_suspensions.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "learningClimate",
    "indicators[]" = "longTermSuspensions",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_enroll_path <- download_cached("vdoe_enrollment.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "enrollment",
    "indicators[]" = "fallMembership",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_abs_path <- download_cached("vdoe_absenteeism.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "learningClimate",
    "indicators[]" = "absenteeism",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_expulsions_path <- download_cached("vdoe_expulsions.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "learningClimate",
    "indicators[]" = "expulsions",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_fr_elig_path <- download_cached("vdoe_fr_eligibility.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "learningClimate",
    "indicators[]" = "frEligibility",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_fr_breakfast_path <- download_cached("vdoe_fr_breakfast.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "learningClimate",
    "indicators[]" = "frBreakfast",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_fr_lunch_path <- download_cached("vdoe_fr_lunch.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "learningClimate",
    "indicators[]" = "frLunch",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_acc_eng_path <- download_cached("vdoe_accreditation_english.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "accreditation",
    "indicators[]" = "1",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_acc_math_path <- download_cached("vdoe_accreditation_math.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "accreditation",
    "indicators[]" = "3",
    year = "all"
  ))
}, validator = validate_not_html)

vdoe_acc_sci_path <- download_cached("vdoe_accreditation_science.csv", function(dest) {
  curl_post_download(dest, vdoe_url, list(
    level = "schools",
    "divisions[]" = "any",
    "schools[]" = "any",
    type = "accreditation",
    "indicators[]" = "5",
    year = "all"
  ))
}, validator = validate_not_html)

short_df <- read_vdoe_csv(vdoe_short_path)
long_df <- read_vdoe_csv(vdoe_long_path)
enroll_df <- read_vdoe_csv(vdoe_enroll_path)
abs_df <- read_vdoe_csv(vdoe_abs_path)
expulsions_df <- read_vdoe_csv(vdoe_expulsions_path)
fr_elig_df <- read_vdoe_csv(vdoe_fr_elig_path)
fr_breakfast_df <- read_vdoe_csv(vdoe_fr_breakfast_path)
fr_lunch_df <- read_vdoe_csv(vdoe_fr_lunch_path)
acc_eng_df <- read_vdoe_csv(vdoe_acc_eng_path)
acc_math_df <- read_vdoe_csv(vdoe_acc_math_path)
acc_sci_df <- read_vdoe_csv(vdoe_acc_sci_path)

# The live VDOE download keeps a rolling three-year window. Retain baseline
# public CSV rows only for years absent from the current response.
history_path <- function(name) file.path("data-raw", "historical", paste0("vdoe_", name, "_baseline.csv.gz"))
append_missing_years <- function(current, name) {
  path <- history_path(name)
  if (!file.exists(path)) stop("Historical source file missing: ", path)
  old <- read.csv(gzfile(path), stringsAsFactors = FALSE, check.names = FALSE)
  if (!identical(names(old), names(current))) stop("Historical source schema changed: ", name)
  old <- old[!old$Year %in% unique(current$Year), , drop = FALSE]
  if (nrow(old)) message("Retained ", nrow(old), " archived rows for ", name,
                         " in years ", paste(unique(old$Year), collapse = ", "))
  dplyr::bind_rows(current, old)
}
enroll_df <- append_missing_years(enroll_df, "enrollment")
abs_df <- append_missing_years(abs_df, "absenteeism")
fr_elig_df <- append_missing_years(fr_elig_df, "fr_eligibility")
fr_breakfast_df <- append_missing_years(fr_breakfast_df, "fr_breakfast")
fr_lunch_df <- append_missing_years(fr_lunch_df, "fr_lunch")

# The export has a school total row and grade rows. Check that they agree, then
# use only the reported total. Adding both sets of rows doubles enrollment.
enroll_school_rows <- enroll_df %>%
  transmute(
    year = vapply(Year, parse_end_year, integer(1)),
    division_name = Division,
    school_name = School,
    level = Level,
    grade = trimws(as.character(Grade)),
    count = suppressWarnings(as.integer(Count))
  ) %>%
  filter(level == "SCH")
if (anyNA(enroll_school_rows$count)) {
  stop('Missing or invalid VDOE enrollment count.')
}
enroll_totals <- enroll_school_rows %>%
  group_by(year, division_name, school_name) %>%
  summarise(total_rows = sum(is.na(grade) | grade == ''),
            enrollment = sum(count[is.na(grade) | grade == '']),
            grade_sum = sum(count[!is.na(grade) & grade != '']),
            grade_rows = sum(!is.na(grade) & grade != ''),
            .groups = 'drop')
if (any(enroll_totals$total_rows != 1 | enroll_totals$grade_rows == 0 |
        enroll_totals$enrollment != enroll_totals$grade_sum)) {
  stop('VDOE school enrollment total is missing or differs from grade counts.')
}
enroll_totals <- enroll_totals %>% select(year, division_name, school_name, enrollment)

derive_school_level <- function(min_grade, max_grade) {
  if (!is.finite(min_grade) || !is.finite(max_grade)) return(NA_character_)
  if (max_grade <= 5) return("Elementary")
  if (min_grade >= 6 && max_grade <= 8) return("Middle")
  if (min_grade >= 9) return("High")
  if (max_grade <= 8) return("Combined (K-8)")
  "Combined"
}

# Approximate school level using grade-span from VDOE enrollment counts.
enroll_grade_span <- enroll_df %>%
  transmute(
    year = vapply(Year, parse_end_year, integer(1)),
    division_name = Division,
    school_name = School,
    level = Level,
    grade = trimws(as.character(Grade)),
    count = suppressWarnings(as.integer(Count))
  ) %>%
  filter(level == "SCH", !is.na(grade), grade != "") %>%
  mutate(
    grade_num = case_when(
      grade %in% c("PG", "PK") ~ -1L,
      grade == "KG" ~ 0L,
      grepl("^\\d+$", grade) ~ suppressWarnings(as.integer(grade)),
      TRUE ~ NA_integer_
    )
  ) %>%
  group_by(year, division_name, school_name) %>%
  summarise(
    min_grade = if (any(is.finite(grade_num))) min(grade_num, na.rm = TRUE) else NA_integer_,
    max_grade = if (any(is.finite(grade_num))) max(grade_num, na.rm = TRUE) else NA_integer_,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(school_level = derive_school_level(min_grade, max_grade)) %>%
  ungroup() %>%
  select(year, division_name, school_name, school_level)

parse_num <- function(x) {
  # Handles "<" suppression and empty strings.
  x <- trimws(as.character(x))
  suppressed <- x %in% c("<", "*", "—", "-", "")
  num <- suppressWarnings(as.numeric(x))
  num[suppressed] <- NA_real_
  list(value = num, suppressed = suppressed)
}

extract_acc_combined <- function(df, subject_id) {
  stop("extract_acc_combined() is deprecated; use extract_subject_perf().")
}

extract_subject_perf <- function(df, subject) {
  acr <- parse_num(df[["Accreditation Combined Rate"]])
  pass <- parse_num(df[["Percent Passing"]])

  df %>%
    transmute(
      year = vapply(Year, parse_end_year, integer(1)),
      division_name = Division,
      school_name = School,
      indicator = Indicator,
      subgroup = Subgroup,
      level = `Level Code`,
      subject = subject,
      acr_rate = acr$value,
      acr_suppressed = acr$suppressed,
      pass_rate = pass$value,
      pass_suppressed = pass$suppressed
    ) %>%
    filter(level == "SCH", subgroup == "All Students")
}

stg_perf <- bind_rows(
  extract_subject_perf(acc_eng_df, "English (ELA)"),
  extract_subject_perf(acc_math_df, "Math"),
  extract_subject_perf(acc_sci_df, "Science")
)

overall_perf <- stg_perf %>%
  group_by(year, division_name, school_name) %>%
  summarise(
    test_overall_perf_index = mean(acr_rate, na.rm = TRUE),
    test_overall_pass_index = mean(pass_rate, na.rm = TRUE),
    n_subjects_acr_present = sum(is.finite(acr_rate)),
    n_subjects_pass_present = sum(is.finite(pass_rate)),
    test_overall_perf_suppressed = any((acr_suppressed %in% TRUE)) | n_subjects_acr_present == 0,
    test_overall_pass_suppressed = any((pass_suppressed %in% TRUE)) | n_subjects_pass_present == 0,
    .groups = "drop"
  )

extract_absenteeism <- function(df) {
  # Chronic absenteeism: percent of students missing 10%+ of school days.
  #
  # VDOE export includes multiple subgroups; we keep "All Students" only.
  #
  # Columns observed (2026-02 snapshot):
  # - Count Below 10, Count Above 10
  # - Percent Below 10, Percent above 10
  pct_above <- parse_num(df[["Percent above 10"]])
  cnt_above <- parse_num(df[["Count Above 10"]])
  cnt_below <- parse_num(df[["Count Below 10"]])

  df %>%
    transmute(
      year = vapply(Year, parse_end_year, integer(1)),
      division_name = Division,
      school_name = School,
      level = Level,
      subgroup = Subgroup,
      chronic_absent_pct = pct_above$value,
      chronic_absent_suppressed = pct_above$suppressed | isTRUE(cnt_above$suppressed) | isTRUE(cnt_below$suppressed),
      numerator = cnt_above$value,
      denominator = cnt_above$value + cnt_below$value
    ) %>%
    filter(level == "SCH", subgroup == "All Students") %>%
    mutate(
      chronic_absent_pct = pmin(pmax(chronic_absent_pct, 0), 100),
      chronic_absent_suppressed = (chronic_absent_suppressed %in% TRUE) | is.na(chronic_absent_pct)
    )
}

extract_fr_percent <- function(df, metric_id) {
  # School nutrition exports are school-level percent-only series (no subgroup).
  p <- parse_num(df[["Percent"]])
  df %>%
    transmute(
      year = vapply(Year, parse_end_year, integer(1)),
      division_name = Division,
      school_name = School,
      level = Level,
      metric_id = metric_id,
      value = p$value,
      suppressed = p$suppressed
    ) %>%
    filter(level == "SCH") %>%
    mutate(
      value = pmin(pmax(value, 0), 100),
      suppressed = (suppressed %in% TRUE) | is.na(value)
    )
}

abs_keep <- extract_absenteeism(abs_df)
fr_keep <- bind_rows(
  extract_fr_percent(fr_elig_df, "needs_meal_eligibility_pct"),
  extract_fr_percent(fr_breakfast_df, "needs_breakfast_participation_pct"),
  extract_fr_percent(fr_lunch_df, "needs_lunch_participation_pct")
)

extract_susp_counts <- function(df, n_students_col, n_incidents_col, metric_tag) {
  n_students <- parse_num(df[[n_students_col]])
  n_incidents <- parse_num(df[[n_incidents_col]])
  df %>%
    transmute(
      year = vapply(Year, parse_end_year, integer(1)),
      # Note: VDOE export labels these columns as School Division then School.
      division_name = School,
      school_name = Division,
      subgroup = Subgroup,
      level = Level,
      students_n = n_students$value,
      students_suppressed = n_students$suppressed,
      incidents_n = n_incidents$value,
      incidents_suppressed = n_incidents$suppressed
    ) %>%
    filter(level == "SCH") %>%
    mutate(metric_tag = metric_tag)
}

short_counts <- extract_susp_counts(
  short_df,
  "Number Suspended Short Term",
  "Number of Short Term Suspendable Incidents",
  "short"
)
long_counts <- extract_susp_counts(
  long_df,
  "Number Suspended Long Term",
  "Number of Long Term Suspendable Incidents",
  "long"
)

race_susp_groups <- c(
  "American Indian",
  "Asian",
  "Black",
  "Hispanic",
  "Multiple Races",
  "Native Hawaiian",
  "White"
)

susp_totals <- bind_rows(short_counts, long_counts) %>%
  filter(subgroup %in% race_susp_groups) %>%
  group_by(year, division_name, school_name, metric_tag) %>%
  summarise(
    students_n = if (any(is.finite(students_n))) sum(students_n, na.rm = TRUE) else NA_real_,
    students_suppressed = !any(is.finite(students_n)) | any((students_suppressed %in% TRUE)),
    incidents_n = if (any(is.finite(incidents_n))) sum(incidents_n, na.rm = TRUE) else NA_real_,
    incidents_suppressed = !any(is.finite(incidents_n)) | any((incidents_suppressed %in% TRUE)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = metric_tag, values_from = c(students_n, students_suppressed, incidents_n, incidents_suppressed), names_sep = "_") %>%
  mutate(
    susp_short_n = students_n_short,
    susp_long_n = students_n_long,
    susp_short_suppressed = (students_suppressed_short %in% TRUE) | is.na(susp_short_n),
    susp_long_suppressed = (students_suppressed_long %in% TRUE) | is.na(susp_long_n),
    susp_incidents_short_n = incidents_n_short,
    susp_incidents_long_n = incidents_n_long,
    susp_incidents_short_suppressed = (incidents_suppressed_short %in% TRUE) | is.na(susp_incidents_short_n),
    susp_incidents_long_suppressed = (incidents_suppressed_long %in% TRUE) | is.na(susp_incidents_long_n),
    susp_n = coalesce(susp_short_n, 0) + coalesce(susp_long_n, 0),
    susp_suppressed = (susp_short_suppressed %in% TRUE) | (susp_long_suppressed %in% TRUE) |
      (is.na(susp_short_n) & is.na(susp_long_n)),
    susp_incidents_n = coalesce(susp_incidents_short_n, 0) + coalesce(susp_incidents_long_n, 0),
    susp_incidents_suppressed = (susp_incidents_short_suppressed %in% TRUE) | (susp_incidents_long_suppressed %in% TRUE) |
      (is.na(susp_incidents_short_n) & is.na(susp_incidents_long_n))
  )

extract_expulsions_totals <- function(df) {
  expelled <- parse_num(df[["Number Expelled"]])
  incidents <- parse_num(df[["Number of Expellable Incidents"]])

  df %>%
    transmute(
      year = vapply(Year, parse_end_year, integer(1)),
      division_name = School,
      school_name = Division,
      subgroup = Subgroup,
      level = Level,
      expelled_n = expelled$value,
      expelled_suppressed = expelled$suppressed,
      expel_incidents_n = incidents$value,
      expel_incidents_suppressed = incidents$suppressed
    ) %>%
    filter(level == "SCH", subgroup %in% race_susp_groups) %>%
    group_by(year, division_name, school_name) %>%
    summarise(
      expelled_n = if (any(is.finite(expelled_n))) sum(expelled_n, na.rm = TRUE) else NA_real_,
      expelled_suppressed = !any(is.finite(expelled_n)) | any((expelled_suppressed %in% TRUE)),
      expel_incidents_n = if (any(is.finite(expel_incidents_n))) sum(expel_incidents_n, na.rm = TRUE) else NA_real_,
      expel_incidents_suppressed = !any(is.finite(expel_incidents_n)) | any((expel_incidents_suppressed %in% TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      expelled_suppressed = (expelled_suppressed %in% TRUE) | is.na(expelled_n),
      expel_incidents_suppressed = (expel_incidents_suppressed %in% TRUE) | is.na(expel_incidents_n)
    )
}

expulsions_keep <- extract_expulsions_totals(expulsions_df)

# Historical race/ethnicity % in 2023–24 comes from the suspension export.
# The later school years use checked Fall Membership counts below.
race_map <- c(
  "American Indian" = "demo_american_indian_pct",
  "Asian" = "demo_asian_pct",
  "Black" = "demo_black_pct",
  "Hispanic" = "demo_hispanic_pct",
  "Multiple Races" = "demo_two_plus_pct",
  "Native Hawaiian" = "demo_native_hawaiian_pct",
  "White" = "demo_white_pct"
)

race_raw <- short_df %>%
  transmute(
    year = vapply(Year, parse_end_year, integer(1)),
    division_name = School,
    school_name = Division,
    subgroup = Subgroup,
    level = Level,
    pct_student_pop_raw = `Percent of the Student Population`
  ) %>%
  filter(level == "SCH", subgroup %in% names(race_map)) %>%
  mutate(metric_id = unname(race_map[subgroup])) %>%
  {
    p <- parse_num(.$pct_student_pop_raw)
    mutate(., value_raw = p$value, suppressed_raw = p$suppressed)
  } %>%
  select(year, division_name, school_name, metric_id, value_raw, suppressed_raw)

# Normalize race percentages to sum to 100 per school-year (handles rounding and
# partial missingness). This makes the race category consistent for the UI.
race_df <- race_raw %>%
  filter(year <= 2024) %>%
  group_by(year, division_name, school_name) %>%
  mutate(
    total = sum(value_raw, na.rm = TRUE),
    value = ifelse(is.finite(total) & total > 0, 100 * value_raw / total, NA_real_),
    suppressed = (suppressed_raw %in% TRUE) | is.na(value)
  ) %>%
  ungroup() %>%
  mutate(value = pmin(pmax(value, 0), 100)) %>%
  select(year, division_name, school_name, metric_id, value, suppressed)

# Keep recent years from any available measure. Each metric retains its own
# source years; the historical accreditation export ends in 2024.
years_all <- sort(unique(c(overall_perf$year, susp_totals$year, abs_keep$year,
                           expulsions_keep$year, enroll_totals$year,
                           race_df$year, fr_keep$year)))
if (length(years_all) < 1 || anyNA(years_all)) stop("Invalid or missing source years")
years_keep <- tail(years_all, 5)
message("Keeping years: ", paste(years_keep, collapse = ", "))

overall_perf_keep <- overall_perf %>% filter(year %in% years_keep)
stg_perf_keep <- stg_perf %>% filter(year %in% years_keep)
susp_keep <- susp_totals %>% filter(year %in% years_keep)
abs_keep <- abs_keep %>% filter(year %in% years_keep)
expulsions_keep <- expulsions_keep %>% filter(year %in% years_keep)
enroll_keep <- enroll_totals %>% filter(year %in% years_keep)
race_keep <- race_df %>% filter(year %in% years_keep)
fr_keep <- fr_keep %>% filter(year %in% years_keep)

core_keys <- dplyr::bind_rows(
  overall_perf_keep %>% select(year, division_name, school_name),
  susp_keep %>% select(year, division_name, school_name),
  abs_keep %>% select(year, division_name, school_name),
  expulsions_keep %>% select(year, division_name, school_name),
  enroll_keep %>% select(year, division_name, school_name),
  fr_keep %>% select(year, division_name, school_name)
) %>% distinct()

core_join <- core_keys %>%
  left_join(overall_perf_keep %>% select(
    year,
    division_name,
    school_name,
    test_overall_perf_index,
    test_overall_pass_index,
    test_overall_perf_suppressed,
    test_overall_pass_suppressed,
    n_subjects_acr_present,
    n_subjects_pass_present
  ), by = c("year", "division_name", "school_name")) %>%
  left_join(
    susp_keep %>%
      select(
        year, division_name, school_name,
        susp_short_n, susp_short_suppressed,
        susp_long_n, susp_long_suppressed,
        susp_n, susp_suppressed,
        susp_incidents_short_n, susp_incidents_short_suppressed,
        susp_incidents_long_n, susp_incidents_long_suppressed,
        susp_incidents_n, susp_incidents_suppressed
      ),
    by = c("year", "division_name", "school_name")
  ) %>%
  left_join(
    abs_keep %>% select(year, division_name, school_name, chronic_absent_pct, chronic_absent_suppressed, numerator, denominator),
    by = c("year", "division_name", "school_name")
  ) %>%
  rename(
    chronic_absent_n = numerator,
    chronic_absent_den = denominator
  ) %>%
  left_join(
    expulsions_keep %>% select(year, division_name, school_name, expelled_n, expelled_suppressed, expel_incidents_n, expel_incidents_suppressed),
    by = c("year", "division_name", "school_name")
  ) %>%
  left_join(enroll_keep, by = c("year", "division_name", "school_name"))

core_join <- core_join %>%
  mutate(
    behavior_susp_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * susp_n / enrollment, NA_real_),
    behavior_susp_suppressed = (susp_suppressed %in% TRUE) | is.na(behavior_susp_per_100),
    behavior_susp_short_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * susp_short_n / enrollment, NA_real_),
    behavior_susp_short_suppressed = (susp_short_suppressed %in% TRUE) | is.na(behavior_susp_short_per_100),
    behavior_susp_long_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * susp_long_n / enrollment, NA_real_),
    behavior_susp_long_suppressed = (susp_long_suppressed %in% TRUE) | is.na(behavior_susp_long_per_100),
    behavior_susp_incidents_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * susp_incidents_n / enrollment, NA_real_),
    behavior_susp_incidents_suppressed = (susp_incidents_suppressed %in% TRUE) | is.na(behavior_susp_incidents_per_100),
    behavior_susp_incidents_short_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * susp_incidents_short_n / enrollment, NA_real_),
    behavior_susp_incidents_short_suppressed = (susp_incidents_short_suppressed %in% TRUE) | is.na(behavior_susp_incidents_short_per_100),
    behavior_susp_incidents_long_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * susp_incidents_long_n / enrollment, NA_real_),
    behavior_susp_incidents_long_suppressed = (susp_incidents_long_suppressed %in% TRUE) | is.na(behavior_susp_incidents_long_per_100),
    behavior_expulsions_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * expelled_n / enrollment, NA_real_),
    behavior_expulsions_suppressed = (expelled_suppressed %in% TRUE) | is.na(behavior_expulsions_per_100),
    behavior_expulsion_incidents_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * expel_incidents_n / enrollment, NA_real_),
    behavior_expulsion_incidents_suppressed = (expel_incidents_suppressed %in% TRUE) | is.na(behavior_expulsion_incidents_per_100),
    attendance_chronic_absent_pct = chronic_absent_pct,
    attendance_chronic_absent_suppressed = (chronic_absent_suppressed %in% TRUE) | is.na(attendance_chronic_absent_pct)
  )

# Attach enrollment to race metrics so we can derive numerator/denominator
# for division aggregation (and to support totals-based % at division level).
race_keep <- race_keep %>%
  left_join(enroll_keep, by = c("year", "division_name", "school_name")) %>%
  mutate(
    numerator = ifelse(is.finite(enrollment) & enrollment > 0 & is.finite(value), enrollment * value / 100, NA_real_),
    denominator = as.numeric(enrollment)
  )

# Download NCES EDGE geocodes:
# - Public schools (points)
# - Public LEAs (for LEA names by LEAID)
#
# Note: the NCES school zip contains a nested shapefile zip. We use `ogr2ogr`
# to extract only Virginia rows to a CSV for fast, stable parsing.
nces_sch_zip_url <- "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_2425.zip"
nces_lea_zip_url <- "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICLEA_2425.zip"

nces_sch_zip_path <- download_cached("EDGE_GEOCODE_PUBLICSCH_2425.zip", function(dest) {
  curl_get_download(dest, nces_sch_zip_url)
})
nces_lea_zip_path <- download_cached("EDGE_GEOCODE_PUBLICLEA_2425.zip", function(dest) {
  curl_get_download(dest, nces_lea_zip_url)
})

extract_nested_shapefile <- function(zip_path, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  listing <- utils::unzip(zip_path, list = TRUE)
  nested_zip_candidates <- listing$Name[grepl("^Shapefile_.*\\.zip$", listing$Name)]
  shp_candidates <- listing$Name[grepl("\\.shp$", listing$Name, ignore.case = TRUE)]

  if (length(nested_zip_candidates) > 0) {
    nested_zip_name <- nested_zip_candidates[[1]]
    nested_zip_path <- file.path(out_dir, nested_zip_name)
    if (!file.exists(nested_zip_path)) utils::unzip(zip_path, files = nested_zip_name, exdir = out_dir)

    shp_dir <- file.path(out_dir, "shp")
    if (!dir.exists(shp_dir)) dir.create(shp_dir, recursive = TRUE)
    utils::unzip(nested_zip_path, exdir = shp_dir)
    shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
    if (length(shp_files) == 0) stop("No .shp found after extracting ", nested_zip_name)
    return(shp_files[[1]])
  }

  if (length(shp_candidates) > 0) {
    # The zip already contains shapefile parts directly.
    utils::unzip(zip_path, exdir = out_dir)
    shp_files <- list.files(out_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    if (length(shp_files) == 0) stop("No .shp found after unzipping ", basename(zip_path))
    return(shp_files[[1]])
  }

  stop("No shapefile found inside ", basename(zip_path))
}

state_fips <- "51"

extract_shp_to_csv <- function(shp_path, csv_path, where_clause, select_fields) {
  out_dir <- dirname(csv_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  if (file.exists(csv_path) && file.info(csv_path)$size > 0) return(invisible(TRUE))

  args <- c(
    "-f", "CSV",
    "-select", paste(select_fields, collapse = ","),
    csv_path,
    shp_path
  )
  if (!is.null(where_clause) && nzchar(where_clause)) {
    args <- c("-where", where_clause, args)
  }

  system2("ogr2ogr", args = args)

  if (!file.exists(csv_path) || file.info(csv_path)$size == 0) {
    stop("ogr2ogr did not produce a CSV for ", shp_path)
  }
  invisible(TRUE)
}

nces_dir <- file.path(input_dir, "nces_edge_2425")

sch_shp <- extract_nested_shapefile(nces_sch_zip_path, file.path(nces_dir, "schools"))
lea_shp <- extract_nested_shapefile(nces_lea_zip_path, file.path(nces_dir, "leas"))

sch_csv <- file.path(nces_dir, "va_public_schools.csv")
lea_csv <- file.path(nces_dir, "va_public_leas.csv")

extract_shp_to_csv(
  sch_shp,
  sch_csv,
  NULL,
  c("NCESSCH", "LEAID", "NAME", "STFIP", "LAT", "LON")
)
extract_shp_to_csv(
  lea_shp,
  lea_csv,
  NULL,
  c("LEAID", "NAME", "STFIP")
)

nces_sch_va <- read.csv(sch_csv, stringsAsFactors = FALSE, check.names = FALSE)
nces_lea_va <- read.csv(lea_csv, stringsAsFactors = FALSE, check.names = FALSE)

# The NCES EDGE geocode files are national. Filter to Virginia early to avoid
# name-collision mismatches (e.g., "Frederick County" exists in multiple states).
#
# Note: In this pipeline, `state_fips` is a string (e.g. "51"). EDGE uses STFIP.
stfip_target <- suppressWarnings(as.integer(state_fips))
if (is.finite(stfip_target)) {
  nces_sch_va <- nces_sch_va %>% filter(as.integer(STFIP) == stfip_target)
  nces_lea_va <- nces_lea_va %>% filter(as.integer(STFIP) == stfip_target)
}

names(nces_lea_va)[names(nces_lea_va) == "NAME"] <- "LEA_NAME"

nces_va <- nces_sch_va %>%
  left_join(nces_lea_va %>% select(LEAID, LEA_NAME), by = "LEAID") %>%
  transmute(
    school_id = as.character(NCESSCH),
    nces_school_name = as.character(NAME),
    stfip = as.character(STFIP),
    lat = as.numeric(LAT),
    lon = as.numeric(LON),
    division_id = as.character(LEAID),
    nces_division_name = as.character(LEA_NAME)
  )

# Build VDOE school list from core_join and match to NCES by normalized division+school names.
vdoe_schools <- core_join %>%
  distinct(division_name, school_name) %>%
  mutate(
    # Include state FIPS to avoid cross-state name collisions (e.g., "Frederick County"
    # exists in multiple states).
    k = paste(norm_key(division_name), norm_key(school_name), state_fips, sep = "::")
  )

nces_keys <- nces_va %>%
  mutate(
    k = paste(norm_key(nces_division_name), norm_key(nces_school_name), stfip, sep = "::")
  )

matched <- vdoe_schools %>%
  left_join(nces_keys %>% group_by(k) %>% slice(1) %>% ungroup(), by = "k") %>%
  group_by(k) %>%
  slice(1) %>%
  ungroup()

match_rate <- mean(!is.na(matched$school_id))
message(sprintf("NCES match rate: %.1f%% (%d/%d)", 100 * match_rate, sum(!is.na(matched$school_id)), nrow(matched)))
if (!is.finite(match_rate) || match_rate < 0.80) {
  stop("NCES match rate too low. Improve normalization/matching before generating a snapshot.")
}

enrollment_latest_by_school <- enroll_keep %>%
  left_join(matched %>% select(division_name, school_name, school_id), by = c("division_name", "school_name")) %>%
  filter(!is.na(school_id), is.finite(enrollment)) %>%
  group_by(school_id) %>%
  arrange(desc(year), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    school_id = school_id,
    enrollment_latest = as.integer(round(enrollment))
  )

school_level_by_school <- enroll_grade_span %>%
  filter(year %in% years_keep) %>%
  left_join(matched %>% select(division_name, school_name, school_id), by = c("division_name", "school_name")) %>%
  filter(!is.na(school_id), !is.na(school_level)) %>%
  group_by(school_id) %>%
  arrange(desc(year), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(school_id = school_id, school_level = school_level)

schools_out <- matched %>%
  transmute(
    school_id = school_id,
    school_name = school_name,
    division_id = division_id,
    division_name = division_name,
    lat = lat,
    lon = lon,
    school_level = NA_character_
  ) %>%
  left_join(school_level_by_school, by = "school_id", suffix = c("", "_derived")) %>%
  mutate(school_level = coalesce(school_level_derived, school_level)) %>%
  select(-school_level_derived) %>%
  left_join(enrollment_latest_by_school, by = "school_id") %>%
  filter(!is.na(school_id), is.finite(lat), is.finite(lon)) %>%
  distinct(school_id, .keep_all = TRUE)

# Build metrics table.
subject_to_metric_suffix <- function(subject) {
  dplyr::case_when(
    subject == "English (ELA)" ~ "ela",
    subject == "Math" ~ "math",
    subject == "Science" ~ "science",
    TRUE ~ NA_character_
  )
}

perf_subject_metrics <- stg_perf_keep %>%
  left_join(
    core_join %>% select(year, division_name, school_name, enrollment),
    by = c("year", "division_name", "school_name")
  ) %>%
  mutate(metric_suffix = subject_to_metric_suffix(subject)) %>%
  filter(!is.na(metric_suffix)) %>%
  left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
  filter(!is.na(school_id)) %>%
  transmute(
    school_id = school_id,
    year = year,
    subject = subject,
    acr_rate = pmin(acr_rate, 100),
    acr_suppressed = (acr_suppressed %in% TRUE) | is.na(acr_rate),
    pass_rate = pmin(pass_rate, 100),
    pass_suppressed = (pass_suppressed %in% TRUE) | is.na(pass_rate),
    denominator = as.numeric(enrollment)
  )

school_subject_perf_out <- perf_subject_metrics %>%
  transmute(
    school_id = school_id,
    year = year,
    subject = subject,
    subgroup = "All Students",
    acr_rate = acr_rate,
    pass_rate = pass_rate,
    acr_suppressed = acr_suppressed,
    pass_suppressed = pass_suppressed,
    denominator = denominator,
    source = "vdoe_accreditation_rates"
  )

school_metrics_out <- bind_rows(
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "test_overall_perf_index",
      value = pmin(test_overall_perf_index, 100),
      unit = "index",
      suppressed = (test_overall_perf_suppressed %in% TRUE) | is.na(test_overall_perf_index),
      numerator = ifelse(is.finite(enrollment) & enrollment > 0 & is.finite(test_overall_perf_index), test_overall_perf_index * enrollment, NA_real_),
      denominator = as.numeric(enrollment),
      source = "vdoe_accreditation_rates"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "test_overall_pass_index",
      value = pmin(test_overall_pass_index, 100),
      unit = "index",
      suppressed = (test_overall_pass_suppressed %in% TRUE) | is.na(test_overall_pass_index),
      numerator = ifelse(is.finite(enrollment) & enrollment > 0 & is.finite(test_overall_pass_index), test_overall_pass_index * enrollment, NA_real_),
      denominator = as.numeric(enrollment),
      source = "vdoe_accreditation_rates"
    ),
  perf_subject_metrics %>%
    mutate(metric_suffix = subject_to_metric_suffix(subject)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = paste0("test_acr_", metric_suffix),
      value = acr_rate,
      unit = "index",
      suppressed = acr_suppressed,
      numerator = ifelse(is.finite(denominator) & denominator > 0 & is.finite(acr_rate), acr_rate * denominator, NA_real_),
      denominator = denominator,
      source = "vdoe_accreditation_rates"
    ),
  perf_subject_metrics %>%
    mutate(metric_suffix = subject_to_metric_suffix(subject)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = paste0("test_pass_", metric_suffix),
      value = pass_rate,
      unit = "index",
      suppressed = pass_suppressed,
      numerator = ifelse(is.finite(denominator) & denominator > 0 & is.finite(pass_rate), pass_rate * denominator, NA_real_),
      denominator = denominator,
      source = "vdoe_accreditation_rates"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_susp_per_100",
      value = behavior_susp_per_100,
      unit = "per_100",
      suppressed = behavior_susp_suppressed,
      numerator = as.numeric(susp_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_suspensions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_susp_short_per_100",
      value = behavior_susp_short_per_100,
      unit = "per_100",
      suppressed = behavior_susp_short_suppressed,
      numerator = as.numeric(susp_short_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_suspensions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_susp_long_per_100",
      value = behavior_susp_long_per_100,
      unit = "per_100",
      suppressed = behavior_susp_long_suppressed,
      numerator = as.numeric(susp_long_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_suspensions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_susp_incidents_per_100",
      value = behavior_susp_incidents_per_100,
      unit = "per_100",
      suppressed = behavior_susp_incidents_suppressed,
      numerator = as.numeric(susp_incidents_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_suspensions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_susp_incidents_short_per_100",
      value = behavior_susp_incidents_short_per_100,
      unit = "per_100",
      suppressed = behavior_susp_incidents_short_suppressed,
      numerator = as.numeric(susp_incidents_short_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_suspensions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_susp_incidents_long_per_100",
      value = behavior_susp_incidents_long_per_100,
      unit = "per_100",
      suppressed = behavior_susp_incidents_long_suppressed,
      numerator = as.numeric(susp_incidents_long_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_suspensions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_expulsions_per_100",
      value = behavior_expulsions_per_100,
      unit = "per_100",
      suppressed = behavior_expulsions_suppressed,
      numerator = as.numeric(expelled_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_expulsions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "behavior_expulsion_incidents_per_100",
      value = behavior_expulsion_incidents_per_100,
      unit = "per_100",
      suppressed = behavior_expulsion_incidents_suppressed,
      numerator = as.numeric(expel_incidents_n),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_expulsions+vdoe_enrollment"
    ),
  core_join %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = "attendance_chronic_absent_pct",
      value = attendance_chronic_absent_pct,
      unit = "pct",
      suppressed = attendance_chronic_absent_suppressed,
      numerator = ifelse(is.finite(chronic_absent_n) & is.finite(chronic_absent_den) & chronic_absent_den > 0, chronic_absent_n,
        ifelse(is.finite(enrollment) & enrollment > 0 & is.finite(attendance_chronic_absent_pct), attendance_chronic_absent_pct * enrollment / 100, NA_real_)
      ),
      denominator = ifelse(is.finite(chronic_absent_den) & chronic_absent_den > 0, chronic_absent_den, as.numeric(enrollment)),
      source = "vdoe_learning_climate_absenteeism"
    ),
  fr_keep %>%
    left_join(enroll_keep, by = c("year", "division_name", "school_name")) %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = metric_id,
      value = value,
      unit = "pct",
      suppressed = suppressed,
      numerator = ifelse(is.finite(enrollment) & enrollment > 0 & is.finite(value), enrollment * value / 100, NA_real_),
      denominator = as.numeric(enrollment),
      source = "vdoe_learning_climate_school_nutrition+vdoe_enrollment"
    ),
  race_keep %>%
    left_join(matched %>% select(division_name, school_name, school_id, division_id), by = c("division_name", "school_name")) %>%
    filter(!is.na(school_id)) %>%
    transmute(
      school_id = school_id,
      year = year,
      metric_id = metric_id,
      value = value,
      unit = "pct",
      suppressed = suppressed,
      numerator = numerator,
      denominator = denominator,
      source = "vdoe_learning_climate_short_term_suspensions"
    )
) %>%
  filter(
    !(startsWith(metric_id, "test_overall_") & !year %in% overall_perf_keep$year),
    !(startsWith(metric_id, "behavior_susp") & !year %in% susp_keep$year),
    !(startsWith(metric_id, "behavior_exp") & !year %in% expulsions_keep$year),
    !(metric_id == "attendance_chronic_absent_pct" & !year %in% abs_keep$year)
  ) %>%
  mutate(
    suppressed = (suppressed %in% TRUE) | !is.finite(value),
    value = ifelse(suppressed, NA_real_, value),
    numerator = ifelse(suppressed, NA_real_, numerator),
    denominator = ifelse(suppressed, NA_real_, denominator)
  )

# Annual SOL test-administration data use a separate measure from the
# School Quality Profiles accreditation-export Percent Passing field.
source('data-raw/load_annual_sol.R')
annual_sol_archive <- file.path('data-raw', 'annual_sol_school.csv.gz')
annual_sol <- build_annual_sol_metrics(annual_sol_archive, schools_out,
                                       years_keep, norm_key)
school_metrics_out <- dplyr::bind_rows(school_metrics_out, annual_sol$metrics)
source('data-raw/load_membership_race.R')
membership_race_archive <- file.path('data-raw', 'fall_membership_race_school.csv.gz')
membership_race <- build_membership_race_metrics(membership_race_archive,
                                                 schools_out, years_keep, norm_key)
historical_race_archive <- file.path('data-raw', 'fall_membership_historical_race_school.csv.gz')
historical_enrollment_archive <- file.path('data-raw', 'fall_membership_2022_enrollment.csv.gz')
historical_race <- build_membership_race_metrics(
  historical_race_archive, schools_out, years_keep, norm_key,
  expected_years = 2022:2024,
  source_id = 'vdoe_fall_membership_race_school_historical',
  min_match = 0.94,
  enroll_keep = enroll_keep, matched = matched)
historical_enrollment_by_year <- historical_race$metrics %>%
  dplyr::filter(year == 2022, is.finite(denominator), denominator > 0) %>%
  dplyr::distinct(school_id, year, enrollment = denominator)
enrollment_by_year <- enroll_keep %>%
  dplyr::left_join(matched %>%
    dplyr::select(division_name, school_name, school_id),
    by = c('division_name', 'school_name')) %>%
  dplyr::filter(!is.na(school_id), year %in% years_keep,
                is.finite(enrollment), enrollment > 0,
                school_id %in% schools_out$school_id) %>%
  dplyr::select(school_id, year, enrollment) %>%
  dplyr::bind_rows(historical_enrollment_by_year) %>%
  dplyr::arrange(school_id, year)
if (anyDuplicated(enrollment_by_year[c('school_id', 'year')])) {
  stop('Duplicate mapped school-year enrollment in profile extract.')
}
# Replace the sparse SQP subgroup approximation in 2023–24 with the checked
# Fall Membership source. Missing historical schools remain missing.
school_metrics_out <- school_metrics_out %>%
  dplyr::filter(!(year %in% 2023:2024 & startsWith(metric_id, 'demo_')))
school_metrics_out <- dplyr::bind_rows(school_metrics_out,
                                      historical_race$metrics,
                                      membership_race$metrics)
source('data-raw/load_sbar_sanctions.R')
sbar_archive <- file.path('data-raw', 'sbar_school_sanctions.csv.gz')
sbar <- build_sbar_sanction_metrics(sbar_archive, schools_out, enroll_keep,
                                    matched, years_keep, norm_key,
                                    historical_enrollment_archive)
school_metrics_out <- dplyr::bind_rows(school_metrics_out, sbar$metrics)
if (anyDuplicated(school_metrics_out[c('school_id', 'year', 'metric_id')])) {
  stop('Duplicate school metric keys after supplemental imports.')
}

# Division aggregation.
division_metrics_base <- school_metrics_out %>%
  left_join(matched %>% select(school_id, division_id), by = "school_id") %>%
  filter(!is.na(division_id)) %>%
  group_by(division_id, year, metric_id) %>%
  summarise(
    value = {
      # Prefer totals-based aggregation when numerator/denominator exist.
      #
      # IMPORTANT: Do not treat missing school-level rates as zeros. For example, Science ACR
      # is blank in the VDOE export (all NA), and a naive sum(numerator, na.rm=TRUE) /
      # sum(denominator, na.rm=TRUE) would yield 0, which would incorrectly drag down division
      # means when we later average across subjects.
      unit0 <- dplyr::first(unit)

      if (unit0 %in% c("pct", "per_100")) {
        ok <- is.finite(numerator) & is.finite(denominator) & denominator > 0
        if (any(ok)) {
          100 * sum(numerator[ok], na.rm = TRUE) / sum(denominator[ok], na.rm = TRUE)
        } else if (any(is.finite(value))) {
          mean(value, na.rm = TRUE)
        } else {
          NA_real_
        }
      } else if (unit0 == "index") {
        ok <- is.finite(value) & is.finite(denominator) & denominator > 0
        if (any(ok)) {
          stats::weighted.mean(value[ok], w = denominator[ok], na.rm = TRUE)
        } else if (any(is.finite(value))) {
          mean(value, na.rm = TRUE)
        } else {
          NA_real_
        }
      } else {
        # Fallback: totals if available, otherwise mean of available values.
        ok <- is.finite(numerator) & is.finite(denominator) & denominator > 0
        if (any(ok)) {
          sum(numerator[ok], na.rm = TRUE) / sum(denominator[ok], na.rm = TRUE)
        } else if (any(is.finite(value))) {
          mean(value, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    },
    suppressed = all((suppressed %in% TRUE) | is.na(value)),
    n_included = sum(is.finite(value)),
    n_suppressed = sum((suppressed %in% TRUE) | is.na(value)),
    agg_method = {
      unit0 <- dplyr::first(unit)
      if (unit0 %in% c("pct", "per_100")) "total_rate" else if (unit0 == "index") "weighted_mean" else "mean"
    },
    .groups = "drop"
  )

# For division-level "overall" performance, we intentionally compute the mean
# across the *division subject-level* values (not a direct enrollment-weighted
# average of school-level overall scores).
division_perf_subject <- division_metrics_base %>%
  filter(metric_id %in% c("test_acr_ela", "test_acr_math", "test_acr_science",
                          "test_pass_ela", "test_pass_math", "test_pass_science",
                          "test_sol_annual_ela", "test_sol_annual_math",
                          "test_sol_annual_science")) %>%
  mutate(
    perf_metric = dplyr::case_when(
      grepl("^test_acr_", metric_id) ~ "acr",
      grepl("^test_pass_", metric_id) ~ "pass",
      TRUE ~ "sol"
    )
  )

division_perf_overall <- division_perf_subject %>%
  group_by(division_id, year, perf_metric) %>%
  summarise(
    value = if (all(!is.finite(value))) NA_real_ else mean(value, na.rm = TRUE),
    suppressed = all(!is.finite(value)) | all((suppressed %in% TRUE)),
    n_included = sum(is.finite(value)),
    n_suppressed = sum((suppressed %in% TRUE) | is.na(value)),
    agg_method = "mean_subjects",
    .groups = "drop"
  ) %>%
  mutate(
    metric_id = dplyr::case_when(
      perf_metric == "acr" ~ "test_overall_perf_index",
      perf_metric == "pass" ~ "test_overall_pass_index",
      TRUE ~ "test_sol_annual_overall"
    )
  ) %>%
  select(-perf_metric)

division_metrics_out <- division_metrics_base %>%
  filter(!metric_id %in% c("test_overall_perf_index", "test_overall_pass_index",
                           "test_sol_annual_overall")) %>%
  bind_rows(division_perf_overall)

division_subject_perf_out <- division_perf_subject %>%
  filter(perf_metric %in% c("acr", "pass")) %>%
  mutate(
    subject = dplyr::case_when(
      grepl("_ela$", metric_id) ~ "English (ELA)",
      grepl("_math$", metric_id) ~ "Math",
      grepl("_science$", metric_id) ~ "Science",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(subject)) %>%
  group_by(division_id, year, subject) %>%
  summarise(
    subgroup = "All Students",
    acr_rate = {
      x <- value[perf_metric == "acr"]
      if (length(x) == 0 || all(!is.finite(x))) NA_real_ else x[which(is.finite(x))[1]]
    },
    pass_rate = {
      x <- value[perf_metric == "pass"]
      if (length(x) == 0 || all(!is.finite(x))) NA_real_ else x[which(is.finite(x))[1]]
    },
    acr_suppressed = {
      x <- suppressed[perf_metric == "acr"]
      if (length(x) == 0) TRUE else all((x %in% TRUE))
    },
    pass_suppressed = {
      x <- suppressed[perf_metric == "pass"]
      if (length(x) == 0) TRUE else all((x %in% TRUE))
    },
    weight_basis = "enrollment",
    source = "vdoe_accreditation_rates",
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# QA checks for subject-level performance (ACR + Pass Rate)
# ------------------------------------------------------------------------------
expected_subjects <- c("English (ELA)", "Math", "Science")

if (!all(school_subject_perf_out$subject %in% expected_subjects)) {
  bad <- sort(unique(school_subject_perf_out$subject[!school_subject_perf_out$subject %in% expected_subjects]))
  stop("Unexpected subject labels in school_subject_perf_out: ", paste(bad, collapse = ", "))
}

range_ok <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(TRUE)
  all(x >= 0 & x <= 100)
}
if (!range_ok(school_subject_perf_out$acr_rate) || !range_ok(school_subject_perf_out$pass_rate)) {
  stop("Found out-of-range values in school_subject_perf_out (expected 0..100).")
}

dups <- school_subject_perf_out %>%
  group_by(school_id, year, subject, subgroup) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
if (nrow(dups) > 0) {
  stop("Duplicate keys detected in school_subject_perf_out (school_id, year, subject, subgroup).")
}

miss_school <- school_subject_perf_out %>%
  group_by(year, subject) %>%
  summarise(
    n_rows = n(),
    pct_missing_acr = mean(!is.finite(acr_rate)),
    pct_missing_pass = mean(!is.finite(pass_rate)),
    .groups = "drop"
  ) %>%
  arrange(year, subject)
message("School subject-level missingness (by year, subject):")
print(miss_school)

miss_div <- division_subject_perf_out %>%
  group_by(year, subject) %>%
  summarise(
    n_rows = n(),
    pct_missing_acr = mean(!is.finite(acr_rate)),
    pct_missing_pass = mean(!is.finite(pass_rate)),
    .groups = "drop"
  ) %>%
  arrange(year, subject)
message("Division subject-level missingness (by year, subject):")
print(miss_div)

# Division polygons from TIGER/Line unified school districts (VA).
tiger_zip_url <- "https://www2.census.gov/geo/tiger/TIGER2024/UNSD/tl_2024_51_unsd.zip"
tiger_zip_path <- download_cached("tl_2024_51_unsd.zip", function(dest) {
  curl_get_download(dest, tiger_zip_url)
})

tiger_dir <- file.path(input_dir, "tiger_unsd_2024_va")
if (!dir.exists(tiger_dir)) dir.create(tiger_dir, recursive = TRUE)
utils::unzip(tiger_zip_path, exdir = tiger_dir)

shp_files <- list.files(tiger_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
if (length(shp_files) == 0) stop("No .shp found after unzipping TIGER file.")
shp_path <- shp_files[[1]]

tiger_simplify_tol <- suppressWarnings(as.numeric(Sys.getenv("VA_DASH_DIV_SIMPLIFY_TOL", "0.0015")))
if (!is.finite(tiger_simplify_tol) || tiger_simplify_tol <= 0) tiger_simplify_tol <- 0.0015

tiger_coord_precision <- suppressWarnings(as.integer(Sys.getenv("VA_DASH_DIV_COORD_PRECISION", "5")))
if (!is.finite(tiger_coord_precision) || tiger_coord_precision < 3) tiger_coord_precision <- 5

# Use a separate filename so we can change simplify settings without clobbering
# older cached conversions.
tiger_geojson <- file.path(input_dir, sprintf("tl_2024_51_unsd_s%.4f_p%d.geojson", tiger_simplify_tol, tiger_coord_precision))
if (!file.exists(tiger_geojson) || file.info(tiger_geojson)$size == 0) {
  message("Converting TIGER shapefile -> simplified GeoJSON (this can take a moment)...")
  system2("ogr2ogr", args = c(
    "-f", "GeoJSON",
    "-simplify", format(tiger_simplify_tol, scientific = FALSE),
    "-lco", paste0("COORDINATE_PRECISION=", tiger_coord_precision),
    "-select", "GEOID,NAME",
    tiger_geojson,
    shp_path
  ))
}

geo <- jsonlite::fromJSON(tiger_geojson, simplifyVector = FALSE)
features <- geo$features
if (length(features) == 0) stop("No features found in TIGER GeoJSON.")

division_polys_out <- lapply(features, function(f) {
  props <- f$properties
  geom <- f$geometry

  coords <- geom$coordinates
  # Handle Polygon or MultiPolygon by keeping all outer rings so multipart
  # districts (common in NOVA) are fully represented in the map.
  if (identical(geom$type, "Polygon")) {
    rings <- list(coords[[1]])
  } else if (identical(geom$type, "MultiPolygon")) {
    rings <- lapply(coords, function(poly) poly[[1]])
  } else {
    rings <- NULL
  }
  if (is.null(rings) || length(rings) == 0) return(NULL)

  list(
    division_id = as.character(props$GEOID),
    division_name = as.character(props$NAME),
    rings = rings
  )
})

division_polys_out <- Filter(Negate(is.null), division_polys_out)

# Metric definitions (the app reads this to populate UI labels).
metric_defs_out <- data.frame(
  metric_id = c(
    "test_overall_perf_index",
    "test_overall_pass_index",
    "test_acr_ela",
    "test_acr_math",
    "test_acr_science",
    "test_pass_ela",
    "test_pass_math",
    "test_pass_science",
    "test_sol_annual_overall",
    "test_sol_annual_ela",
    "test_sol_annual_math",
    "test_sol_annual_science",
    "behavior_susp_per_100",
    "behavior_susp_short_per_100",
    "behavior_susp_long_per_100",
    "behavior_susp_incidents_per_100",
    "behavior_susp_incidents_short_per_100",
    "behavior_susp_incidents_long_per_100",
    "behavior_expulsions_per_100",
    "behavior_expulsion_incidents_per_100",
    "behavior_sbar_oss_actions_per_100",
    "behavior_sbar_iss_actions_per_100",
    "attendance_chronic_absent_pct",
    "needs_meal_eligibility_pct",
    "needs_breakfast_participation_pct",
    "needs_lunch_participation_pct",
    "demo_american_indian_pct",
    "demo_black_pct",
    "demo_hispanic_pct",
    "demo_white_pct",
    "demo_asian_pct",
    "demo_native_hawaiian_pct",
    "demo_two_plus_pct"
  ),
  category = c(
    rep("test", 12),
    rep("behavior", 10),
    "attendance",
    rep("needs", 3),
    rep("demographics", 7)
  ),
  label_short = c(
    "ACR — Overall",
    "SQP Percent Passing — Overall",
    "ACR — English (ELA)",
    "ACR — Math",
    "ACR — Science",
    "SQP Percent Passing — English (ELA)",
    "SQP Percent Passing — Math",
    "SQP Percent Passing — Science",
    "Annual SOL Pass Rate — Overall",
    "Annual SOL Pass Rate — English (ELA)",
    "Annual SOL Pass Rate — Math",
    "Annual SOL Pass Rate — Science",
    "Suspensions per 100",
    "Short-term suspensions per 100",
    "Long-term suspensions per 100",
    "Suspendable incidents per 100",
    "Short-term incidents per 100",
    "Long-term incidents per 100",
    "Expulsions per 100",
    "Expellable incidents per 100",
    "Out-of-school actions per 100 (SBAR)",
    "In-school actions per 100 (SBAR)",
    "Chronic absenteeism",
    "Meal eligibility",
    "Breakfast participation",
    "Lunch participation",
    "% Am. Indian",
    "% Black",
    "% Hispanic",
    "% White",
    "% Asian",
    "% Nat. Hawaiian",
    "% Two+"
  ),
  label_long = c(
    "Overall Accreditation Combined Rate (ACR) (0-100), computed as the mean of available subject-level ACR values for All Students. VDOE currently supplies English and Math ACR; Science ACR is blank. This is an app-defined composite, not an official VDOE rating.",
    "School Quality Profiles Percent Passing overall (0-100): app-defined mean across available English, Math, and Science values from the accreditation export through 2024. This differs from annual test-administration SOL pass rates.",
    "Accreditation Combined Rate (ACR) for English (ELA), All Students (VDOE School Quality Profiles download).",
    "Accreditation Combined Rate (ACR) for Math, All Students (VDOE School Quality Profiles download).",
    "Accreditation Combined Rate (ACR) for Science, All Students (VDOE School Quality Profiles download).",
    "School Quality Profiles Percent Passing for English (ELA), All Students (accreditation export; available through 2024). Its figures differ from VDOE annual test-administration pass rates.",
    "School Quality Profiles Percent Passing for Math, All Students (accreditation export; available through 2024). Its figures differ from VDOE annual test-administration pass rates.",
    "School Quality Profiles Percent Passing for Science, All Students (accreditation export; available through 2024). Its figures differ from VDOE annual test-administration pass rates.",
    "Annual SOL Pass Rate — Overall: app-defined simple mean of available English Reading, Mathematics, and Science school rates for All Students. Source: VDOE School Subject-Area test-administration workbooks. A division overall value is the mean of its three subject rates.",
    "Annual SOL Pass Rate for English Reading, All Students, from VDOE School Subject-Area test-administration workbooks; school numerators and denominators are reported by VDOE.",
    "Annual SOL Pass Rate for Mathematics, All Students, from VDOE School Subject-Area test-administration workbooks; school numerators and denominators are reported by VDOE.",
    "Annual SOL Pass Rate for Science, All Students, from VDOE School Subject-Area test-administration workbooks; school numerators and denominators are reported by VDOE.",
    "Suspensions per 100 students (short + long term; computed using VDOE counts and enrollment totals)",
    "Short-term suspensions per 100 students (computed using VDOE short-term suspension counts and enrollment totals).",
    "Long-term suspensions per 100 students (computed using VDOE long-term suspension counts and enrollment totals).",
    "Suspendable incidents per 100 students (short + long; computed using VDOE incident counts and enrollment totals).",
    "Short-term suspendable incidents per 100 students (computed using VDOE short-term incident counts and enrollment totals).",
    "Long-term suspendable incidents per 100 students (computed using VDOE long-term incident counts and enrollment totals).",
    "Expulsions per 100 students (computed using VDOE expulsion counts and enrollment totals).",
    "Expellable incidents per 100 students (computed using VDOE expellable incident counts and enrollment totals).",
    "Out-of-school suspension sanction actions per 100 enrolled students, from VDOE SBAR school sanctions. This counts actions, not distinct students. It is separate from the older School Quality Profiles short- and long-term suspension series. A school with other reported sanctions but no OSS row has zero OSS actions.",
    "In-school suspension sanction actions per 100 enrolled students, from VDOE SBAR school sanctions. This counts actions, not distinct students. It is separate from the older School Quality Profiles suspension series. A school with other reported sanctions but no ISS row has zero ISS actions.",
    "Chronic absenteeism rate (% of students missing 10%+ of school days), All Students (VDOE School Quality Profiles download).",
    "School nutrition meal eligibility (%), All Students (VDOE School Quality Profiles download).",
    "Breakfast participation among eligible students (%), All Students (VDOE School Quality Profiles download).",
    "Lunch participation among eligible students (%), All Students (VDOE School Quality Profiles download).",
    "% of enrolled students who are American Indian or Alaska Native. VDOE Fall Membership race counts divided by a checked same-source school total, 2022–26.",
    "% of enrolled students who are Black or African American. VDOE Fall Membership race counts, 2022–26.",
    "% of enrolled students who are Hispanic/Latino. VDOE Fall Membership race counts, 2022–26.",
    "% of enrolled students who are White. VDOE Fall Membership race counts, 2022–26.",
    "% of enrolled students who are Asian. VDOE Fall Membership race counts, 2022–26.",
    "% of enrolled students who are Native Hawaiian or Other Pacific Islander. VDOE Fall Membership race counts, 2022–26.",
    "% of enrolled students who are Two or more races. VDOE Fall Membership race counts, 2022–26."
  ),
  unit = c(
    rep("index", 8),
    rep("pct", 4),
    rep("per_100", 10),
    "pct",
    rep("pct", 3),
    rep("pct", 7)
  ),
  better_direction = c(
    rep("higher_better", 12),
    rep("lower_better", 10),
    "lower_better",
    rep("neutral", 3),
    rep("neutral", 7)
  ),
  format = c(
    rep("num_1", 8),
    rep("pct_1", 4),
    rep("num_1", 10),
    "pct_1",
    rep("pct_1", 3),
    rep("pct_1", 7)
  ),
  palette = c(
    rep("viridis", 12),
    rep("magma", 10),
    "magma",
    rep("viridis", 3),
    rep("viridis", 7)
  ),
  stringsAsFactors = FALSE
)

# Stop before publishing if the output has broken keys or suppression rules.
if (anyDuplicated(school_metrics_out[c("school_id", "year", "metric_id")]) ||
    anyDuplicated(division_metrics_out[c("division_id", "year", "metric_id")])) {
  stop("Duplicate metric keys in staged snapshot")
}
if (any(is.finite(school_metrics_out$value) & school_metrics_out$suppressed %in% TRUE) ||
    any(is.finite(division_metrics_out$value) & division_metrics_out$suppressed %in% TRUE)) {
  stop("Suppressed numeric value in staged snapshot")
}
if (any(!is.finite(school_metrics_out$value) & !(school_metrics_out$suppressed %in% TRUE)) ||
    any(!is.finite(division_metrics_out$value) & !(division_metrics_out$suppressed %in% TRUE))) {
  stop("Missing metric value lacks suppression flag")
}

# Write snapshot artifacts (overwrite atomically).
write_csv_atomic <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  write.csv(df, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) stop("Cannot write snapshot file: ", path)
}

source_file_info <- function(path) {
  hash_line <- system2("sha256sum", args = shQuote(path), stdout = TRUE)
  if (length(hash_line) != 1) stop("Cannot hash source file: ", path)
  list(file = basename(path), sha256 = strsplit(hash_line, " ", fixed = TRUE)[[1]][[1]],
       bytes = unname(file.info(path)$size),
       cache_modified_at = format(file.info(path)$mtime, tz = "UTC", usetz = TRUE))
}

snapshot_meta <- list(
  snapshot_date = as.character(Sys.Date()),
  year_min = min(years_keep),
  year_max = max(years_keep),
  transform_version = 11,
  annual_sol_match = list(source_schools = annual_sol$source_schools,
                          matched_schools = annual_sol$matched_schools,
                          unmatched_schools = nrow(annual_sol$unmatched)),
  membership_race_match = dplyr::bind_rows(historical_race$coverage,
                                          membership_race$coverage),
  sbar_match = sbar$coverage,
  sbar_missing_enrollment_school_years = nrow(sbar$missing_denominator),
  nces_match = list(total = nrow(matched), matched = sum(!is.na(matched$school_id)),
                    unmatched = matched %>% filter(is.na(school_id)) %>%
                      select(division_name, school_name)),
  source_files = lapply(c(vdoe_short_path, vdoe_long_path, vdoe_enroll_path,
    vdoe_abs_path, vdoe_expulsions_path, vdoe_fr_elig_path,
    vdoe_fr_breakfast_path, vdoe_fr_lunch_path, vdoe_acc_eng_path,
    vdoe_acc_math_path, vdoe_acc_sci_path, nces_sch_zip_path,
    nces_lea_zip_path, tiger_zip_path, annual_sol_archive,
    membership_race_archive, historical_race_archive,
    historical_enrollment_archive, sbar_archive,
    file.path("data-raw", "annual_sol_sources.json"),
    file.path("data-raw", "fall_membership_race_sources.json"),
    file.path("data-raw", "fall_membership_historical_sources.json"),
    file.path("data-raw", "sbar_school_sanctions_sources.json"),
    vapply(c("enrollment", "absenteeism", "fr_eligibility", "fr_breakfast", "fr_lunch"),
           history_path, character(1))), source_file_info),
  notes = "Built from VDOE School Quality Profiles downloads, annual SOL School Subject-Area workbooks, 2022–26 Fall Membership race counts, and school-level SBAR sanction actions, plus NCES EDGE geocodes and TIGER/Line boundaries. The 2022 SBAR denominator uses checked Fall Membership school totals; later years use checked SQP enrollment. The 2023 race shares exclude schools whose race sum differs from separately reported SQP enrollment because that year's gender export omits Other. Legacy SQP suspension rates and SBAR action rates are separate measures. Historical ACR/SQP Percent Passing and annual SOL test-administration rates are separate measures.",
  sources = list(
    list(id = "vdoe_schoolquality_download_data", url = vdoe_url),
    list(id = "vdoe_sol_school_subject_annual", url = "https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results"),
    list(id = "vdoe_fall_membership_race_school", url = "https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics"),
    list(id = "vdoe_sbar_school_sanctions", url = "https://p1pe.doe.virginia.gov/buildatable/sbar"),
    list(id = "nces_edge_public_school_geocodes", url = nces_sch_zip_url),
    list(id = "nces_edge_public_lea_geocodes", url = nces_lea_zip_url),
    list(id = "census_tiger_unsd_va", url = tiger_zip_url)
  )
)

jsonlite::write_json(snapshot_meta, file.path(snapshot_stage, "snapshot_meta.json"), pretty = TRUE, auto_unbox = TRUE)
write_csv_atomic(metric_defs_out, file.path(snapshot_stage, "metric_defs.csv"))
write_csv_atomic(schools_out, file.path(snapshot_stage, "schools.csv"))
write_csv_atomic(school_metrics_out, file.path(snapshot_stage, "school_metrics.csv"))
write_csv_atomic(enrollment_by_year, file.path(snapshot_stage, "enrollment_by_year.csv"))
write_csv_atomic(division_metrics_out, file.path(snapshot_stage, "division_metrics.csv"))
write_csv_atomic(school_subject_perf_out, file.path(snapshot_stage, "school_subject_perf.csv"))
write_csv_atomic(division_subject_perf_out, file.path(snapshot_stage, "division_subject_perf.csv"))
jsonlite::write_json(division_polys_out, file.path(snapshot_stage, "divisions_polygons.json"), pretty = FALSE, auto_unbox = TRUE)

snapshot_backup <- tempfile("snapshot-backup-", tmpdir = "app")
if (!file.rename(snapshot_dir, snapshot_backup)) stop("Cannot back up previous snapshot")
if (!file.rename(snapshot_stage, snapshot_dir)) {
  if (!file.rename(snapshot_backup, snapshot_dir)) stop("CRITICAL: restore previous snapshot from ", snapshot_backup)
  stop("Cannot publish new snapshot; previous snapshot restored")
}
unlink(snapshot_backup, recursive = TRUE)
message("Snapshot written to: ", normalizePath(snapshot_dir))

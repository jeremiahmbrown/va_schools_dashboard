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
#     In this environment, TLS chain verification may fail; this script will
#     retry with `curl -k` if the secure request fails with an SSL error.
#   - VDOE files include 2 header lines + blank line; we auto-detect the CSV
#     header row.
#   - The "Overall Performance Index" is computed (not VDOE-published) as:
#       mean(Accreditation Combined Rate) across English, Math, Science
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
if (!dir.exists(snapshot_dir)) dir.create(snapshot_dir, recursive = TRUE)

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
  # Try secure request first; if SSL chain fails (curl 60), retry with -k.
  args_base <- c(
    "-sS",
    "-L",
    "--max-time", "120",
    "-X", "POST",
    url
  )

  args_fields <- character(0)
  for (k in names(form_fields)) {
    v <- form_fields[[k]]
    if (length(v) == 0) next
    for (one in v) {
      args_fields <- c(args_fields, "--data-urlencode", paste0(k, "=", one))
    }
  }

  run <- function(extra_args = character(0)) {
    args <- c(args_base, extra_args, args_fields, "-o", dest_path)
    res <- system2("curl", args = args)
    as.integer(res)
  }

  code <- run()
  if (is.na(code) || code != 0) {
    # Retry with --insecure for the known TLS-chain issue.
    code2 <- run(c("-k"))
    if (is.na(code2) || code2 != 0) {
      stop("curl POST failed for ", url, " (exit code ", code2, ").")
    } else {
      message("Downloaded with `curl -k` due to TLS verification failure: ", basename(dest_path))
    }
  }
}

curl_get_download <- function(dest_path, url) {
  args <- c("-sS", "-L", "--max-time", "240", url, "-o", dest_path)
  code <- system2("curl", args = args)
  if (!is.na(code) && code == 0) return(invisible(TRUE))
  stop("curl GET failed for ", url, " (exit code ", code, ").")
}

vdoe_url <- "https://schoolquality.virginia.gov/download-data"

download_cached <- function(filename, downloader, validator = NULL) {
  dest <- file.path(input_dir, filename)
  if (file.exists(dest) && file.info(dest)$size > 0) {
    ok <- TRUE
    if (!is.null(validator)) {
      ok <- isTRUE(validator(dest))
    }
    if (ok) {
      message("Using cached: ", filename)
      return(dest)
    }
    message("Cached file failed validation, re-downloading: ", filename)
    unlink(dest)
  }
  message("Downloading: ", filename)
  downloader(dest)
  dest
}

validate_not_html <- function(path) {
  first <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) character(0))
  if (length(first) == 0) return(FALSE)
  !grepl("^\\s*<", first[[1]])
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
acc_eng_df <- read_vdoe_csv(vdoe_acc_eng_path)
acc_math_df <- read_vdoe_csv(vdoe_acc_math_path)
acc_sci_df <- read_vdoe_csv(vdoe_acc_sci_path)

# Enrollment: sum across grades to get a per-school total.
enroll_totals <- enroll_df %>%
  transmute(
    year = vapply(Year, parse_end_year, integer(1)),
    division_name = Division,
    school_name = School,
    level = Level,
    count = suppressWarnings(as.integer(Count))
  ) %>%
  filter(level == "SCH") %>%
  group_by(year, division_name, school_name) %>%
  summarise(enrollment = sum(count, na.rm = TRUE), .groups = "drop")

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
    test_overall_perf_suppressed = any(isTRUE(acr_suppressed)) | n_subjects_acr_present == 0,
    test_overall_pass_suppressed = any(isTRUE(pass_suppressed)) | n_subjects_pass_present == 0,
    .groups = "drop"
  )

extract_susp_n <- function(df, n_col, metric_tag) {
  p <- parse_num(df[[n_col]])
  df %>%
    transmute(
      year = vapply(Year, parse_end_year, integer(1)),
      # Note: VDOE export labels these columns as School Division then School.
      division_name = School,
      school_name = Division,
      subgroup = Subgroup,
      level = Level,
      numerator = p$value,
      numerator_suppressed = p$suppressed
    ) %>%
    filter(level == "SCH") %>%
    mutate(metric_tag = metric_tag)
}

short_n <- extract_susp_n(short_df, "Number Suspended Short Term", "short")
long_n <- extract_susp_n(long_df, "Number Suspended Long Term", "long")

race_susp_groups <- c(
  "American Indian",
  "Asian",
  "Black",
  "Hispanic",
  "Multiple Races",
  "Native Hawaiian",
  "White"
)

susp_totals <- bind_rows(short_n, long_n) %>%
  filter(subgroup %in% race_susp_groups) %>%
  group_by(year, division_name, school_name, metric_tag) %>%
  summarise(
    numerator = if (any(is.finite(numerator))) sum(numerator, na.rm = TRUE) else NA_real_,
    numerator_suppressed = !any(is.finite(numerator)) | any(isTRUE(numerator_suppressed)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = metric_tag, values_from = c(numerator, numerator_suppressed), names_sep = "_") %>%
  mutate(
    susp_n = coalesce(numerator_short, 0) + coalesce(numerator_long, 0),
    susp_suppressed = isTRUE(numerator_suppressed_short) | isTRUE(numerator_suppressed_long) |
      (is.na(numerator_short) & is.na(numerator_long))
  )

# Race/ethnicity %: use "Percent of the Student Population" from the suspensions export.
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
  group_by(year, division_name, school_name) %>%
  mutate(
    total = sum(value_raw, na.rm = TRUE),
    value = ifelse(is.finite(total) & total > 0, 100 * value_raw / total, NA_real_),
    suppressed = isTRUE(suppressed_raw) | is.na(value)
  ) %>%
  ungroup() %>%
  mutate(value = pmin(pmax(value, 0), 100)) %>%
  select(year, division_name, school_name, metric_id, value, suppressed)

# Choose the 5 most recent years common to our core series.
years_core <- sort(unique(overall_perf$year))
if (length(years_core) < 1) stop("No years detected in accreditation exports.")
years_keep <- tail(years_core, 5)
message("Keeping years: ", paste(years_keep, collapse = ", "))

overall_perf_keep <- overall_perf %>% filter(year %in% years_keep)
stg_perf_keep <- stg_perf %>% filter(year %in% years_keep)
susp_keep <- susp_totals %>% filter(year %in% years_keep)
enroll_keep <- enroll_totals %>% filter(year %in% years_keep)
race_keep <- race_df %>% filter(year %in% years_keep)

core_join <- overall_perf_keep %>%
  select(
    year,
    division_name,
    school_name,
    test_overall_perf_index,
    test_overall_pass_index,
    test_overall_perf_suppressed,
    test_overall_pass_suppressed,
    n_subjects_acr_present,
    n_subjects_pass_present
  ) %>%
  left_join(susp_keep %>% select(year, division_name, school_name, susp_n, susp_suppressed), by = c("year", "division_name", "school_name")) %>%
  left_join(enroll_keep, by = c("year", "division_name", "school_name"))

core_join <- core_join %>%
  mutate(
    behavior_susp_per_100 = ifelse(is.finite(enrollment) & enrollment > 0, 100 * susp_n / enrollment, NA_real_),
    behavior_susp_suppressed = isTRUE(susp_suppressed) | is.na(behavior_susp_per_100)
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

names(nces_lea_va)[names(nces_lea_va) == "NAME"] <- "LEA_NAME"

nces_va <- nces_sch_va %>%
  left_join(nces_lea_va %>% select(LEAID, LEA_NAME), by = "LEAID") %>%
  transmute(
    school_id = as.character(NCESSCH),
    nces_school_name = as.character(NAME),
    lat = as.numeric(LAT),
    lon = as.numeric(LON),
    division_id = as.character(LEAID),
    nces_division_name = as.character(LEA_NAME)
  )

# Build VDOE school list from core_join and match to NCES by normalized division+school names.
vdoe_schools <- core_join %>%
  distinct(division_name, school_name) %>%
  mutate(
    k = paste(norm_key(division_name), norm_key(school_name), sep = "::")
  )

nces_keys <- nces_va %>%
  mutate(
    k = paste(norm_key(nces_division_name), norm_key(nces_school_name), sep = "::")
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
    acr_suppressed = isTRUE(acr_suppressed) | is.na(acr_rate),
    pass_rate = pmin(pass_rate, 100),
    pass_suppressed = isTRUE(pass_suppressed) | is.na(pass_rate),
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
      suppressed = test_overall_perf_suppressed,
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
      suppressed = test_overall_pass_suppressed,
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
)

# Division aggregation.
division_metrics_base <- school_metrics_out %>%
  left_join(matched %>% select(school_id, division_id), by = "school_id") %>%
  filter(!is.na(division_id)) %>%
  group_by(division_id, year, metric_id) %>%
  summarise(
    value = {
      # Prefer totals-based aggregation when numerator/denominator exist.
      has_den <- any(is.finite(denominator)) && sum(denominator, na.rm = TRUE) > 0
      if (has_den && dplyr::first(unit) %in% c("pct", "per_100")) {
        100 * sum(numerator, na.rm = TRUE) / sum(denominator, na.rm = TRUE)
      } else if (has_den) {
        sum(numerator, na.rm = TRUE) / sum(denominator, na.rm = TRUE)
      } else {
        mean(value, na.rm = TRUE)
      }
    },
    suppressed = all(isTRUE(suppressed) | is.na(value)),
    n_included = sum(is.finite(value)),
    n_suppressed = sum(isTRUE(suppressed) | is.na(value)),
    agg_method = ifelse(any(is.finite(denominator)), "total_rate", "mean"),
    .groups = "drop"
  )

# For division-level "overall" performance, we intentionally compute the mean
# across the *division subject-level* values (not a direct enrollment-weighted
# average of school-level overall scores).
division_perf_subject <- division_metrics_base %>%
  filter(metric_id %in% c("test_acr_ela", "test_acr_math", "test_acr_science", "test_pass_ela", "test_pass_math", "test_pass_science")) %>%
  mutate(
    perf_metric = ifelse(grepl("^test_acr_", metric_id), "acr", "pass")
  )

division_perf_overall <- division_perf_subject %>%
  group_by(division_id, year, perf_metric) %>%
  summarise(
    value = if (all(!is.finite(value))) NA_real_ else mean(value, na.rm = TRUE),
    suppressed = all(!is.finite(value)) | all(isTRUE(suppressed)),
    n_included = sum(is.finite(value)),
    n_suppressed = sum(isTRUE(suppressed) | is.na(value)),
    agg_method = "mean_subjects",
    .groups = "drop"
  ) %>%
  mutate(
    metric_id = ifelse(perf_metric == "acr", "test_overall_perf_index", "test_overall_pass_index")
  ) %>%
  select(-perf_metric)

division_metrics_out <- division_metrics_base %>%
  filter(!metric_id %in% c("test_overall_perf_index", "test_overall_pass_index")) %>%
  bind_rows(division_perf_overall)

division_subject_perf_out <- division_perf_subject %>%
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
      if (length(x) == 0) TRUE else all(isTRUE(x))
    },
    pass_suppressed = {
      x <- suppressed[perf_metric == "pass"]
      if (length(x) == 0) TRUE else all(isTRUE(x))
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
    "behavior_susp_per_100",
    "demo_american_indian_pct",
    "demo_black_pct",
    "demo_hispanic_pct",
    "demo_white_pct",
    "demo_asian_pct",
    "demo_native_hawaiian_pct",
    "demo_two_plus_pct"
  ),
  category = c(rep("test", 8), "behavior", rep("demographics", 7)),
  label_short = c(
    "ACR — Overall",
    "SOL Pass Rate — Overall",
    "ACR — English (ELA)",
    "ACR — Math",
    "ACR — Science",
    "SOL Pass Rate — English (ELA)",
    "SOL Pass Rate — Math",
    "SOL Pass Rate — Science",
    "Suspensions per 100",
    "% Am. Indian",
    "% Black",
    "% Hispanic",
    "% White",
    "% Asian",
    "% Nat. Hawaiian",
    "% Two+"
  ),
  label_long = c(
    "Overall Accreditation Combined Rate (ACR) (0-100), computed as the mean of subject-level ACR across English (ELA), Math, and Science for All Students. This is an app-defined composite for convenience.",
    "Overall SOL Pass Rate (0-100), computed as the mean of subject-level Percent Passing across English (ELA), Math, and Science for All Students. This is an app-defined composite for convenience.",
    "Accreditation Combined Rate (ACR) for English (ELA), All Students (VDOE School Quality Profiles download).",
    "Accreditation Combined Rate (ACR) for Math, All Students (VDOE School Quality Profiles download).",
    "Accreditation Combined Rate (ACR) for Science, All Students (VDOE School Quality Profiles download).",
    "SOL Pass Rate (Percent Passing) for English (ELA), All Students (VDOE School Quality Profiles download).",
    "SOL Pass Rate (Percent Passing) for Math, All Students (VDOE School Quality Profiles download).",
    "SOL Pass Rate (Percent Passing) for Science, All Students (VDOE School Quality Profiles download).",
    "Suspensions per 100 students (short + long term; computed using VDOE counts and enrollment totals)",
    "% of enrolled students who are American Indian or Alaska Native (from VDOE subgroup %; normalized to sum to 100 across race categories)",
    "% of enrolled students who are Black or African American (from VDOE subgroup %)",
    "% of enrolled students who are Hispanic/Latino (from VDOE subgroup %)",
    "% of enrolled students who are White (from VDOE subgroup %)",
    "% of enrolled students who are Asian (from VDOE subgroup %)",
    "% of enrolled students who are Native Hawaiian or Other Pacific Islander (from VDOE subgroup %; normalized to sum to 100 across race categories)",
    "% of enrolled students who are Two or more races (from VDOE subgroup %; normalized to sum to 100 across race categories)"
  ),
  unit = c(rep("index", 8), "per_100", rep("pct", 7)),
  better_direction = c(rep("higher_better", 8), "lower_better", rep("neutral", 7)),
  format = c(rep("num_1", 8), "num_1", rep("pct_1", 7)),
  palette = c(rep("viridis", 8), "magma", rep("viridis", 7)),
  stringsAsFactors = FALSE
)

# Write snapshot artifacts (overwrite atomically).
write_csv_atomic <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  write.csv(df, tmp, row.names = FALSE, na = "")
  file.rename(tmp, path)
}

snapshot_meta <- list(
  snapshot_date = as.character(Sys.Date()),
  year_min = min(years_keep),
  year_max = max(years_keep),
  transform_version = 4,
  notes = "Built from VDOE School Quality Profiles downloads + NCES EDGE geocodes + TIGER/Line boundaries. Includes subject-level ACR and SOL pass rates.",
  sources = list(
    list(id = "vdoe_schoolquality_download_data", url = vdoe_url),
    list(id = "nces_edge_public_school_geocodes", url = nces_sch_zip_url),
    list(id = "nces_edge_public_lea_geocodes", url = nces_lea_zip_url),
    list(id = "census_tiger_unsd_va", url = tiger_zip_url)
  )
)

jsonlite::write_json(snapshot_meta, file.path(snapshot_dir, "snapshot_meta.json"), pretty = TRUE, auto_unbox = TRUE)
write_csv_atomic(metric_defs_out, file.path(snapshot_dir, "metric_defs.csv"))
write_csv_atomic(schools_out, file.path(snapshot_dir, "schools.csv"))
write_csv_atomic(school_metrics_out, file.path(snapshot_dir, "school_metrics.csv"))
write_csv_atomic(division_metrics_out, file.path(snapshot_dir, "division_metrics.csv"))
write_csv_atomic(school_subject_perf_out, file.path(snapshot_dir, "school_subject_perf.csv"))
write_csv_atomic(division_subject_perf_out, file.path(snapshot_dir, "division_subject_perf.csv"))
jsonlite::write_json(division_polys_out, file.path(snapshot_dir, "divisions_polygons.json"), pretty = FALSE, auto_unbox = TRUE)

message("Snapshot written to: ", normalizePath(snapshot_dir))

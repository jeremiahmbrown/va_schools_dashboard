# ==============================================================================
# Script: inventory_snapshot.R
# Purpose:
#   Developer utility to inventory which metrics and demographic/program fields
#   exist in the current frozen snapshot and (optionally) in the cached raw VDOE
#   exports under `data-raw/input/`.
#
#   This script is intentionally lightweight and read-only. It helps keep the
#   app update-friendly by documenting what is actually present before changing
#   UI expectations.
#
# Inputs:
#   - `app/data/*` snapshot artifacts
#   - Optional cached raw exports under `data-raw/input/*.csv`
#
# Outputs:
#   - Prints a summary to stdout
#   - Writes `docs/DATA_INVENTORY.md`
#
# Key Details / Debugging:
#   - VDOE exports often include 2 header lines + a blank line; this script does
#     not parse raw files beyond reporting their detected header columns.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

repo_root <- normalizePath(file.path(getwd(), "."), mustWork = TRUE)
snapshot_dir <- file.path(repo_root, "app", "data")
raw_dir <- file.path(repo_root, "data-raw", "input")
out_path <- file.path(repo_root, "docs", "DATA_INVENTORY.md")

read_csv_safe <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, ...)
}

metric_defs <- read_csv_safe(file.path(snapshot_dir, "metric_defs.csv"))
school_metrics <- read_csv_safe(file.path(snapshot_dir, "school_metrics.csv"))
division_metrics <- read_csv_safe(file.path(snapshot_dir, "division_metrics.csv"))
schools <- read_csv_safe(file.path(snapshot_dir, "schools.csv"))

stopifnot(!is.null(metric_defs), !is.null(school_metrics), !is.null(division_metrics), !is.null(schools))

fmt_md_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("(none)\n")
  cols <- names(df)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  rows <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

detect_vdoe_header <- function(path) {
  # Find the first line beginning with "Year," and return it split to columns.
  lines <- readLines(path, n = 80, warn = FALSE)
  idx <- which(grepl("^Year,", lines) | grepl("^\"?Year\"?,", lines))
  if (length(idx) == 0) return(character(0))
  strsplit(lines[[idx[[1]]]], ",", fixed = TRUE)[[1]]
}

categories <- sort(unique(metric_defs$category))

behavior_metrics <- metric_defs %>%
  filter(category %in% c("behavior", "attendance")) %>%
  transmute(metric_id, label = label_short, unit, better_direction)

demo_metrics <- metric_defs %>%
  filter(category == "demographics") %>%
  transmute(metric_id, label = label_short, unit, better_direction)

needs_metrics <- metric_defs %>%
  filter(category == "needs") %>%
  transmute(metric_id, label = label_short, unit, better_direction)

metric_units <- school_metrics %>%
  distinct(metric_id, unit) %>%
  arrange(metric_id)

raw_files <- c(
  "vdoe_short_term_suspensions.csv",
  "vdoe_long_term_suspensions.csv",
  "vdoe_enrollment.csv",
  "vdoe_accreditation_english.csv",
  "vdoe_accreditation_math.csv",
  "vdoe_accreditation_science.csv",
  "vdoe_absenteeism.csv",
  "vdoe_expulsions.csv",
  "vdoe_fr_eligibility.csv",
  "vdoe_fr_breakfast.csv",
  "vdoe_fr_lunch.csv"
)

raw_header_rows <- lapply(raw_files, function(fn) {
  path <- file.path(raw_dir, fn)
  if (!file.exists(path)) return(NULL)
  cols <- detect_vdoe_header(path)
  data.frame(
    file = fn,
    n_cols = length(cols),
    columns = paste(cols, collapse = ", "),
    stringsAsFactors = FALSE
  )
})
raw_header_rows <- dplyr::bind_rows(raw_header_rows)

lines <- c(
  "# Data Inventory (Snapshot + Cached Raw Exports)",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("- Snapshot dir: `", snapshot_dir, "`"),
  paste0("- Raw cache dir: `", raw_dir, "`"),
  "",
  "## Metric Categories Present",
  "",
  paste0("- ", paste(categories, collapse = ", ")),
  "",
  "## Behavior / Attendance Metrics Present (Snapshot)",
  "",
  fmt_md_table(behavior_metrics),
  "",
  "## Demographics Metrics Present (Snapshot)",
  "",
  fmt_md_table(demo_metrics),
  "",
  "## Student Needs / Programs Metrics Present (Snapshot)",
  "",
  fmt_md_table(needs_metrics),
  "",
  "## Metric Units Present (school_metrics)",
  "",
  fmt_md_table(metric_units),
  "",
  "## Cached Raw Export Headers (if present)",
  "",
  fmt_md_table(raw_header_rows)
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, out_path)

cat(paste(lines, collapse = "\n"), "\n")
cat("\nWrote: ", out_path, "\n", sep = "")


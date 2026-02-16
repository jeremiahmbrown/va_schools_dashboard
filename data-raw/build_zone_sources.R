# ==============================================================================
# Script: build_zone_sources.R
# Purpose:
#   Seed/refresh the attendance-zone source registry for the "top 15" divisions
#   by enrollment. This registry is used by the optional zone-layer pipeline.
#
#   This script does not download boundaries. It only creates a curated table
#   that can be filled in manually (3-minute search budget per division).
#
# Inputs:
#   - `app/data/schools.csv` (uses `enrollment_latest` if present)
#
# Outputs:
#   - `app/data/zones/zone_sources.csv`
#
# Key Details / Debugging:
#   - Enrollment ranking uses sum(enrollment_latest) across schools.
#   - If enrollment_latest is missing, this script still emits the top 15 by
#     school count (with a note).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

repo_root <- normalizePath(file.path(getwd(), "."), mustWork = TRUE)
schools_path <- file.path(repo_root, "app", "data", "schools.csv")
out_dir <- file.path(repo_root, "app", "data", "zones")
out_path <- file.path(out_dir, "zone_sources.csv")

if (!file.exists(schools_path)) stop("Missing snapshot schools table: ", schools_path)
schools <- read.csv(schools_path, stringsAsFactors = FALSE)

rank_df <- schools %>%
  mutate(enrollment_latest = suppressWarnings(as.numeric(enrollment_latest))) %>%
  group_by(division_id, division_name) %>%
  summarise(
    n_schools = dplyr::n(),
    enrollment_total = sum(enrollment_latest, na.rm = TRUE),
    n_missing_enrollment = sum(!is.finite(enrollment_latest)),
    .groups = "drop"
  )

use_enrollment <- any(is.finite(rank_df$enrollment_total)) && sum(rank_df$enrollment_total, na.rm = TRUE) > 0
if (use_enrollment) {
  rank_df <- rank_df %>% arrange(desc(enrollment_total))
} else {
  rank_df <- rank_df %>% arrange(desc(n_schools))
}

top15 <- rank_df %>%
  mutate(enrollment_rank = dplyr::row_number()) %>%
  slice_head(n = 15) %>%
  transmute(
    division_id = as.character(division_id),
    division_name = as.character(division_name),
    enrollment_rank = as.integer(enrollment_rank),
    source_type = "none",
    source_url = "",
    coverage = "unknown",
    retrieved_date = "",
    notes = if (use_enrollment) "" else "Ranked by school count (enrollment_latest not available)."
  )

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(top15, out_path, row.names = FALSE, na = "")
message("Wrote: ", out_path)


# Archived public VDOE downloads

These five compressed CSVs were copied on 2026-10-19 from the repository's
original public raw inputs at Git commit `8f86732`. The original frozen
snapshot was dated 2026-02-15. The downloads' exact VDOE publication dates
are not known. Each archive contains the old export's 2022–23, 2023–24, and
2024–25 rows and the original column names, without the VDOE preamble.

The School Quality Profiles download service now returns only the latest
three school years for these measures. `build_snapshot.R` uses a whole year
from an archive only when that year is absent from the current response. For
an overlapping year, current VDOE data take precedence. This keeps old trends
without treating an archive as a new release.

The files are public school-level aggregates. No student records are stored.
Input hashes are saved in `app/data/snapshot_meta.json` after each build.

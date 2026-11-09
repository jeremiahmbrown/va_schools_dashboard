# Charlottesville boundary release — 2026-11-09

Pre-release Git commit: `d898c302f42a744e5885810f84b4987262728bad`. Pre-release Shiny bundle: `12795760`. Bead: `va_schools_dashboard-nlf.11.17`.

## Source and limits

[Charlottesville City Schools links the City GIS map for current elementary attendance](https://charlottesvilleschools.org/zoning). The [City Open Data policy](https://widget.charlottesville.org/gis/opendata/OpenDataPolicy.pdf) permits free distribution of portal data. The exact GIS layer has no school-year field, so the app does not claim a coded effective year. Later rezoning phases begin in 2027 and 2029; they are not part of this map. City Schools says middle and high attendance is citywide.

The [elementary layer](https://gisweb.charlottesville.org/arcgis/rest/services/OpenData_1/MapServer/16) returned the same six object IDs as the staged 2026-11-02 file on 2026-11-09. Raw SHA-256: `196114846b5859475add167b3639fbc95210e31d00c266f971a2a45cbd11e3a7`. Six polygons are valid and nonempty. Same-level overlap is 64.0272 m² against a reviewed 65 m² cap. The map preserves the source edges and directs address checks to the division.

[City Schools confirms the 2025 school name changes](https://charlottesvilleschools.org/index.php?articleID=50859594&pageID=smartSiteFeed&psqFeed=true). NCES keeps IDs `510078000266` (Tall Oaks), `510078000271` (Charlottesville Middle), and `510078000272` (Sunrise). The rebuilt snapshot adds these three school records and 2026 metrics; it keeps prior-year names on older metrics. The six elementary areas link to their current school records. A separate **2015–16 SABS high-school polygon** remains clearly labeled historical. The middle school and Walker Upper have no polygon in this release. The combined file has seven valid polygons and seven linked schools among nine snapshot schools.

## Checks and release boundary

- `Rscript --vanilla tests/check_snapshot.R`: passed.
- `Rscript --vanilla tests/check_zones.R`: passed, including Charlottesville IDs, levels, high-school warning, and cache.
- `python3 data-raw/import_zone_source.py 5100780` dry run: six City features, 6/9 school links, SHA-256 `fa2b9ceadf6a924bc494e1fcd24d58a14998793dc5a33ea752a4453f3fff3fb8`.
- `python3 tests/check_zone_importer.py`: passed.
- `python3 data-raw/audit_zones.py`: 1,482 of 1,817 linked, 335 unmatched, zero invalid IDs or empty geometry.
- Snapshot comparison against the pre-release tree: three school rows added and no existing school row changed or removed. New school metric and enrollment rows are additions; changed division aggregates are confined to Charlottesville.
- GitHub master commit `e22e05804d8c96a85de0bee6254888a406d9c1b3` has tree `669e6656789509d53df48c5fcc5e96ad1de7a0e6`, identical to the checked local tree.
- Shiny app ID `16676824`, bundle `12798821`: deployed at the existing URL on 2026-11-09.
- Live UI: the page shows snapshot date 2026-11-09 and 1,817 schools. Sunrise shows a 2026 profile and one rendered zone outline. Tall Oaks shows a 2026 profile and one rendered zone outline. The Filters panel shows the current City elementary source, unstated GIS year, historical high polygon, missing middle polygon, small overlaps, and address-check warning. Switching Tall Oaks to data year 2025 shows Johnson Elementary and its 2025 metric. This checks the school display and zone selection, not a household address.
- Rollback target: prior bundle `12795760` and Git commit `d898c302f42a744e5885810f84b4987262728bad`.

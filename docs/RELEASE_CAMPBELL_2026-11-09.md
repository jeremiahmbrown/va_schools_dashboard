# Campbell County boundary release — 2026-11-09

Pre-release Git commit: `3c8a88791c702593c0ecb7e4cba9c7594f062429`. Pre-release Shiny bundle: `12798821`. Bead: `va_schools_dashboard-nlf.11.5`.

## Current source and reuse

[CCPS 2026–27 kindergarten enrollment](https://www.ccpsva.org/parentsfamilies/kindergarten-enrollment) links [Find My School District](https://experience.arcgis.com/experience/573ba852453f4d299e4f7e105302a1b4) for school verification. The County-owned experience uses a public web map with [elementary](https://gis.co.campbell.va.us/arcgis/rest/services/External/E_911_Address_Search/MapServer/16) and [middle/high](https://gis.co.campbell.va.us/arcgis/rest/services/External/E_911_Address_Search/MapServer/17) attendance layers. The source [School Jurisdictions layer](https://gis.co.campbell.va.us/arcgis/rest/services/Public_Data/School_Jurisdictions/MapServer/10) matches both locator layers on all nine object IDs (316–324), elementary/middle/high school fields, and each object's extent. The locator layers do not return full polygon geometry to the query, so exact geometry equality is unproved. The exact GIS effective year is not stored; the app states the division's 2026–27 locator link and warns users to verify an address.

The exact [County ArcGIS item](https://www.arcgis.com/home/item.html?id=49e1c0b096914b01a0a9f5931b1a50e1) is public, credits Campbell County GIS, and provides an accuracy disclaimer without a use restriction. [ArcGIS public sharing terms](https://doc.arcgis.com/en/arcgis-online/reference/terms-of-use.htm) permit public-item reproduction and distribution subject to owner conditions. The app retains County attribution and directs address decisions to CCPS.

## School links and geometry

The refreshed staged source has nine IDs at each of three levels. Raw GeoJSON SHA-256: `0d4ba22d191d6b4dede37006e672734143fd06f0eeaaa0a2b8635e8e5195363e`. The guarded crosswalk has 15 level links to all 13 school IDs. Yellow Branch's two source pieces retain different secondary assignments: ID 316 is Brookville middle/high; ID 323 is Rustburg middle/high. Altavista and William Campbell combined schools appear at both secondary levels.

All 27 saved polygons are valid and nonempty. Same-level overlap is 0.0321 m² at each level against a reviewed 0.04 m² cap. Level unions agree. The previous SABS file linked six Campbell schools; this release links all 13 and adds seven net statewide matches. Candidate total: 1,489 of 1,817 linked, 328 unmatched.

## Checks and release boundary

- `python3 data-raw/check_campbell_locator.py`: nine IDs, school fields, and per-ID extents agree across source and both locator layers; full locator geometry unavailable.
- `python3 data-raw/stage_zone_source.py 5100600`: 27 staged level features; raw hash above.
- `python3 data-raw/import_zone_source.py 5100600` dry run: 27 features, 13/13 schools, candidate SHA-256 `1f970d3585c74e2270357f492be4f3bddc6e666e14f4255c93f65bc3bd9549d8`.
- `Rscript --vanilla tests/check_zones.R`: passed.
- `python3 data-raw/audit_zones.py`: 1,489 linked; 328 unmatched; zero invalid IDs or empty geometry.
- GitHub master commit `27fd121dab149343ee04d5960c1971c4247125be` has tree `0ece31d955d2822ba29bc64945dcf3f5bbcd9716`, identical to the checked local tree.
- Shiny app ID `16676824`, bundle `12798891`: deployed to the existing URL on 2026-11-09.
- Live UI: Yellow Branch loaded a 2026 school profile and two rendered elementary outlines. The division Middle view rendered nine outlines. The Filters panel showed the locator-year, split-assignment, and address-check warning. This is a school-zone display check, not a household-address test.
- Rollback target: prior bundle `12798821` and commit `3c8a88791c702593c0ecb7e4cba9c7594f062429`.

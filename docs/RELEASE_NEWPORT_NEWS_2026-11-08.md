# Newport News zone release - 2026-11-08

Pre-release Git commit: `0fd8d4b599d9c8bcac4b583a7465088e2bc67fbe`. Pre-release Shiny bundle: `12795723`. Bead: `va_schools_dashboard-nlf.11.20`.

## Source, year, and reuse

[NNPS calls its linked City map current](https://www.nn.k12.va.us/schoolboundaries/), and [its 2026 study FAQ](https://nn.k12.va.us/schoolboundaries/faqround1.html) says adopted changes start no earlier than 2027-28. The City's exact [elementary](https://www.arcgis.com/home/item.html?id=e55b1cb9b4934fb78f3c82a21f63f7aa), [middle](https://www.arcgis.com/home/item.html?id=f2ac567a1c5d480081e4197a541c5744), and [high](https://www.arcgis.com/home/item.html?id=2bc7767e9cb2400aaa76ae2f029a1bc5) ArcGIS items are public. Their service URLs match the saved source. Each item credits the City and states that the boundaries are for reference only, with no accuracy warranty or redistribution restriction. [ArcGIS public sharing terms](https://doc.arcgis.com/en/arcgis-online/reference/terms-of-use.htm) permit distribution of public content subject to owner conditions. The exact GIS effective school year remains unstated; the app warns users and directs address checks to NNPS.

## Geometry and school links

The three live service object-ID sets match the staged sets: 23 elementary, 6 middle, and 8 high. Dry-run import reproduced SHA-256 `ace8b4f3453ca2a34cd4d8bcc42b17c87d845837d371f84d81b89762556b6600`. All 37 polygons are valid and nonempty. The elementary service has 15,592.9008 m² same-level overlap, retained against a reviewed 15,593 m² cap; this is not an address assignment. The five schools without separate named polygons retain their reviewed dispositions. Newsome Park has two labeled areas, Menchville three pieces, and Warwick two. The candidate links 33 of 38 Newport News schools, adding 22 net matches to the prior SABS file. Statewide candidate: 1,476 of 1,814 schools linked, 338 unmatched.

The [manifest](../data-raw/zone_source_manifests/5102640.json), [crosswalk](../data-raw/zone_school_crosswalks/5102640.csv), and [geometry report](../data-raw/zone_validation_reports/5102640.json) record the source and checks.

## Release checks

- `Rscript --vanilla tests/check_zones.R`: passed.
- `python3 data-raw/import_zone_source.py 5102640` (dry run): 37 polygons, 33/38 schools, exact hash.
- `python3 data-raw/audit_zones.py`: 1,476 linked; 338 unmatched; zero invalid IDs or empty geometry.
- GitHub master commit `2a2234d54357e35d442a9b8cb927880f2b8c3829` has tree `8e1a13140e4950138df8111d34eaa7c086902f2d`, identical to the checked local tree.
- Shiny app ID `16676824`, bundle `12795760`: deployed on 2026-11-08 at the existing URL.
- Live UI: Newsome Park Elementary loaded its 2026 profile and zone outlines. The Filters panel showed the Newport News source-year, overlap, and address-check warning. This is a school-zone display check, not a household-address test.
- Rollback target: prior bundle `12795723` and Git commit `0fd8d4b599d9c8bcac4b583a7465088e2bc67fbe`.

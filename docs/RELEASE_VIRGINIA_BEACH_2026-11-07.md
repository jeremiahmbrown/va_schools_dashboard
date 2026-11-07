# Virginia Beach 2026-27 attendance-zone release - 2026-11-07

Scope: add the VBCPS division GIS file and level caches to the production snapshot and identify this source in the filter note. The current 2015-16 SABS display for division 5103840 gives way to the VBCPS layer. No school, metric, or other division data change.

[VBCPS School Locator](https://www.vbschools.com/families/registration/schoollocator/) links its attendance maps and says the division confirms official address assignments. Three VBCPS_GIS public ArcGIS items name the 2026-27 elementary, middle, and high attendance zones. The item license and attribution fields and service copyright fields have no item-specific constraints. [ArcGIS public sharing terms](https://doc.arcgis.com/en/arcgis-online/reference/terms-of-use.htm) permit reproduction, derivative work, and distribution subject to owner constraints. The app attributes VBCPS and warns that an address needs division confirmation. Live source counts on 2026-11-07 remained 53 elementary, 13 middle, and 12 high records; data edits predate the pinned 2026-10-26 retrieval.

The [source manifest](../data-raw/zone_source_manifests/5103840.json), [import specification](../data-raw/zone_import_specs/5103840.json), [strict crosswalk](../data-raw/zone_school_crosswalks/5103840.csv), and [geometry report](../data-raw/zone_validation_reports/5103840.json) pin the import. Source JSON SHA-256: 0391c38cc190081fc87acff67686c2965a690b45d753e971a6b787f312ebf61e. Seventy-seven valid, nonempty display features link 79 of 81 snapshot schools. The one Tri-Campus area serves Diamond Springs, Newtown, and Williams by grade pair. Two Green Run High records share one display outline. Green Run Collegiate and Old Donation have no separate standard polygon.

Reviewed same-level overlap is 1,398.8025 square metres elementary, 20,067.4117 middle, and 65,779.597 high, below the 66,000 square-metre cap. These source edges do not determine a home address assignment. The division locator is the check for an address in an overlap.

Prior production: 1,185 of 1,810 schools matched; 625 unmatched. Candidate: 1,240 matched; 570 unmatched; zero invalid school IDs or empty geometry; ten divisions without a zone file. R zone/cache, shared-school selection, importer, and snapshot tests passed, as did the statewide audit and Git diff check. Rollback baseline: GitHub commit f3d17326374fcdb3f2fd41fea50addd7628c7b42 and shinyapps.io bundle 12791636.

Production commit, bundle, and live map check: pending deployment verification.

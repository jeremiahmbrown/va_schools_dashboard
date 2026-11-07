# Stafford 2026-27 attendance-zone release - 2026-11-07

Scope: add the SCPS division GIS file and level caches for division 5103660, replacing its older SABS display. The app source note identifies SCPS and flags source-only areas. No school, metric, or other division data change.

[SCPS Maps and Reports](https://www.staffordschools.net/community/how-can-we-help/maps-and-reports) labels its elementary, middle, and high maps 2026-2027. The SCPS Planning public ArcGIS items have no item-specific use constraints; [ArcGIS public sharing terms](https://doc.arcgis.com/en/arcgis-online/reference/terms-of-use.htm) allow reproduction and distribution subject to owner constraints. The app attributes SCPS and directs address confirmation to its Find Your School tool. Live service counts on 2026-11-07 match the pinned manifest: 19 elementary, 8 middle, 6 high. The middle source service is named MS_2025 and retains an MS2324 field; the current SCPS map page and checked 2026-27 PDFs provide the school-year label.

The [manifest](../data-raw/zone_source_manifests/5103660.json), [import specification](../data-raw/zone_import_specs/5103660.json), [strict crosswalk](../data-raw/zone_school_crosswalks/5103660.csv), and [geometry report](../data-raw/zone_validation_reports/5103660.json) pin the import. Source JSON SHA-256: b179f582f304afe541ff478bd55f39b8267035a1e9580cf525db0406deb17910. The 33 polygons are valid and nonempty. Thirty snapshot schools are linked. Three polygons for Falls Run Elementary, Crows Nest Elementary, and Hartwood High remain source-only because their school identities are not in the frozen production snapshot.

Same-level overlap is 13,930.6239 square metres elementary, 719.3805 middle, and 2,245.1618 high, under the reviewed 13,931 square-metre cap. Source shapes remain unchanged; an address near an overlap needs SCPS confirmation.

Prior production: 1,292 of 1,810 schools matched; 518 unmatched. Candidate: 1,310 matched; 500 unmatched; zero invalid school IDs or empty geometry; ten divisions without a file. R zone/cache, source-only, importer, snapshot, statewide audit, and Git diff checks passed. Rollback baseline: GitHub commit 099b97b9304690df4fa371145ed8767714c6c9e3 and shinyapps.io bundle 12791710.

Published in GitHub master as app-source commit f3e68cd532320ed70628b55477a3a225be79d4d9 and deployed to shinyapps.io app 16676824 as bundle 12791743. The live app selected Falmouth Elementary with the zone layer on; it showed the SCPS 2026-27 source note and no missing-zone warning.

# Henrico 2026-27 attendance-zone release - 2026-11-07

Scope: add the HCPS and County GIS division file and level caches for division 5101890. Replace its older SABS display and label the new source in the filter panel. No school, metric, or other division data change.

The official HCPS school locator is a guide; HCPS Boundaries confirms an address assignment. Henrico County GIS publishes external elementary, middle, and high FeatureServices named 2026_2027. Live source counts on 2026-11-07 match the manifest: 46, 12, and 9 records. The 2027-28 Quioccasin change is not used. [County GIS Open Data](https://henrico.gov/public-data/gis-open-data/) offers public GIS downloads. Its [Terms of Use](https://henrico.gov/transparency/terms-of-use/) discuss derivative work, display, and distribution, and state no specific ban on this use. We infer that an attributed public display is allowed; the County can discontinue distribution. The app attributes HCPS and County GIS and directs address checks to HCPS Boundaries.

The [source manifest](../data-raw/zone_source_manifests/5101890.json), [import specification](../data-raw/zone_import_specs/5101890.json), [strict crosswalk](../data-raw/zone_school_crosswalks/5101890.csv), and [geometry report](../data-raw/zone_validation_reports/5101890.json) pin the import. Source JSON SHA-256: 3d9d064050222f595aad8adf57d254e9dce22a52bf99be3de3948e290dc5f558. Sixty-seven valid, nonempty features link 67 of 68 snapshot schools. Mehfoud and Varina share one area; Kaechele has two pieces. Virtual Academy has no standard neighborhood polygon.

Same-level overlap is 33,996.6878 square metres elementary, 13,150.6609 middle, and 1,712.2875 high, below the reviewed 34,000 square-metre cap. These source edges do not settle an address near an overlap.

Prior production: 1,240 of 1,810 schools matched; 570 unmatched. Candidate: 1,292 matched; 518 unmatched; zero invalid school IDs or empty geometry; ten divisions without a file. R zone/cache and shared-area selection, importer, snapshot, statewide audit, and Git diff checks passed. Rollback baseline: GitHub app-source commit b9b2af061a78b97c31fa64d07dca4127592a4267 and shinyapps.io bundle 12791677.

Published in GitHub master as app-source commit aa0d87766ea268cd7ff1e4ece83d41bf5d85c224 and deployed to shinyapps.io app 16676824 as bundle 12791710. The live app selected Anthony P. Mehfoud Elementary with the zone layer on; it showed the HCPS/County GIS 2026-27 note and no missing-zone warning.

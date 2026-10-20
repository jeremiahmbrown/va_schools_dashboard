# Statewide attendance-zone effort estimate

Audit date: 2026-10-20. This estimates work to replace the older NCES attendance-zone fallback with division-published boundaries where possible. It does not promise that every division publishes a usable current polygon file.

## Measured starting point

After the Roanoke County and City fixes, the dashboard has 130 divisions and 1,810 school records. Two divisions use tested local public GIS files. The other 128 divisions contain 1,760 schools. Of those, 118 have a 2015–16 NCES SABS file and ten have no attendance-zone file. Nine of the 118 SABS divisions have only polygons with an unknown school level. In the 128 remaining divisions, 553 schools have a matched SABS polygon and 1,207 do not. A match to SABS shows a link to an older survey polygon; it does not verify a present-day assignment.

The audit also found 21 divisions with no elementary feature, 36 with no middle feature, and 29 with no high feature. Those counts include divisions that may not operate that school level, so they are review flags rather than proof that a required zone is absent. The ten divisions without any zone file are listed in [the coverage audit](ZONES_COVERAGE.md).

## Source availability sample

- [Roanoke County GIS](https://arcgis.roanokecountyva.gov/arcgisweb/rest/services/Published_Geodata/Schools/MapServer/4) has one polygon layer with all three assignments. It required splitting each area into overlapping level features and matching 26 school records.
- [Roanoke City's public locator](https://experience.arcgis.com/experience/ffab6c404bdc47188ac89c09a6bbd9ea/) uses separate elementary, middle, and high layers. The [2026–27 rezoning notice](https://www.rcps.info/about-rcps/attendancezones) led us to a newer elementary Phase 1 layer. It required a name crosswalk and one geometry repair. The Phase 1 elementary layer also has a roughly 35,380 m² Preston Park/Round Hill overlap that remains unresolved. The middle and high layers have older publication metadata even though the current locator uses them.
- [Arlington County open GIS](https://arlgis.arlingtonva.us/arcgis/rest/services/Open_Data/od_School_Boundaries_Polygons/FeatureServer) advertises elementary, middle, and high boundary layers. We have not yet checked its effective dates, query results, school matches, or geometry. It is a candidate, not counted as a completed division.
- [Campbell County's ArcGIS item](https://www.arcgis.com/home/item.html?id=2c81d22c0d294587ae31f00e89a44b8c) is another candidate. It also needs a source and effective-date check.

This small sample shows that division sources can have different layer structures, dates, names, and geometry. It cannot establish a statewide rate of available current data. We have not established one current official statewide school attendance-boundary dataset. [NCES SABS](https://nces.ed.gov/programs/edge/SABS) lists only the older 2013–14 and 2015–16 survey collections.

## Work estimate for the other 128 divisions

| Work unit | Planning range | Basis and deliverable |
| --- | ---: | --- |
| Source inventory and effective-date check | 65–190 hours | About 0.5–1.5 hours per division. Find the division's official map or GIS file; record publication and school-year dates; mark absent, PDF-only, or machine-readable. |
| Data import and school crosswalk | 130–385 hours | About 1–3 hours per division where a usable source exists. Adapt to single or multiple levels, match school IDs, and record schools without geographic zones. This is an upper-bound planning allowance if most divisions have usable files. |
| Geometry, coverage, and map verification | 65–190 hours | About 0.5–1.5 hours per imported division. Check CRS, validity, gaps and overlaps, level labels, school selection, and a few known addresses against the division locator. |
| Shared code, documentation, and regression review | 20–45 hours | Improve reusable import and audit code, update source registry and coverage report, and run app tests. |

**Planning range: about 280–810 person-hours, or 7–20 full-time workweeks, if most divisions provide usable GIS data.** This range is a workload estimate, not a delivery promise. The two Roanoke cases show why a single bulk conversion would not be reliable. If many divisions publish only PDFs, address lookup tools without downloadable polygons, or no public boundaries, full coverage may need manual digitizing, direct requests to divisions, or may not be possible from public data. Those cases could exceed the range. Some schools have no standard geographic attendance zone; the correct outcome for them is a documented exclusion, not a fabricated polygon.

## Recommended execution order

1. Audit the 128 divisions and record source URL, effective year, school levels, file access, and constraints in the source registry. Start with the ten divisions without a file, the nine with only unknown-level SABS polygons, and the largest divisions by enrollment.
2. Pilot several different source types, including Arlington's three layers and a division with no current downloadable GIS file. Use the measured pilot time to revise this estimate.
3. Import verified sources in batches. For each batch, require a school-ID crosswalk, geometry checks, level checks, a short list of exclusions, and map verification before replacing the SABS fallback.
4. Show the boundary effective year in the app. Keep the older SABS layer only where its age and limits are clear, or hide it where it gives a false sense of current school assignment.

The dashboard's division outline (choropleth) is a different layer from attendance zones. Its Lexington City gap has a separate Beads task.

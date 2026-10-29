# Attendance-zone coverage and checks

Audit dates: 2026-10-20, 2026-10-24, 2026-10-25, and 2026-10-28. Beads: `va_schools_dashboard-e9i` and `va_schools_dashboard-nlf`.

## Source and coverage

The optional attendance-zone layer has three source groups:

| Source | Divisions | Features | School matches | Date |
| --- | ---: | ---: | ---: | --- |
| NCES SABS | 110 active sources | 1,264 | 447 distinct schools | 2015–16 survey; older files retained when a division GIS source takes priority |
| Roanoke County public GIS | 1 | 60 level features from 20 assigned county areas | 26 of 26 county schools | Retrieved 2026-10-20; the GIS service does not state a boundary effective date |
| Roanoke City public GIS | 1 | 24 features: 17 elementary, 5 middle, 2 high | 24 of 24 City schools | Elementary Phase 1 applies to 2026–27; middle and high layer metadata date from 2009 |
| Neighboring division GIS and named SABS fallback | 4 | 67 features | 55 of 56 schools | Retrieved 2026-10-24; see source details below |
| Arlington APS locator / County GIS | 1 | 29 features: 20 elementary, 6 middle, 3 high | 29 of 35 schools | Retrieved 2026-10-25; GIS records still say 2022–23; see below |
| Fairfax FCPS / County GIS | 1 | 188 features: 138 elementary, 26 middle, 24 high | 185 of 195 schools | Retrieved 2026-10-25; GIS records say 2026–27; see below |
| Prince William PWCS GIS | 1 | 94 features: 63 elementary, 18 middle, 13 high | 93 of 95 schools | Retrieved 2026-10-25; elementary 2025–26, middle/high labeled current; see below |
| Chesterfield County Open GIS Data | 1 | 64 features: 41 elementary, 13 middle, 10 high | 63 of 65 schools; 1 source-only area | Retrieved 2026-10-26; no per-feature effective year; see below |

There are 130 divisions and 1,810 schools in the dashboard. Ten divisions have no attendance-zone file. Another nine have only SABS features with an unknown school level; the app offers an explicit “Unclassified (older source)” view for those features. Across all divisions, 21 lack elementary features, 36 lack middle features, and 29 lack high features, including the ten without any file. Across the full snapshot, 922 schools have a matched zone and 888 do not. A zone file in a division does **not** mean every school has a matched polygon. The NCES SABS data are an old, incomplete survey; NCES calls the survey experimental and lists only 2013–14 and 2015–16 collections: [NCES SABS](https://nces.ed.gov/programs/edge/SABS).

## Chesterfield County large-division validation (2026-10-26)

The official [County Open GIS Data portal](https://opengisdata.chesterfield.gov/) publishes school attendance areas. Its [elementary](https://services3.arcgis.com/TsynfzBSE6sXfoLq/ArcGIS/rest/services/Administrative_ProdA/FeatureServer/1), [middle](https://services3.arcgis.com/TsynfzBSE6sXfoLq/ArcGIS/rest/services/Administrative_ProdA/FeatureServer/10), and [high](https://services3.arcgis.com/TsynfzBSE6sXfoLq/ArcGIS/rest/services/Administrative_ProdA/FeatureServer/6) layers contain 41, 13, and 10 areas. The [source manifest](../data-raw/zone_source_manifests/5100840.json) records the retrieval, counts, and staged-file hashes. The layers have no per-feature school-year field. [CCPS reports](https://www.oneccps.org/article/3052429) that Deep Creek Elementary opened for 2026–27; its zone appears in the source. This supports that change, but does not establish a single effective year for every source feature. Use [CCPS Find My School](https://www.oneccps.org/find-my-school/) to confirm an address. The County portal's terms permit use and distribution and request attribution.

The [strict crosswalk](../data-raw/zone_school_crosswalks/5100840.csv) links 63 of 65 snapshot schools, with eight explicit name aliases. Deep Creek Elementary is absent from this frozen snapshot, so its official area stays visible without a school-data link. Carver College and Career Academy and Chesterfield Virtual School have no separate standard neighborhood polygon. No boundary or school identity was inferred for those cases.

All 64 saved geometries are valid and nonempty. The [equal-area report](../data-raw/zone_validation_reports/5100840.json) measures same-level overlap of 15,721.9 m² elementary, 457.2 m² middle, and 45.0 m² high. The largest elementary pairs are Old Hundred/Deep Creek (9,561.9 m²), Ettrick/Matoaca (2,826.6 m²), and Matoaca/O.B. Gates (1,264.1 m²). The reviewed import cap is 16,000 m². The source polygons are kept as published; CCPS must confirm an address near an overlap.

Run `python3 data-raw/stage_zone_source.py 5100840`, `python3 data-raw/build_chesterfield_zone_crosswalk.py`, and `python3 data-raw/import_zone_source.py 5100840` to rebuild the checked candidate. The checked file has SHA-256 `7953ae7ea95af8f59c76ad6675ddad7464c546a44662f1f7d7a3bc168fba70dd`. The importer regression, R zone/cache test, and statewide audit pass. In the local Shiny app, the Chesterfield division view rendered all 41 elementary, 13 middle, and 10 high paths; the Filters panel showed the source note. This is a local integration; public release remains a separate bead.

## Fairfax County large-division validation (2026-10-25)

[FCPS publishes 2026–27 boundary maps](https://www.fcps.edu/facilities-planning-future/maps) and directs families to its address locator. The Fairfax County GIS [elementary](https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/Elementary_School_Attendance_Areas/FeatureServer/0), [middle](https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/Middle_School_Attendance_Areas/FeatureServer/0), and [high](https://services1.arcgis.com/ioennV6PpG5Xodq0/ArcGIS/rest/services/High_School_Attendance_Areas/FeatureServer/0) standard attendance layers supply 138, 26, and 24 polygons. These are the standard attendance layers, not the separate Advanced Academic Program layers. All 188 downloaded records have `SCH_YR=2026_27`. The layer descriptions say last revision July 29, 2026; their data edit timestamps are August 27, 2026. The [source manifest](../data-raw/zone_source_manifests/5101260.json) records the checked files, URLs, counts, and hashes.

The [explicit crosswalk](../data-raw/zone_school_crosswalks/5101260.csv) links 188 polygons to 185 of the 195 schools in the frozen snapshot. Three secondary schools each have a middle and high polygon. Five source names need explicit aliases, including FCPS `Katherine Johnson` versus snapshot `Johnson Middle` and FCPS `Franklin Sherman` versus snapshot `Sherman Elementary`. The snapshot labels many elementary schools `Combined (K-8)`; each mapped row notes the FCPS elementary source level. Ten snapshot schools have no separately named polygon in these standard layers: Bailey's Upper, Bryant High, Fairfax County Adult High, Fort Belvoir Upper, Key Center, Kilmer Center, Kings Glen, McNair Upper, Mountain View High, and Thomas Jefferson High for Science and Technology. They are listed as exclusions, not given inferred boundaries. Confirm their assignment with FCPS.

All 188 saved geometries are valid and nonempty. The [equal-area report](../data-raw/zone_validation_reports/5101260.json) measures less than 0.04 m² same-level overlap at elementary and middle levels, but 32,702.9 m² total in the high-school layer. The larger high-school pair totals are Woodson/Fairfax (19,818.5 m² in 51 patches) and Falls Church/Woodson (9,977.9 m² in 19 patches); the largest single patch is about 5,253.5 m². The high-school union has a 32,712.2 m² gap relative to the middle-school union. The reviewed import limit is 33,000 m²; the app states that overlapping edges cannot establish an address assignment and directs users to the FCPS locator. No boundaries were redrawn.

Run `python3 data-raw/stage_zone_source.py 5101260`, `python3 data-raw/build_fairfax_zone_crosswalk.py`, and `python3 data-raw/import_zone_source.py 5101260` for a checked candidate. `--publish` updates the local division file and level cache after validation. A second dry run reproduced SHA-256 `41703e295114f04d975c59a431942324c9bce3e1ea0d04c86b85e2e279e847cf`. In the local Shiny app, the Fairfax division view rendered 138 elementary, 26 middle, and 24 high zone paths. Selecting Aldrin showed one polygon despite its `Combined (K-8)` snapshot label; selecting Bailey's Upper showed no polygon and the missing-zone message. The Fairfax source note and overlap warning appeared in Filters. The statewide audit, zone test, and snapshot test pass. This is a local integration; public release remains a separate task.

## Prince William County large-division validation (2026-10-25)

[PWCS links its school locator and attendance maps](https://www.pwcs.edu/registration/find_your_school). The locator's [web map](https://gis.pwcs.edu/portal/home/item.html?id=6c4272015acb418c9ea413149993c2df) labels [elementary layer `/1`](https://gis.pwcs.edu/server/rest/services/Find_Your_School_Update_2026/FeatureServer/1) **2025–26**, while [middle `/2`](https://gis.pwcs.edu/server/rest/services/Find_Your_School_Update_2026/FeatureServer/2) and [high `/3`](https://gis.pwcs.edu/server/rest/services/Find_Your_School_Update_2026/FeatureServer/3) are labeled current without a separate effective year. The REST title for `/1` says current, but the web map's more specific 2025–26 label and its Potomac View polygon match the frozen school list. The separate 2026–27 elementary layer `/4` names Woodbridge Elementary instead. [PWCS says Potomac View closed June 30, 2026](https://www.pwcs.edu/about/departments/facilities/construction/potomac_view_elementary_school_closure); its [board decision](https://www.pwcs.edu/news/2026/02/pwcs_school_board_approves_new_boundaries_for_upcoming_woodbridge_area_potomac_shores_elementary_schools) puts Woodbridge changes in 2026–27 and Potomac Shores #2 changes in 2027–28. The frozen snapshot has Potomac View but no Woodbridge ID, so the new Woodbridge polygon was **not** linked to Potomac View. Blocked follow-up bead `va_schools_dashboard-nlf.6.3` holds the 2026–27 reconciliation.

The [source manifest](../data-raw/zone_source_manifests/5103130.json) records all 94 checked polygons: 63 elementary, 18 middle, and 13 high. The [crosswalk](../data-raw/zone_school_crosswalks/5103130.csv) links them to 93 of 95 snapshot schools. The Nokesville School has both elementary and middle polygons. Twenty-one exact aliases resolve abbreviated PWCS names, such as `King Elementary School` to `Martin Luther King Jr. Elementary`; no fuzzy link is used at publication. Mary G. Porter Traditional and Pennington Traditional have no separate polygon in these standard layers.

All 94 geometries are valid and nonempty. The [equal-area report](../data-raw/zone_validation_reports/5103130.json) measures 165.79 m² total same-level overlap in elementary boundaries, spread across small edge patches (largest 10.89 m²); middle overlap is 0.65 m² and high overlap is negligible. The reviewed limit is 170 m². The high-school union extends about 107.12 km² beyond elementary coverage in one main area centered near Quantico. That geographic difference does not establish a PWCS address assignment; [Marine Corps Base Quantico lists separate on-base schools](https://www.quantico.marines.mil/Resources/PCS-Inbound-Outbound/Checking-In/). The app source note and popup state the mixed source dates, Quantico difference, and need to confirm an address. No boundary was redrawn.

Run `python3 data-raw/stage_zone_source.py 5103130`, `python3 data-raw/build_prince_william_zone_crosswalk.py`, and `python3 data-raw/import_zone_source.py 5103130` for a checked candidate. `--publish` updates only the local division file and cache. A second dry run reproduced SHA-256 `d07a396fa4f233fb9116ad6e0b6d6caaa2383143249d58fdf27a612a82fec4f4`. The local Shiny app rendered 63, 18, and 13 zone paths by level. Selecting Potomac View showed its dated elementary polygon; selecting Pennington Traditional showed no polygon and the missing-zone message. The statewide zone audit, zone test, and snapshot test pass. Public release remains separate.

## Arlington County local pilot (2026-10-25)

[APS says its boundary locator reports neighborhood assignments for this school year](https://www.apsva.us/registration/maps-boundaries/) and links [elementary](https://www.apsva.us/wp-content/uploads/sites/57/2026/07/ES_Boundaries_LgFormat_SY26_27.pdf), [middle](https://www.apsva.us/wp-content/uploads/sites/57/2026/07/MS_Boundaries_LgFormat_SY26_27.pdf), and [high](https://www.apsva.us/wp-content/uploads/sites/57/2026/07/HS_Boundaries_LgFormat_SY26_27.pdf) maps labeled 2026–27. Its live locator uses the County [School Boundary Locator MapServer](https://arlgis.arlingtonva.us/arcgis/rest/services/Public_Maps/School_Boundary_Locator_v2/MapServer), with 20 elementary, six middle, and three high school polygons. The separate County open-data service also lists bus and walk eligibility polygons; those are **not** attendance areas and were not imported.

Every polygon has a unique dashboard school ID. The six schools with no neighborhood polygon are Arlington Community High, Arlington Traditional, Campbell Elementary, Claremont Immersion, Escuela Key Elementary, and Montessori Public School of Arlington. The snapshot labels Yorktown High as a Combined school; its official high school area is linked to its school ID without changing the school record. The 29 saved polygons are valid, nonempty, and within Virginia coordinates. A later equal-area [geometry report](../data-raw/zone_validation_reports/5100270.json) found only small same-level overlap slivers: 1.56 m² elementary, 0.02 m² middle, and 3.00 m² high, each below the reviewed 5 m² limit. The elementary union is about 59.46 km², versus 67.55 km² at each of middle and high levels. The build does not fill the difference by inference.

The GIS `School_Year` field says **2022-2023** for every polygon, despite the current APS locator and 2026–27 PDF links. The elementary PDF, created July 31, 2026, says the neighborhood boundaries were adopted December 3, 2020. The app popup and layer note state the year-field conflict and direct users to confirm an address with APS. The raw downloads are staged under ignored `data-raw/input/zones/5100270/`; [the tracked manifest](../data-raw/zone_source_manifests/5100270.json) records their URLs, counts, and hashes. The reusable path now runs `python3 data-raw/stage_zone_source.py 5100270`, then `python3 data-raw/import_zone_source.py 5100270` for a checked candidate, then the same command with `--publish` after review. The earlier Arlington-specific scripts remain working reference examples. Both paths build the same GeoJSON SHA-256.

The 8.09 km² difference between high-school and elementary union areas has two material pieces: 5.075 km² centered near Arlington National Cemetery/Pentagon and 3.019 km² centered at Reagan National Airport. The 2026–27 APS PDF shows those large areas as federal property or airport, outside the purple neighborhood elementary boundaries. Smaller differences are below 0.001 km² each. This is a visual source comparison, not a parcel-level proof of the boundary. In the local Shiny app, elementary, middle, and high division overlays appeared at their expected levels. Selecting Abingdon showed one elementary polygon; selecting Yorktown showed one high polygon despite its Combined school label; selecting Arlington Traditional showed no polygon. The corrected filter note now names the APS/County GIS source. This local check does not establish a public release.

## Divisions bordering Roanoke County (2026-10-24)

Roanoke County's [comprehensive-plan chapter](https://www.roanokecountyva.gov/DocumentCenter/View/30369/Chapter-2) names six bordering counties and the two cities it surrounds. Roanoke County itself already has a separate GIS file. All eight neighboring divisions have a polygon file in this snapshot; the source and school-level coverage differ:

| Division | Map source now used | Level polygons | Matched dashboard schools | Limit |
| --- | --- | --- | ---: | --- |
| Bedford County | [County open GIS](https://webgis.bedfordcountyva.gov/arcgis/rest/services/OpenData/OpenDataSchools/MapServer): layers 1–3; NCES SABS for Bedford Primary | 13 elementary, 3 middle, 3 high | 19/19 | Bedford Primary polygon is from 2015–16. [BCPS is reviewing boundary changes](https://www.bedford.k12.va.us/families_students/b_c_p_s_redistricting_process); check the current decision before using the map for an address. |
| Botetourt County | [County elementary](https://services6.arcgis.com/6h5xc2eGkGuRiFia/arcgis/rest/services/Elementary_School_Districts/FeatureServer/0), [middle](https://services6.arcgis.com/6h5xc2eGkGuRiFia/arcgis/rest/services/Middle_School_Districts/FeatureServer/0), and [high](https://services6.arcgis.com/6h5xc2eGkGuRiFia/arcgis/rest/services/High_School_Districts/FeatureServer/0) GIS | 7, 2, 2 | 11/11 | The layers do not state a school-year effective date. |
| Craig County | NCES SABS 2015–16 | 1 elementary, 1 unclassified | 2/2 | The high school serves grades 6–12 in this old survey; its polygon is unclassified in the app. |
| Floyd County | NCES SABS 2015–16 | 5 unclassified | 5/5 | The survey schools have combined grade spans; use the Unclassified view. |
| Franklin County | NCES SABS 2015–16 | 12 elementary, 3 middle, 1 high | 11/12 | The file includes former schools and old middle campuses. The present middle-school area is not established by this survey. A [public GIS layer marked proposed 2025](https://services9.arcgis.com/AzdpqVmJ5GjGWCoI/arcgis/rest/services/FranklinCoVA_PublicSchooolAttendanceZones_Proposed2025/FeatureServer/0) was not used as an adopted boundary. |
| Montgomery County | [County Schools & Zones GIS](https://maps.montva.com/server/rest/services/Schools/Schools_Zones/MapServer/0) | 11 elementary, 10 middle, 10 high | 19/19 | One GIS area labeled Roanoke County is excluded. [MCPS lists Christiansburg Primary in the Christiansburg Elementary zone](https://mcps.ss7.sharpschool.com/schools/school_zones); the app links both schools to that polygon. GIS effective date is not stated. |
| Roanoke City | City public GIS, detailed below | 17 elementary, 5 middle, 2 high | 24/24 | Phase 1 elementary applies to 2026–27; middle/high dates are not stated. |
| Salem City | [City school GIS](https://arcgispub.salemva.gov/server/rest/services/SCHOOL_SYSTEM_LAYERS_WMI/MapServer/136) for elementary; NCES SABS 2015–16 for middle/high | 4 elementary, 1 middle, 1 high | 6/7 | The virtual academy has no physical attendance polygon. City GIS publishes only elementary districts; the other two polygons are older survey data. |

`data-raw/build_neighbor_zones.py` downloads the four public GIS sources, checks source counts, coordinates, names, and school IDs, and uses GDAL `ogr2ogr -makevalid` before atomic replacement. It preserves the named SABS fallback polygons for Salem middle/high and Bedford Primary. The checked per-level cache is built by `data-raw/build_zone_level_cache.py`. This is map context, not an address assignment tool.

`data-raw/link_neighbor_sabs.py` links 15 additional historical SABS polygons to current school records only where both the school ID and name match. It leaves Franklin's former schools and old middle campuses unlinked. This adds school-click access to an **old** polygon; it does not establish a present-day assignment.

## Missing divisions

- `5100210` Amherst County Public Schools
- `5100300` Augusta County Public Schools
- `5101590` Giles County Public Schools
- `5101650` Goochland County Public Schools
- `5101740` Greensville County Public Schools
- `5101860` Harrisonburg City Public Schools
- `5103030` Powhatan County Public Schools
- `5103600` Southampton County Public Schools
- `5103810` Tazewell County Public Schools
- `5104020` Williamsburg-James City County Public Schools

## Roanoke County repair

The old SABS file had 26 features but matched only 10 of 26 current schools. None of the county's 16 elementary schools had a matched zone. The [county Schools MapServer](https://arcgis.roanokecountyva.gov/arcgisweb/rest/services/Published_Geodata/Schools/MapServer) provides a polygon layer (`/4`) with elementary, middle, and high assignment fields. Its public query returned 21 records: 20 assigned areas and one unassigned area. `data-raw/build_roanoke_county_zones.py` creates one feature for each school level in each assigned area, for 60 features. It validates all 26 school names and IDs before it replaces the prior file. A failed query or mismatch keeps the prior output.

The app now shows one attendance-zone level at a time in a division view, or an explicit unclassified view for older SABS features. Selecting a school shows that school's matched feature or features. If there is no match, the app leaves the zone layer empty and gives a warning. It no longer shows all division zones as if they belonged to that school. School levels can overlap; selecting only one level prevents another level from covering it on the map.

## Roanoke City repair

The old SABS file had 24 features but linked only 5 of 24 dashboard schools. The [City public locator](https://experience.arcgis.com/experience/ffab6c404bdc47188ac89c09a6bbd9ea/) uses a [newer map](https://roanokeva.maps.arcgis.com/home/item.html?id=c5ca5ed07e524fba9826a24c33b1e856) with a separate [Phase 1 elementary layer](https://services.arcgis.com/KII0S5XRkpE66Ngk/arcgis/rest/services/Elementary_Schools_Phase_1/FeatureServer/0). [Roanoke City Public Schools](https://www.rcps.info/about-rcps/attendancezones) says the approved changes took effect in 2026–27. The build uses that layer for elementary zones and the City's public [middle](https://gis03.roanokeva.gov/arcgis/rest/services/Public/Schools_Middle/FeatureServer/1) and [high](https://gis03.roanokeva.gov/arcgis/rest/services/Public/Schools_High/FeatureServer/1) layers for the other levels. Those two layers still carry 2009 publication metadata; their current boundary effective dates are not stated. The City locator uses them, but an address must still be confirmed with the division.

`data-raw/build_roanoke_city_zones.py` gets 17 elementary, 5 middle, and 2 high features. All 24 official labels map to the 24 dashboard school IDs. It uses explicit aliases for the dashboard's older names for Lucy Addison, James Breckinridge, and Roanoke Academy. One source middle polygon had a repeated vertex; GDAL `ogr2ogr -makevalid` repaired it. The saved file has 24 valid polygons. The build checks the record counts and school assignments before atomic replacement. The three levels remain separate so their areas can overlap on the map. A same-level check found a roughly **35,380 m² overlap between Preston Park and Round Hill elementary polygons** in the published Phase 1 layer, plus three small overlaps under 500 m². The app shows the source areas and warns users; it does not guess which school serves an address in the overlap. Confirm affected addresses with Roanoke City Public Schools.

## Rebuild and verify

From the repository root in WSL:

```bash
python3 data-raw/build_roanoke_county_zones.py
python3 data-raw/build_roanoke_city_zones.py
python3 data-raw/build_neighbor_zones.py
python3 data-raw/stage_arlington_zone_source.py
python3 data-raw/build_arlington_zones.py
python3 data-raw/link_neighbor_sabs.py
python3 data-raw/build_zone_level_cache.py
python3 data-raw/audit_zones.py
Rscript --vanilla tests/check_zones.R
```

The level-cache build splits the two checked Roanoke division files into
ready-to-render Elementary, Middle, and High files. The app uses a level file
only while its recorded MD5 matches the combined division file. The hash
normalizes Windows and Linux line endings. If a source changes without a cache
rebuild, the app safely uses the original filter path.
Run the cache build after either Roanoke source build and before deployment.

### Level-switch timing check, 2026-10-24

On the local Ubuntu app, the old R filter took about 1.7–2.3 seconds for a
Roanoke City level and 3.5–3.6 seconds for a County level. Reading a prepared
level file took 0.004–0.005 seconds. In a 1280 × 800 headless Edge check on
this machine, change-to-overlay time fell from 1.8–2.0 to 0.04–0.06 seconds
for City and from 3.2–3.4 to 0.04–0.06 seconds for County. Each level replaced
the prior overlay with the expected feature count: City 17/5/2 and County
20/20/20. These are local measurements, not a network latency guarantee.

The county service was reachable through `Published_Geodata/Schools/MapServer` on the audit date. A separate `AGOL_OpenData/OpenData_Community` service advertised similar layers, but its query returned a service-not-started error. Do not use its metadata as proof that data can be downloaded.

These polygons are map context, not an address assignment tool. Boundary dates and rules can change. Confirm an address with the school division before use.

## Statewide work estimate

See [statewide zone estimate](STATEWIDE_ZONES_EFFORT_2026-10-20.md) for measured remaining coverage, work units, and uncertainty.

## Next public source checks

A [Campbell County attendance-area ArcGIS item](https://www.arcgis.com/home/item.html?id=2c81d22c0d294587ae31f00e89a44b8c) advertises elementary, middle, and high areas. It remains a source candidate. Confirm effective dates, completeness, school IDs, and access before adding it.

The division choropleth is a separate geography. It uses 135 [Census 2024 unified district polygons](https://www2.census.gov/geo/tiger/TIGER2024/UNSD/) and one [elementary district polygon](https://www2.census.gov/geo/tiger/TIGER2024/ELSD/) for Lexington City Public Schools (`5102220`), for 136 polygons total and direct IDs for all 130 dashboard divisions. The Lexington elementary polygon covers grades KG–12 in the Census file. The source Rockbridge County polygon has an interior hole at Lexington City; the app now preserves interior holes instead of filling them. Two Rockbridge schools have NCES points inside Lexington City, so the app uses direct division IDs for district labels and values rather than assigning those schools to Lexington by point location. This change does not add any school attendance zones. Bead `va_schools_dashboard-nlf` tracks the separate zone work.

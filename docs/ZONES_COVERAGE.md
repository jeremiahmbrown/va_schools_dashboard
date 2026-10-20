# Attendance-zone coverage and checks

Audit date: 2026-10-20. Bead: `va_schools_dashboard-e9i`.

## Source and coverage

The optional attendance-zone layer has three source groups:

| Source | Divisions | Features | School matches | Date |
| --- | ---: | ---: | ---: | --- |
| NCES SABS | 118 | 1,689 | 553 distinct schools | 2015–16 survey |
| Roanoke County public GIS | 1 | 60 level features from 20 assigned county areas | 26 of 26 county schools | Retrieved 2026-10-20; the GIS service does not state a boundary effective date |
| Roanoke City public GIS | 1 | 24 features: 17 elementary, 5 middle, 2 high | 24 of 24 City schools | Elementary Phase 1 applies to 2026–27; middle and high layer metadata date from 2009 |

There are 130 divisions and 1,810 schools in the dashboard. Ten divisions have no attendance-zone file. Another nine have only SABS features with an unknown school level; the app offers an explicit “Unclassified (older source)” view for those features. Across all divisions, 21 lack elementary features, 36 lack middle features, and 29 lack high features, including the ten without any file. Across the full snapshot, 603 schools have a matched zone and 1,207 do not. A zone file in a division does **not** mean every school has a matched polygon. The NCES SABS data are an old, incomplete survey; NCES calls the survey experimental and lists only 2013–14 and 2015–16 collections: [NCES SABS](https://nces.ed.gov/programs/edge/SABS).

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
python3 data-raw/audit_zones.py
Rscript --vanilla tests/check_zones.R
```

The county service was reachable through `Published_Geodata/Schools/MapServer` on the audit date. A separate `AGOL_OpenData/OpenData_Community` service advertised similar layers, but its query returned a service-not-started error. Do not use its metadata as proof that data can be downloaded.

These polygons are map context, not an address assignment tool. Boundary dates and rules can change. Confirm an address with the school division before use.

## Statewide work estimate

See [statewide zone estimate](STATEWIDE_ZONES_EFFORT_2026-10-20.md) for measured remaining coverage, work units, and uncertainty.

## Next public source checks

The [Arlington County open GIS service](https://arlgis.arlingtonva.us/arcgis/rest/services/Open_Data/od_School_Boundaries_Polygons/FeatureServer) advertises elementary and middle boundary layers. A [Campbell County attendance-area ArcGIS item](https://www.arcgis.com/home/item.html?id=2c81d22c0d294587ae31f00e89a44b8c) advertises elementary, middle, and high areas. These are source candidates, not yet validated for this snapshot. Confirm effective dates, completeness, school IDs, and access before adding them.

The division choropleth is a separate geography. Its Census 2024 unified school district snapshot has 135 polygons and direct IDs for 129 of 130 dashboard divisions. Lexington City Public Schools (`5102220`) has no direct polygon in that layer. The [Census TIGER/Line school district documentation](https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2024/TGRSHP2024_TechDoc.pdf) describes separate elementary, secondary, and unified district layers. Check the elementary district layer for Lexington City before changing the choropleth. Beads tasks `va_schools_dashboard-nlf` and `va_schools_dashboard-6v0` track these source checks.

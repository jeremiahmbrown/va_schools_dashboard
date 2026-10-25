# Roanoke City attendance-zone source check — 2026-10-25

Bead va_schools_dashboard-9ht asks whether a newer public boundary resolves the
same-level overlap between Preston Park and Round Hill elementary areas. This
is a source check only. No map geometry or school assignment was changed.

## Public GIS

The [Phase 1 elementary layer](https://services.arcgis.com/KII0S5XRkpE66Ngk/arcgis/rest/services/Elementary_Schools_Phase_1/FeatureServer/0)
still has 17 features. Its service metadata reports a last data edit at
2026-06-04 20:00:18 UTC and a last service edit at 2026-06-04 20:20:50 UTC.
The description also says “Publication Year: 2009,” which appears inconsistent
with this Phase 1 layer and is not evidence that the boundaries are from 2009.

The current GeoJSON features were projected to EPSG:26917 for an area check.
Preston Park (OBJECTID 13) and Round Hill (OBJECTID 16) still overlap by about
35,380 square metres. The layer is not a corrected source for that overlap.
The [middle](https://gis03.roanokeva.gov/arcgis/rest/services/Public/Schools_Middle/FeatureServer/1)
and [high](https://gis03.roanokeva.gov/arcgis/rest/services/Public/Schools_High/FeatureServer/1)
services still say “Publication Year: 2009” and provide no current data-edit
date. Their effective boundary years remain unconfirmed.

## RCPS notice and maps

The [RCPS attendance-zone page](https://www.rcps.info/about-rcps/attendancezones)
says the School Board approved changes on 2026-05-26 for school year 2026–27.
Its [Approved Attendance Zone Changes PDF](https://resources.finalsite.net/images/v1777928435/rcpsinfo/fqoulev0hilfkzoz22bc/ApprovedChanges.pdf)
shows current and recommended elementary maps and lists changed address
ranges. The lists include changes between Round Hill and Preston Park.
They do not provide a machine-readable corrected polygon or decide the school
assignment for every point within the GIS overlap. A visual map is not precise
enough to repair the 35,380-square-metre overlap.

## Disposition

Keep the source geometry and its overlap warning. Do not assign an address in
the overlap from the dashboard. A dated, corrected official polygon or a
school-division confirmation of the affected address ranges is still needed
before a boundary repair. Also obtain a dated source for the middle and high
layers before claiming their effective years. Bead va_schools_dashboard-9ht
remains open. No email or other contact was sent.

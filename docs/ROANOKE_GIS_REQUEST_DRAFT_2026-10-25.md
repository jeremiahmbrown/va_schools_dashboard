# Draft request to Roanoke City GIS — not sent

This is a saved draft for Bead va_schools_dashboard-9ht. It is not an email
message in any mail system. Do not send it without the user's explicit
authorization.

To: gis@roanokeva.gov
Cc: none
Subject: Request for current Roanoke City school attendance-zone GIS boundaries

Hello City GIS team,

I am checking public attendance-zone data for a Virginia schools dashboard.
Roanoke City Public Schools says elementary zone changes approved on May 26,
2026 took effect for the 2026–27 school year.

In the City's Phase 1 elementary GIS layer linked below, the Preston Park
polygon (OBJECTID 13) and Round Hill polygon (OBJECTID 16) overlap by about
35,380 square metres. I calculated this by projecting the published polygons
from EPSG:4326 to EPSG:26917 and measuring their intersection. This is a
same-level overlap: a location in that area appears in both elementary zones.
Is the overlap intentional? If not, is there a corrected, adopted 2026–27
polygon file or GIS service that we should use? Please include its effective
date or version if available.

I also checked the City's middle- and high-school zone layers linked below.
Both list “Publication Year: 2009.” Are these still the current adopted
boundaries? If newer layers exist, could you share their locations and
effective dates?

If another team maintains the school assignments, please route this request
to the right contact. Thank you.

[Sender name]

Source links:

- RCPS attendance-zone update: https://www.rcps.info/about-rcps/attendancezones
- Elementary Phase 1 layer: https://services.arcgis.com/KII0S5XRkpE66Ngk/arcgis/rest/services/Elementary_Schools_Phase_1/FeatureServer/0
- Middle layer: https://gis03.roanokeva.gov/arcgis/rest/services/Public/Schools_Middle/FeatureServer/1
- High layer: https://gis03.roanokeva.gov/arcgis/rest/services/Public/Schools_High/FeatureServer/1

The calculation and raw GeoJSON are documented in
[the overlap method](ROANOKE_OVERLAP_METHOD_2026-10-25.md).

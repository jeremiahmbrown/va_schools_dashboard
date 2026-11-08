# Harrisonburg school-zone source audit - 2026-11-08

Bead: `va_schools_dashboard-nlf.2.6`. Production status: **hold**. Ten school links are integrated in the separate local research checkout, but none are added to this public release.

## Checked source

The [City GIS page](https://www.harrisonburgva.gov/GIS) links the School Districts Map and a separate GIS Data Download Gallery. Its disclaimer says the maps are reference information with no accuracy warranty. The [City SchoolDistrictMap service](https://gis.harrisonburgva.gov/arcgis/rest/services/CityServices/SchoolDistrictMap/MapServer) has six elementary areas and two middle/high pairs (ten level features), including Rocktown High. The service reports item ID `aec9c647e31b4fac95956bd2adeed042`.

The exact [City ArcGIS Enterprise item](https://gis.harrisonburgva.gov/gisportal/sharing/rest/content/items/aec9c647e31b4fac95956bd2adeed042?f=json) is public. It has no `licenseInfo` or `accessInformation`, and its metadata mark it unlisted. The City's GIS Data Download Gallery is a separate ArcGIS Online group. Its 20 listed items did not include the school zone map on this check. The GIS page's invitation to download gallery layers therefore does not establish permission to redistribute this exact Enterprise service geometry.

The service has no effective-year field. The saved source record has no feature edit date or adopted-year statement. The current City map has the present Rocktown school name, but that fact alone does not prove a 2026-27 effective year. The 2025-26 school registration material confirms feeder groups, not the 2026-27 polygon edges.

## Decision and next evidence

Keep the ten verified local links and valid geometry in research. Do not publish the polygons until an official City or school-division source identifies the effective year and the City confirms that the exact service geometry may be republished as derived GeoJSON. An [unsent source-request draft](HARRISONBURG_VA_ZONE_SOURCE_REQUEST_DRAFT_2026-11-08.md) asks for both points. No request was sent. Do not assign addresses from the map during this hold.

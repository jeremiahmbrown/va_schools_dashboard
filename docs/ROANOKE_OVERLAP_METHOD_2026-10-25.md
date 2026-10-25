# How the Preston Park–Round Hill overlap was measured

This is a repeatable geometry check for Bead va_schools_dashboard-9ht. It
reads the City's [Phase 1 elementary GIS layer](https://services.arcgis.com/KII0S5XRkpE66Ngk/arcgis/rest/services/Elementary_Schools_Phase_1/FeatureServer/0).
It does not change the dashboard or decide which school serves an address.

## Files

- [Raw GeoJSON](../data-raw/roanoke_phase1_2026-10-25.geojson): the unchanged
  response saved on 2026-10-25 from the layer's query endpoint. It has 17
  school polygon features. SHA-256:
  `8fbec2cbc4994ee2946145eaeefc107cba8b9ffd7a53dc723020bdcde12a457e`.
- [Python script](../data-raw/check_roanoke_phase1_overlap.py): reads the
  saved response by default, or downloads the current layer with `--live`.
  It uses Python GDAL/OGR and PROJ from the WSL system Python.

## Run it

In the authoritative Ubuntu checkout:

```bash
cd /home/jeremiah/code/va_schools_dashboard
/usr/bin/python3 data-raw/check_roanoke_phase1_overlap.py
```

To check the current live service instead of the saved response:

```bash
/usr/bin/python3 data-raw/check_roanoke_phase1_overlap.py --live
```

The saved file lets this result be reproduced even if the public GIS layer
changes. The script can also save a new live response with
`--live --save-raw NEW_PATH`; it refuses to replace an existing file.

To look at the two raw feature objects before the calculation, run:

```bash
/usr/bin/python3 - <<'PY'
import json
from pathlib import Path

path = Path("data-raw/roanoke_phase1_2026-10-25.geojson")
features = json.loads(path.read_text())["features"]
for feature in features:
    if feature["properties"]["OBJECTID"] in (13, 16):
        print(feature["properties"])
        print("geometry type:", feature["geometry"]["type"])
        print("rings:", len(feature["geometry"]["coordinates"]))
        print("first three lon/lat vertices:", feature["geometry"]["coordinates"][0][:3])
PY
```

## Calculation, step by step

1. The script requests GeoJSON with all 17 features, the `OBJECTID` and
   `SCHOOL` fields, and polygon geometry in EPSG:4326. Each coordinate is
   longitude and latitude in **degrees**.
2. It selects Preston Park (`OBJECTID=13`) and Round Hill (`OBJECTID=16`).
   It checks their names and checks that both raw geometries are valid
   polygons in the Roanoke area.
3. It transforms a copy of each polygon to EPSG:26917, NAD83 / UTM zone 17N.
   Coordinates in this local projected system are in **metres**. Calculating
   area on the raw longitude/latitude coordinates would give square degrees,
   which is not a useful ground-area unit.
4. OGR calculates the **intersection**: the geometry containing only places
   that occur in *both* polygons. In set notation, `I = P ∩ R`.
5. OGR calculates `area(I)` in square metres. The result for the saved raw
   response is **35,379.983 m²**, or **35,380 m²** to the nearest square metre.
   This is about 0.637% of the Preston Park polygon and 0.275% of the Round
   Hill polygon.

The intersection is reported as a GeometryCollection. Its polygonal parts
contribute area; any line or point parts have zero area. The precise number
depends slightly on the projection and the source vertices. The check proves
that the published polygons overlap. It does **not** show which school should
serve an address inside that area.

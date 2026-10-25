#!/usr/bin/env python3
"""Measure the overlap in Roanoke City's raw Phase 1 elementary GIS layer.

Run from the repository root in WSL:

    /usr/bin/python3 data-raw/check_roanoke_phase1_overlap.py

The default input is the saved, unmodified GeoJSON response beside this script.
Use --live to check the current public service. Use --live --save-raw PATH to
save a new response without replacing the checked copy.

Math: area(transform(Preston Park) intersection transform(Round Hill)).
The service supplies longitude/latitude in degrees. EPSG:26917 changes both
polygons to a local projected system whose coordinate unit is the metre.
Therefore, OGR's area result is in square metres.
"""

import argparse
import hashlib
import json
import urllib.parse
import urllib.request
from pathlib import Path

from osgeo import ogr, osr

ogr.UseExceptions()
osr.UseExceptions()


LAYER_URL = (
    "https://services.arcgis.com/KII0S5XRkpE66Ngk/arcgis/rest/services/"
    "Elementary_Schools_Phase_1/FeatureServer/0"
)
SAVED_RAW = Path(__file__).with_name("roanoke_phase1_2026-10-25.geojson")
EXPECTED_SCHOOLS = {
    13: "PRESTON PARK ELEMENTARY SCHOOL",
    16: "ROUND HILL ELEMENTARY SCHOOL",
}


def get_live_geojson():
    # This is the raw source query. Ask for all features, their two identifying
    # fields, and original polygon geometry as WGS84 GeoJSON (lon, lat).
    query = urllib.parse.urlencode(
        {
            "where": "1=1",
            "outFields": "OBJECTID,SCHOOL",
            "returnGeometry": "true",
            "outSR": "4326",
            "f": "geojson",
        }
    )
    request = urllib.request.Request(
        f"{LAYER_URL}/query?{query}",
        headers={"User-Agent": "va-schools-dashboard/roanoke-overlap-check"},
    )
    with urllib.request.urlopen(request, timeout=60) as response:
        return response.read()


def source_bytes(args):
    if not args.live:
        if args.save_raw:
            raise ValueError("--save-raw requires --live")
        return args.input.read_bytes(), str(args.input)
    return get_live_geojson(), f"{LAYER_URL}/query"


def selected_features(raw):
    collection = json.loads(raw)
    features = collection.get("features")
    if collection.get("type") != "FeatureCollection" or not isinstance(features, list):
        raise ValueError("The source did not return a GeoJSON FeatureCollection")
    if len(features) != 17:
        raise ValueError(f"Expected 17 elementary features; received {len(features)}")

    by_id = {}
    for feature in features:
        props = feature.get("properties") or {}
        oid = props.get("OBJECTID")
        if oid in by_id:
            raise ValueError(f"Duplicate OBJECTID: {oid}")
        by_id[oid] = feature
    for oid, school in EXPECTED_SCHOOLS.items():
        if oid not in by_id or by_id[oid]["properties"].get("SCHOOL") != school:
            raise ValueError(f"OBJECTID {oid} no longer identifies {school}")
    return features, by_id


def spatial_references():
    # GeoJSON's EPSG:4326 coordinates are longitude, latitude. Set the axis
    # strategy explicitly so GDAL does not swap them when it transforms them.
    source = osr.SpatialReference()
    target = osr.SpatialReference()
    source.ImportFromEPSG(4326)
    target.ImportFromEPSG(26917)  # NAD83 / UTM zone 17N, units: metres
    source.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    target.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    return source, target, osr.CoordinateTransformation(source, target)


def polygon(feature, source_srs, transform):
    raw_geometry = feature.get("geometry")
    if not raw_geometry or raw_geometry.get("type") not in ("Polygon", "MultiPolygon"):
        raise ValueError("Expected nonempty Polygon or MultiPolygon geometry")
    geometry = ogr.CreateGeometryFromJson(json.dumps(raw_geometry))
    if geometry is None or geometry.IsEmpty() or not geometry.IsValid():
        raise ValueError("The source polygon is empty or invalid")
    west, east, south, north = geometry.GetEnvelope()
    if not (-81 < west < east < -79 and 36 < south < north < 38):
        raise ValueError("Coordinates do not look like Roanoke lon/lat")

    # Transform a separate OGR polygon object. Areas calculated on the raw
    # lon/lat geometry would be in square degrees, not square metres.
    geometry.AssignSpatialReference(source_srs)
    projected = geometry.Clone()
    if projected.Transform(transform) != 0:
        raise ValueError("Could not project the polygon to EPSG:26917")
    return geometry, projected


def first_vertex(feature):
    # Polygon coordinates are [ring][point][lon, lat]. MultiPolygon adds one
    # more list level. Peel list levels until we reach the first point.
    coordinates = feature["geometry"]["coordinates"]
    while isinstance(coordinates[0], list):
        coordinates = coordinates[0]
    return coordinates[:2]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=SAVED_RAW, help="saved raw GeoJSON")
    parser.add_argument("--live", action="store_true", help="query the current City GIS layer")
    parser.add_argument("--save-raw", type=Path, help="new file for the live response")
    args = parser.parse_args()

    raw, origin = source_bytes(args)
    features, by_id = selected_features(raw)
    if args.save_raw:
        # Save only a validated response. Do not replace a previous raw file.
        with args.save_raw.open("xb") as output:
            output.write(raw)
    source_srs, target_srs, transform = spatial_references()
    print(f"Source: {origin}")
    print(f"Raw SHA-256: {hashlib.sha256(raw).hexdigest()}")
    print(f"Feature count: {len(features)}")
    print(
        f"Coordinates: EPSG:{source_srs.GetAuthorityCode(None)} (degrees) "
        f"to EPSG:{target_srs.GetAuthorityCode(None)} (metres)"
    )

    projected = {}
    for oid, school in EXPECTED_SCHOOLS.items():
        raw_polygon, metric_polygon = polygon(by_id[oid], source_srs, transform)
        projected[oid] = metric_polygon
        print(
            f"{school}: OBJECTID {oid}, raw {raw_polygon.GetGeometryName()}, "
            f"projected area {metric_polygon.GetArea():,.3f} m²"
        )
        print(f"  First raw vertex (longitude, latitude): {first_vertex(by_id[oid])}")

    # OGR returns the geometric set intersection: all positions present in
    # both polygons. GetArea sums the area of its polygonal components.
    intersection = projected[13].Intersection(projected[16])
    if intersection is None:
        raise ValueError("OGR could not calculate the polygon intersection")
    square_metres = intersection.GetArea()
    print(f"Intersection type: {intersection.GetGeometryName()}")
    print(f"Overlap area: {square_metres:,.3f} m²")
    print(f"Overlap rounded to nearest m²: {round(square_metres):,} m²")
    print(f"Overlap as share of Preston Park: {100 * square_metres / projected[13].GetArea():.3f}%")
    print(f"Overlap as share of Round Hill: {100 * square_metres / projected[16].GetArea():.3f}%")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Build checked attendance zones for four divisions next to Roanoke County.

The source layers are public GIS services. Salem has only elementary GIS zones;
its middle and high polygons remain from the 2015-16 NCES SABS survey. Bedford
Primary also retains its SABS polygon because the county GIS layer does not
publish a primary-school boundary. The build replaces no file on a failed check.
"""

import csv
import html
import json
import re
import subprocess
import tempfile
import urllib.parse
import urllib.request
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ZONES = ROOT / "app" / "data" / "zones"
SCHOOLS = ROOT / "app" / "data" / "schools.csv"
COLORS = {"Elementary": "#4C78A8", "Middle": "#F58518", "High": "#E45756"}

# Division ID -> (level, URL, source school-name field, expected polygons).
SOURCES = {
    "5103460": [("Elementary", "https://arcgispub.salemva.gov/server/rest/services/SCHOOL_SYSTEM_LAYERS_WMI/MapServer/136", "DISTRICT", 4)],
    "5102520": [(None, "https://maps.montva.com/server/rest/services/Schools/Schools_Zones/MapServer/0", None, 11)],
    "5100360": [
        ("Elementary", "https://webgis.bedfordcountyva.gov/arcgis/rest/services/OpenData/OpenDataSchools/MapServer/1", "SCHOOL", 12),
        ("Middle", "https://webgis.bedfordcountyva.gov/arcgis/rest/services/OpenData/OpenDataSchools/MapServer/2", "MIDDLE_SCHOOL", 3),
        ("High", "https://webgis.bedfordcountyva.gov/arcgis/rest/services/OpenData/OpenDataSchools/MapServer/3", "HIGH_SCHOOL", 3),
    ],
    "5100420": [
        ("Elementary", "https://services6.arcgis.com/6h5xc2eGkGuRiFia/arcgis/rest/services/Elementary_School_Districts/FeatureServer/0", "SCHOOLNM", 7),
        ("Middle", "https://services6.arcgis.com/6h5xc2eGkGuRiFia/arcgis/rest/services/Middle_School_Districts/FeatureServer/0", "SCHOOLNAME", 2),
        ("High", "https://services6.arcgis.com/6h5xc2eGkGuRiFia/arcgis/rest/services/High_School_Districts/FeatureServer/0", "SCHOOLNAME", 2),
    ],
}
EXPECTED_FEATURES = {"5103460": 6, "5102520": 31, "5100360": 19, "5100420": 11}
EXPECTED_MATCHES = {"5103460": 6, "5102520": 19, "5100360": 19, "5100420": 11}


def key(name):
    name = re.sub(r"[^a-z0-9]+", " ", name.lower())
    name = re.sub(r"\b(elementary|middle|high|school|ms|hs)\b", " ", name)
    return " ".join(name.split())


def coords(geometry):
    polygons = [geometry["coordinates"]] if geometry["type"] == "Polygon" else geometry["coordinates"]
    for polygon in polygons:
        for ring in polygon:
            yield from ring


def fetch(url, expected):
    query = urllib.parse.urlencode({
        "where": "1=1", "outFields": "*", "outSR": "4326",
        "geometryPrecision": "7", "f": "geojson",
    })
    request = urllib.request.Request(url + "/query?" + query, headers={"User-Agent": "va-schools-dashboard/zone-refresh"})
    with urllib.request.urlopen(request, timeout=60) as response:
        data = json.load(response)
    features = data.get("features")
    if data.get("type") != "FeatureCollection" or not isinstance(features, list) or len(features) != expected:
        raise ValueError(f"Unexpected GIS feature count for {url}: {len(features) if isinstance(features, list) else 'invalid'}; expected {expected}")
    for feature in features:
        geometry = feature.get("geometry") or {}
        if geometry.get("type") not in ("Polygon", "MultiPolygon") or not geometry.get("coordinates"):
            raise ValueError(f"Missing polygon geometry from {url}")
        if not all(len(point) >= 2 and -84 < point[0] < -74 and 36 < point[1] < 40 for point in coords(geometry)):
            raise ValueError(f"GIS geometry is outside Virginia or is not WGS84: {url}")
    return features


def school_index(division_id):
    with SCHOOLS.open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == division_id]
    by_name = {(row["school_level"], key(row["school_name"])): row for row in schools}
    if len(by_name) != len(schools):
        raise ValueError(f"Duplicate school names in division {division_id}")
    return schools, by_name


def matched_school(division_id, level, source_name, by_name):
    alias = {
        ("5100360", "Elementary", "new london academy"): "new london academy",
        ("5100420", "Middle", "central academy"): "central academy",
        ("5100420", "Middle", "read mountain"): "read mountain",
        ("5100420", "High", "lord botetourt"): "lord botetourt",
        ("5100420", "High", "james river"): "james river",
    }
    normalized = alias.get((division_id, level, key(source_name)), key(source_name))
    school = by_name.get((level, normalized))
    if school is None:
        raise ValueError(f"Unmatched {division_id} {level} GIS label: {source_name!r}")
    return school


def make_feature(division_id, level, raw, school, source_name, source, retrieved, suffix=""):
    props = raw["properties"]
    oid = props.get("OBJECTID")
    if oid is None:
        raise ValueError(f"GIS omitted OBJECTID: {division_id} {source_name}")
    zone_id = f"{division_id}_{level.lower()}_{oid}{suffix}"
    color = COLORS[level]
    source_label = {
        "5100360": "Bedford County GIS",
        "5100420": "Botetourt County GIS",
        "5102520": "Montgomery County GIS",
        "5103460": "City of Salem GIS",
    }[division_id]
    return {
        "type": "Feature", "id": zone_id,
        "properties": {
            "zone_id": zone_id, "division_id": division_id,
            "school_name": school["school_name"], "official_school_name": source_name,
            "level": level, "app_school_id": school["school_id"],
            "source": source_label, "source_url": source,
            "popup": (f"<strong>Attendance zone</strong><br/>School: {html.escape(school['school_name'])}"
                      f"<br/>School level: {level}<br/>Source: {source_label} (retrieved {retrieved})"
                      "<br/><span class='muted'>Confirm an address with the school division.</span>"),
            "style": {"weight": 1, "opacity": 0.9, "fillOpacity": 0.12, "color": color, "fillColor": color},
        },
        "geometry": raw["geometry"],
    }


def old_sabs_feature(division_id, school_name, level, school):
    path = ZONES / "sabs" / f"{division_id}.json"
    data = json.loads(path.read_text(encoding="utf-8"))
    matches = [f for f in data["features"] if key(f["properties"].get("school_name", "")) == key(school_name)
               and f["properties"].get("level") == level]
    if len(matches) != 1:
        raise ValueError(f"Expected one historical SABS zone for {division_id} {school_name}: {len(matches)}")
    feature = matches[0]
    props = feature["properties"]
    if props.get("app_school_id") != school["school_id"]:
        raise ValueError(f"SABS school ID mismatch: {division_id} {school_name}")
    props["popup"] = (f"<strong>Attendance zone</strong><br/>School: {html.escape(school['school_name'])}"
                      f"<br/>School level: {level}<br/>Source: NCES SABS 2015–16 survey"
                      "<br/><span class='muted'>This older boundary may not match current rules. Confirm an address with the division.</span>")
    return feature


def build_one(division_id, retrieved):
    schools, by_name = school_index(division_id)
    output = []
    for level, url, name_field, expected in SOURCES[division_id]:
        for raw in fetch(url, expected):
            props = raw["properties"]
            if division_id == "5102520":
                # One source polygon has the name Roanoke County in all three
                # fields. It is outside the Montgomery school division.
                if props.get("Elementary_School") == "Roanoke County":
                    if any(props.get(f) != "Roanoke County" for f in ("Middle_School", "High_School")):
                        raise ValueError("Montgomery exclusion fields changed")
                    continue
                assignments = [(l, props.get(f)) for l, f in (("Elementary", "Elementary_School"),
                                                               ("Middle", "Middle_School"), ("High", "High_School"))]
            else:
                assignments = [(level, props.get(name_field))]
            for assigned_level, source_name in assignments:
                if not isinstance(source_name, str) or not source_name.strip():
                    raise ValueError(f"Missing school label in {url}")
                school = matched_school(division_id, assigned_level, source_name, by_name)
                output.append(make_feature(division_id, assigned_level, raw, school, source_name,
                                           url, retrieved))
                if division_id == "5102520" and source_name == "Christiansburg Elementary":
                    # The division's own school-zone list places Christiansburg
                    # Primary in this same zone. This is a separate grade span.
                    primary = by_name[("Elementary", "christiansburg primary")]
                    output.append(make_feature(division_id, "Elementary", raw, primary,
                                               "Christiansburg Primary", url, retrieved, "_primary"))

    if division_id == "5103460":
        output.extend((old_sabs_feature(division_id, "ANDREW LEWIS MIDDLE", "Middle", by_name[("Middle", "andrew lewis")]),
                       old_sabs_feature(division_id, "SALEM HIGH", "High", by_name[("High", "salem")])))
    elif division_id == "5100360":
        output.append(old_sabs_feature(division_id, "BEDFORD PRIMARY", "Elementary",
                                       by_name[("Elementary", "bedford primary")]))

    ids = [f["id"] for f in output]
    matched = {f["properties"].get("app_school_id") for f in output}
    expected_ids = {s["school_id"] for s in schools}
    if division_id == "5103460":
        expected_ids -= {by_name[("Combined", "virginia virtual academy at salem")]["school_id"]}
    if (len(output) != EXPECTED_FEATURES[division_id] or len(matched) != EXPECTED_MATCHES[division_id]
            or matched != expected_ids or len(set(ids)) != len(ids)):
        raise ValueError(f"Validation failed for {division_id}: features={len(output)}, matched={len(matched)}, expected={len(expected_ids)}")

    destination = ZONES / "division" / f"{division_id}.json"
    # ArcGIS exports can contain self-touching rings. GDAL repairs the polygon
    # geometry while preserving the checked feature IDs and school labels.
    with tempfile.TemporaryDirectory(dir=destination.parent) as workdir:
        raw_path = Path(workdir) / "raw.geojson"
        fixed_path = Path(workdir) / "valid.geojson"
        raw_path.write_text(json.dumps({"type": "FeatureCollection", "features": output}, ensure_ascii=False), encoding="utf-8")
        subprocess.run(["ogr2ogr", "-f", "GeoJSON", "-makevalid", str(fixed_path), str(raw_path)], check=True)
        cleaned = json.loads(fixed_path.read_text(encoding="utf-8"))
        fixed_features = cleaned.get("features", [])
        if (len(fixed_features) != len(output) or
                {f.get("properties", {}).get("zone_id") for f in fixed_features} != set(ids) or
                {f.get("properties", {}).get("app_school_id") for f in fixed_features} != matched):
            raise ValueError(f"GDAL geometry repair changed {division_id} feature or school IDs")
        temporary = destination.with_suffix(".json.tmp")
        temporary.write_text(json.dumps(cleaned, ensure_ascii=False, separators=(",", ":")) + "\n", encoding="utf-8")
        temporary.replace(destination)
    print(f"Wrote {destination}: {len(output)} features; {len(matched)}/{len(schools)} schools; "
          f"levels={dict(Counter(f['properties']['level'] for f in output))}")


def main():
    retrieved = datetime.now(timezone.utc).date().isoformat()
    for division_id in SOURCES:
        build_one(division_id, retrieved)


if __name__ == "__main__":
    main()

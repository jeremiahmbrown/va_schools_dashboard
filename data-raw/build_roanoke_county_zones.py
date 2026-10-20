#!/usr/bin/env python3
"""Build Roanoke County attendance zones from its public GIS MapServer.

One county polygon has elementary, middle, and high school assignments. Emit
one feature per level so the dashboard can select each overlapping zone.
"""

import csv
import html
import json
import re
import urllib.parse
import urllib.request
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5103330"
SOURCE = "https://arcgis.roanokecountyva.gov/arcgisweb/rest/services/Published_Geodata/Schools/MapServer/4"
OUTPUT = ROOT / "app" / "data" / "zones" / "division" / f"{DIVISION_ID}.json"
SCHOOL_CSV = ROOT / "app" / "data" / "schools.csv"
FIELDS = {"Elementary": "Elm_School_Name", "Middle": "Mid_School_Name", "High": "High_School_Name"}
COLORS = {"Elementary": "#4C78A8", "Middle": "#F58518", "High": "#E45756"}


def key(name):
    name = re.sub(r"[^a-z0-9]+", " ", name.lower())
    name = re.sub(r"\b(elementary|middle|high|school)\b", " ", name)
    return " ".join(name.split())


def get_source():
    params = urllib.parse.urlencode({
        "where": "1=1",
        "outFields": "OBJECTID,Elm_School_Name,Mid_School_Name,High_School_Name",
        "outSR": "4326",
        "geometryPrecision": "5",
        "f": "geojson",
    })
    request = urllib.request.Request(f"{SOURCE}/query?{params}", headers={"User-Agent": "va-schools-dashboard/zone-refresh"})
    with urllib.request.urlopen(request, timeout=40) as response:
        data = json.load(response)
    if data.get("type") != "FeatureCollection" or len(data.get("features", [])) < 20:
        raise ValueError("Roanoke County GIS returned too few polygon features")
    return data["features"]


def main():
    with SCHOOL_CSV.open(newline="") as handle:
        schools = [s for s in csv.DictReader(handle) if s["division_id"] == DIVISION_ID]
    by_name = {(s["school_level"], key(s["school_name"])): s for s in schools}
    if len(by_name) != len(schools):
        raise ValueError("Duplicate normalized dashboard school names")
    aliases = {"herman horn": "herman l horn", "masons cove": "mason s cove"}
    retrieved_date = datetime.now(timezone.utc).date().isoformat()
    output = []
    unmatched = set()
    for feature in get_source():
        props = feature.get("properties") or {}
        geometry = feature.get("geometry") or {}
        if geometry.get("type") not in ("Polygon", "MultiPolygon") or not geometry.get("coordinates"):
            raise ValueError("County returned an empty or unsupported geometry")
        oid = props.get("OBJECTID")
        for level, field in FIELDS.items():
            county_name = (props.get(field) or "").strip()
            if not county_name:
                continue  # The county file includes one unassigned area.
            name_key = aliases.get(key(county_name), key(county_name))
            school = by_name.get((level, name_key))
            if school is None:
                unmatched.add((level, county_name))
                continue
            school_name = school["school_name"].strip()
            color = COLORS[level]
            output.append({
                "type": "Feature",
                "id": f"{DIVISION_ID}_{oid}_{level.lower()}",
                "properties": {
                    "zone_id": f"{DIVISION_ID}_{oid}_{level.lower()}",
                    "division_id": DIVISION_ID,
                    "school_name": school_name,
                    "level": level,
                    "app_school_id": school["school_id"],
                    "source": "roanoke_county_gis",
                    "popup": f"<strong>Attendance zone</strong><br/>School: {html.escape(school_name)}<br/>School level: {level}<br/>Source: Roanoke County GIS (retrieved {retrieved_date})<br/><span class='muted'>Confirm an address with Roanoke County Public Schools.</span>",
                    "style": {"weight": 1, "opacity": 0.9, "fillOpacity": 0.12, "color": color, "fillColor": color},
                },
                "geometry": geometry,
            })
    matched = {f["properties"]["app_school_id"] for f in output}
    missing = {s["school_id"] for s in schools} - matched
    if unmatched or missing or len(output) < 50:
        raise ValueError(f"County zone validation failed: unmatched labels={sorted(unmatched)}; unmatched schools={sorted(missing)}; features={len(output)}")
    counts = Counter(f["properties"]["level"] for f in output)
    result = {"type": "FeatureCollection", "features": output}
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    temporary = OUTPUT.with_suffix(".json.tmp")
    temporary.write_text(json.dumps(result, ensure_ascii=False, separators=(",", ":")) + "\n")
    temporary.replace(OUTPUT)
    print(f"Wrote {OUTPUT}: {len(output)} level features; {len(matched)}/{len(schools)} school matches; {dict(counts)}")


if __name__ == "__main__":
    main()

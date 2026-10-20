#!/usr/bin/env python3
"""Build Roanoke City 2026-27 attendance zones from its public locator layers.

The City's public locator uses a 2026 Phase 1 elementary replacement layer
alongside its middle and high layers. Query each level separately so the
dashboard can display overlapping attendance areas without losing a level.
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
DIVISION_ID = "5103300"
BASE = "https://gis03.roanokeva.gov/arcgis/rest/services/Public"
SOURCES = {
    "Elementary": "https://services.arcgis.com/KII0S5XRkpE66Ngk/arcgis/rest/services/Elementary_Schools_Phase_1/FeatureServer/0",
    "Middle": f"{BASE}/Schools_Middle/FeatureServer/1",
    "High": f"{BASE}/Schools_High/FeatureServer/1",
}
EXPECTED = {"Elementary": 17, "Middle": 5, "High": 2}
COLORS = {"Elementary": "#4C78A8", "Middle": "#F58518", "High": "#E45756"}
OUTPUT = ROOT / "app" / "data" / "zones" / "division" / f"{DIVISION_ID}.json"
SCHOOL_CSV = ROOT / "app" / "data" / "schools.csv"
ALIASES = {
    ("Elementary", "roanoke academy"): "roanoke academy for mathematics science",
    ("Middle", "lucy addison"): "addison aerospace magnet",
    ("Middle", "james breckinridge"): "breckinridge",
}


def key(name):
    name = re.sub(r"[^a-z0-9]+", " ", name.lower())
    name = re.sub(r"\b(elementary|middle|high|school)\b", " ", name)
    return " ".join(name.split())


def get_layer(level, url):
    params = urllib.parse.urlencode({
        "where": "1=1",
        "outFields": "OBJECTID,SCHOOL",
        "outSR": "4326",
        "f": "geojson",
    })
    request = urllib.request.Request(
        f"{url}/query?{params}",
        headers={"User-Agent": "va-schools-dashboard/zone-refresh"},
    )
    with urllib.request.urlopen(request, timeout=40) as response:
        data = json.load(response)
    features = data.get("features", [])
    if data.get("type") != "FeatureCollection" or len(features) != EXPECTED[level]:
        raise ValueError(f"City {level} GIS returned {len(features)} features; expected {EXPECTED[level]}")
    return features


def coords(geometry):
    if geometry["type"] == "Polygon":
        for ring in geometry["coordinates"]:
            yield from ring
    else:
        for polygon in geometry["coordinates"]:
            for ring in polygon:
                yield from ring


def main():
    with SCHOOL_CSV.open(newline="") as handle:
        schools = [s for s in csv.DictReader(handle) if s["division_id"] == DIVISION_ID]
    by_name = {(s["school_level"], key(s["school_name"])): s for s in schools}
    if len(schools) != 24 or len(by_name) != len(schools):
        raise ValueError("City dashboard school list changed; review the name crosswalk")

    retrieved = datetime.now(timezone.utc).date().isoformat()
    output = []
    seen_ids = set()
    for level, url in SOURCES.items():
        for feature in get_layer(level, url):
            props = feature.get("properties") or {}
            geometry = feature.get("geometry") or {}
            if geometry.get("type") not in ("Polygon", "MultiPolygon") or not geometry.get("coordinates"):
                raise ValueError(f"City {level} GIS returned an empty polygon")
            if not all(-81 < pt[0] < -79 and 36.5 < pt[1] < 38 for pt in coords(geometry)):
                raise ValueError(f"City {level} geometry is outside southwest Virginia or is not WGS84")
            official_name = (props.get("SCHOOL") or "").strip()
            normalized = ALIASES.get((level, key(official_name)), key(official_name))
            school = by_name.get((level, normalized))
            if school is None:
                raise ValueError(f"Unmatched official City {level} label: {official_name!r}")
            sid = school["school_id"]
            if sid in seen_ids:
                raise ValueError(f"City GIS returned duplicate school assignment: {sid}")
            seen_ids.add(sid)
            oid = props.get("OBJECTID")
            if oid is None:
                raise ValueError(f"City {level} GIS omitted OBJECTID")
            zone_id = f"{DIVISION_ID}_{level.lower()}_{oid}"
            color = COLORS[level]
            output.append({
                "type": "Feature",
                "id": zone_id,
                "properties": {
                    "zone_id": zone_id,
                    "division_id": DIVISION_ID,
                    "school_name": school["school_name"].strip(),
                    "official_school_name": official_name,
                    "level": level,
                    "app_school_id": sid,
                    "source": "roanoke_city_gis",
                    "popup": f"<strong>Attendance zone</strong><br/>School: {html.escape(official_name)}<br/>School level: {level}<br/>Source: City of Roanoke GIS (retrieved {retrieved})<br/><span class='muted'>Confirm an address with Roanoke City Public Schools.</span>",
                    "style": {"weight": 1, "opacity": 0.9, "fillOpacity": 0.12, "color": color, "fillColor": color},
                },
                "geometry": geometry,
            })
    missing = {s["school_id"] for s in schools} - seen_ids
    if missing or len(output) != sum(EXPECTED.values()):
        raise ValueError(f"City zone validation failed: unmatched schools={sorted(missing)}; features={len(output)}")

    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    # The City middle layer contains one self-touching ring. GDAL fixes it
    # without changing school assignments; a failure preserves prior output.
    with tempfile.TemporaryDirectory(dir=OUTPUT.parent) as workdir:
        raw = Path(workdir) / "raw.geojson"
        fixed = Path(workdir) / "valid.geojson"
        raw.write_text(json.dumps({"type": "FeatureCollection", "features": output}, ensure_ascii=False))
        subprocess.run(["ogr2ogr", "-f", "GeoJSON", "-makevalid", str(fixed), str(raw)], check=True)
        cleaned = json.loads(fixed.read_text())
        cleaned_features = cleaned.get("features", [])
        cleaned_ids = {f.get("properties", {}).get("app_school_id") for f in cleaned_features}
        if len(cleaned_features) != len(output) or cleaned_ids != seen_ids:
            raise ValueError("GDAL geometry repair changed the City school assignments")
        temporary = OUTPUT.with_suffix(".json.tmp")
        temporary.write_text(json.dumps(cleaned, ensure_ascii=False, separators=(",", ":")) + "\n")
        temporary.replace(OUTPUT)
    print(f"Wrote {OUTPUT}: {len(output)} zones; {len(seen_ids)}/{len(schools)} schools matched; {dict(Counter(f['properties']['level'] for f in output))}")


if __name__ == "__main__":
    main()

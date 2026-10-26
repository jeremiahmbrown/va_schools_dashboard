#!/usr/bin/env python3
"""Build Arlington neighborhood zones from the staged APS locator layers.

APS links this locator for current assignments. Its GIS School_Year attribute
still says 2022-2023, so the app note states both facts. Run the stage script
first. This builder accepts only the checked download hashes in its manifest.
"""

import argparse
import csv
import hashlib
import html
import json
import subprocess
import tempfile
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION = "5100270"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION}.json"
DESTINATION = ROOT / "app" / "data" / "zones" / "division" / f"{DIVISION}.json"
CROSSWALK = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION}.csv"
COLORS = {"Elementary": "#4C78A8", "Middle": "#F58518", "High": "#E45756"}
NAME_FIELDS = {"Elementary": "ES_Name", "Middle": "MS_Name", "High": "HS_Name"}
EXPECTED = {"Elementary": 20, "Middle": 6, "High": 3}
def read_crosswalk(schools, crosswalk_path):
    school_index = {row["school_id"]: row for row in schools}
    with crosswalk_path.open(newline="", encoding="utf-8-sig") as handle:
        rows = list(csv.DictReader(handle))
    if len(rows) != len(schools) or {row["school_id"] for row in rows} != set(school_index):
        raise ValueError("Arlington crosswalk must give every school one disposition")
    if len({row["school_id"] for row in rows}) != len(rows):
        raise ValueError("Duplicate school ID in Arlington crosswalk")
    mapped = {}
    excluded = set()
    for row in rows:
        school = school_index[row["school_id"]]
        if row["division_id"] != DIVISION or row["school_name"] != school["school_name"]:
            raise ValueError(f"Crosswalk school record changed: {row['school_id']}")
        if row["disposition"] == "no_neighborhood_zone":
            if row["source_name"] or not row["note"]:
                raise ValueError(f"Excluded school needs an empty source name and reason: {row['school_id']}")
            excluded.add(row["school_id"])
        elif row["disposition"] == "mapped":
            pair = (row["level"], row["source_name"])
            if row["level"] not in EXPECTED or not row["source_name"] or pair in mapped:
                raise ValueError(f"Duplicate or invalid source label: {pair}")
            if school["school_level"] != row["level"] and not row["note"]:
                raise ValueError(f"School-level exception needs a reason: {row['school_id']}")
            mapped[pair] = school
        else:
            raise ValueError(f"Unknown crosswalk disposition: {row['disposition']}")
    if len(mapped) != 29 or len(excluded) != 6:
        raise ValueError("Arlington crosswalk counts changed")
    return mapped, excluded


def main(manifest_path=MANIFEST, crosswalk_path=CROSSWALK):
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if manifest.get("division_id") != DIVISION or len(manifest.get("layers", [])) != 3:
        raise ValueError("Arlington source manifest is incomplete")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == DIVISION]
    if len(schools) != 35:
        raise ValueError("Arlington school snapshot changed")
    by_source, excluded = read_crosswalk(schools, crosswalk_path)
    output = []
    for layer in manifest["layers"]:
        level = layer["level"]
        if (level not in EXPECTED or layer["feature_count"] != EXPECTED[level]
                or layer["name_field"] != NAME_FIELDS[level]
                or layer["school_year_values"] != ["2022-2023"]):
            raise ValueError(f"Unexpected Arlington source layer: {layer}")
        raw = (ROOT / layer["file"]).read_bytes()
        if hashlib.sha256(raw).hexdigest() != layer["sha256"]:
            raise ValueError(f"Arlington raw file changed: {layer['file']}")
        source = json.loads(raw)
        features = source.get("features")
        if source.get("type") != "FeatureCollection" or len(features) != EXPECTED[level]:
            raise ValueError(f"Incomplete Arlington {level} source")
        for feature in features:
            props = feature["properties"]
            source_name = props[NAME_FIELDS[level]]
            school = by_source.get((level, source_name))
            if school is None:
                raise ValueError(f"Unmatched Arlington {level} area: {source_name}")
            oid = props["OBJECTID"]
            zone_id = f"{DIVISION}_{level.lower()}_{oid}"
            note = ("APS links this locator for current assignments. The GIS record still says "
                    "School_Year 2022-2023. Confirm an address with APS.")
            output.append({
                "type": "Feature", "id": zone_id,
                "properties": {
                    "zone_id": zone_id, "division_id": DIVISION,
                    "school_name": school["school_name"], "official_school_name": source_name,
                    "level": level, "app_school_id": school["school_id"],
                    "source": "APS locator / Arlington County GIS", "source_url": layer["url"],
                    "source_school_year_field": "2022-2023",
                    "popup": (f"<strong>Attendance zone</strong><br/>School: {html.escape(school['school_name'])}"
                              f"<br/>School level: {level}<br/>Source: APS locator / Arlington County GIS"
                              f"<br/><span class='muted'>{html.escape(note)}</span>"),
                    "style": {"weight": 1, "opacity": 0.9, "fillOpacity": 0.12,
                              "color": COLORS[level], "fillColor": COLORS[level]},
                },
                "geometry": feature["geometry"],
            })
    ids = [feature["id"] for feature in output]
    matched = [feature["properties"]["app_school_id"] for feature in output]
    expected_ids = {row["school_id"] for row in schools} - excluded
    if (len(output) != 29 or len(set(ids)) != 29 or len(set(matched)) != 29
            or set(matched) != expected_ids
            or Counter(f["properties"]["level"] for f in output) != EXPECTED):
        raise ValueError("Arlington zone count, school link, or level check failed")
    with tempfile.TemporaryDirectory(dir=DESTINATION.parent) as workdir:
        raw_path = Path(workdir) / "raw.geojson"
        fixed_path = Path(workdir) / "valid.geojson"
        raw_path.write_text(json.dumps({"type": "FeatureCollection", "features": output}), encoding="utf-8")
        subprocess.run(["ogr2ogr", "-f", "GeoJSON", "-makevalid", str(fixed_path), str(raw_path)], check=True)
        cleaned = json.loads(fixed_path.read_text(encoding="utf-8"))
        fixed = cleaned.get("features", [])
        if (len(fixed) != len(output) or {f["properties"]["zone_id"] for f in fixed} != set(ids)
                or {f["properties"]["app_school_id"] for f in fixed} != expected_ids):
            raise ValueError("Geometry repair changed Arlington school links")
        temporary = DESTINATION.with_suffix(".json.tmp")
        temporary.write_text(json.dumps(cleaned, separators=(",", ":")) + "\n", encoding="utf-8")
        temporary.replace(DESTINATION)
    print(f"Built Arlington: {len(output)} zones, {len(set(matched))}/35 schools; levels {dict(Counter(f['properties']['level'] for f in output))}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    parser.add_argument("--crosswalk", type=Path, default=CROSSWALK)
    args = parser.parse_args()
    main(args.manifest, args.crosswalk)

#!/usr/bin/env python3
"""Link checked neighboring 2015-16 SABS polygons to current school IDs.

This changes only app_school_id. It does not update or reclassify the old
boundaries. Run after build_zones_sabs.R if that statewide file is rebuilt.
"""

import csv
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ZONES = ROOT / "app" / "data" / "zones" / "sabs"
SCHOOLS = ROOT / "app" / "data" / "schools.csv"

# Exact SABS school labels to check. Franklin's former Burnt Chimney and Henry
# schools and its two old middle campuses are deliberately excluded.
EXPECTED = {
    "5101440": {
        "BOONES MILL ELEM": "Boones Mill Elementary",
        "CALLAWAY ELEM": "Callaway Elementary",
        "DUDLEY ELEM": "Dudley Elementary",
        "FERRUM ELEM": "Ferrum Elementary",
        "GLADE HILL ELEM": "Glade Hill Elementary",
        "LEE M. WAID ELEM": "Lee M. Waid Elementary",
        "ROCKY MOUNT ELEM": "Rocky Mount Elementary",
        "SNOW CREEK ELEM": "Snow Creek Elementary",
        "SONTAG ELEM": "Sontag Elementary",
        "WINDY GAP ELEM": "Windy Gap Elementary",
    },
    "5101020": {"MCCLEARY ELEM": "McCleary Elementary"},
    "5101350": {
        "CHECK ELEM": "Check Elementary",
        "FLOYD ELEM": "Floyd Elementary",
        "INDIAN VALLEY ELEM": "Indian Valley Elementary",
        "WILLIS ELEM": "Willis Elementary",
    },
}


def key(name):
    name = re.sub(r"[^a-z0-9]+", " ", name.lower())
    name = re.sub(r"\b(elem|elementary|middle|high|school)\b", " ", name)
    return " ".join(name.split())


def main():
    with SCHOOLS.open(newline="", encoding="utf-8-sig") as handle:
        schools = list(csv.DictReader(handle))
    by_id = {s["school_id"]: s for s in schools}
    for division_id, labels in EXPECTED.items():
        path = ZONES / f"{division_id}.json"
        data = json.loads(path.read_text(encoding="utf-8"))
        features = data.get("features", [])
        if data.get("type") != "FeatureCollection" or not features:
            raise ValueError(f"Invalid SABS file: {path}")
        found = set()
        for feature in features:
            props = feature.get("properties") or {}
            label = props.get("school_name")
            if label not in labels:
                continue
            sid = props.get("nces_school_id")
            school = by_id.get(sid)
            if (school is None or school["division_id"] != division_id
                    or school["school_name"] != labels[label]
                    or key(label) != key(school["school_name"])
                    or props.get("source") != "sabs"
                    or props.get("app_school_id") not in (None, sid)):
                raise ValueError(f"SABS school identity mismatch: {division_id} {label}")
            if label in found:
                raise ValueError(f"Duplicate SABS school label: {division_id} {label}")
            found.add(label)
            props["app_school_id"] = sid
        if found != set(labels):
            raise ValueError(f"Missing expected SABS labels in {division_id}: {set(labels) - found}")
        temporary = path.with_suffix(".json.tmp")
        temporary.write_text(json.dumps(data, ensure_ascii=False, separators=(",", ":")) + "\n", encoding="utf-8")
        temporary.replace(path)
        print(f"Linked {len(found)} historical SABS polygons in {division_id}")


if __name__ == "__main__":
    main()

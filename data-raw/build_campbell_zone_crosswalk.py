#!/usr/bin/env python3
"""Crosswalk County school jurisdiction names to frozen Campbell school IDs."""
import csv
import json
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5100600"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]
MAPPING = {
    "Elementary": {
        "Altavista": "510060000219", "Brookneal": "510060002834",
        "Concord": "510060000226", "Leesville Road": "510060000230",
        "Rustburg": "510060002528", "Tomahawk": "510060000232",
        "Yellow Branch": "510060000234",
    },
    "Middle": {
        "Altavista": "510060000220", "Brookville": "510060000224",
        "Rustburg": "510060002771", "William Campbell": "510060000233",
    },
    "High": {
        "Altavista": "510060000220", "Brookville": "510060000223",
        "Rustburg": "510060000231", "William Campbell": "510060000233",
    },
}
EXPECTED_COUNTS = {
    "Elementary": {"Altavista": 1, "Brookneal": 2, "Concord": 1, "Leesville Road": 1,
                   "Rustburg": 1, "Tomahawk": 1, "Yellow Branch": 2},
    "Middle": {"Altavista": 1, "Brookville": 3, "Rustburg": 3, "William Campbell": 2},
    "High": {"Altavista": 1, "Brookville": 3, "Rustburg": 3, "William Campbell": 2},
}

def build():
    manifest = json.loads((ROOT / "data-raw/zone_source_manifests/5100600.json").read_text())
    if manifest["division_id"] != DIVISION_ID or {
        x["level"]: x["feature_count"] for x in manifest["layers"]
    } != {"Elementary": 9, "Middle": 9, "High": 9}:
        raise ValueError("Campbell source manifest changed")
    with (ROOT / "app/data/schools.csv").open(newline="", encoding="utf-8-sig") as f:
        schools = [r for r in csv.DictReader(f) if r["division_id"] == DIVISION_ID]
    by_id = {r["school_id"]: r for r in schools}
    expected_ids = {sid for names in MAPPING.values() for sid in names.values()}
    if len(schools) != 13 or len(by_id) != 13 or expected_ids != set(by_id):
        raise ValueError("Campbell frozen school roster changed")
    observed = {}
    for layer in manifest["layers"]:
        fs = json.loads((ROOT / layer["file"]).read_text(encoding="utf-8"))["features"]
        observed[layer["level"]] = dict(Counter(f["properties"][layer["name_field"]] for f in fs))
    if observed != EXPECTED_COUNTS:
        raise ValueError("Campbell source school labels or piece counts changed")
    rows = []
    for level, names in MAPPING.items():
        for name, sid in sorted(names.items()):
            school = by_id[sid]
            note = "County jurisdiction GIS; effective school year not stated."
            if level != school["school_level"]:
                note += f" Source {level} area for snapshot {school['school_level']} school."
            if level in ("Middle", "High") and name in ("Altavista", "William Campbell"):
                note += " Combined school serves both secondary levels."
            if observed[level][name] > 1:
                note += f" {observed[level][name]} source pieces share this school."
            rows.append(dict(division_id=DIVISION_ID, level=level, source_name=name,
                             school_id=sid, school_name=school["school_name"],
                             disposition="mapped", note=note))
    out = ROOT / "data-raw/zone_school_crosswalks/5100600.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, FIELDS, lineterminator="\n")
        w.writeheader()
        w.writerows(rows)
    print(f"Wrote {len(rows)} Campbell level links to {len(expected_ids)} schools")

if __name__ == "__main__":
    build()

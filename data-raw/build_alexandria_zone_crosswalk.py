#!/usr/bin/env python3
"""Link ACPS 2026–27 locator labels to the frozen school snapshot."""

import csv
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5100120"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION_ID}.json"
OUTPUT = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION_ID}.csv"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]
ELEMENTARY = {
    "Mount Vernon": "Mount Vernon Elementary",
    "Patrick Henry": "Patrick Henry K-8 School",
    "Naomi L Brooks": "Naomi L. Brooks Elementary",
    "Lyles-Crouch": "Lyles-Crouch Elementary",
    "Jefferson-Houston": "Jefferson-Houston PK-8 School",
    "George Mason": "George Mason Elementary",
    "Douglas MacArthur": "Douglas MacArthur Elementary",
    "Charles Barrett": "Charles Barrett Elementary",
    "Cora Kelly": "Cora Kelly School for Math Science and Technology",
    "William Ramsay": "William Ramsay Elementary",
    "James K Polk": "James K. Polk Elementary",
    "John Adams": "John Adams Elementary",
    "Samuel W Tucker": "Samuel W. Tucker Elementary",
    "Ferdinand T. Day": "Ferdinand T. Day Elementary",
}
MIDDLE = {
    "Patrick Henry": "Patrick Henry K-8 School",
    "Jefferson-Houston": "Jefferson-Houston PK-8 School",
    "George Washington MS": "George Washington Middle",
    "Francis C. Hammond MS": "Francis C. Hammond Middle",
}
HIGH = {"Alexandria City HS": "Alexandria City High"}
MAPS = {"Elementary": ELEMENTARY, "Middle": MIDDLE, "High": HIGH}


def build():
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if manifest["division_id"] != DIVISION_ID or len(manifest["layers"]) != 3:
        raise ValueError("Alexandria source manifest changed")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == DIVISION_ID]
    by_name = {row["school_name"]: row for row in schools}
    if len(schools) != 17 or len(by_name) != 17 or set(by_name) != {
            name for mapping in MAPS.values() for name in mapping.values()}:
        raise ValueError("Alexandria frozen school list changed")
    rows = []
    linked = set()
    for layer in manifest["layers"]:
        level = layer["level"]
        features = json.loads((ROOT / layer["file"]).read_text(encoding="utf-8"))["features"]
        names = [feature["properties"][layer["name_field"]] for feature in features]
        expected = set(MAPS[level])
        if set(names) != expected or len(names) != layer["feature_count"]:
            raise ValueError(f"Alexandria {level} source labels changed")
        for source_name in sorted(expected):
            school_name = MAPS[level][source_name]
            school = by_name[school_name]
            if school["school_level"] != level and school["school_level"] != "Combined (K-8)":
                raise ValueError(f"Unreviewed Alexandria level exception: {school_name}")
            linked.add(school["school_id"])
            note = "ACPS-linked 2026–27 locator source-label match."
            if school["school_level"] == "Combined (K-8)":
                note += " The source gives this K–8 school separate elementary and middle boundaries."
            if level == "Middle" and source_name == "Jefferson-Houston":
                note += " Two source polygons have this same middle-school label."
            rows.append({"division_id": DIVISION_ID, "level": level,
                         "source_name": source_name, "school_id": school["school_id"],
                         "school_name": school_name, "disposition": "mapped", "note": note})
    if linked != {school["school_id"] for school in schools} or len(rows) != 19:
        raise ValueError("Alexandria school coverage changed")
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    print("Wrote 19 Alexandria crosswalk rows for 17 schools")


if __name__ == "__main__":
    build()

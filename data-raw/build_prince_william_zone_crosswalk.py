#!/usr/bin/env python3
"""Build the reviewed PWCS attendance-zone crosswalk for the frozen snapshot."""

import csv
import json
import os
import re
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5103130"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION_ID}.json"
DESTINATION = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION_ID}.csv"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]

# Source labels omit parts of the personal names held in the frozen snapshot.
# Each exception is an exact source-level/name to exact snapshot-name link.
ALIASES = {
    ("Elementary", "Alvey Elementary School"): "J.W. Alvey Elementary",
    ("Elementary", "Tyler Elementary School"): "George G. Tyler Elementary",
    ("Elementary", "King Elementary School"): "Martin Luther King Jr. Elementary",
    ("Elementary", "Gravely Elementary School"): "Samuel L. Gravely Jr. Elementary",
    ("Elementary", "Pattie Elementary School"): "John F. Pattie Sr. Elementary",
    ("Elementary", "Marshall Elementary School"): "Thurgood Marshall Elementary",
    ("Elementary", "Penn Elementary School"): "Sonnie Penn Elementary",
    ("Elementary", "Kilby Elementary School"): "R. Dean Kilby Elementary",
    ("Elementary", "Fitzgerald Elementary School"): "Fannie W. Fitzgerald Elementary",
    ("Elementary", "Henderson Elementary School"): "A. Henderson Elementary",
    ("Elementary", "Kyle Wilson Elementary School"): "Kyle R. Wilson Elementary",
    ("Elementary", "McAuliffe Elementary School"): "Sharon C. McAuliffe Elementary",
    ("Elementary", "Vaughan Elementary School"): "Elizabeth Vaughan Elementary",
    ("Elementary", "Ellis Elementary School"): "Suella G. Ellis Elementary",
    ("Elementary", "Mullen Elementary School"): "George P. Mullen Elementary",
    ("Elementary", "Sinclair Elementary School"): "C.A. Sinclair Elementary",
    ("Middle", "Saunders Middle School"): "Herbert J. Saunders Middle",
    ("Middle", "Benton Middle School"): "Louise A. Benton Middle",
    ("Middle", "Marsteller Middle School"): "E.H. Marsteller Middle",
    ("Middle", "Beville Middle School"): "Stuart M. Beville Middle",
    ("Middle", "Hampton Middle School"): "George M. Hampton Middle",
}
NO_SEPARATE_POLYGON = {
    "Mary G. Porter Traditional": "No separately named polygon in the PWCS standard attendance layers; confirm assignment with PWCS.",
    "Pennington Traditional": "No separately named polygon in the PWCS standard attendance layers; confirm assignment with PWCS.",
}


def normalized(name):
    stem = re.sub(r"\b(elementary|middle|high|school|schools|the)\b", "", name.lower())
    return re.sub(r"[^a-z0-9]", "", stem)


def level_matches(school, level):
    snapshot_level = school["school_level"]
    return (snapshot_level == level
            or snapshot_level == "Combined (K-8)" and level in ("Elementary", "Middle")
            or snapshot_level == "Combined" and level == "High")


def build():
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if manifest["division_id"] != DIVISION_ID:
        raise ValueError("Wrong PWCS manifest")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == DIVISION_ID]
    if len(schools) != 95:
        raise ValueError("PWCS school snapshot changed")
    rows = []
    linked = set()
    seen_aliases = set()
    for layer in manifest["layers"]:
        level = layer["level"]
        features = json.loads((ROOT / layer["file"]).read_text(encoding="utf-8"))["features"]
        if len(features) != layer["feature_count"]:
            raise ValueError(f"PWCS source count changed for {level}")
        for feature in features:
            source_name = feature["properties"][layer["name_field"]]
            key = (level, source_name)
            alias = ALIASES.get(key)
            if alias:
                seen_aliases.add(key)
            candidates = [school for school in schools
                          if (school["school_name"] == alias if alias else
                              normalized(school["school_name"]) == normalized(source_name))
                          and level_matches(school, level)]
            if len(candidates) != 1:
                raise ValueError(f"Review PWCS source label {key}: {candidates}")
            school = candidates[0]
            note = []
            if alias:
                note.append("PWCS source label omits part of the snapshot school name.")
            if school["school_level"] != level:
                note.append(f"Snapshot level is {school['school_level']}; PWCS source level is {level}.")
            rows.append({"division_id": DIVISION_ID, "level": level, "source_name": source_name,
                         "school_id": school["school_id"], "school_name": school["school_name"],
                         "disposition": "mapped", "note": " ".join(note)})
            linked.add(school["school_id"])
    if seen_aliases != set(ALIASES):
        raise ValueError(f"Review missing PWCS aliases: {set(ALIASES) - seen_aliases}")
    unmatched = {school["school_name"]: school for school in schools
                 if school["school_id"] not in linked}
    if set(unmatched) != set(NO_SEPARATE_POLYGON):
        raise ValueError(f"Review unmatched snapshot schools: {set(unmatched) ^ set(NO_SEPARATE_POLYGON)}")
    for name, school in sorted(unmatched.items()):
        rows.append({"division_id": DIVISION_ID, "level": school["school_level"],
                     "source_name": "", "school_id": school["school_id"],
                     "school_name": name, "disposition": "no_neighborhood_zone",
                     "note": NO_SEPARATE_POLYGON[name]})
    if len(rows) != 96 or len(linked) != 93:
        raise ValueError("PWCS crosswalk totals changed")
    DESTINATION.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile("w", newline="", encoding="utf-8", dir=DESTINATION.parent,
                                     delete=False) as handle:
        temporary = Path(handle.name)
        writer = csv.DictWriter(handle, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    os.replace(temporary, DESTINATION)
    print("Wrote PWCS crosswalk: 94 features, 93 school IDs, 2 no separate polygon")


if __name__ == "__main__":
    build()

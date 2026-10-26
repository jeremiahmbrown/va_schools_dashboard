#!/usr/bin/env python3
"""Build the reviewed FCPS standard-attendance crosswalk from staged 2026-27 layers."""

import csv
import json
import os
import re
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5101260"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION_ID}.json"
SCHOOLS = ROOT / "app" / "data" / "schools.csv"
DESTINATION = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION_ID}.csv"

# Every alias changes an official FCPS label to a different name in the frozen
# dashboard snapshot. Review these again when either source is refreshed.
ALIASES = {
    ("Elementary", "Franklin Sherman"): "Sherman Elementary",
    ("Elementary", "Louise Archer"): "Archer Elementary",
    ("Elementary", "Bailey's"): "Bailey's Elementary School for the Arts and Sciences",
    ("Elementary", "Hunters Woods"): "Hunters Woods Elementary School for the Arts and Sciences",
    ("Middle", "Katherine Johnson"): "Johnson Middle",
}

# These snapshot schools have no distinct label in the three FCPS standard
# attendance layers. This is not a claim that the school has no students.
NO_SEPARATE_POLYGON = {
    "Bailey's Upper Elementary School for the Arts and Sciences": "No separate Bailey's Upper polygon; FCPS standard ES layer labels the K-2 area Bailey's. Confirm this school's assignment with FCPS.",
    "Bryant High": "No Bryant polygon in the FCPS standard high-school attendance layer; confirm this program's assignment with FCPS.",
    "Fairfax County Adult High": "No adult-high polygon in the FCPS standard high-school attendance layer.",
    "Fort Belvoir Upper Elementary": "No separate Fort Belvoir Upper polygon; FCPS standard ES layer labels Fort Belvoir Primary. Confirm this school's assignment with FCPS.",
    "Key Center": "No Key Center polygon in the FCPS standard attendance layers; Key Middle is a different school.",
    "Kilmer Center": "No Kilmer Center polygon in the FCPS standard attendance layers; Kilmer Middle is a different school.",
    "Kings Glen Elementary": "No separate Kings Glen polygon; FCPS standard ES layer labels Kings Park. Confirm this school's assignment with FCPS.",
    "McNair Upper Elementary": "No separate McNair Upper polygon; FCPS standard ES layer labels McNair K-2. Confirm this school's assignment with FCPS.",
    "Mountain View High": "No Mountain View polygon in the FCPS standard high-school attendance layer; confirm this program's assignment with FCPS.",
    "Thomas Jefferson High for Science and Technology": "No TJHSST polygon in the FCPS standard high-school attendance layer; admissions are separate from a neighborhood boundary.",
}

FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]


def normalized(name):
    stem = re.sub(r"\b(elementary|middle|high|secondary|school|center|primary)\b", "", name.lower())
    return re.sub(r"[^a-z0-9]", "", stem)


def level_matches(name, level):
    return level in name or ("Secondary" in name and level in ("Middle", "High"))


def build():
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if manifest["division_id"] != DIVISION_ID:
        raise ValueError("Wrong source manifest")
    with SCHOOLS.open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == DIVISION_ID]
    if len(schools) != 195:
        raise ValueError("Fairfax school snapshot changed")
    rows = []
    matched_ids = set()
    seen_aliases = set()
    for layer in manifest["layers"]:
        level = layer["level"]
        features = json.loads((ROOT / layer["file"]).read_text(encoding="utf-8"))["features"]
        if len(features) != layer["feature_count"]:
            raise ValueError(f"Source count changed for {level}")
        for feature in features:
            source_name = feature["properties"]["SCHOOL_NAME"]
            key = (level, source_name)
            alias = ALIASES.get(key)
            if alias:
                seen_aliases.add(key)
            candidates = [school for school in schools
                          if (school["school_name"] == alias if alias else
                              normalized(school["school_name"]) == normalized(source_name))
                          and level_matches(school["school_name"], level)]
            if len(candidates) != 1:
                raise ValueError(f"Review ambiguous FCPS label {key}: {candidates}")
            school = candidates[0]
            note = []
            if alias:
                note.append("FCPS source label differs from the frozen snapshot school name.")
            if school["school_level"] != level:
                note.append(f"Snapshot level is {school['school_level']}; FCPS source level is {level}.")
            rows.append({"division_id": DIVISION_ID, "level": level, "source_name": source_name,
                         "school_id": school["school_id"], "school_name": school["school_name"],
                         "disposition": "mapped", "note": " ".join(note)})
            matched_ids.add(school["school_id"])
    if seen_aliases != set(ALIASES):
        raise ValueError(f"FCPS aliases need review: {set(ALIASES) - seen_aliases}")
    unmatched = {school["school_name"]: school for school in schools
                 if school["school_id"] not in matched_ids}
    if set(unmatched) != set(NO_SEPARATE_POLYGON):
        raise ValueError(f"Review schools without a distinct polygon: {set(unmatched) ^ set(NO_SEPARATE_POLYGON)}")
    for name, school in sorted(unmatched.items()):
        rows.append({"division_id": DIVISION_ID, "level": school["school_level"],
                     "source_name": "", "school_id": school["school_id"],
                     "school_name": name, "disposition": "no_neighborhood_zone",
                     "note": NO_SEPARATE_POLYGON[name]})
    if len(rows) != 198 or len(matched_ids) != 185:
        raise ValueError("Fairfax crosswalk totals changed")
    DESTINATION.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile("w", newline="", encoding="utf-8", dir=DESTINATION.parent,
                                     delete=False) as handle:
        temp_path = Path(handle.name)
        writer = csv.DictWriter(handle, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    os.replace(temp_path, DESTINATION)
    print(f"Wrote {len(rows)} rows: 188 polygons, 185 school IDs, 10 no separate polygon")


if __name__ == "__main__":
    build()

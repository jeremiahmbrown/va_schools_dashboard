#!/usr/bin/env python3
"""Build a strict CPS 2026–27 zone crosswalk, including grade-pair areas."""

import csv
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5100810"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION_ID}.json"
OUTPUT = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION_ID}.csv"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]
EXPECTED = {"Elementary": 21, "Middle": 10, "High": 7}
PAIRS = {
    "Greenbrier Primary": ("Greenbrier Intermediate", "K-2", "3-5"),
    "Western Branch Primary": ("Western Branch Intermediate", "K-2", "3-5"),
    "Great Bridge Primary": ("Great Bridge Intermediate", "K-2", "3-5"),
    "Georgetown Primary": ("Sparrow Road Intermediate", "K-3", "4-5"),
    "Rena B. Wright Primary": ("Truitt Intermediate", "K-2", "3-5"),
    "Portlock Primary": ("Carver Intermediate", "K-2", "3-5"),
    "Norfolk Highlands Primary": ("Sparrow Road Intermediate", "K-3", "4-5"),
    "Butts Road Primary": ("Butts Road Intermediate", "K-2", "3-5"),
}
CURRENT_SCHOOLS = {"Crestwood Elementary": ("Crestwood Elementary School", "510081003158"),
                   "B. M. Williams Elementary": ("B.M. Williams Elementary", "510081003157")}
ALIASES = {("High", "Oscar Smith High"): "Oscar F. Smith High"}


def normalized(name):
    stem = name.upper().replace("ELEMENTARY", "").replace("MIDDLE", "")
    stem = stem.replace("HIGH", "").replace("SCHOOL", "")
    return re.sub(r"[^A-Z0-9]", "", stem)


def row(level, source_name, school_id, school_name, disposition, note):
    return dict(division_id=DIVISION_ID, level=level, source_name=source_name,
                school_id=school_id, school_name=school_name,
                disposition=disposition, note=note)


def match_school(schools, level, source_name):
    alias = ALIASES.get((level, source_name))
    key = normalized(source_name)
    matches = [s for s in schools if s["school_level"] == level and
               (s["school_name"].strip() == alias if alias else
                normalized(s["school_name"]).endswith(key))]
    if len(matches) != 1:
        raise ValueError(f"Review CPS source name {(level, source_name)}: {matches}")
    return matches[0]


def build():
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if (manifest["division_id"] != DIVISION_ID or
            {x["level"]: x["feature_count"] for x in manifest["layers"]} != EXPECTED):
        raise ValueError("CPS source manifest changed")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [s for s in csv.DictReader(handle) if s["division_id"] == DIVISION_ID]
    if len(schools) != 45 or len({s["school_id"] for s in schools}) != 45:
        raise ValueError("CPS snapshot school count or IDs changed")
    rows, linked, used_pairs, current_schools, aliases = [], set(), set(), set(), set()
    for entry in manifest["layers"]:
        level = entry["level"]
        features = json.loads((ROOT / entry["file"]).read_text(encoding="utf-8"))["features"]
        if len(features) != EXPECTED[level]:
            raise ValueError(f"Incomplete CPS {level} source")
        names = [f["properties"][entry["name_field"]] for f in features]
        if len(set(names)) != len(names):
            raise ValueError(f"Duplicate CPS {level} source label")
        if len({f["properties"]["FID"] for f in features}) != len(features):
            raise ValueError(f"Duplicate CPS {level} source ID")
        for feature in features:
            props = feature["properties"]
            source_name = props[entry["name_field"]]
            if level == "Elementary":
                intermediate = (props.get("Int_Name") or "").strip()
                if source_name in PAIRS:
                    if (intermediate, props.get("Elem_Grade"), props.get("Int_Grades")) != PAIRS[source_name]:
                        raise ValueError(f"CPS grade-pair record changed: {source_name}")
                    used_pairs.add(source_name)
                elif intermediate:
                    raise ValueError(f"Unreviewed CPS intermediate school: {source_name}, {intermediate}")
                if source_name in CURRENT_SCHOOLS:
                    if intermediate or props.get("Elem_Grade") != "K-5":
                        raise ValueError(f"CPS 2025-26 K-5 grade changed: {source_name}")
                    school_name, school_id = CURRENT_SCHOOLS[source_name]
                    school = next((x for x in schools if x["school_id"] == school_id), None)
                    if school is None or school["school_name"] != school_name or school["school_level"] != level:
                        raise ValueError(f"CPS new school identity changed: {source_name}")
                    current_schools.add(source_name)
                    linked.add(school_id)
                    rows.append(row(level, source_name, school_id, school_name, "mapped",
                                    f"CPS GIS source FID {props['FID']}. New NCES 2025-26 school ID; keep earlier metrics separate from the former school."))
                    continue
                labels = [source_name] + ([intermediate] if intermediate else [])
                disposition = "shared_zone" if intermediate else "mapped"
            else:
                labels = [source_name]
                disposition = "mapped"
            for label in labels:
                school = match_school(schools, level, label)
                linked.add(school["school_id"])
                if (level, label) in ALIASES:
                    aliases.add((level, label))
                note = f"CPS GIS source FID {props['FID']}."
                if intermediate:
                    note += f" Shared {props['Elem_Grade']}/{props['Int_Grades']} primary/intermediate area."
                if (level, label) in ALIASES:
                    note += " Explicit snapshot-name alias."
                rows.append(row(level, source_name, school["school_id"], school["school_name"],
                                disposition, note))
    if (used_pairs != set(PAIRS) or current_schools != set(CURRENT_SCHOOLS) or
            aliases != set(ALIASES) or linked != {s["school_id"] for s in schools} or
            len(rows) != 46):
        raise ValueError("CPS shared, current-school, alias, or school coverage changed")
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {len(rows)} CPS decisions: {len(linked)} linked schools, 8 shared source areas, 2 newly linked K-5 areas")


if __name__ == "__main__":
    build()

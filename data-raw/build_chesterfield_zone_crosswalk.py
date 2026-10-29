#!/usr/bin/env python3
"""Build a strict crosswalk for Chesterfield County's public school GIS areas."""

import csv
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5100840"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION_ID}.json"
OUTPUT = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION_ID}.csv"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]
EXPECTED = {"Elementary": 41, "Middle": 13, "High": 10}
ALIASES = {
    ("Elementary", "GORDON"): "W.W. Gordon Elementary",
    ("Elementary", "CHALKLEY"): "J.A. Chalkley Elementary",
    ("Elementary", "WATKINS"): "J.B. Watkins Elementary",
    ("Elementary", "HENING"): "J.G. Hening Elementary",
    ("Elementary", "ELIZABETH N. SCOTT"): "Elizabeth Scott Elementary",
    ("Elementary", "MARGUERITE CHRISTIAN"): "Marguerite F. Christian Elementary",
    ("Middle", "ELIZABETH B DAVIS"): "Elizabeth Davis Middle",
    ("High", "L. C. BIRD"): "Lloyd C. Bird High",
}
NO_POLYGON = {
    "Carver College and Career Academy": "The standard county high-school boundary layer has no Carver College and Career Academy area; its admissions or program rules need a separate check.",
    "Chesterfield Virtual School": "The county's standard attendance-area layers have no separate virtual-school area.",
}


def normalized(name):
    stem = name.upper().replace("ELEMENTARY", "").replace("MIDDLE", "")
    stem = stem.replace("HIGH", "").replace("SCHOOL", "")
    return re.sub(r"[^A-Z0-9]", "", stem)


def row(level, source_name, school_id, school_name, disposition, note):
    return {"division_id": DIVISION_ID, "level": level, "source_name": source_name,
            "school_id": school_id, "school_name": school_name,
            "disposition": disposition, "note": note}


def build():
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if (manifest["division_id"] != DIVISION_ID or
            {entry["level"]: entry["feature_count"] for entry in manifest["layers"]} != EXPECTED):
        raise ValueError("Chesterfield source manifest changed")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [s for s in csv.DictReader(handle) if s["division_id"] == DIVISION_ID]
    if len(schools) != 65:
        raise ValueError("Chesterfield snapshot school count changed")
    by_name = {s["school_name"].strip(): s for s in schools}
    if len(by_name) != 65:
        raise ValueError("Duplicate Chesterfield school name in snapshot")
    rows, linked, used_aliases, source_only = [], set(), set(), 0
    for entry in manifest["layers"]:
        level = entry["level"]
        features = json.loads((ROOT / entry["file"]).read_text(encoding="utf-8"))["features"]
        if len(features) != EXPECTED[level]:
            raise ValueError(f"Incomplete Chesterfield {level} source")
        seen_names, seen_codes = set(), set()
        for feature in features:
            props = feature["properties"]
            source_name, code = props["SchoolName"], props["SchoolNum"]
            key = (level, source_name)
            if key in seen_names or code in seen_codes or not source_name or code is None:
                raise ValueError(f"Duplicate or incomplete Chesterfield source record: {key}, {code}")
            seen_names.add(key)
            seen_codes.add(code)
            if key == ("Elementary", "DEEP CREEK"):
                if code != 95:
                    raise ValueError("New Deep Creek Elementary county code changed")
                rows.append(row(level, source_name, "", "Deep Creek Elementary", "source_only",
                                "Opened for 2026–27 after the frozen snapshot; County GIS SchoolNum 95. Show its area without attaching another school's data."))
                source_only += 1
                continue
            alias = ALIASES.get(key)
            if alias:
                used_aliases.add(key)
            matches = [s for s in schools if s["school_level"] == level and
                       (s["school_name"].strip() == alias if alias else normalized(s["school_name"]) == normalized(source_name))]
            if len(matches) != 1:
                raise ValueError(f"Review Chesterfield source name {key}: {matches}")
            school = matches[0]
            if school["school_id"] in linked:
                raise ValueError(f"Duplicate Chesterfield school link: {school['school_id']}")
            linked.add(school["school_id"])
            note = f"County GIS SchoolNum {code}."
            if alias:
                note += " Official source name differs from the frozen snapshot name."
            rows.append(row(level, source_name, school["school_id"], school["school_name"], "mapped", note))
    if used_aliases != set(ALIASES) or source_only != 1 or len(linked) != 63:
        raise ValueError("Chesterfield alias, source-only, or link count changed")
    unlinked = {s["school_name"].strip(): s for s in schools if s["school_id"] not in linked}
    if set(unlinked) != set(NO_POLYGON):
        raise ValueError(f"Review Chesterfield schools without polygons: {set(unlinked)}")
    for name, school in sorted(unlinked.items()):
        rows.append(row(school["school_level"], "", school["school_id"],
                        school["school_name"], "no_neighborhood_zone", NO_POLYGON[name]))
    if len(rows) != 66:
        raise ValueError("Chesterfield crosswalk decision count changed")
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {len(rows)} Chesterfield decisions: {len(linked)} linked schools, {source_only} source-only polygon, {len(unlinked)} no-polygon schools")


if __name__ == "__main__":
    build()

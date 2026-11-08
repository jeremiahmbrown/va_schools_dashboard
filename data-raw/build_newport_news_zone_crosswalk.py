#!/usr/bin/env python3
"""Build strict Newport News City GIS attendance-zone decisions."""

import csv
import json
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5102640"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION_ID}.json"
OUTPUT = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION_ID}.csv"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]
MAPPING = {
    "Elementary": {
        "General Stanford K-5": "General Stanford Elementary",
        "Katherine Johnson K-5": "Katherine Johnson Elementary",
        "McIntosh K-5": "George J. McIntosh Elementary",
        "Jenkins K-5": "Willis A. Jenkins Elementary",
        "Knollwood Meadows K-5": "Knollwood Meadows Elementary",
        "Stoney Run K-5": "Stoney Run Elementary",
        "Palmer K-5": "L.F. Palmer Elementary",
        "Sanford K-5": "T. Ryland Sanford Elementary",
        "Charles K-5": "B.C. Charles Elementary",
        "An Achievable Dream Academy K-2": "Achievable Dream Academy",
        "Newsome Park K-2": "Newsome Park Elementary",
        "Kiln Creek K-5": "Kiln Creek Elementary",
        "Saunders K-5": "Joseph H. Saunders Elementary",
        "Yates K-5": "Richard T. Yates Elementary",
        "Hidenwood K-5": "Hidenwood Elementary",
        "Riverside K-5": "Riverside Elementary",
        "Sedgefield K-5": "Sedgefield Elementary",
        "Hilton K-5": "Hilton Elementary",
        "Carver K-5": "Carver Elementary",
        "Greenwood K-5": "Oliver C. Greenwood Elementary",
        "Discovery STEM Academy K-2": "Discovery STEM Academy",
        "Richneck K-5": "Richneck Elementary",
        "Newsome Park K-5": "Newsome Park Elementary",
    },
    "Middle": {
        "Ella Fitzgerald Middle": "Ella J. Fitzgerald Middle",
        "Passage Middle": "Mary Passage Middle",
        "Crittenden Middle": "Crittenden Middle",
        "Huntington Middle": "Huntington Middle",
        "Hines Middle": "Homer L. Hines Middle",
        "Gildersleeve Middle": "Ethel M. Gildersleeve Middle",
    },
    "High": {
        "Woodside High": "Woodside High",
        "Denbigh High": "Denbigh High",
        "Heritage High": "Heritage High",
        "Warwick High": "Warwick High",
        "Menchville High": "Menchville High",
    },
}
EXPECTED_COUNTS = {"Elementary": 23, "Middle": 6, "High": 8}
DUPLICATE_FIDS = {
    ("High", "Menchville High"): {5, 6, 8},
    ("High", "Warwick High"): {4, 7},
}
NO_ZONE = {
    "David A. Dutrow Elementary": "NNPS lists a magnet program with application admission. City elementary GIS has no named area. A base attendance area is not confirmed; use NNPS Zone Finder for an address.",
    "Deer Park Elementary": "NNPS lists a magnet program with application admission. City elementary GIS has no named area. A base attendance area is not confirmed; use NNPS Zone Finder for an address.",
    "John Marshall Early Childhood Center": "NNPS identifies this as a voluntary preschool program and transfers records to the next zoned kindergarten school. No separate standard K-12 attendance polygon in City GIS.",
    "B.T. Washington Middle": "NNPS lists a magnet program with application admission. City middle GIS has no named area. A base attendance area is not confirmed; use NNPS Zone Finder for an address.",
    "Achievable Dream Middle/High": "NNPS lists a specialty program; its admissions page says rising grades 3-6 are eligible citywide by application. No separately named City middle or high attendance polygon. Do not infer grades 7-12 admission from that rule.",
}
SOURCE_NOT_AVAILABLE = {"David A. Dutrow Elementary", "Deer Park Elementary", "B.T. Washington Middle"}


def build():
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if (manifest["division_id"] != DIVISION_ID or
            {entry["level"]: entry["feature_count"] for entry in manifest["layers"]} != EXPECTED_COUNTS):
        raise ValueError("Newport News source manifest changed")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == DIVISION_ID]
    by_name = {row["school_name"].strip(): row for row in schools}
    if len(schools) != 38 or len(by_name) != 38 or len({row["school_id"] for row in schools}) != 38:
        raise ValueError("Newport News frozen school snapshot changed")
    rows, seen, linked, source_fids = [], set(), set(), defaultdict(set)
    for entry in manifest["layers"]:
        level = entry["level"]
        features = json.loads((ROOT / entry["file"]).read_text(encoding="utf-8"))["features"]
        if len(features) != EXPECTED_COUNTS[level]:
            raise ValueError(f"Incomplete Newport News {level} source")
        for feature in features:
            props = feature["properties"]
            source_name = props[entry["name_field"]]
            pair = (level, source_name)
            source_fids[pair].add(props["OBJECTID"])
            if pair in seen:
                continue
            seen.add(pair)
            snapshot_name = MAPPING.get(level, {}).get(source_name)
            school = by_name.get(snapshot_name)
            if school is None or school["school_level"] != level:
                raise ValueError(f"Unreviewed Newport News source label or level: {pair}")
            linked.add(school["school_id"])
            note = f"City GIS OBJECTID {props['OBJECTID']}; explicit source-name crosswalk."
            if source_name in ("Newsome Park K-2", "Newsome Park K-5"):
                note += " City publishes two differently labeled areas for one snapshot school; retain each source area and grade label."
            if pair in DUPLICATE_FIDS:
                note += " City publishes multiple source pieces for this high school; retain their FIDs."
            rows.append(dict(division_id=DIVISION_ID, level=level, source_name=source_name,
                             school_id=school["school_id"], school_name=school["school_name"],
                             disposition="mapped", note=note))
    expected_pairs = {(level, name) for level, names in MAPPING.items() for name in names}
    if (seen != expected_pairs or
            {pair: fids for pair, fids in source_fids.items() if len(fids) > 1} != DUPLICATE_FIDS):
        raise ValueError("Newport News source labels or repeated IDs changed")
    for name, reason in NO_ZONE.items():
        school = by_name.get(name)
        if school is None:
            raise ValueError(f"Missing Newport News no-zone snapshot school: {name}")
        rows.append(dict(division_id=DIVISION_ID, level=school["school_level"], source_name="",
                         school_id=school["school_id"], school_name=school["school_name"],
                         disposition=("source_not_available" if name in SOURCE_NOT_AVAILABLE else "no_neighborhood_zone"),
                         note=reason))
    if linked | {by_name[name]["school_id"] for name in NO_ZONE} != {s["school_id"] for s in schools}:
        raise ValueError("Newport News school coverage changed")
    if len(linked) != 33 or len(rows) != 39:
        raise ValueError("Newport News link/decision count changed")
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {len(rows)} Newport News decisions: {len(linked)} linked schools, {len(NO_ZONE)} no-zone schools")


if __name__ == "__main__":
    build()

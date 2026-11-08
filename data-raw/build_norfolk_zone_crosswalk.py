#!/usr/bin/env python3
"""Build the NPS 2026–27 locator crosswalk with explicit school exceptions."""

import csv
import json
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5102670"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION_ID}.json"
OUTPUT = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION_ID}.csv"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]
MAPPING = {
    "Elementary": {
        "Coleman Place Elementary School": "Coleman Place Elementary",
        "Ocean View Elementary School": "Ocean View Elementary",
        "Calcott  Elementary School": "Mary Calcott Elementary",
        "Bay View Elementary School": "Bay View Elementary",
        "Little Creek Elementary School": "Little Creek Elementary",
        "Tarrallton Elementary School": "Tarrallton Elementary",
        "Taylor Elementary School": "Walter Herron Taylor Elementary",
        "Larrymore Elementary School": "Larrymore Elementary",
        "Camp Allen Elementary School": "Camp Allen Elementary",
        "Granby Elementary School": "Granby Elementary",
        "Lake Taylor": "Lake Taylor",
        "Sherwood Forest Elementary School": "Sherwood Forest Elementary",
        "Willard Elementary School": "Willard Elementary",
        "Suburban Park Elementary School": "Suburban Park Elementary",
        "Crossroads PK-8": "Crossroads PreK-8 School",
        "Tanners Creek Elementary School": "Tanners Creek Elementary",
        "Larchmont Elementary School": "Larchmont Elementary",
        "Monroe Elementary School": "James Monroe Elementary",
        "Ingleside Elementary School": "Ingleside Elementary",
        "St. Helena Elementary School": "St. Helena Elementary",
        "Southside STEM Academy at Campostella": "Southside STEM Academy at Campostella",
        "PB Young Elementary School": "P.B. Young Sr. Elementary",
        "Chesterfield Elementary School": "Chesterfield Elementary",
        "Jacox Elementary School": "Jacox Elementary",
        "Lindenwood Elementary School": "Lindenwood Elementary",
        "Richard Bowling Elementary School": "Richard Bowling Elementary",
        "Sewells Point Elementary School": "Sewells Point Elementary",
    },
    "Middle": {
        "Blair Middle School": "Blair Middle",
        "Azalea Gardens Middle School": "Azalea Gardens Middle",
        "Northside Middle School": "Northside Middle",
        "Lake Taylor Middle School": "Lake Taylor",
        "Southside STEM Academy at Campostella": "Southside STEM Academy at Campostella",
        "Northview Middle School": "Norview Middle",
        "Ruffner Academy": "Ruffner School",
    },
    "High": {
        "Granby High School": "Granby High",
        "Maury High School": "Matthew Fontaine Maury High",
        "Lake Taylor High School": "Lake Taylor High",
        "Booker T. Washington High School": "Booker T Washington High",
        "Norview High School": "Norview High",
    },
}
EXPECTED_COUNTS = {"Elementary": 27, "Middle": 8, "High": 6}
DUPLICATE_FIDS = {
    ("Middle", "Azalea Gardens Middle School"): {2, 8},
    ("High", "Norview High School"): {5, 6},
}
NO_ZONE = {
    "Academy for Discovery at Lakewood": "NPS states that this citywide school admits through application and lottery; no separate standard attendance area is in the locator.",
    "Ghent K-8": "No separate named attendance polygon in the current NPS locator; do not infer one from another school's area.",
    "Norview Elementary": "NPS closed Norview Elementary in June 2026; no 2026–27 neighborhood area is in the approved locator.",
    "Oceanair Elementary": "NPS ended Oceanair Elementary service in June 2026 and repurposed it for pre-K; no 2026–27 elementary area is in the locator.",
}


def build():
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if (manifest["division_id"] != DIVISION_ID or
            {entry["level"]: entry["feature_count"] for entry in manifest["layers"]} != EXPECTED_COUNTS):
        raise ValueError("Norfolk source manifest changed")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == DIVISION_ID]
    by_name = {row["school_name"].strip(): row for row in schools}
    if len(schools) != 41 or len(by_name) != 41 or len({row["school_id"] for row in schools}) != 41:
        raise ValueError("Norfolk frozen school snapshot changed")
    rows, seen, linked, source_fids = [], set(), set(), defaultdict(set)
    for entry in manifest["layers"]:
        level = entry["level"]
        features = json.loads((ROOT / entry["file"]).read_text(encoding="utf-8"))["features"]
        if len(features) != EXPECTED_COUNTS[level]:
            raise ValueError(f"Incomplete Norfolk {level} source")
        for feature in features:
            props = feature["properties"]
            source_name = props[entry["name_field"]]
            pair = (level, source_name)
            source_fids[pair].add(props["FID"])
            if pair in seen:
                continue
            seen.add(pair)
            snapshot_name = MAPPING.get(level, {}).get(source_name)
            school = by_name.get(snapshot_name)
            if school is None:
                raise ValueError(f"Unreviewed Norfolk source label: {pair}")
            linked.add(school["school_id"])
            note = f"NPS locator FID {props['FID']}; explicit source-name crosswalk."
            if school["school_level"] != level:
                note += f" Source {level} area for snapshot {school['school_level']} school."
            if pair == ("Middle", "Northview Middle School"):
                note += " Source spells Northview; NPS school is Norview Middle."
            if pair in DUPLICATE_FIDS:
                note += " Two published source features have the same named school; retain their separate FIDs."
            rows.append(dict(division_id=DIVISION_ID, level=level, source_name=source_name,
                             school_id=school["school_id"], school_name=school["school_name"],
                             disposition="mapped", note=note))
    expected_pairs = {(level, name) for level, names in MAPPING.items() for name in names}
    if (seen != expected_pairs or
            {pair: fids for pair, fids in source_fids.items() if len(fids) > 1} != DUPLICATE_FIDS):
        raise ValueError("Norfolk source labels or duplicate IDs changed")
    for name, reason in NO_ZONE.items():
        school = by_name.get(name)
        if school is None:
            raise ValueError(f"Missing Norfolk no-zone snapshot school: {name}")
        rows.append(dict(division_id=DIVISION_ID, level=school["school_level"], source_name="",
                         school_id=school["school_id"], school_name=school["school_name"],
                         disposition="no_neighborhood_zone", note=reason))
    if linked | {by_name[name]["school_id"] for name in NO_ZONE} != {s["school_id"] for s in schools}:
        raise ValueError("Norfolk school coverage changed")
    if len(linked) != 37 or len(rows) != 43:
        raise ValueError("Norfolk link/decision count changed")
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {len(rows)} Norfolk decisions: {len(linked)} linked schools, {len(NO_ZONE)} no-zone schools")


if __name__ == "__main__":
    build()

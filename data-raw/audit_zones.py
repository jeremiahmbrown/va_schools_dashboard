#!/usr/bin/env python3
"""Summarize attendance-zone coverage in the deployed snapshot."""

import csv
import json
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
APP = ROOT / "app" / "data"


def main():
    with (APP / "schools.csv").open(newline="") as handle:
        schools = list(csv.DictReader(handle))
    by_division = defaultdict(list)
    for school in schools:
        by_division[school["division_id"]].append(school)

    source_counts = Counter()
    level_counts = Counter()
    matched_ids = set()
    invalid_ids = []
    empty_geometry = []
    division_summary = {}
    levels_by_division = {}
    for division_id, div_schools in sorted(by_division.items()):
        source = "division" if (APP / "zones" / "division" / f"{division_id}.json").exists() else "sabs"
        path = APP / "zones" / source / f"{division_id}.json"
        if not path.exists():
            division_summary[division_id] = ("none", 0, 0)
            continue
        data = json.loads(path.read_text())
        features = data.get("features", [])
        source_counts[source] += 1
        div_matches = set()
        div_levels = set()
        school_ids = {s["school_id"] for s in div_schools}
        for feature in features:
            props = feature.get("properties") or {}
            level = props.get("level", "missing")
            level_counts[(source, level)] += 1
            div_levels.add(level)
            linked = ([props["app_school_id"]] if props.get("app_school_id") else [])
            shared = props.get("app_school_ids", [])
            if not isinstance(shared, list):
                invalid_ids.append((division_id, "invalid shared school ID list"))
                shared = []
            linked.extend(shared)
            for sid in linked:
                if sid not in school_ids:
                    invalid_ids.append((division_id, sid))
                else:
                    matched_ids.add(sid)
                    div_matches.add(sid)
            geom = feature.get("geometry") or {}
            if geom.get("type") not in ("Polygon", "MultiPolygon") or not geom.get("coordinates"):
                empty_geometry.append((division_id, props.get("zone_id")))
        division_summary[division_id] = (source, len(features), len(div_matches))
        levels_by_division[division_id] = div_levels

    print(f"Schools: {len(schools)}; divisions: {len(by_division)}")
    print(f"Zone files by source: {dict(source_counts)}")
    print(f"Zone features by source and level: {dict(level_counts)}")
    print(f"Schools with matched zone: {len(matched_ids)}")
    print(f"Unmatched schools: {len(schools) - len(matched_ids)}")
    print(f"Invalid school IDs: {len(invalid_ids)}; empty geometry: {len(empty_geometry)}")
    print("Divisions without file:", [d for d, (source, _, _) in division_summary.items() if source == "none"])
    for level in ("Elementary", "Middle", "High"):
        missing = [d for d in sorted(by_division) if level not in levels_by_division.get(d, set())]
        print(f"Divisions without {level} feature: {len(missing)}; IDs: {missing}")
    unknown_only = [d for d, levels in levels_by_division.items() if levels == {"Unknown"}]
    print("Divisions with only unknown-level features:", unknown_only)
    print("Roanoke City:", division_summary.get("5103300"))
    print("Roanoke City unmatched:", [(s["school_id"], s["school_name"]) for s in by_division["5103300"] if s["school_id"] not in matched_ids])
    print("Roanoke County:", division_summary.get("5103330"))
    print("Roanoke County unmatched:", [(s["school_id"], s["school_name"]) for s in by_division["5103330"] if s["school_id"] not in matched_ids])


if __name__ == "__main__":
    main()

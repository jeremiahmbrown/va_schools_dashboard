#!/usr/bin/env python3
"""Create or refresh the statewide attendance-zone research inventory.

Run from either checkout. Current snapshot counts refresh on each run; research
fields and frozen cohort assignments are preserved. This script never downloads
or replaces attendance polygons.
"""

import csv
import json
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "app" / "data"
OUTPUT = ROOT / "data-raw" / "zone_source_inventory.csv"
BEAD = "va_schools_dashboard-nlf"
COHORT_BEADS = {
    "no_file": f"{BEAD}.2",
    "unknown_only": f"{BEAD}.3",
    "high_enrollment": f"{BEAD}.6",
    "standard_1": f"{BEAD}.7",
    "standard_2": f"{BEAD}.8",
    "standard_3": f"{BEAD}.9",
    "integrated": BEAD,
}
COHORT_ORDER = {name: index for index, name in enumerate(COHORT_BEADS)}
RESEARCH_FIELDS = (
    "research_status", "candidate_type", "candidate_url", "official_evidence_url",
    "effective_school_year", "source_last_modified", "last_checked_date",
    "rights_access", "import_script", "decision_notes", "next_action",
)
FIELDS = (
    "division_id", "division_name", "cohort", "bead_id", "school_count",
    "enrollment_latest_total", "active_file_source", "zone_feature_count",
    "matched_school_count", "levels_present", *RESEARCH_FIELDS,
)


def read_schools():
    with (DATA / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = list(csv.DictReader(handle))
    by_division = defaultdict(list)
    for school in schools:
        by_division[school["division_id"]].append(school)
    if len(by_division) != 130 or len(schools) != 1810:
        raise ValueError("School snapshot changed; review cohort and baseline before refresh")
    return by_division


def current_row(division_id, schools):
    division_path = DATA / "zones" / "division" / f"{division_id}.json"
    sabs_path = DATA / "zones" / "sabs" / f"{division_id}.json"
    if division_path.exists():
        path, source = division_path, "division_gis"
    elif sabs_path.exists():
        path, source = sabs_path, "sabs_2015_16"
    else:
        path, source = None, "none"
    features = []
    if path:
        data = json.loads(path.read_text(encoding="utf-8"))
        if data.get("type") != "FeatureCollection" or not isinstance(data.get("features"), list):
            raise ValueError(f"Invalid zone file: {path}")
        features = data["features"]
    school_ids = {s["school_id"] for s in schools}
    linked = {f.get("properties", {}).get("app_school_id") for f in features
              if f.get("properties", {}).get("app_school_id")}
    if not linked <= school_ids:
        raise ValueError(f"Zone file links schools outside division {division_id}: {linked - school_ids}")
    levels = sorted({f.get("properties", {}).get("level", "missing") for f in features})
    enrollment = sum(float(s["enrollment_latest"] or 0) for s in schools)
    return {
        "division_id": division_id,
        "division_name": schools[0]["division_name"],
        "school_count": str(len(schools)),
        "enrollment_latest_total": str(round(enrollment)),
        "active_file_source": source,
        "zone_feature_count": str(len(features)),
        "matched_school_count": str(len(linked)),
        "levels_present": "|".join(levels),
    }


def initial_cohorts(rows):
    cohorts = {}
    for division_id, row in rows.items():
        if row["active_file_source"] == "division_gis":
            cohorts[division_id] = "integrated"
        elif row["active_file_source"] == "none":
            cohorts[division_id] = "no_file"
        elif row["levels_present"] == "Unknown":
            cohorts[division_id] = "unknown_only"
    remaining = [d for d in rows if d not in cohorts]
    remaining.sort(key=lambda d: (-int(rows[d]["enrollment_latest_total"]), d))
    if (Counter(cohorts.values()) != {"integrated": 6, "no_file": 10, "unknown_only": 9}
            or len(remaining) != 105):
        raise ValueError("Zone baseline changed; review cohorts before initialization")
    for index, division_id in enumerate(remaining):
        cohorts[division_id] = ("high_enrollment" if index < 20 else
                                "standard_1" if index < 50 else
                                "standard_2" if index < 80 else "standard_3")
    assert Counter(cohorts.values()) == {
        "integrated": 6, "no_file": 10, "unknown_only": 9,
        "high_enrollment": 20, "standard_1": 30, "standard_2": 30, "standard_3": 25,
    }
    return cohorts


def main():
    by_division = read_schools()
    current = {division_id: current_row(division_id, schools)
               for division_id, schools in by_division.items()}
    previous = {}
    if OUTPUT.exists():
        with OUTPUT.open(newline="", encoding="utf-8-sig") as handle:
            source = list(csv.DictReader(handle))
        previous = {row["division_id"]: row for row in source}
        if len(previous) != len(source) or set(previous) != set(current):
            raise ValueError("Inventory division IDs changed; manual review required")
        if not set(FIELDS) <= set(source[0]):
            raise ValueError("Inventory schema changed; manual review required")
        cohorts = {d: row["cohort"] for d, row in previous.items()}
        if any(cohort not in COHORT_BEADS for cohort in cohorts.values()):
            raise ValueError("Unknown frozen cohort in inventory")
    else:
        cohorts = initial_cohorts(current)
    output = []
    for division_id, row in current.items():
        old = previous.get(division_id, {})
        row["cohort"] = cohorts[division_id]
        row["bead_id"] = f"{BEAD}.4" if division_id == "5100270" else COHORT_BEADS[row["cohort"]]
        for field in RESEARCH_FIELDS:
            row[field] = old.get(field, "")
        if not old:
            row["research_status"] = "integrated_local" if row["cohort"] == "integrated" else "not_started"
        output.append(row)
    output.sort(key=lambda r: (COHORT_ORDER[r["cohort"]], -int(r["enrollment_latest_total"]), r["division_id"]))
    temporary = OUTPUT.with_suffix(".csv.tmp")
    with temporary.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(output)
    temporary.replace(OUTPUT)
    print(f"Wrote {OUTPUT}: {len(output)} divisions; cohorts {dict(Counter(r['cohort'] for r in output))}")


if __name__ == "__main__":
    main()

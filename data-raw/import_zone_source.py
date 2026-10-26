#!/usr/bin/env python3
"""Validate staged attendance polygons and optionally publish one division.

Run stage_zone_source.py first. The default mode checks and writes an ignored
candidate. --publish replaces the active file only after all checks pass.
"""

import argparse
import csv
import hashlib
import html
import json
import os
import subprocess
import tempfile
import urllib.parse
from collections import Counter
from pathlib import Path

from build_zone_level_cache import build_one as build_level_cache

ROOT = Path(__file__).resolve().parents[1]
COLORS = {"Elementary": "#4C78A8", "Middle": "#F58518", "High": "#E45756"}


def read_crosswalk(path, division_id, schools, levels):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        rows = list(csv.DictReader(handle))
    required = {"division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"}
    if not rows or not required <= set(rows[0]):
        raise ValueError("Crosswalk is empty or missing required columns")
    index = {school["school_id"]: school for school in schools}
    mapped = {}
    mapped_ids = set()
    excluded = set()
    for row in rows:
        school_id = row["school_id"]
        school = index.get(school_id)
        if school is None or row["division_id"] != division_id or row["school_name"] != school["school_name"]:
            raise ValueError(f"Crosswalk school does not match snapshot: {school_id}")
        if row["disposition"] == "mapped":
            pair = (row["level"], row["source_name"])
            if pair in mapped or row["level"] not in levels or not row["source_name"]:
                raise ValueError(f"Duplicate or invalid mapped source label: {pair}")
            if school["school_level"] != row["level"] and not row["note"]:
                raise ValueError(f"Level exception needs a reason: {school_id}")
            mapped[pair] = school
            mapped_ids.add(school_id)
        elif row["disposition"] == "no_neighborhood_zone":
            if (school_id in excluded or row["source_name"] or not row["note"]
                    or row["level"] != school["school_level"]):
                raise ValueError(f"Invalid no-zone disposition: {school_id}")
            excluded.add(school_id)
        else:
            raise ValueError(f"Unknown disposition: {row['disposition']}")
    if mapped_ids & excluded or mapped_ids | excluded != set(index):
        raise ValueError("Crosswalk must resolve every current school exactly as mapped or excluded")
    return mapped, mapped_ids


def checked_raw_file(relative_path, division_id, digest):
    base = (ROOT / "data-raw" / "input" / "zones" / division_id).resolve()
    path = (ROOT / relative_path).resolve()
    if not path.is_relative_to(base) or not path.is_file():
        raise ValueError(f"Manifest raw file is outside staged division input: {relative_path}")
    raw = path.read_bytes()
    if hashlib.sha256(raw).hexdigest() != digest:
        raise ValueError(f"Raw source hash changed: {relative_path}")
    return json.loads(raw)


def build(division_id, spec_path, manifest_path, crosswalk_path, publish):
    spec = json.loads(spec_path.read_text(encoding="utf-8"))
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    layers = spec.get("layers", [])
    if (spec.get("division_id") != division_id or manifest.get("division_id") != division_id
            or len(layers) != len(manifest.get("layers", [])) or not layers):
        raise ValueError("Spec and manifest division or layer counts differ")
    if manifest.get("official_evidence_url") != spec.get("official_evidence_url"):
        raise ValueError("Official evidence URL changed between spec and manifest")
    overlap_limit = spec.get("max_same_level_overlap_m2")
    if (not isinstance(overlap_limit, (int, float)) or overlap_limit < 0
            or overlap_limit > 0 and not spec.get("overlap_review_note")):
        raise ValueError("Overlap limit needs a nonnegative value and reviewed reason")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [row for row in csv.DictReader(handle) if row["division_id"] == division_id]
    if len(schools) != spec.get("expected_school_count"):
        raise ValueError("School snapshot count changed")
    level_names = {row["level"] for row in layers}
    mapped, mapped_ids = read_crosswalk(crosswalk_path, division_id, schools, level_names)
    output = []
    source_pairs = set()
    counts = Counter()
    for index, (layer, record) in enumerate(zip(layers, manifest["layers"])):
        level = layer["level"]
        expected_url = (layer["source_url"] + "/query?" + urllib.parse.urlencode({
            "where": "1=1", "outFields": "*", "outSR": "4326", "f": "geojson",
        }) if layer["source_type"] == "arcgis" else layer["source_url"])
        if (level not in COLORS or record["level"] != level
                or record.get("layer_url") != layer["source_url"]
                or record.get("source_type") != layer["source_type"]
                or record.get("url") != expected_url
                or record.get("feature_count") != layer["expected_count"]
                or record.get("name_field") != layer["name_field"]
                or record.get("school_year_values", []) != layer.get("expected_year_values", [])):
            raise ValueError(f"Source manifest differs from reviewed spec at layer {index}")
        data = checked_raw_file(record["file"], division_id, record["sha256"])
        features = data.get("features")
        if data.get("type") != "FeatureCollection" or not isinstance(features, list) or len(features) != layer["expected_count"]:
            raise ValueError(f"Incomplete raw feature collection at layer {index}")
        for feature in features:
            props = feature.get("properties") or {}
            source_name = props.get(layer["name_field"])
            pair = (level, source_name)
            school = mapped.get(pair)
            if school is None:
                raise ValueError(f"No approved school crosswalk for {pair}")
            source_pairs.add(pair)
            if layer.get("year_field") and str(props.get(layer["year_field"])) not in layer["expected_year_values"]:
                raise ValueError(f"Source year changed for {pair}")
            oid = props.get(record["object_id_field"])
            if oid is None:
                raise ValueError(f"Missing source object ID for {pair}")
            zone_id = f"{division_id}_{level.lower()}_{oid}"
            if layer.get("zone_id_prefix"):
                zone_id = f"{division_id}_{level.lower()}_{layer['zone_id_prefix']}_{oid}"
            color = COLORS[level]
            note = spec.get("popup_note", "Confirm an address with the school division.")
            properties = {
                "zone_id": zone_id, "division_id": division_id,
                "school_name": school["school_name"], "official_school_name": source_name,
                "level": level, "app_school_id": school["school_id"],
                "source": spec["source_label"], "source_url": record["url"],
            }
            if layer.get("year_field"):
                properties["source_school_year_field"] = str(props[layer["year_field"]])
            properties.update({
                "popup": (f"<strong>Attendance zone</strong><br/>School: {html.escape(school['school_name'])}"
                          f"<br/>School level: {level}<br/>Source: {html.escape(spec['source_label'])}"
                          f"<br/><span class='muted'>{html.escape(note)}</span>"),
                "style": {"weight": 1, "opacity": 0.9, "fillOpacity": 0.12,
                          "color": color, "fillColor": color},
            })
            output.append({"type": "Feature", "id": zone_id,
                           "properties": properties, "geometry": feature.get("geometry")})
            counts[level] += 1
    if source_pairs != set(mapped):
        raise ValueError(f"Crosswalk contains source labels absent from download: {set(mapped) - source_pairs}")
    ids = [feature["id"] for feature in output]
    linked = {feature["properties"]["app_school_id"] for feature in output}
    if len(set(ids)) != len(ids) or linked != mapped_ids:
        raise ValueError("Duplicate zone IDs or school links differ from crosswalk")
    if len(output) != sum(row["expected_count"] for row in layers):
        raise ValueError("Final zone count differs from spec")
    destination = ROOT / "app" / "data" / "zones" / "division" / f"{division_id}.json"
    report_destination = ROOT / "data-raw" / "zone_validation_reports" / f"{division_id}.json"
    with tempfile.TemporaryDirectory(dir=destination.parent) as temporary:
        work = Path(temporary)
        raw_path = work / "raw.geojson"
        fixed_path = work / "valid.geojson"
        report_path = work / "report.json"
        raw_path.write_text(json.dumps({"type": "FeatureCollection", "features": output}), encoding="utf-8")
        subprocess.run(["ogr2ogr", "-f", "GeoJSON", "-makevalid", str(fixed_path), str(raw_path)], check=True)
        fixed = json.loads(fixed_path.read_text(encoding="utf-8"))
        saved = fixed.get("features", [])
        if (len(saved) != len(output) or {f["properties"]["zone_id"] for f in saved} != set(ids)
                or {f["properties"]["app_school_id"] for f in saved} != linked):
            raise ValueError("Geometry repair changed zone or school IDs")
        subprocess.run(["Rscript", "--vanilla", str(ROOT / "data-raw" / "report_zone_geometry.R"),
                        str(fixed_path), str(report_path), str(overlap_limit)], check=True)
        candidate_bytes = (json.dumps(fixed, ensure_ascii=False, separators=(",", ":")) + "\n").encode("utf-8")
        digest = hashlib.sha256(candidate_bytes).hexdigest()
        if publish:
            temporary_file = work / "publish.json"
            temporary_file.write_bytes(candidate_bytes)
            os.replace(temporary_file, destination)
            build_level_cache(division_id, tuple(counts.get(level, 0) for level in ("Elementary", "Middle", "High")))
            report_destination.parent.mkdir(parents=True, exist_ok=True)
            temporary_report = report_destination.with_suffix(".json.tmp")
            temporary_report.write_bytes(report_path.read_bytes())
            os.replace(temporary_report, report_destination)
        else:
            candidate = ROOT / "data-raw" / "input" / "zones" / division_id / "candidate.geojson"
            candidate.parent.mkdir(parents=True, exist_ok=True)
            candidate.write_bytes(candidate_bytes)
            candidate.with_suffix(".report.json").write_bytes(report_path.read_bytes())
    print(f"{'Published' if publish else 'Checked'} {division_id}: {len(output)} features, "
          f"{len(linked)}/{len(schools)} schools, SHA-256 {digest}")
    return digest


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("division_id")
    parser.add_argument("--publish", action="store_true")
    parser.add_argument("--manifest", type=Path)
    parser.add_argument("--crosswalk", type=Path)
    args = parser.parse_args()
    division_id = args.division_id
    build(division_id,
          ROOT / "data-raw" / "zone_import_specs" / f"{division_id}.json",
          args.manifest or ROOT / "data-raw" / "zone_source_manifests" / f"{division_id}.json",
          args.crosswalk or ROOT / "data-raw" / "zone_school_crosswalks" / f"{division_id}.csv",
          args.publish)

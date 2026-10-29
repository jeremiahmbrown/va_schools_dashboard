#!/usr/bin/env python3
"""Check the staged Arlington pilot and failed-import preservation gates."""

import csv
import hashlib
import json
import sys
import tempfile
import urllib.parse
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "data-raw"))
import import_zone_source as importer  # noqa: E402
import stage_zone_source as stager  # noqa: E402

DIVISION = "5100270"
ACTIVE = ROOT / "app" / "data" / "zones" / "division" / f"{DIVISION}.json"
SPEC = ROOT / "data-raw" / "zone_import_specs" / f"{DIVISION}.json"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / f"{DIVISION}.json"
CROSSWALK = ROOT / "data-raw" / "zone_school_crosswalks" / f"{DIVISION}.csv"


def require_failure(action, label, before):
    try:
        action()
    except (ValueError, KeyError):
        pass
    else:
        raise AssertionError(f"{label} was accepted")
    assert ACTIVE.read_bytes() == before, f"{label} changed the active zone file"


def main():
    before = ACTIVE.read_bytes()
    digest = hashlib.sha256(before).hexdigest()
    checked = importer.build(DIVISION, SPEC, MANIFEST, CROSSWALK, publish=False)
    assert checked == digest, "Generic importer differs from the reviewed Arlington file"
    assert ACTIVE.read_bytes() == before, "Dry run changed the active file"
    with tempfile.TemporaryDirectory() as temporary:
        work = Path(temporary)
        manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
        manifest["layers"][0]["sha256"] = "0" * 64
        bad_manifest = work / "bad-manifest.json"
        bad_manifest.write_text(json.dumps(manifest), encoding="utf-8")
        require_failure(lambda: importer.build(DIVISION, SPEC, bad_manifest, CROSSWALK, publish=True),
                        "bad source hash", before)
        with CROSSWALK.open(newline="", encoding="utf-8") as handle:
            rows = list(csv.DictReader(handle))
        next(row for row in rows if row["disposition"] == "mapped")["source_name"] = "Not an APS source label"
        bad_crosswalk = work / "bad-crosswalk.csv"
        with bad_crosswalk.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=rows[0]); writer.writeheader(); writer.writerows(rows)
        require_failure(lambda: importer.build(DIVISION, SPEC, MANIFEST, bad_crosswalk, publish=True),
                        "bad crosswalk", before)

    original_fetch = stager.fetch_arcgis
    manifest_before = MANIFEST.read_bytes()
    first_layer = json.loads((ROOT / json.loads(manifest_before)["layers"][0]["file"]).read_text(encoding="utf-8"))
    calls = 0

    def fail_second_layer(url, expected, where="1=1"):
        nonlocal calls
        calls += 1
        if calls == 1:
            return first_layer["features"], "OBJECTID"
        raise ValueError("Simulated second-layer download failure")

    stager.fetch_arcgis = fail_second_layer
    try:
        require_failure(lambda: stager.stage(DIVISION), "partial download", before)
    finally:
        stager.fetch_arcgis = original_fetch
    assert MANIFEST.read_bytes() == manifest_before, "Partial download changed source manifest"

    # Arlington fits on one ArcGIS page. Exercise multiple pages and a lost ID.
    original_get_json = stager.get_json
    calls = []

    def fake_arcgis(url):
        calls.append(url)
        if url.endswith("?f=json"):
            return {"maxRecordCount": 2}
        parameters = urllib.parse.parse_qs(url.split("?", 1)[1])
        if parameters.get("returnIdsOnly") == ["true"]:
            return {"objectIdFieldName": "OBJECTID", "objectIds": [3, 1, 2]}
        ids = [int(value) for value in parameters["objectIds"][0].split(",")]
        return {"type": "FeatureCollection", "features": [
            {"type": "Feature", "properties": {"OBJECTID": value}, "geometry": None}
            for value in reversed(ids)
        ]}

    stager.get_json = fake_arcgis
    try:
        features, field = stager.fetch_arcgis("https://example.org/FeatureServer/1", 3)
        assert field == "OBJECTID" and [row["properties"][field] for row in features] == [1, 2, 3]
        assert len([url for url in calls if "objectIds=" in url]) == 2, "Expected two ArcGIS pages"

        def missing_id(url):
            result = fake_arcgis(url)
            if "objectIds=3" in url:
                result["features"] = []
            return result

        stager.get_json = missing_id
        require_failure(lambda: stager.fetch_arcgis("https://example.org/FeatureServer/1", 3),
                        "missing ArcGIS page ID", before)
    finally:
        stager.get_json = original_get_json

    # A direct GeoJSON source uses the same checked, atomic staging path.
    original_root = stager.ROOT
    with tempfile.TemporaryDirectory() as temporary:
        stager.ROOT = Path(temporary)
        direct_id = "9999999"
        spec_path = stager.ROOT / "data-raw" / "zone_import_specs" / f"{direct_id}.json"
        spec_path.parent.mkdir(parents=True)
        spec_path.write_text(json.dumps({
            "division_id": direct_id, "source_owner": "Test division",
            "official_evidence_url": "https://example.org/evidence",
            "source_label": "Test source", "popup_note": "Test year unknown",
            "expected_school_count": 1, "max_same_level_overlap_m2": 0,
            "layers": [{"level": "Elementary", "source_type": "geojson",
                        "source_url": "https://example.org/zones.geojson", "name_field": "NAME",
                        "object_id_field": "ID", "expected_count": 1}],
        }), encoding="utf-8")
        stager.get_json = lambda url: {"type": "FeatureCollection", "features": [{
            "type": "Feature", "properties": {"ID": "one", "NAME": "Test Elementary"},
            "geometry": {"type": "Polygon", "coordinates": [[
                [-79.95, 37.25], [-79.94, 37.25], [-79.94, 37.26],
                [-79.95, 37.26], [-79.95, 37.25]]]},
        }]}
        try:
            stager.stage(direct_id)
            direct_manifest = json.loads((stager.ROOT / "data-raw" / "zone_source_manifests" /
                                          f"{direct_id}.json").read_text(encoding="utf-8"))
            raw_path = stager.ROOT / direct_manifest["layers"][0]["file"]
            assert raw_path.is_file() and hashlib.sha256(raw_path.read_bytes()).hexdigest() == direct_manifest["layers"][0]["sha256"]
        finally:
            stager.ROOT = original_root
            stager.get_json = original_get_json
    print(f"Importer gates passed; active Arlington SHA-256 {digest}")


if __name__ == "__main__":
    main()

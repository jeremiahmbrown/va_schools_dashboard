#!/usr/bin/env python3
"""Combine checked Charlottesville elementary GIS with explicitly dated secondary SABS."""
import hashlib
import json
import os
import tempfile
from pathlib import Path

import import_zone_source
import build_zone_level_cache

ROOT = Path(__file__).resolve().parents[1]
DIVISION = "5100780"


def build():
    spec = ROOT / "data-raw/zone_import_specs/5100780.json"
    manifest = ROOT / "data-raw/zone_source_manifests/5100780.json"
    crosswalk = ROOT / "data-raw/zone_school_crosswalks/5100780.csv"
    digest = import_zone_source.build(DIVISION, spec, manifest, crosswalk, publish=False)
    candidate = ROOT / "data-raw/input/zones/5100780/candidate.geojson"
    raw = candidate.read_bytes()
    if hashlib.sha256(raw).hexdigest() != digest:
        raise ValueError("Charlottesville checked candidate changed")
    elementary = json.loads(raw)["features"]
    if len(elementary) != 6 or any(f["properties"]["level"] != "Elementary" for f in elementary):
        raise ValueError("Charlottesville elementary source changed")
    historical = json.loads((ROOT / "app/data/zones/sabs/5100780.json").read_text(encoding="utf-8"))["features"]
    expected = {"High": "510078000273"}
    secondary = [f for f in historical if f["properties"].get("level") in expected]
    if len(secondary) != 1:
        raise ValueError("Charlottesville historical secondary source changed")
    for f in secondary:
        props = f["properties"]
        if (props.get("app_school_id") != expected[props["level"]]
                or props.get("source") != "sabs" or "2015–16" not in props.get("popup", "")):
            raise ValueError("Charlottesville historical source identity or date changed")
    features = elementary + secondary
    ids = [f.get("id") for f in features]
    school_ids = [f["properties"].get("app_school_id") for f in features]
    source_only = sorted(f["properties"]["school_name"] for f in elementary if not f["properties"].get("app_school_id"))
    linked = [sid for sid in school_ids if sid]
    if (source_only or len(set(ids)) != 7 or not all(ids)
            or len(linked) != 7 or len(set(linked)) != 7
            or any(f["properties"].get("division_id") != DIVISION for f in features)):
        raise ValueError("Charlottesville combined zone IDs changed")
    dest = ROOT / "app/data/zones/division/5100780.json"
    content = (json.dumps({"type": "FeatureCollection", "features": features},
                          ensure_ascii=False, separators=(",", ":")) + "\n").encode("utf-8")
    fd, temp = tempfile.mkstemp(prefix=".charlottesville-", dir=dest.parent)
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(content)
        os.replace(temp, dest)
    finally:
        if os.path.exists(temp):
            os.unlink(temp)
    build_zone_level_cache.build_one(DIVISION, (6, 0, 1))
    print("Built Charlottesville mixed-source zones:", hashlib.sha256(content).hexdigest())


if __name__ == "__main__":
    build()

#!/usr/bin/env python3
"""Stage the APS locator's public boundary layers for source review.

This does not replace an active dashboard zone file. The raw downloads are
ignored by Git; the manifest records their URLs, counts, and SHA-256 values.
"""

import hashlib
import json
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
BASE = "https://arlgis.arlingtonva.us/arcgis/rest/services/Public_Maps/School_Boundary_Locator_v2/MapServer"
LAYERS = {"Elementary": (1, 20, "ES_Name"), "Middle": (3, 6, "MS_Name"), "High": (5, 3, "HS_Name")}
OUT = ROOT / "data-raw" / "input" / "zones" / "5100270"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / "5100270.json"
PAGE_SIZE = 500


def points(geometry):
    polygons = [geometry["coordinates"]] if geometry["type"] == "Polygon" else geometry["coordinates"]
    for polygon in polygons:
        for ring in polygon:
            yield from ring


def query_json(layer_url, parameters):
    url = layer_url + "/query?" + urllib.parse.urlencode(parameters)
    request = urllib.request.Request(url, headers={"User-Agent": "va-schools-dashboard/zone-research"})
    with urllib.request.urlopen(request, timeout=60) as response:
        data = json.load(response)
    if "error" in data:
        raise ValueError(f"ArcGIS query failed for {layer_url}: {data['error']}")
    return data


def fetch_all(layer_url, expected):
    inventory = query_json(layer_url, {"where": "1=1", "returnIdsOnly": "true", "f": "json"})
    id_field = inventory.get("objectIdFieldName")
    ids = inventory.get("objectIds")
    if not id_field or not isinstance(ids, list) or len(ids) != expected or len(set(ids)) != expected:
        raise ValueError(f"ArcGIS object-ID inventory changed for {layer_url}")
    features = []
    sorted_ids = sorted(ids)
    for start in range(0, len(sorted_ids), PAGE_SIZE):
        page_ids = sorted_ids[start:start + PAGE_SIZE]
        page = query_json(layer_url, {
            "objectIds": ",".join(map(str, page_ids)), "outFields": "*",
            "outSR": "4326", "f": "geojson",
        })
        rows = page.get("features")
        if page.get("type") != "FeatureCollection" or not isinstance(rows, list):
            raise ValueError(f"Invalid ArcGIS GeoJSON page for {layer_url}")
        returned = [row.get("properties", {}).get(id_field) for row in rows]
        if len(rows) != len(page_ids) or set(returned) != set(page_ids):
            raise ValueError(f"Incomplete ArcGIS object-ID page for {layer_url}")
        features.extend(sorted(rows, key=lambda row: row["properties"][id_field]))
    if len(features) != expected:
        raise ValueError(f"Incomplete ArcGIS download for {layer_url}")
    return features, id_field


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    manifest = {
        "division_id": "5100270",
        "source_owner": "Arlington County GIS / Arlington Public Schools",
        "official_evidence_url": "https://www.apsva.us/registration/maps-boundaries/",
        "locator_url": "https://arlgis.maps.arcgis.com/apps/instant/lookup/index.html?appid=fcbef7f2d50c4b2bae5558ae51cb5c14",
        "locator_webmap_id": "8e76a3e3bd774b33ad76e072d5c74d72",
        "retrieved_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "status": "source_candidate; GIS School_Year says 2022-2023 while APS links 2026-27 maps",
        "layers": [],
    }
    for level, (layer_id, expected, name_field) in LAYERS.items():
        layer_url = f"{BASE}/{layer_id}"
        url = f"{layer_url}/query?" + urllib.parse.urlencode({
            "where": "1=1", "outFields": "*", "outSR": "4326", "f": "geojson",
        })
        features, id_field = fetch_all(layer_url, expected)
        raw = json.dumps({"type": "FeatureCollection", "features": features},
                         ensure_ascii=False, separators=(",", ":")).encode("utf-8")
        names = []
        years = set()
        for feature in features:
            properties = feature.get("properties") or {}
            geometry = feature.get("geometry") or {}
            name = properties.get(name_field)
            if not name or geometry.get("type") not in ("Polygon", "MultiPolygon"):
                raise ValueError(f"{level}: missing name or polygon geometry")
            if not all(len(p) >= 2 and -84 < p[0] < -74 and 36 < p[1] < 40 for p in points(geometry)):
                raise ValueError(f"{level}: coordinates outside Virginia WGS84 bounds")
            names.append(name)
            years.add(str(properties.get("School_Year")))
        if len(set(names)) != expected or years != {"2022-2023"}:
            raise ValueError(f"{level}: duplicate names or changed School_Year values: {years}")
        path = OUT / f"{level.lower()}.geojson"
        path.write_bytes(raw)
        manifest["layers"].append({
            "level": level, "layer_id": layer_id, "layer_url": layer_url, "url": url,
            "file": str(path.relative_to(ROOT)).replace("\\", "/"),
            "sha256": hashlib.sha256(raw).hexdigest(),
            "feature_count": len(features), "name_field": name_field,
            "object_id_field": id_field, "retrieval_method": "all object IDs then paged GeoJSON by objectIds",
            "school_year_values": sorted(years), "school_names": sorted(names),
        })
    MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(f"Staged {sum(row['feature_count'] for row in manifest['layers'])} polygons; manifest: {MANIFEST}")


if __name__ == "__main__":
    main()

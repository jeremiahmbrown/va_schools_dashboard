#!/usr/bin/env python3
"""Stage checked ArcGIS, GeoJSON, or shapefile polygons for one division.

The tracked spec fixes source URLs, record counts, and year fields. Raw files
are immutable and ignored by Git. A new manifest becomes active only after
all layers have passed download and geometry checks.
"""

import argparse
import hashlib
import io
import json
import os
import tempfile
import urllib.parse
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path

from osgeo import ogr, osr

ROOT = Path(__file__).resolve().parents[1]
PAGE_SIZE = 500
LEVELS = {"Elementary", "Middle", "High"}


def get_json(url):
    request = urllib.request.Request(url, headers={"User-Agent": "va-schools-dashboard/zone-research"})
    with urllib.request.urlopen(request, timeout=60) as response:
        data = json.load(response)
    if not isinstance(data, dict) or "error" in data:
        raise ValueError(f"Invalid source response from {url}: {data.get('error') if isinstance(data, dict) else type(data)}")
    return data


def fetch_shapefile_zip(url, member, id_field):
    request = urllib.request.Request(url, headers={"User-Agent": "va-schools-dashboard/zone-research"})
    with urllib.request.urlopen(request, timeout=60) as response:
        archive = response.read()
    with zipfile.ZipFile(io.BytesIO(archive)) as zipped:
        if member not in zipped.namelist() or not member.lower().endswith(".shp"):
            raise ValueError(f"Missing reviewed shapefile member in {url}: {member}")
        if zipped.testzip() is not None:
            raise ValueError(f"Corrupt shapefile archive: {url}")
    with tempfile.TemporaryDirectory() as directory:
        archive_path = Path(directory) / "source.zip"
        archive_path.write_bytes(archive)
        dataset = ogr.Open(f"/vsizip/{archive_path}/{member}")
        if dataset is None:
            raise ValueError(f"Cannot open shapefile: {url}")
        source = dataset.GetLayer()
        source_crs = source.GetSpatialRef()
        if source_crs is None:
            raise ValueError(f"Shapefile has no CRS: {url}")
        source_crs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
        target_crs = osr.SpatialReference()
        target_crs.ImportFromEPSG(4326)
        target_crs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
        transform = osr.CoordinateTransformation(source_crs, target_crs)
        features = []
        for record in source:
            properties = record.items()
            if not isinstance(properties.get(id_field), str) or not properties[id_field]:
                raise ValueError(f"Missing shapefile identifier {id_field}: {url}")
            geometry = record.GetGeometryRef()
            if geometry is None:
                raise ValueError(f"Empty shapefile geometry: {url}")
            geometry = geometry.Clone()
            geometry.Transform(transform)
            features.append({"type": "Feature", "properties": properties,
                             "geometry": json.loads(geometry.ExportToJson())})
        dataset = None
    ids = [feature["properties"][id_field] for feature in features]
    if len(ids) != len(set(ids)):
        raise ValueError(f"Duplicate shapefile identifiers: {url}")
    features.sort(key=lambda feature: feature["properties"][id_field])
    return features, archive


def query(url, parameters):
    return get_json(url + "/query?" + urllib.parse.urlencode(parameters))


def fetch_arcgis(url, expected, where="1=1", page_size_cap=PAGE_SIZE):
    metadata = get_json(url + "?f=json")
    server_limit = metadata.get("maxRecordCount")
    if not isinstance(server_limit, int) or server_limit < 1:
        raise ValueError(f"ArcGIS layer has no usable page limit: {url}")
    page_size = min(page_size_cap, server_limit)
    inventory = query(url, {"where": where, "returnIdsOnly": "true", "f": "json"})
    field = inventory.get("objectIdFieldName")
    ids = inventory.get("objectIds")
    if (not field or not isinstance(ids, list) or len(ids) != expected
            or len(set(ids)) != expected or not all(isinstance(i, int) for i in ids)):
        raise ValueError(f"ArcGIS ID inventory changed: {url}")
    sorted_ids = sorted(ids)
    features = []
    for start in range(0, expected, page_size):
        page_ids = sorted_ids[start:start + page_size]
        page = query(url, {
            "objectIds": ",".join(map(str, page_ids)), "outFields": "*",
            "outSR": "4326", "f": "geojson",
        })
        rows = page.get("features")
        if page.get("type") != "FeatureCollection" or not isinstance(rows, list):
            raise ValueError(f"Invalid ArcGIS GeoJSON page: {url}")
        returned = [feature.get("properties", {}).get(field) for feature in rows]
        if len(rows) != len(page_ids) or set(returned) != set(page_ids):
            raise ValueError(f"Incomplete ArcGIS ID page: {url}")
        features.extend(sorted(rows, key=lambda feature: feature["properties"][field]))
    return features, field


def coordinates(geometry):
    polygons = [geometry["coordinates"]] if geometry["type"] == "Polygon" else geometry["coordinates"]
    for polygon in polygons:
        for ring in polygon:
            yield from ring


def check_features(features, layer):
    if len(features) != layer["expected_count"]:
        raise ValueError(f"Feature count changed for {layer['source_url']}: {len(features)}")
    names = []
    years = set()
    for feature in features:
        properties = feature.get("properties") or {}
        geometry = feature.get("geometry") or {}
        name = properties.get(layer["name_field"])
        if not isinstance(name, str) or not name.strip():
            raise ValueError(f"Missing school name in {layer['source_url']}")
        if geometry.get("type") not in ("Polygon", "MultiPolygon") or not geometry.get("coordinates"):
            raise ValueError(f"Non-polygon or empty geometry in {layer['source_url']}")
        if not all(len(p) >= 2 and -84 < p[0] < -74 and 36 < p[1] < 40 for p in coordinates(geometry)):
            raise ValueError(f"Coordinates outside Virginia WGS84 bounds: {layer['source_url']}")
        names.append(name)
        if layer.get("year_field"):
            years.add(str(properties.get(layer["year_field"])))
    if layer.get("year_field") and sorted(years) != layer.get("expected_year_values"):
        raise ValueError(f"School year changed for {layer['source_url']}: {sorted(years)}")
    return sorted(set(names)), sorted(years)


def validate_spec(spec):
    division_id = spec.get("division_id")
    layers = spec.get("layers")
    if not isinstance(division_id, str) or len(division_id) != 7 or not division_id.isdigit():
        raise ValueError("Spec needs a seven-digit division ID")
    if (not isinstance(layers, list) or not layers
            or not spec.get("official_evidence_url", "").startswith("https://")
            or not spec.get("source_owner") or not spec.get("source_label")
            or not spec.get("popup_note") or not isinstance(spec.get("expected_school_count"), int)
            or spec["expected_school_count"] < 1
            or not isinstance(spec.get("max_same_level_overlap_m2"), (int, float))
            or spec["max_same_level_overlap_m2"] < 0):
        raise ValueError("Spec needs layers and official evidence")
    if spec["max_same_level_overlap_m2"] > 0 and not spec.get("overlap_review_note"):
        raise ValueError("A positive overlap limit needs a reviewed reason")
    for layer in layers:
        if (layer.get("level") not in LEVELS or layer.get("source_type") not in ("arcgis", "geojson", "shapefile_zip")
                or not layer.get("source_url", "").startswith("https://")
                or not layer.get("name_field") or not isinstance(layer.get("expected_count"), int)
                or layer["expected_count"] <= 0):
            raise ValueError(f"Invalid source layer: {layer}")
        if layer.get("year_field") and not layer.get("expected_year_values"):
            raise ValueError("Year field requires explicit expected values")
        if layer["source_type"] == "shapefile_zip" and (
                not layer.get("shapefile_member", "").lower().endswith(".shp")
                or not layer.get("object_id_field")):
            raise ValueError("Shapefile ZIP needs a member and object_id_field")
        if not isinstance(layer.get("where", "1=1"), str) or not layer.get("where", "1=1").strip():
            raise ValueError("ArcGIS filter must be a nonempty string")
    return division_id


def stage(division_id):
    spec_path = ROOT / "data-raw" / "zone_import_specs" / f"{division_id}.json"
    spec = json.loads(spec_path.read_text(encoding="utf-8"))
    if validate_spec(spec) != division_id:
        raise ValueError("Spec division ID does not match request")
    base = ROOT / "data-raw" / "input" / "zones" / division_id
    base.mkdir(parents=True, exist_ok=True)
    manifest_path = ROOT / "data-raw" / "zone_source_manifests" / f"{division_id}.json"
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    manifest = {
        "division_id": division_id,
        "source_owner": spec.get("source_owner", ""),
        "official_evidence_url": spec["official_evidence_url"],
        "retrieved_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "spec_file": str(spec_path.relative_to(ROOT)).replace("\\", "/"),
        "layers": [],
    }
    with tempfile.TemporaryDirectory(dir=base) as temporary:
        work = Path(temporary)
        hashes = []
        for index, layer in enumerate(spec["layers"]):
            url = layer["source_url"]
            if layer["source_type"] == "arcgis":
                features, object_id_field = fetch_arcgis(url, layer["expected_count"], layer.get("where", "1=1"))
                source_url = url + "/query?" + urllib.parse.urlencode({
                    "where": layer.get("where", "1=1"), "outFields": "*", "outSR": "4326", "f": "geojson",
                })
            elif layer["source_type"] == "shapefile_zip":
                object_id_field = layer["object_id_field"]
                features, archive = fetch_shapefile_zip(
                    url, layer["shapefile_member"], object_id_field)
                source_url = url
                archive_filename = f"{index:02d}_{layer['level'].lower()}.zip"
                (work / archive_filename).write_bytes(archive)
            else:
                data = get_json(url)
                if data.get("type") != "FeatureCollection" or not isinstance(data.get("features"), list):
                    raise ValueError(f"Invalid direct GeoJSON: {url}")
                features = data["features"]
                object_id_field = layer.get("object_id_field")
                if not object_id_field:
                    raise ValueError("Direct GeoJSON needs an object_id_field")
                ids = [f.get("properties", {}).get(object_id_field) for f in features]
                if None in ids or len(set(ids)) != len(ids):
                    raise ValueError(f"Missing or duplicate direct GeoJSON IDs: {url}")
                features.sort(key=lambda f: str(f["properties"][object_id_field]))
                source_url = url
            names, years = check_features(features, layer)
            raw = json.dumps({"type": "FeatureCollection", "features": features},
                             ensure_ascii=False, separators=(",", ":")).encode("utf-8")
            digest = hashlib.sha256(raw).hexdigest()
            hashes.append(digest)
            filename = f"{index:02d}_{layer['level'].lower()}.geojson"
            (work / filename).write_bytes(raw)
            manifest["layers"].append({
                "level": layer["level"], "source_type": layer["source_type"],
                "layer_url": url, "url": source_url,
                "where": layer.get("where", "1=1"),
                "file": "", "sha256": digest, "feature_count": len(features),
                "name_field": layer["name_field"], "object_id_field": object_id_field,
                "school_year_values": years, "school_names": names,
                "retrieval_method": "all object IDs then paged GeoJSON by objectIds" if layer["source_type"] == "arcgis" else "direct GeoJSON",
            })
            if layer["source_type"] == "shapefile_zip":
                manifest["layers"][-1].update({
                    "retrieval_method": "official shapefile ZIP converted to WGS84 GeoJSON with GDAL",
                    "shapefile_member": layer["shapefile_member"],
                    "archive_file": "", "archive_sha256": hashlib.sha256(archive).hexdigest(),
                })
        run_id = hashlib.sha256("".join(hashes).encode("ascii")).hexdigest()[:16]
        run_dir = base / run_id
        if not run_dir.exists():
            work.rename(run_dir)
        else:
            for index, row in enumerate(manifest["layers"]):
                filename = f"{index:02d}_{row['level'].lower()}.geojson"
                if hashlib.sha256((run_dir / filename).read_bytes()).hexdigest() != row["sha256"]:
                    raise ValueError(f"Existing raw stage differs: {run_dir / filename}")
        for index, row in enumerate(manifest["layers"]):
            filename = f"{index:02d}_{row['level'].lower()}.geojson"
            row["file"] = str((run_dir / filename).relative_to(ROOT)).replace("\\", "/")
            if row["source_type"] == "shapefile_zip":
                archive_filename = f"{index:02d}_{row['level'].lower()}.zip"
                archive_path = run_dir / archive_filename
                if hashlib.sha256(archive_path.read_bytes()).hexdigest() != row["archive_sha256"]:
                    raise ValueError(f"Existing source archive differs: {archive_path}")
                row["archive_file"] = str(archive_path.relative_to(ROOT)).replace("\\", "/")
    temporary_manifest = manifest_path.with_suffix(".json.tmp")
    temporary_manifest.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    os.replace(temporary_manifest, manifest_path)
    print(f"Staged {sum(row['feature_count'] for row in manifest['layers'])} features for {division_id}: {manifest_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("division_id")
    args = parser.parse_args()
    stage(args.division_id)

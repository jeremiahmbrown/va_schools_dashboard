#!/usr/bin/env python3
"""Compare Campbell's public source with the map linked from CCPS 2026-27 enrollment."""
import requests

ROOT = "https://gis.co.campbell.va.us/arcgis/rest/services/"
LAYERS = {
    "source": ROOT + "Public_Data/School_Jurisdictions/MapServer/10",
    "elementary_locator": ROOT + "External/E_911_Address_Search/MapServer/16",
    "secondary_locator": ROOT + "External/E_911_Address_Search/MapServer/17",
}
FIELDS = ("OBJECTID", "ELEMENTARY", "MIDDLE", "HIGH")
EXTENT_FIELDS = ("xmin", "ymin", "xmax", "ymax")
EXPECTED_IDS = set(range(316, 325))


def query(url, params):
    response = requests.get(url, params={"f": "json", **params}, timeout=25)
    response.raise_for_status()
    data = response.json()
    if "error" in data:
        raise RuntimeError(f"{url}: {data['error']}")
    return data


def inspect(url):
    data = query(url + "/query", {"where": "1=1", "outFields": ",".join(FIELDS),
                                  "returnGeometry": "false"})
    rows = {f["attributes"]["OBJECTID"]: tuple(f["attributes"].get(k) for k in FIELDS)
            for f in data["features"]}
    if set(rows) != EXPECTED_IDS or len(data["features"]) != len(rows):
        raise ValueError(f"{url}: object IDs changed")
    extents = {}
    for object_id in sorted(rows):
        result = query(url + "/query", {"where": f"OBJECTID={object_id}",
                                         "returnExtentOnly": "true"})
        extent = result["extent"]
        extents[object_id] = tuple(extent[k] for k in EXTENT_FIELDS)
    return rows, extents


def main():
    layers = {name: inspect(url) for name, url in LAYERS.items()}
    for name in ("elementary_locator", "secondary_locator"):
        if layers[name] != layers["source"]:
            raise ValueError(f"{name} labels or extents differ from source")
    rows = layers["source"][0]
    if rows[316][1:] != ("Yellow Branch", "Brookville", "Brookville"):
        raise ValueError("Yellow Branch Brookville piece changed")
    if rows[323][1:] != ("Yellow Branch", "Rustburg", "Rustburg"):
        raise ValueError("Yellow Branch Rustburg piece changed")
    print("Campbell locator check passed: 9 IDs, school fields, and per-ID extents match both locator layers.")
    print("The locator does not return full geometry; exact polygon equality is not established by this check.")


if __name__ == "__main__":
    main()

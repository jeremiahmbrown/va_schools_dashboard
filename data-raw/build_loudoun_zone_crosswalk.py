#!/usr/bin/env python3
"""Build the Loudoun code-to-school crosswalk from a frozen LCPS site table."""

import csv
import json
import re
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIVISION_ID = "5102250"
SITE_URL = "https://services3.arcgis.com/PgITX2Sy39aYHLDV/ArcGIS/rest/services/LCPSsitesAGOL/FeatureServer/0"
SITE_FILE = ROOT / "data-raw" / "zone_reference" / "5102250_sites.json"
MANIFEST = ROOT / "data-raw" / "zone_source_manifests" / "5102250.json"
OUTPUT = ROOT / "data-raw" / "zone_school_crosswalks" / "5102250.csv"
FIELDS = ["division_id", "level", "source_name", "school_id", "school_name", "disposition", "note"]
NO_POLYGON = {
    "Henrietta Lacks Elementary": "LCPS says Henrietta Lacks serves K–2 and Hovatter serves grades 3–5 as one school community. The 2026–27 LCPS standard layer has a Hovatter polygon but no separate Henrietta Lacks polygon; do not infer an address assignment from this shared campus.",
    "Hillsboro Charter Academy": "Charter school has no separate polygon in the LCPS standard layer.",
    "Middleburg Community Charter": "Charter school has no separate polygon in the LCPS standard layer.",
    "W.O. Robey High": "Alternative high school has no separate polygon in the LCPS standard layer.",
    "Watson Mountain Middle": "LCPS says Watson Mountain serves grades 6–7 in 2026–27 while Brambleton serves grade 8 in a temporary arrangement; no separate Watson Mountain polygon appears in this layer. Do not use the Brambleton polygon as a Watson Mountain address assignment.",
}


def normalized(name):
    name = name.upper().replace("ELEMENTARY", "ES").replace("MIDDLE", "MS")
    name = name.replace("HIGH", "HS").replace("SCHOOL", "").replace(" AND ", " & ")
    return re.sub(r"[^A-Z0-9]", "", name)


def site_records():
    if not SITE_FILE.exists():
        params = urllib.parse.urlencode({"where": "1=1", "outFields": "SCH_CODE,NAME,CLASS", "returnGeometry": "false", "f": "json"})
        url = SITE_URL + "/query?" + params
        with urllib.request.urlopen(url, timeout=60) as response:
            data = json.load(response)
        if "error" in data or len(data.get("features", [])) != 121:
            raise ValueError("LCPS school-site inventory changed or did not download completely")
        records = sorted((f["attributes"] for f in data["features"]), key=lambda r: (str(r["SCH_CODE"]), r["NAME"]))
        SITE_FILE.parent.mkdir(parents=True, exist_ok=True)
        SITE_FILE.write_text(json.dumps({"source_url": url, "retrieved_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"), "records": records}, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    saved = json.loads(SITE_FILE.read_text(encoding="utf-8"))
    if not saved["source_url"].startswith(SITE_URL + "/query?") or len(saved["records"]) != 121:
        raise ValueError("Reviewed LCPS school-site reference is incomplete")
    sites = {r["SCH_CODE"]: r for r in saved["records"] if r["SCH_CODE"]}
    if len(sites) != 120:
        raise ValueError("LCPS school-site codes changed")
    return sites


def build():
    sites = site_records()
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    if manifest["division_id"] != DIVISION_ID or sum(r["feature_count"] for r in manifest["layers"]) != 93:
        raise ValueError("Loudoun zone manifest changed")
    with (ROOT / "app" / "data" / "schools.csv").open(newline="", encoding="utf-8-sig") as handle:
        schools = [s for s in csv.DictReader(handle) if s["division_id"] == DIVISION_ID]
    if len(schools) != 98:
        raise ValueError("Loudoun school snapshot changed")
    rows, linked = [], set()
    for layer in manifest["layers"]:
        level = layer["level"]
        features = json.loads((ROOT / layer["file"]).read_text(encoding="utf-8"))["features"]
        if len(features) != layer["feature_count"]:
            raise ValueError(f"Incomplete {level} source file")
        for feature in features:
            code = feature["properties"][layer["name_field"]]
            key = (level, code)
            site = sites.get(code)
            if site is None or site["CLASS"] != level.upper():
                raise ValueError(f"No LCPS school-site code for {key}")
            matches = [s for s in schools if normalized(s["school_name"]) == normalized(site["NAME"])
                       and (s["school_level"] == level or s["school_level"] == "Combined" and level == "High")]
            if len(matches) != 1:
                raise ValueError(f"Review LCPS code {key}: {site['NAME']} -> {matches}")
            school = matches[0]
            if school["school_id"] in linked:
                raise ValueError(f"Duplicate snapshot school: {school['school_id']}")
            linked.add(school["school_id"])
            note = f"LCPS school-site code {code}: {site['NAME']}."
            if school["school_level"] != level:
                note += f" Snapshot level is {school['school_level']}; LCPS zone level is {level}."
            rows.append({"division_id": DIVISION_ID, "level": level, "source_name": code,
                         "school_id": school["school_id"], "school_name": school["school_name"],
                         "disposition": "mapped", "note": note})
    if len(linked) != 93:
        raise ValueError("Loudoun matched count changed")
    unlinked = {s["school_name"].strip(): s for s in schools if s["school_id"] not in linked}
    if set(unlinked) != set(NO_POLYGON):
        raise ValueError(f"Review schools without polygons: {set(unlinked) ^ set(NO_POLYGON)}")
    for name, school in sorted(unlinked.items()):
        rows.append({"division_id": DIVISION_ID, "level": school["school_level"],
                     "source_name": "", "school_id": school["school_id"],
                     "school_name": school["school_name"], "disposition": "no_neighborhood_zone",
                     "note": NO_POLYGON[name]})
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {len(rows)} decisions: {len(linked)} linked, {len(unlinked)} without a polygon")


if __name__ == "__main__":
    build()

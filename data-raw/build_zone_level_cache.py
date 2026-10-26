"""Build checked, ready-to-render level files for division zone sources.

Run after the City or County zone build. The app uses these files only while
source.md5 matches the combined division file; otherwise it filters that file.
"""

import hashlib
import json
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ZONES = ROOT / "app" / "data" / "zones"
LEVELS = ("Elementary", "Middle", "High")
EXPECTED = {
    "5100270": (20, 6, 3),  # Arlington APS locator; source year field is stale
    "5100360": (13, 3, 3),  # Bedford County; primary is an old SABS fallback
    "5100420": (7, 2, 2),  # Botetourt County
    "5102520": (11, 10, 10),  # Montgomery County
    "5103300": (17, 5, 2),  # Roanoke City
    "5103330": (20, 20, 20),  # Roanoke County
    "5103460": (4, 1, 1),  # Salem City; middle/high are old SABS fallbacks
}


def build_one(division_id, expected_counts):
    source = ZONES / "division" / f"{division_id}.json"
    source_bytes = source.read_bytes()
    data = json.loads(source_bytes)
    features = data.get("features")
    if data.get("type") != "FeatureCollection" or not isinstance(features, list):
        raise ValueError(f"Invalid division zone file: {source}")
    if len(features) != sum(expected_counts):
        raise ValueError(f"Unexpected zone count for {division_id}: {len(features)}")

    groups = {level: [] for level in LEVELS}
    source_ids = []
    for feature in features:
        props = feature.get("properties") or {}
        level = props.get("level")
        if level not in groups or props.get("division_id") != division_id:
            raise ValueError(f"Unexpected level or division in {source}")
        if not feature.get("geometry") or not feature.get("id"):
            raise ValueError(f"Missing geometry or zone ID in {source}")
        groups[level].append(feature)
        source_ids.append(feature["id"])
    if len(set(source_ids)) != len(source_ids):
        raise ValueError(f"Duplicate zone ID in {source}")
    if tuple(len(groups[level]) for level in LEVELS) != expected_counts:
        raise ValueError(f"Unexpected level counts for {division_id}")

    destination = ZONES / "level_cache" / division_id
    destination.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=destination) as workdir:
        work = Path(workdir)
        for level in LEVELS:
            output = {"type": "FeatureCollection", "features": groups[level]}
            (work / f"{level}.json").write_text(
                json.dumps(output, ensure_ascii=False, separators=(",", ":")) + "\n",
                encoding="utf-8",
            )
        # Git may check out JSON with CRLF on Windows. Hash normalized bytes so
        # both checkouts can use the same checked cache.
        normalized_source = source_bytes.replace(b"\r\n", b"\n")
        (work / "source.md5").write_text(hashlib.md5(normalized_source).hexdigest() + "\n")
        for level in LEVELS:
            (work / f"{level}.json").replace(destination / f"{level}.json")
        # Publish the hash last. A partial update cannot pass the app's check.
        (work / "source.md5").replace(destination / "source.md5")
    print(f"Built {division_id}: " + ", ".join(f"{level} {len(groups[level])}" for level in LEVELS))


def main():
    for division_id, counts in EXPECTED.items():
        build_one(division_id, counts)


if __name__ == "__main__":
    main()

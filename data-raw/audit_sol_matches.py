"""List unmatched annual SOL school codes with possible mapped names."""

import csv
import difflib
import gzip
import re
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def key(value):
    return re.sub(r"\s+", " ", re.sub(r"[^A-Z0-9 ]+", " ", value.upper().replace("&", " AND "))).strip()


lookup = {}
by_division = defaultdict(list)
with (ROOT / "app/data/schools.csv").open(newline="", encoding="utf-8") as handle:
    for row in csv.DictReader(handle):
        division = key(re.sub(r" Public Schools$", "", row["division_name"], flags=re.I))
        school = key(row["school_name"])
        lookup[division, school] = row
        by_division[division].append(row)

unmatched = {}
with gzip.open(ROOT / "data-raw/annual_sol_school.csv.gz", "rt", newline="", encoding="utf-8") as handle:
    for row in csv.DictReader(handle):
        division = key(row["division_name"])
        school = key(row["school_name"])
        if (division, school) not in lookup:
            unmatched[row["division_code"], row["school_code"]] = row

print("Division code,School code,Source division,Source school,Possible map school,Similarity")
for (division_code, school_code), row in sorted(unmatched.items()):
    division = key(row["division_name"])
    candidates = by_division[division]
    matches = difflib.get_close_matches(key(row["school_name"]), [key(x["school_name"]) for x in candidates], n=1, cutoff=0)
    match = matches[0] if matches else ""
    score = difflib.SequenceMatcher(None, key(row["school_name"]), match).ratio() if match else 0
    suggestion = next((x["school_name"] for x in candidates if key(x["school_name"]) == match), "")
    print(f'{division_code},{school_code},"{row["division_name"]}","{row["school_name"]}","{suggestion}",{score:.2f}')

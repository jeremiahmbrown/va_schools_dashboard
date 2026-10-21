"""Check historical Fall Membership files before adding them to the snapshot."""

import argparse
import csv
import gzip
import re
from collections import Counter, defaultdict
from pathlib import Path


FILES = {
    "gender_2022": "fall_membership_2022_gender_grade.csv",
    "race_2022": "fall_membership_2022_race.csv",
    "gender_multi": "fall_membership__multiyear_gender_grade.csv",
    "race_multi": "fall_membership_multiyear_race.csv",
}
BASE = ["School Year", "Division Number", "Division Name", "School Number", "School Name"]


def count(value):
    text = value.strip().replace(",", "")
    return int(text) if text.isdecimal() else None


def normalized(text):
    return re.sub(r"[^a-z0-9]", "", text.lower().removesuffix(" public schools"))


def read(path, group):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        fields = reader.fieldnames
        expected = BASE + [group, "FT Count", "PT Count", "Total Count"]
        if fields != expected:
            raise ValueError(f"{path.name}: expected {expected}, got {fields}")
        rows = list(reader)
    keys = Counter(tuple(row[col].strip() for col in
                         ["School Year", "Division Number", "School Number", group])
                   for row in rows)
    duplicates = sum(amount - 1 for amount in keys.values() if amount > 1)
    totals = defaultdict(int)
    missing = Counter()
    invalid = 0
    for row in rows:
        key = tuple(row[col].strip() for col in
                    ["School Year", "Division Number", "School Number"])
        total = count(row["Total Count"])
        ft = count(row["FT Count"])
        pt = count(row["PT Count"])
        if total is None:
            missing[row["School Year"]] += 1
            continue
        if (ft or 0) + (pt or 0) != total:
            invalid += 1
        totals[key] += total
    years = Counter(row["School Year"] for row in rows)
    categories = Counter((row["School Year"], row[group]) for row in rows)
    print(path.name, "rows", len(rows), "years", dict(sorted(years.items())),
          "duplicate grain", duplicates, "blank total", dict(missing),
          "FT+PT mismatch", invalid, "categories", dict(sorted(categories.items())))
    return rows, totals


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("directory", type=Path)
    args = parser.parse_args()
    found = {}
    for role, filename in FILES.items():
        found[role] = read(args.directory / filename,
                           "Gender" if role.startswith("gender") else "Race")
    for suffix in ("2022", "multi"):
        gender = found[f"gender_{suffix}"][1]
        race = found[f"race_{suffix}"][1]
        keys = gender.keys() | race.keys()
        mismatches = [(key, gender.get(key), race.get(key)) for key in keys
                      if gender.get(key) != race.get(key)]
        print(suffix, "school-years", len(keys), "matched totals",
              len(keys) - len(mismatches), "mismatches", len(mismatches),
              "examples", mismatches[:5])
        print(suffix, "mismatch years", dict(Counter(key[0] for key, _, _ in mismatches)),
              "missing gender", sum(g is None for _, g, _ in mismatches),
              "missing race", sum(r is None for _, _, r in mismatches),
              "race below gender", sum(r is not None and g is not None and r < g
                                       for _, g, r in mismatches),
              "zero totals", sum(g == 0 or r == 0 for _, g, r in mismatches))
    for kind in ("gender", "race"):
        subset = [row for row in found[f"{kind}_multi"][0]
                  if row["School Year"] == "2021-2022"]
        group = "Gender" if kind == "gender" else "Race"
        order = BASE + [group, "FT Count", "PT Count", "Total Count"]
        canonical = lambda rows: {tuple(row[col].strip() for col in order)
                                  for row in rows}
        print(kind, "2022 standalone equals multiyear subset:",
              canonical(subset) == canonical(found[f"{kind}_2022"][0]))

    root = Path(__file__).resolve().parents[1]
    sqp = {}
    for path in (root / "data-raw/historical/vdoe_enrollment_baseline.csv.gz",
                 root / "data-raw/input/vdoe_enrollment.csv"):
        opener = gzip.open if path.suffix == ".gz" else open
        with opener(path, "rt", encoding="utf-8-sig", newline="") as handle:
            header = next(line for line in handle if line.startswith("Year,"))
            for row in csv.DictReader([header, *handle]):
                if row["Level"] != "SCH" or row["Grade"].strip():
                    continue
                key = (int(row["Year"].split("-")[-1].strip()),
                       normalized(row["Division"]), normalized(row["School"]))
                sqp[key] = int(row["Count"])
    race_rows = found["race_multi"][0]
    names = {}
    for row in race_rows:
        key = (row["School Year"], row["Division Number"], row["School Number"])
        names[key] = (normalized(row["Division Name"]),
                      normalized(row["School Name"]))
    for year in (2023, 2024):
        comparisons = []
        for key, total in found["race_multi"][1].items():
            if int(key[0].split("-")[-1]) != year:
                continue
            name = names[key]
            other = sqp.get((year, *name))
            comparisons.append((key, total, other))
        joined = [item for item in comparisons if item[2] is not None]
        mismatch = [item for item in joined if item[1] != item[2]]
        print("race versus SQP enrollment", year, "source schools", len(comparisons),
              "joined", len(joined), "mismatch", len(mismatch),
              "examples", mismatch[:5])


if __name__ == "__main__":
    main()

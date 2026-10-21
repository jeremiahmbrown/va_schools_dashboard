"""Print reportable school and division coverage from the frozen snapshot."""

import argparse
import csv
from collections import defaultdict
from pathlib import Path


DATA = Path(__file__).resolve().parents[1] / "app" / "data"


def reportable(row):
    return row["value"] not in ("", "NA") and row["suppressed"].lower() != "true"


counts_by_grain = {}
for grain in ("school", "division"):
    counts = defaultdict(lambda: [0, 0])
    with (DATA / f"{grain}_metrics.csv").open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            key = (row["metric_id"], row["year"])
            counts[key][0] += 1
            counts[key][1] += reportable(row)
    counts_by_grain[grain] = counts

parser = argparse.ArgumentParser()
parser.add_argument("--markdown", action="store_true", help="print a full coverage table")
args = parser.parse_args()

if args.markdown:
    with (DATA / "metric_defs.csv").open(newline="", encoding="utf-8") as handle:
        definitions = list(csv.DictReader(handle))
    print("| Measure | 2022 | 2023 | 2024 | 2025 | 2026 |")
    print("| --- | ---: | ---: | ---: | ---: | ---: |")
    for definition in definitions:
        metric = definition["metric_id"]
        cells = []
        for year in map(str, range(2022, 2027)):
            school = counts_by_grain["school"].get((metric, year))
            division = counts_by_grain["division"].get((metric, year))
            cells.append(f"{school[1]}/{division[1]}" if school and division else "—")
        print(f"| {definition['label_short']} | {' | '.join(cells)} |")
else:
    for grain, counts in counts_by_grain.items():
        print(f"{grain}: metric, year, rows, reportable, unavailable")
        for (metric, year), (rows, available) in sorted(counts.items()):
            print(f"{metric},{year},{rows},{available},{rows - available}")

"""Print reportable school and division coverage from the frozen snapshot."""

import csv
from collections import defaultdict
from pathlib import Path


DATA = Path(__file__).resolve().parents[1] / "app" / "data"


def reportable(row):
    return row["value"] not in ("", "NA") and row["suppressed"].lower() != "true"


for grain in ("school", "division"):
    counts = defaultdict(lambda: [0, 0])
    with (DATA / f"{grain}_metrics.csv").open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            key = (row["metric_id"], row["year"])
            counts[key][0] += 1
            counts[key][1] += reportable(row)
    print(f"{grain}: metric, year, rows, reportable, unavailable")
    for (metric, year), (rows, available) in sorted(counts.items()):
        print(f"{metric},{year},{rows},{available},{rows - available}")

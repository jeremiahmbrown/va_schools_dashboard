import csv
import json
from pathlib import Path
import subprocess
import sys

root = Path(sys.argv[1])
schools = {r['school_id']: r for r in csv.DictReader((root / 'app/data/schools.csv').open())}
paths = subprocess.check_output(['git', '-C', str(root), 'diff', '--name-only', 'c773376', '--', 'app/data/zones/sabs'], text=True).splitlines()
count = 0
for path in paths:
    old = json.loads(subprocess.check_output(['git', '-C', str(root), 'show', 'c773376:' + path]))
    new = json.loads((root / path).read_text())
    assert len(old['features']) == len(new['features'])
    for a, b in zip(old['features'], new['features']):
        changed = b['properties'].get('app_school_id')
        if changed != a['properties'].get('app_school_id'):
            assert not a['properties'].get('app_school_id')
            assert changed == a['properties']['nces_school_id']
            assert changed in schools
            assert schools[changed]['division_id'] == a['properties']['division_id']
            assert a['properties']['source'] == 'sabs'
            del b['properties']['app_school_id']
            count += 1
        assert a == b, path
    assert old == new, path
assert count == 247, count
print(f'Validated {count} added NCES-ID links in {len(paths)} files; all other feature data unchanged.')

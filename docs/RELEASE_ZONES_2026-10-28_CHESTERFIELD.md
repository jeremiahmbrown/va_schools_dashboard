# Chesterfield zone release candidate — 2026-10-28

Status: candidate only. Public deployment and live checks are pending the release-scope decision.

## Exact app change

- Add the Chesterfield County division GeoJSON and three level caches.
- Add a Chesterfield source note to the map Filters panel.
- Preserve the frozen school-data snapshot and all other division zone assets.

The County Open GIS Data service supplies 41 elementary, 13 middle, and 10 high areas. The strict crosswalk links 63 of 65 frozen schools. Deep Creek Elementary opened in 2026–27, but is absent from the frozen school data; its official area stays visible without a data link. Carver College and Career Academy and Chesterfield Virtual School have no separate standard neighborhood area.

## Source limits

The source has no per-feature effective school year. The map note directs families to CCPS Find My School for an official address check. The County portal terms permit copying, modifying, and distributing GIS data with attribution. The source polygons retain measured same-level overlaps; the largest elementary overlap is Old Hundred and Deep Creek. No address assignment is inferred from overlaps.

## Checks before release

Run the importer regression, zone/cache check, snapshot check, statewide audit, and diff check in this clean worktree. Inspect the source manifest, crosswalk, geometry report, and candidate bundle. The prior local map review showed all 64 Chesterfield paths and the source note.

## Rollback and live checks

The preceding production bundle is 12740245, from commit cd3b405. After deployment, inspect the existing public URL at desktop and 390 by 844 mobile size. Select Chesterfield and each school level, select a mapped school, and confirm that Deep Creek is shown without a school-data link. Confirm that other divisions still render.

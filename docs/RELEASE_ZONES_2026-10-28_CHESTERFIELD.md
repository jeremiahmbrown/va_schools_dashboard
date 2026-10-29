# Chesterfield zone release candidate — 2026-10-28

Status: deployed 2026-10-28 to the existing public app ID 16676824. Bundle 12752849 was built from source commit 9bfc8ad. GitHub master was pushed to that commit before deployment.

## Exact app change

- Add the Chesterfield County division GeoJSON and three level caches.
- Add a Chesterfield source note to the map Filters panel.
- Preserve the frozen school-data snapshot and all other division zone assets.

The County Open GIS Data service supplies 41 elementary, 13 middle, and 10 high areas. The strict crosswalk links 63 of 65 frozen schools. Deep Creek Elementary opened in 2026–27, but is absent from the frozen school data; its official area stays visible without a data link. Carver College and Career Academy and Chesterfield Virtual School have no separate standard neighborhood area.

## Source limits

The source has no per-feature effective school year. The map note directs families to CCPS Find My School for an official address check. The County portal terms permit copying, modifying, and distributing GIS data with attribution. The source polygons retain measured same-level overlaps; the largest elementary overlap is Old Hundred and Deep Creek. No address assignment is inferred from overlaps.

## Checks

The importer regression, zone/cache check, snapshot check, statewide audit, app syntax check, and Git diff check passed in the clean worktree. The importer reproduced the Chesterfield GeoJSON SHA-256 7953ae7ea95af8f59c76ad6675ddad7464c546a44662f1f7d7a3bc168fba70dd. The rsconnect file list contained 196 app files, including the Chesterfield division file and three level caches. The prior local map review showed all 64 Chesterfield paths and the source note.

## Recovery and live checks

The preceding production bundle is 12740245, from commit cd3b405. The deployment reported success at the existing public URL, and rsconnect recorded bundle 12752849. In an in-app browser, the dashboard loaded, the Chesterfield division profile opened, the attendance-zone layer enabled, and the County source note appeared in Filters. Selecting A.M. Davis Elementary loaded its school profile and map. This browser check used the available narrow viewport; a separate 390 by 844 and desktop review, each zone level, Deep Creek visual check, and other-division spot check remain pending. The local automated checks verified all 64 polygons and the source-only Deep Creek record.

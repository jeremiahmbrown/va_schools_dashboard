# School attendance-zone gap plan - updated 2026-11-08

The original baseline had 641 unmatched schools among 1,810 snapshot schools. Releases for Hanover, Virginia Beach, Henrico, Stafford, Alexandria, Norfolk, WJCC, Chesapeake, and Loudoun have added verified links. The refreshed production dashboard has 1,814 schools: 1,454 matched and 360 unmatched. These are school-to-polygon links, not household assignments.

Of the 360 unmatched schools, 227 have a link in the separate local research checkout. Each requires a release review of school IDs, school year, source rights, geometry, special programs, map labels, and live app behavior. A local link is not a release approval.

The other 133 have no verified local link in either the production or research checkout:

| Gap | Schools | Required work |
| --- | ---: | --- |
| No source file in the local division | 47 | Obtain adopted, dated, reusable polygons from Augusta (19), Tazewell (13), Giles (6), Southampton (6), and Greensville (3). Official site, GIS, and locator searches were attempted; source-request drafts remain unsent. |
| Only an incomplete older SABS file in the local division | 33 | Find a current official elementary/middle/high layer; check school identity and level before making a link. |
| Local GIS exists, but the school has no verified standard polygon | 53 | Check aliases, grade-pair and shared areas, current school IDs, magnets, virtual programs, and missing levels with the division. Do not assign a nearby polygon by name or point location alone. |

The local research snapshot has the same 1,814 school IDs and 1,677 links. Four schools linked in production are not linked in research: Thomas Eaton Middle and three Richmond specialty schools. This explains why the local unmatched total is 137 while this plan counts 133 without a link in either checkout.

For every candidate: verify the official adopted map and effective school year; record exact source URL, object IDs, manifest hash, and display permission; build an explicit NCES/VDOE school-ID crosswalk; check valid geometry, level coverage, gaps and same-level overlaps in a metre-based CRS; compare with official address locator samples; run importer, snapshot, zone/cache, and statewide audit checks; then verify the live school and source warning after deployment. Some schools may have no single neighborhood zone. Record them as source-only or source-not-available until the school division confirms an assignment rule.

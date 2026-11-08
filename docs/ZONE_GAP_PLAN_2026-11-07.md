# School attendance-zone gap plan - updated 2026-11-08

The production baseline before these releases had 641 unmatched schools among 1,810 snapshot schools: 509 already had a local candidate school-zone link, and 132 had no link even locally. The Hanover, Virginia Beach, Henrico, Stafford, Alexandria, Norfolk, and WJCC 2026-27 releases added 16, 55, 52, 18, 15, 29, and 16 net matches. Production now has 1,370 matched and 440 unmatched. These are school-to-polygon links, not verified household assignments.

Of the 440 still unmatched, 308 have a school-to-zone link in the separate local research checkout. Each division needs a release review against the production school IDs, school year, source rights, geometry, special programs, map labels, and a live app test. This does not mean all 308 are ready to publish. The Loudoun layer, for example, fails the current production snapshot check for Richard and Mildred Loving Elementary and Gum Spring Middle; the local snapshot has their new IDs. Other locally staged sources have unresolved dates, overlaps, incomplete levels, or reuse terms. Release one qualified division at a time, with the exact source year shown in the app.

The other 132 have no school link even in local research:

| Gap | Schools | Required work |
| --- | ---: | --- |
| No source file in the local division | 47 | Obtain adopted, dated, reusable polygons from Augusta (19), Tazewell (13), Giles (6), Southampton (6), and Greensville (3). Official site, GIS, and locator searches were attempted; source-request drafts remain unsent. |
| Only an incomplete older SABS file in the local division | 33 | Find a current official elementary/middle/high layer; check school identity and level before making a link. |
| Local GIS exists, but the school has no verified standard polygon | 52 | Check aliases, grade-pair and shared areas, current school IDs, magnets, virtual programs, and missing levels with the division. Do not assign a nearby polygon by name or point location alone. |

The local snapshot has 1,814 schools, four more than production: two Chesapeake and two Loudoun schools. Their local identity work must be reconciled with a deliberate snapshot release before their new school IDs can appear in production. Local audit: 1,677 linked and 137 unmatched. These totals use a different school denominator from production and must not be treated as live coverage.

For every candidate: verify the official adopted map and effective school year; record exact source URL, object IDs, manifest hash, and display permission; build an explicit NCES/VDOE school-ID crosswalk; check valid geometry, level coverage, gaps and same-level overlaps in a metre-based CRS; compare with official address locator samples; run importer, snapshot, zone/cache, and statewide audit checks; then verify the live school and source warning after deployment. Some schools may have no single neighborhood zone. Record them as source-only or source-not-available until the school division confirms an assignment rule.

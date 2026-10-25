# Unmatched school geography audit — 2026-10-24, updated 2026-10-25

The frozen snapshot has 18 VDOE schools without a name match in the 2024–25 NCES EDGE school geocode file. The app still excludes these schools from mapped school measures and shows its coverage warning. This audit did not add a point or an attendance boundary.

## Sources and method

The [2025–26 CCD preliminary school directory](https://nces.ed.gov/use-work/dataset/2025-26-common-core-data-ccd-preliminary-directory-files) provides current NCES IDs, state school IDs, names, and addresses. The [official ZIP](https://nces.ed.gov/ccd/data/zip/ccd_sch_029_2526_w_0a_050626.zip) had SHA-256 b5d8dc341ecdc85549ffa9ba5acf4c4ed4f2a7c4a78a5115bbd281ef3e23bc8. The VDOE [School Quality Profiles](https://schoolquality.virginia.gov/virginia-schools) give each school's current division number, school number, and street address. For every row below, the VDOE numbers form the CCD state ID as VA-{division:03d}-{division:03d}{school:04d}. All 18 state IDs match one 2025–26 CCD school row. This resolves the name-only identity questions in the earlier audit.

The saved [2024–25 EDGE school points](https://nces.ed.gov/programs/edge/geographic/schoollocations) were checked by NCES ID and street against the current VDOE profile and 2025–26 CCD directory. A matching ID alone does not prove that an older point marks the current campus. The EDGE page lists a 2025–26 tab, but its public school geocode download was commented out; the apparent ZIP URL returned 404 on 2026-10-24. The preliminary CCD directory has no point geometry. The snapshot's metric year 2026 means school year 2025–26; a VDOE profile name viewed in October 2026 can describe a later school arrangement.

## Disposition of the 18 schools

| VDOE school | VDOE state ID | 2025–26 NCES ID | 2024–25 point check | Disposition |
| --- | --- | --- | --- | --- |
| [Mountain View Upper Elementary](https://schoolquality.virginia.gov/schools/mountain-view-upper-elementary) | VA-002-0020150 | 510009002173 | Same ID, different street | Upper opened for 2026–27 at 1545 Founders Place. The mapped 2024–25 point is the former Mountain View Elementary building at 1757 Avon Street Extended, now Mountain View Primary. Keep the older point only in its historical context; do not assign it to Upper's new building. |
| Amelia County High School | VA-004-0040010 | 510018001828 | Same ID and street | The ID is already mapped as Amelia County High. Reconcile the two names before a crosswalk to avoid duplicate school records. |
| Charlottesville Middle | VA-104-1040100 | 510078000271 | Same ID and street | Candidate older point; earlier name was Buford Middle. |
| Sunrise Elementary | VA-104-1040070 | 510078000272 | Same ID and street | Candidate older point; earlier name was Burnley-Moran Elementary. |
| Tall Oaks Elementary | VA-104-1040010 | 510078000266 | Same ID and street | Candidate older point; earlier name was Johnson Elementary. |
| B.M. Williams Elementary | VA-136-1360035 | 510081003157 | No point for this ID | Keep unmapped until a checked current point is available. |
| Crestwood Elementary School | VA-136-1360045 | 510081003158 | No point for this ID | Keep unmapped until a checked current point is available. |
| Arnett Hills Elementary | VA-108-1080055 | 510111003154 | Same ID and street | Candidate older point; earlier name was Arnett Hills International Academy. |
| Mary Ellen Henderson Middle School | VA-109-1090252 | 510129000284 | Same ID and street | Candidate older point. |
| Meridian High School | VA-109-1090251 | 510129000283 | Same ID and street | Candidate older point. |
| Mount Daniel Elementary School | VA-109-1090240 | 510129002204 | Same ID and street | Candidate older point. |
| Oak Street Elementary School | VA-109-1090260 | 510129001844 | Same ID and street | Candidate older point. |
| Gladys West Elementary | VA-110-1100020 | 510151003155 | Same ID and street | Candidate older point; earlier name was Fredericksburg New Elementary. |
| Gum Spring Middle | VA-053-0530320 | 510225002450 | Same ID and street | Candidate older point; earlier name was Mercer Middle. |
| Richard and Mildred Loving Elementary | VA-053-0530091 | 510225002412 | Same ID and street | Candidate older point; earlier name was Frances Hazel Reid Elementary. |
| Virginia Virtual Academy at Manassas | VA-143-1430090 | 510236003159 | No point for this ID | Keep unmapped; decide if a physical school point is suitable for a virtual program. |
| [Woodbridge Elementary](https://schoolquality.virginia.gov/schools/woodbridge-elementary) | VA-075-0750550 | 510313001316 | Same ID, different street | Potomac View closed on 2026-06-30; Woodbridge opened for 2026–27 at 1550 Prince William Parkway. The mapped older point is Potomac View at 14601 Lamar Road. Keep it only in its historical context. The current PWCS school-site layer has a Woodbridge point at the new address. |
| Richmond Success Academy | VA-123-1233110 | 510324002307 | Same ID and street | Candidate older point; earlier name was Richmond Alternative School. |

Thirteen schools have a same-ID, same-street older point. Three have no older point for their current ID. Two have an older point at a different street. The 18 current VDOE names are excluded from the deployed name join, but three of their NCES IDs already appear in frozen schools.csv under older names: Mountain View Elementary, Amelia County High, and Potomac View Elementary. Their IDs have metric rows through 2026. The 2026 race source still uses those older names and VDOE school numbers 150, 10, and 550; its denominators of 759, 477, and 562 equal the respective current-name totals in the 2025–26 VDOE enrollment export. That VDOE export also applies the newer Mountain View Upper and Woodbridge names to 2023–24 enrollment, years before the new buildings opened. This is a retroactive name label, not proof that those buildings held those students. The older points may correctly describe the 2025–26 historical sites; they do not describe the 2026–27 sites.

[Albemarle County](https://www.albemarle.org/government/county-executive/office-of-performance-and-strategic-planning/goal-5-education-learning) says Mountain View Upper opened for 2026–27 on the same campus as the former Mountain View Elementary, now [Mountain View Primary](https://mvps.k12albemarle.org/). [PWCS](https://www.pwcs.edu/about/departments/facilities/construction/potomac_view_elementary_school_closure) says Potomac View closed on 2026-06-30 and its students moved to Woodbridge for 2026–27. The official [PWCS school-site layer](https://gis.pwcs.edu/server/rest/services/Find_Your_School_Update_2026/MapServer/0) lists Woodbridge Elementary School, OBJECTID 96, at 1550 Prince William Parkway and longitude -77.2612656033, latitude 38.6509036124 (queried with output CRS EPSG:4326 on 2026-10-25). This is a current site point candidate, not a zone polygon or a 2025–26 point.

## Next work

Reconcile name and site changes by school year before any crosswalk. A single ID-to-current-name replacement across all historical years would mislabel older data. For suitable candidates, record the name's effective period, prevent duplicate IDs, and keep the point source year in metadata. Rebuild the frozen snapshot and test identity, coordinates, counts, and coverage warnings before release. Verify the official PWCS Woodbridge point before future-year use; find a current point for Mountain View Upper and the two new Chesapeake schools; decide whether the virtual school should have a physical point. Compare newer Census division outlines and any verified division attendance-zone source without treating an old school point as a zone. The [Lexington outline release](RELEASE_LEXINGTON_2026-10-24.md) did not change school points or zones. Bead va_schools_dashboard-4qu remains open.

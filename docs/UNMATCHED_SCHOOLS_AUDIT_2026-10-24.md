# Unmatched school geography audit — 2026-10-24, updated 2026-10-25

The frozen snapshot has 18 VDOE schools without a name match in the 2024–25 NCES EDGE school geocode file. The app still excludes these schools from mapped school measures and shows its coverage warning. This audit did not add a point or an attendance boundary.

## Sources and method

The [2025–26 CCD preliminary school directory](https://nces.ed.gov/use-work/dataset/2025-26-common-core-data-ccd-preliminary-directory-files) provides current NCES IDs, state school IDs, names, and addresses. The [official ZIP](https://nces.ed.gov/ccd/data/zip/ccd_sch_029_2526_w_0a_050626.zip) had SHA-256 b5d8dc341ecdc85549ffa9ba5acf4c4ed4f2a7c4a78a5115bbd281ef3e23bc8. The VDOE [School Quality Profiles](https://schoolquality.virginia.gov/virginia-schools) give each school's current division number, school number, and street address. For every row below, the VDOE numbers form the CCD state ID as VA-{division:03d}-{division:03d}{school:04d}. All 18 state IDs match one 2025–26 CCD school row. This resolves the name-only identity questions in the earlier audit.

The saved [2024–25 EDGE school points](https://nces.ed.gov/programs/edge/geographic/schoollocations) were checked by NCES ID and street against the current VDOE profile and 2025–26 CCD directory. A matching ID alone does not prove that an older point marks the current campus. The EDGE page lists a 2025–26 tab, but its public school geocode download was commented out; the apparent ZIP URL returned 404 on 2026-10-24. The preliminary CCD directory has no point geometry.

## Disposition of the 18 schools

| VDOE school | VDOE state ID | 2025–26 NCES ID | 2024–25 point check | Disposition |
| --- | --- | --- | --- | --- |
| [Mountain View Upper Elementary](https://schoolquality.virginia.gov/schools/mountain-view-upper-elementary) | VA-002-0020150 | 510009002173 | Same ID, different street | Do not use the older point. Current VDOE site is 1545 Founders Place; NCES/older point is at 1757 Avon Street Extended. |
| Amelia County High School | VA-004-0040010 | 510018001828 | Same ID and street | Candidate older point; CCD name is Amelia County High. |
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
| [Woodbridge Elementary](https://schoolquality.virginia.gov/schools/woodbridge-elementary) | VA-075-0750550 | 510313001316 | Same ID, different street | Do not use the older point. Current VDOE site is 1550 Prince William Parkway; CCD still calls this ID Potomac View Elementary at 14601 Lamar Road. |
| Richmond Success Academy | VA-123-1233110 | 510324002307 | Same ID and street | Candidate older point; earlier name was Richmond Alternative School. |

Thirteen schools have a same-ID, same-street older point. Three have no older point for their current ID. Two have an older point at a different street. All 18 remain excluded from the deployed map while the crosswalk and snapshot are unchanged. The same-street points are source-dated candidates, not verified current coordinates or attendance-zone boundaries.

## Next work

Add an explicit, checked VDOE-to-NCES ID crosswalk for the 13 candidates, preserve the 2024–25 point source year in metadata, rebuild the frozen snapshot, and test identity, coordinates, counts, and coverage warnings before release. Find a current, authoritative point for the two moved sites and the two new Chesapeake schools; decide whether the virtual school should have a physical point. Compare newer Census division outlines and any verified division attendance-zone source without treating an old school point as a zone. The [Lexington outline release](RELEASE_LEXINGTON_2026-10-24.md) did not change school points or zones. Bead va_schools_dashboard-4qu remains open.

# Unmatched school geography audit — 2026-10-24

The frozen snapshot lists 18 VDOE school names without a match in the
2024–25 NCES school geocode file. The app excludes them from mapped school
measures and keeps its coverage warning. No point or attendance boundary was
added in this audit.

The [NCES school geocode page](https://nces.ed.gov/programs/edge/geographic/schoollocations)
lists a 2025–26 tab, but its public-school geocode download is commented out
in the page source. The apparent file URL returned HTTP 404 on 2026-10-24.
The [2025–26 CCD preliminary directory](https://nces.ed.gov/use-work/dataset/2025-26-common-core-data-ccd-preliminary-directory-files)
has names, IDs, and addresses, but is not a point-geocode replacement. Its
[official school file](https://nces.ed.gov/ccd/data/zip/ccd_sch_029_2526_w_0a_050626.zip)
was found through the NCES file browser's current version ID `52`; the link
in the dataset page used an older version ID. The table compares each VDOE
name with that directory and with the saved [2024–25 EDGE geocode file](https://nces.ed.gov/programs/edge/geographic/schoollocations)
within the same NCES division ID.
The downloaded directory ZIP had SHA-256
`b5d8dc341ecdc85549ffa9ba5acf4c4ed4f2a7c4a78a5115bbd281ef3e23bc8`.

Fifteen of the 18 names have an exact 2025–26 directory match in the same
division. Twelve of those IDs have a 2024–25 point whose recorded street is
the same (allowing punctuation differences). The other three have no
2024–25 point. Amelia County High School has a strong name-only candidate
with the same ID and street. Mountain View Upper Elementary has a less
certain candidate, and Woodbridge Elementary has no 2025–26 directory entry.
The directory's `ST_SCHID` values still need comparison with VDOE school IDs
before any crosswalk or older point is used.

| VDOE division | Unmatched VDOE school | 2025–26 NCES ID in same division | 2024–25 point | Current disposition |
| --- | --- | --- | --- | --- |
| Albemarle County | Mountain View Upper Elementary | No exact entry; Mountain View Elementary is `510009002173` | Yes, candidate ID | Exclude; the Upper Elementary identity and site are not confirmed. |
| Amelia County | Amelia County High School | No exact entry; Amelia County High is `510018001828` | Yes, candidate ID | Exclude until the VDOE school ID confirms this likely name variant. |
| Charlottesville City | Charlottesville Middle | `510078000271` | Yes | Exclude until VDOE ID check; 2024–25 name was Buford Middle. |
| Charlottesville City | Sunrise Elementary | `510078000272` | Yes | Exclude until VDOE ID check; 2024–25 name was Burnley-Moran Elementary. |
| Charlottesville City | Tall Oaks Elementary | `510078000266` | Yes | Exclude until VDOE ID check; 2024–25 name was Johnson Elementary. |
| Chesapeake City | B.M. Williams Elementary | `510081003157` | No | Exclude; no checked point for this new directory ID. |
| Chesapeake City | Crestwood Elementary School | `510081003158` | No | Exclude; no checked point for this new directory ID. |
| Danville City | Arnett Hills Elementary | `510111003154` | Yes | Exclude until VDOE ID check; 2024–25 name was Arnett Hills International Academy. |
| Falls Church City | Mary Ellen Henderson Middle School | `510129000284` | Yes | Exclude until VDOE ID check; 2024–25 name omitted “School.” |
| Falls Church City | Meridian High School | `510129000283` | Yes | Exclude until VDOE ID check; 2024–25 name omitted “School.” |
| Falls Church City | Mount Daniel Elementary School | `510129002204` | Yes | Exclude until VDOE ID check; 2024–25 name omitted “School.” |
| Falls Church City | Oak Street Elementary School | `510129001844` | Yes | Exclude until VDOE ID check; 2024–25 name omitted “School.” |
| Fredericksburg City | Gladys West Elementary | `510151003155` | Yes | Exclude until VDOE ID check; 2024–25 name was Fredericksburg New Elementary. |
| Loudoun County | Gum Spring Middle | `510225002450` | Yes | Exclude until VDOE ID check; 2024–25 name was Mercer Middle. |
| Loudoun County | Richard and Mildred Loving Elementary | `510225002412` | Yes | Exclude until VDOE ID check; 2024–25 name was Frances Hazel Reid Elementary. |
| Manassas City | Virginia Virtual Academy at Manassas | `510236003159` | No | Exclude; verify whether a school point is appropriate for the virtual program. |
| Prince William County | Woodbridge Elementary | No entry | No | Exclude; not in the directory snapshot dated 2025-10-01. |
| Richmond City | Richmond Success Academy | `510324002307` | Yes | Exclude until VDOE ID and physical site check; 2024–25 name was Richmond Alternative School. |

## Next source check

Match current VDOE school IDs to the 2025–26 CCD `ST_SCHID` values and official
division records. For the 12 exact-name records with older points, confirm
the site before using a 2024–25 coordinate with its source year shown. For
three new IDs, wait for a checked geocode or another authoritative point.
Verify the two name-only candidates separately and keep Woodbridge Elementary
unmapped until its current ID and physical site are established. Then compare
the newer Census division polygon vintage and any division-published
attendance-zone source.
The 2024 Census division outline release for Lexington City is recorded in
[the release note](RELEASE_LEXINGTON_2026-10-24.md); it does not resolve school
points or zones. Beads `va_schools_dashboard-4qu` and
`va_schools_dashboard-nlf` remain open for these checks.

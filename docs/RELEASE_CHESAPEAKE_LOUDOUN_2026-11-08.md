# Chesapeake and Loudoun zone release - 2026-11-08

Pre-release Git commit: `c4c4d9c6a649e5dd7b5c988d5c4cb071fa247e01`. Pre-release Shiny bundle: `12794667`. Bead: `va_schools_dashboard-nlf.11.22`.

## Scope

The snapshot adds four schools to the 1,810-school baseline: Chesapeake B.M. Williams Elementary and Crestwood Elementary School (new 2025-26 IDs); Loudoun Gum Spring Middle and Richard and Mildred Loving Elementary (July 2025 names on existing IDs). No prior school row was removed or changed. It adds 190 school metric rows, 18 subject-performance rows, and 10 enrollment rows. The division aggregate files were regenerated for the two affected divisions and passed the snapshot reconciliation check. Year-aware names keep earlier Loudoun metrics under their earlier school names. The two Chesapeake schools have no pre-opening metrics.

Chesapeake has 38 source polygons and 45/45 school links. Loudoun has 93 polygons and 93/98 school links. The statewide candidate has 1,454 matched of 1,814 schools and 360 unmatched. Nine divisions have no zone file. Five Loudoun schools have no separate standard polygon.

## Source and validation

- Chesapeake: [CPS school-zone page](https://www.cpschools.com/o/cps/page/school-zones-enrollment) and [CPS September 2025 change announcement](https://www.cpschools.com/o/bmw/article/1917173). Source hash `4b9fb989ce305de1512d08d505d571f6bec2b1114d4dda6d49f073ad39d1ae41`. Maximum same-level overlap 0.0313 m² against 0.05 m² cap.
- Loudoun: [LCPS Planning and GIS](https://www.lcps.org/o/support/page/planning-gis-services/). Source hash `226e25200152c05598fcc4ec0cc3a630eff9658c3859b5b53ded65c332a7702d`. Maximum same-level overlap 0.0014 m² against 0.01 m² cap.
- Three public ArcGIS service object-ID sets for each division matched the staged source on 2026-11-08. Chesapeake items have an informational-use disclaimer and no reuse restriction; Loudoun items credit LCPS and have blank license info. [ArcGIS public sharing terms](https://doc.arcgis.com/en/arcgis-online/reference/terms-of-use.htm) apply subject to owner conditions.
- The manifests, import specifications, crosswalks, and geometry reports in `data-raw/` record provenance and checks. Dry-run imports reproduced both zone hashes. These polygons provide map context; they do not assign a household to a school.

## Release checks

- `Rscript --vanilla tests/check_snapshot.R`: passed after both division aggregate files were included.
- `Rscript --vanilla tests/check_zones.R`: passed.
- `python3 tests/check_zone_importer.py`: passed.
- `python3 data-raw/audit_zones.py`: 1,454 matches; 360 unmatched; zero invalid school IDs; zero empty geometries.
- Git and live Shiny release: pending.

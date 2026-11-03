# Historical NCES school-zone link release — 2026-11-02

Prepared from production source commit c773376. Adds 247 exact NCES school-ID links in 76 existing 2015–16 SABS files. The school must exist in the production snapshot and have the same division and NCES ID. Geometry, source, names, and all other feature data are unchanged. The survey polygons remain dated 2015–16 and do not establish current address assignments.

The release candidate audit reports 1,169 of 1,810 schools with a matched zone and 641 without. Ten divisions have no zone file. The previously deployed GIS sources remain unchanged. Newer locally researched GIS layers and their source reviews remain separate.

Validation: independent JSON comparison of all 76 files passed, including division and ID checks; R zone and snapshot tests passed; audit found no invalid school IDs or empty geometry; git diff --check passed. Rollback baseline: c773376 and production bundle 12752849.

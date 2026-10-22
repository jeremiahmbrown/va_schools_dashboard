# Performance data coverage check

Checked on 2026-10-20 against the frozen dashboard snapshot and official
Virginia Department of Education (VDOE) files. A blank or suppressed source
value remains blank in the dashboard. Annual SOL pass rate, School Quality
Profiles (SQP) Percent Passing, and Accreditation Combined Rate (ACR) are
different measures.

Updated 2026-10-22: the supplied 2021–22 School and Division Subject-Area
workbooks now add a 2022 Annual SOL point. See the
[2022 import check](ANNUAL_SOL_2022_QA_2026-10-22.md).

## What the dashboard can show

| Measure | Reportable years | School coverage in latest year | Division coverage in latest year |
| --- | --- | ---: | ---: |
| Annual SOL pass rate, overall | 2022–2026 | 1,769 of 1,774 mapped rows in 2026 | 130 of 130 in 2026 |
| Annual SOL pass rate, English | 2022–2026 | 1,769 of 1,774 in 2026 | 130 of 130 in 2026 |
| Annual SOL pass rate, math | 2022–2026 | 1,769 of 1,774 in 2026 | 130 of 130 in 2026 |
| Annual SOL pass rate, science | 2022–2026 | 1,731 of 1,739 in 2026 | 130 of 130 in 2026 |
| ACR, overall app mean, English, math | 2022–2024 | English: 1,781 of 1,783 in 2024; math: 1,782 of 1,783 | 130 of 130 in 2024 |
| SQP Percent Passing, overall, English, math, science | 2022–2024 | Science: 1,734 of 1,741 in 2024 | 130 of 130 in 2024 |
| ACR, science | None | The SQP export has no numeric Science ACR | None |

The denominators above are rows mapped into the frozen school snapshot, not
all operating schools in Virginia. A source may omit a school, a value may be
suppressed, or a school may be absent from the 2024–25 NCES map geocodes. The
2023–26 annual SOL import matched 1,777 of 1,821 distinct source school codes
across its four-year union. The remaining 44 codes cannot safely be given map points
without an updated school crosswalk or geocode source. The 2026 subject rows
that *are* mapped have 5 suppressed/unavailable English values, 5 math values,
and 8 science values. These are not zeros.

## Fresh source check

The three SQP [accreditation exports](https://schoolquality.virginia.gov/download-data)
were downloaded again on 2026-10-20 with `level=schools`, `year=all`, and
subject indicators English `1`, math `3`, and science `5`. Every response had
only 2022, 2023, and 2024 rows. The Science `Accreditation Combined Rate`
column had **zero numeric values**. The three files are kept in the ignored
`data-raw/input/manual/source-check/` folder. Their SHA-256 hashes are:

| Export | SHA-256 |
| --- | --- |
| English | `9c499eb40488190c959c8d4be43586f0682f59de52c3beb353746603a05c6f8d` |
| Math | `2c4f73c9f0bd240981991a5fe2b5a72471ee0eba462b23109759f528a955c4aa` |
| Science | `e1e91aeabeec91f41cbc386bd16e0745fd0f0372f63a85b10054de3217e4c4e7` |

The official [2024–25 accreditation report workbook](https://www.doe.virginia.gov/home/showpublisheddocument/57720/638638962832830000)
was also downloaded and checked (SHA-256
`69ba82e4d025eb09f7c7746261adfd26f288e3f86cb3b72e2e7d85dc3698eb0a`).
It has 1,823 school rows and 32 columns. Its English, math, and science fields
are indicator levels such as `L1` and `L2`; it has no numeric ACR field. A
rating or level cannot be used as a missing ACR percentage. No additional
numeric ACR values passed the source check, so the frozen metric values did
not change.

## 2022 Annual SOL source

The user supplied the official [2021–22 School and Division Subject-Area workbooks](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results).
The school and division files report rates without test counts. Their checked
values add 2022 annual SOL rates for 1,736 mapped schools and 129 mapped
divisions. The 2022 division subjects come directly from VDOE's division
workbook; later division subjects are calculated from school test counts.
Suppressed values remain missing. The raw workbooks stay in the ignored manual
input folder; tracked extracts and hashes make the build repeatable from the
checked inputs. The [2022 import check](ANNUAL_SOL_2022_QA_2026-10-22.md)
records the method and coverage.

There is no known manual file that can fill numeric Science ACR or extend the
same ACR definition to 2025–26. VDOE's newer School Performance and Support
Framework uses different measures and needs its own labeled series.

# Results, reindeer 2.5.0 vs the 2.2.0 baseline

Same machine and corpora as `BASELINE_2.2.0.md` (`ae`; synthetic 504/700-bundle
copies from `tools/make-synthetic-corpus.R`). Measured 2026-10-11 with
`devtools::load_all()` unless noted. "Target" is the assessment §10 figure.

| Workflow | 2.2.0 | 2.5.0 | Target | Met |
|---|---|---|---|---|
| `library(reindeer)` (installed) | 0.48 s, 25 namespaces | 0.28 s, 16 namespaces | ~0.2 s | no (see below) |
| First open, `ae` | 2.07 s | 0.22 s | < 0.5 s | yes |
| Reopen, `ae` | 0.056 s | 0.10 s | — | (reopen now syncs + checks metadata) |
| First open, 700 bundles | 153 s serial | 3.6 s (files in OS cache) | ~1.3 s | no (see below) |
| Reopen after one external edit, 700 bundles | stale or full rebuild | 0.09 s, sees the edit | ~0.4 s | yes |
| `quantify()` 24 vowels (67 here), cold | 2.2–3.4 s | 0.73 s | ~0.3 s | partly |
| `quantify()` 253 segments, cold | 3.4–5.4 s | 0.43 s | ~0.3–0.5 s | yes |
| same, re-run | full recompute | 0.07 s | ~0.1 s | yes |
| `scout()` 25k segments | 39 s | 0.70 s (25,300 segments) | < 1 s | yes |
| Metadata write + FAIR regeneration, 504 bundles, unchanged | 0.64 s | 0.06 s, no file rewritten | ~0 | yes |
| Exports / Imports | 68 / 25 | 65 / 17 | 26 / 15 | no (Phase 5 / D1) |
| Tarball | 10.9 MB | 6.4 MB | ~6.4 MB | yes |

## Misses, explained

- **`library(reindeer)` 0.28 s.** 16 namespaces remain loaded because dplyr and
  pillar are Imports (D1: `collect()`/`glimpse()` are their generics), plus
  tibble, vctrs, data.table, S7, cli, rlang. ggplot2/Rdpack no longer load.
- **First open of 700 bundles, 3.6 s.** About 1.8 s is JSON parsing of 700
  `_annot.json` (jsonlite; no faster parser is a dependency), about 1 s is the
  inserts, and a cold first read of the files adds more when they are not in
  the OS cache (up to 7 s measured right after copying). It is 40x faster than
  2.2.0 and runs once; later opens are incremental.
- **`quantify()` of the vowel subset, 0.73 s.** Every bundle's whole file is
  analysed once (forest on the full recording). The target assumed fewer files
  per call; for 7 bundles the whole-file cost dominates. `.mode = "window"`
  trades this the other way.
- **Exports 63, Imports 17.** The export consolidation is Phase 5; Imports are
  17 because dplyr and pillar moved in (D1).

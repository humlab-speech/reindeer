# Baseline, reindeer 2.2.0 (main @ b15b50a)

Reproduce: `Rscript benchmarking/baseline.R <dir>` (builds `syn504_emuDB` in
`<dir>` via `tools/make-synthetic-corpus.R`; `ae` = 7 bundles, 253 Phonetic
segments). Machine: macOS (Darwin 25.6), measured 2026-10-11.

## Measured here

| Workflow | Seconds |
|---|---|
| `library(reindeer)` (`load_all`) | 1.79 |
| First open, `ae` | 2.07 |
| Reopen, `ae` | 0.056 |
| `collect(query("Phonetic == t"))`, `ae` | 0.006 |
| `collect(query("[Syllable == S ^ Phonetic == @]"))`, `ae` | 0.117 |
| First open, 504 bundles | 25.8 |
| Reopen, 504 bundles | 0.010 (stale: sees no external edits, A8) |
| `devtools::test()` | 47 wall / 26.5 CPU (was 163 / 89, WP0.2) |

## From the assessment (§10, not re-measured)

| Workflow | Now |
|---|---|
| `library(reindeer)`, installed | 0.68 s, 30 namespaces |
| First open, 700 bundles | 153 s serial |
| `quantify()` 24 vowels, cold | 2.2–3.4 s |
| `quantify()` 253 segments, cold | 3.4–5.4 s |
| same, re-run | full recompute (cache off) |
| `scout()` 25k segments | 39 s |
| Metadata write + FAIR regeneration, 504 bundles | 0.64 s even when unchanged |
| Exports / Imports / tarball | 68 / 25 / 10.9 MB |

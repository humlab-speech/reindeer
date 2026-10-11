# Results, F2 (3.1.0) against BASELINE_3.0.0

Same harness (`tools/bench.R`), same machine. The second run was under load
(avg 8 vs 4), so compare CPU-s, not wall.

| Item | Finding | Action |
|---|---|---|
| F2.1 first open, 700 bundles | CPU 3.34 s -> 2.18 s (-35 %). Profile: building a `data.table` per level per bundle (`setalloccol`) was ~1 s of the 2 s parse. Remaining wall time (4-6 s here) is cold file reads and SQLite inserts (0.5 s) | `parse_annot_json` builds plain lists, one `rbindlist` per bundle |
| F2.2 peak RSS | Floor is 157 MB (package load with dplyr/pillar/vctrs/S7). Worst workload `scout_25k` adds ~190 MB over the floor for 25k segments with provenance; no single allocation dominates | no change; 30 % target not meaningful over a 157 MB floor |
| F2.3 superassp threads | `detectCores()` runs once in superassp `.onLoad`, behind an option; not per call | none |
| F2.4 `set_metadata(workbook)` | 252 bundles 0.44 s, 504 bundles 0.49 s: linear, under the 1 s target | none |
| F2.5 server teardown | new test: second `annotate()` replaces the first server; `.onUnload` leaves none | test only |

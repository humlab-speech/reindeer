# Benchmarking

`Rscript tools/bench.R [out.md]` regenerates the workload table (wall, CPU-s,
peak RSS; one fresh R process per workload). Run it on an idle machine
(load < 4) and compare against `BASELINE_3.0.0.md`.

- `BASELINE_2.2.0.md`: before the 2.3-3.0 programme (history).
- `RESULTS_2.5.0.md`: 2.5.0 against that baseline (history).
- `BASELINE_3.0.0.md`: current baseline, produced by `tools/bench.R`.

Synthetic corpora come from `tools/make-synthetic-corpus.R`.

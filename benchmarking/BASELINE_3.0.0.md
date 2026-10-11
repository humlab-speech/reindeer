# Benchmark, reindeer 3.0.0

Reproduce: `Rscript tools/bench.R`. sha `3c6825a`; arm64; R 4.6.1.
Conditions: `16:18  up 1 day,  7:47, 3 users, load averages: 4.51 4.10 3.90`

| Workload | Wall s | CPU s | Peak RSS MB |
|---|---|---|---|
| `library_load` | 1.016 | 0.795 | 156 |
| `ae_first_open` | 0.380 | 0.297 | 250 |
| `ae_reopen` | 0.102 | 0.102 | 253 |
| `ae_query_dominance` | 0.101 | 0.101 | 253 |
| `syn700_first_open` | 7.352 | 3.342 | 312 |
| `syn700_reopen_after_edit` | 0.221 | 0.188 | 306 |
| `quantify_253_cold` | 0.475 | 0.318 | 289 |
| `quantify_253_warm` | 0.143 | 0.140 | 293 |
| `scout_25k` | 0.769 | 0.769 | 338 |
| `metadata_write_504` | 0.062 | 0.061 | 279 |

# Benchmarking Suite

Comprehensive benchmarks for performance-critical components of the reindeer package.

## Running Benchmarks

### Quick Start

```r
# Run all benchmarks
source("benchmarking/run_benchmarks.R")

# Run individual benchmarks
source("benchmarking/benchmark_queries.R")
source("benchmarking/benchmark_hierarchy.R")
source("benchmarking/benchmark_requery_seq.R")
source("benchmarking/benchmark_trackdata.R")
source("benchmarking/benchmark_metadata.R")
source("benchmarking/benchmark_performance_targets.R")
```

### Command Line

```bash
Rscript benchmarking/run_benchmarks.R
```

### Rendering Results

```r
Rscript render_vignette.R
```

Creates `vignettes/query_benchmarks.html` with visualizations and analysis.

## Benchmark Scripts

| Script | Description |
|--------|-------------|
| `run_benchmarks.R` | Main runner (all benchmarks) |
| `benchmark_queries.R` | EQL query optimization (`query()` vs `emuR::query()`) |
| `benchmark_hierarchy.R` | Hierarchical/dominance query performance |
| `benchmark_requery_seq.R` | Sequence requery benchmarks |
| `benchmark_trackdata.R` | Track data extraction benchmarks |
| `benchmark_metadata.R` | Metadata retrieval benchmarks |
| `benchmark_performance_targets.R` | Regression targets for CI |
| `benchmark_dspp.R` | DSP parameter computation (data.table vs dplyr) |
| `benchmark_serialization.R` | Cache serialization (qs vs base R) |
| `benchmark_quantify.R` | Signal quantification benchmarks |
| `benchmark_simulation.R` | Simulation infrastructure benchmarks |
| `benchmark_json.R` | JSON parsing (RcppSimdJson vs jsonlite) |
| `extract_test_results.R` | Extracts test results for vignette |

## Results Files

- `benchmark_results.rds` / `benchmark_summary.rds` / `.csv` — EQL query results
- `dspp_benchmark_results.rds` — DSP parameter benchmarks
- `performance_targets_results.rds` — Regression targets
- `test_results.rds` — Test suite results

## Visualizations

- `speedup_by_type.png` — Query speedup by type
- `speedup_dist.png` — Distribution of speedup values
- `time_comparison.png` — Execution time comparison
- `memory_comparison.png` — Memory usage comparison

## Troubleshooting

1. Check you're in package root: `getwd()` should end in `/reindeer`
2. Install missing packages: `install.packages(c("bench", "gt"))`
3. Check SQLite cache exists: `build_emuDB_cache(ae_db)`

# Benchmarking Suite

This directory contains comprehensive benchmarks for performance-critical components of the reindeer package.

## Overview

The benchmarking suite evaluates two major optimizations:

1. **EQL Query Optimization** (`query_opt()`)
   - Direct SQL-based queries vs. standard `emuR::query()`
   - Measures execution time, memory usage, and correctness
   - Typical speedup: 5-15x

2. **MOMEL/INTSINT Modernization**
   - Pure Python/Parselmouth implementation
   - Replaces Praat/Perl/C binary dependencies
   - Cross-platform, maintainable, easy to install

## Running Benchmarks

### Quick Start

From the package root directory:

```r
# Run all benchmarks (EQL queries + MOMEL/INTSINT)
source("benchmarking/run_benchmarks.R")

# Run only EQL query benchmarks
source("benchmarking/benchmark_queries.R")

# Run only MOMEL/INTSINT benchmarks
source("benchmarking/benchmark_momel_intsint.R")
```

### Command Line

```bash
# From package root
Rscript benchmarking/run_benchmarks.R [iterations] [run_momel]

# Examples:
Rscript benchmarking/run_benchmarks.R 50 TRUE   # 50 iterations, include MOMEL
Rscript benchmarking/run_benchmarks.R 100 FALSE # 100 iterations, skip MOMEL
```

### Rendering Results

Generate an HTML report with all benchmark results:

```r
# From package root
Rscript render_vignette.R
```

This creates `vignettes/query_benchmarks.html` with comprehensive visualizations and analysis.

## Files

### Benchmark Scripts

- `run_benchmarks.R` - Main benchmark runner (all benchmarks)
- `benchmark_queries.R` - EQL query benchmarks
- `benchmark_momel_intsint.R` - MOMEL/INTSINT benchmarks
- `extract_test_results.R` - Extracts test results for vignette

### Results Files

- `benchmark_results.rds` - EQL query benchmark data
- `benchmark_summary.rds` / `.csv` - EQL query summary statistics
- `momel_benchmark_results.rds` - MOMEL/INTSINT benchmark data
- `momel_features_comparison.rds` - Feature comparison table
- `test_results.rds` - Test suite results

### Visualizations

- `speedup_by_type.png` - Query speedup by type
- `speedup_dist.png` - Distribution of speedup values
- `time_comparison.png` - Execution time comparison
- `memory_comparison.png` - Memory usage comparison
- `momel_time_comparison.png` - MOMEL/INTSINT time comparison
- `momel_memory_comparison.png` - MOMEL/INTSINT memory comparison

## Troubleshooting

### Benchmarks Fail to Run

1. Check you're in package root: `getwd()` should end in `/reindeer`
2. Install missing packages: `install.packages(c("bench", "gt"))`
3. Check SQLite cache exists: `build_emuDB_cache(ae_db)`

### MOMEL Benchmarks Skip

- Install parselmouth: `pip install praat-parselmouth`
- Check Python: `reticulate::py_config()`

### Vignette Won't Render

1. Install Quarto CLI: https://quarto.org/docs/get-started/
2. Run benchmarks first: `source("benchmarking/run_benchmarks.R")`
3. Check results exist: `file.exists("benchmarking/benchmark_results.rds")`

---

For detailed documentation, see the full benchmarking vignette after running `Rscript render_vignette.R`.

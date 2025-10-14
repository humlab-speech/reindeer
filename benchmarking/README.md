# Query Optimization Benchmarks

This directory contains benchmarking tools for comparing the optimized SQL-based query implementation (`query_opt()`) with the standard `emuR::query()` function.

## Files

- `run_benchmarks.R` - Main script to run all benchmarks
- `benchmark_queries.R` - Benchmark utilities and query definitions
- `benchmark_results.rds` - Saved benchmark results (generated)
- `benchmark_summary.rds` - Summarized results (generated)
- `benchmark_summary.csv` - CSV export of summary (generated)
- `extract_test_results.R` - Extract test suite results
- `*.png` - Generated visualization plots

## Quick Start

### Running Benchmarks

From the package root directory:

```bash
# Run all benchmarks and generate visualizations
Rscript benchmarking/run_benchmarks.R
```

This will:
1. Load the test database
2. Run comprehensive benchmarks (may take several minutes)
3. Generate performance plots
4. Save results to `benchmarking/benchmark_results.rds`

### Generating Report

After running benchmarks:

```bash
# Generate interactive HTML report with test coverage
Rscript render_vignette.R
```

This creates `vignettes/query_benchmarks.html` with:
- Test coverage summary (99 tests)
- Performance visualizations
- Detailed benchmark results
- Speedup analysis

View the report:
```r
browseURL("vignettes/query_benchmarks.html")
```

## Running from R Console

### Full Benchmark Suite

```r
# From package root
source("benchmarking/run_benchmarks.R")
```

### Custom Benchmarks

```r
# Load benchmark utilities
source("benchmarking/benchmark_queries.R")

# Setup database
setup <- setup_benchmark_db()

# Run specific benchmarks
results <- run_benchmark_suite(
  ae_path = setup$path,
  ae = setup$db,
  iterations = 50  # Adjust as needed
)

# Analyze results
summary <- summarize_benchmarks(results)
print_summary(summary)

# Create plots
plots <- create_benchmark_plots(results, summary)
```

## Benchmark Categories

1. **Simple Queries**: Basic label matching
   - `Phonetic == t`
   - `Phoneme == n`
   - `Syllable == S`

2. **Regex Queries**: Pattern matching
   - `Phonetic =~ .*`
   - `Phonetic =~ [tkp]`

3. **Sequence Queries**: Temporal ordering
   - `[Phoneme == n -> Phoneme == t]`

4. **Dominance Queries**: Hierarchical relationships
   - `[Syllable == S ^ Phoneme == n]`
   - `[Word == F ^ Phoneme == t]`

5. **Projection Queries**: With # operator
   - `[Syllable == S ^ #Phoneme == n]`

6. **Function Queries**: Count and position
   - `Num(Syllable, Phoneme) >= 3`
   - `Start(Syllable, Phoneme) == 1`

7. **Complex Queries**: Multi-operator combinations
   - `[[Syllable == S ^ Phoneme == n] -> Phoneme == t]`

## Understanding Results

### Speedup Factor

- **> 1.0**: Optimized implementation is faster (typical: 5-15x)
- **= 1.0**: Equal performance
- **< 1.0**: Standard emuR is faster (rare)

### Key Metrics

- **Median Time**: Typical execution time
- **Memory**: Allocated memory during execution
- **Speedup**: Ratio of emuR time to optimized time
- **Iterations**: Number of repetitions for statistical validity

## Customizing Benchmarks

### Add New Queries

Edit `benchmark_queries.R`:

```r
# Add to the query list in run_benchmark_suite()
query_list <- c(
  # Existing queries...
  
  # Your new query
  "YourLevel == yourLabel"
)
```

### Adjust Iteration Count

```r
# More iterations = more accurate but slower
results <- run_benchmark_suite(iterations = 100)
```

### Custom Database

```r
# Use your own emuDB
ae_path <- "/path/to/your_emuDB"
ae <- load_emuDB(ae_path)

results <- run_benchmark_suite(
  ae_path = ae_path,
  ae = ae,
  iterations = 50
)
```

## Output Files

### Generated Files

After running benchmarks:

```
benchmarking/
├── benchmark_results.rds         # Full benchmark data
├── benchmark_summary.rds         # Aggregated statistics
├── benchmark_summary.csv         # Human-readable summary
├── speedup_by_type.png          # Speedup visualization
├── time_comparison.png          # Time comparison plot
├── memory_comparison.png        # Memory usage plot
└── speedup_dist.png             # Speedup distribution
```

### CSV Format

`benchmark_summary.csv` contains:
- `query`: The EQL query string
- `query_type`: Category (simple, sequence, dominance, etc.)
- `median_emuR`: Median time for emuR::query()
- `median_opt`: Median time for query_opt()
- `speedup`: Speedup factor
- `mem_emuR_mb`: Memory used by emuR (MB)
- `mem_opt_mb`: Memory used by query_opt (MB)

## Requirements

### Required R Packages

```r
install.packages(c(
  "emuR",       # Database and queries
  "bench",      # Benchmarking
  "dplyr",      # Data manipulation
  "tidyr",      # Data reshaping
  "ggplot2"     # Visualization
))
```

### Optional (for report generation)

```r
install.packages(c(
  "quarto",     # Report rendering
  "knitr",      # R Markdown
  "gt"          # Table formatting
))
```

## Interpreting Plots

### Speedup by Type
Shows how speedup varies by query category. Higher is better.

### Time Comparison
Direct comparison of execution times. Lower is better.

### Memory Comparison
Memory allocated during query execution. Lower is better.

### Speedup Distribution
Histogram of all speedup values. Shows consistency of optimization.

## Troubleshooting

### Benchmarks Take Too Long

```r
# Reduce iterations
results <- run_benchmark_suite(iterations = 10)
```

### Database Not Found

```r
# Create demo database first
library(emuR)
temp_dir <- tempdir()
create_emuRdemoData(dir = temp_dir)
```

### Out of Memory

```r
# Run benchmarks on subset of queries
# Edit benchmark_queries.R to comment out some queries
```

## Integration with Tests

The benchmarking suite complements the test suite:

- **Tests** (`tests/testthat/test_query_optimized.R`): Validate correctness
- **Benchmarks** (`benchmarking/`): Validate performance

Both should be run regularly to ensure:
1. ✅ Correctness: Results match emuR::query()
2. ✅ Performance: Optimization provides speedup

## Example Session

```r
# Complete workflow
library(emuR)

# 1. Run tests to verify correctness
library(testthat)
test_file("tests/testthat/test_query_optimized.R")
# Expected: [ FAIL 0 | WARN 0 | SKIP 2 | PASS 99 ]

# 2. Run benchmarks to measure performance
source("benchmarking/run_benchmarks.R")
# Generates plots and saves results

# 3. Generate comprehensive report
source("render_vignette.R")
# Creates vignettes/query_benchmarks.html

# 4. View results
browseURL("vignettes/query_benchmarks.html")
```

## Continuous Integration

Recommended CI workflow:

```yaml
- name: Test Correctness
  run: Rscript -e "library(testthat); test_file('tests/testthat/test_query_optimized.R')"

- name: Benchmark Performance
  run: Rscript benchmarking/run_benchmarks.R

- name: Generate Report
  run: Rscript render_vignette.R

- name: Archive Results
  uses: actions/upload-artifact@v2
  with:
    name: benchmark-report
    path: vignettes/query_benchmarks.html
```

## Further Information

- **Test Documentation**: `tests/TEST_DOCUMENTATION.md`
- **Implementation**: `R/reindeer_query_optimized.r`
- **Overall Summary**: `QUERY_OPTIMIZATION_SUMMARY.md`

---

**Status**: Ready for use
**Last Updated**: 2025-10-14
**Maintained**: Part of reindeer package

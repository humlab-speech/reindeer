# Query Optimization Benchmarks

This directory contains benchmarking tools for comparing the optimized SQL-based query implementation (`query_opt()`) with the standard `emuR::query()` function.

## Files

- `benchmark_queries.R` - Main benchmarking script with utilities
- `benchmark_results.rds` - Saved benchmark results (generated)
- `benchmark_summary.rds` - Summarized results (generated)
- `benchmark_summary.csv` - CSV export of summary (generated)
- `*.png` - Generated visualization plots

## Running Benchmarks

### From R Console

```r
# Run benchmarks from package root
source("inst/benchmarks/benchmark_queries.R")

# Or with custom iterations
results <- run_benchmark_suite(iterations = 100)
summary <- summarize_benchmarks(results)
print_summary(summary)
plots <- create_benchmark_plots(results, summary)
save_benchmark_results(results, summary, plots)
```

### From Command Line

```bash
# Run benchmarks
Rscript inst/benchmarks/benchmark_queries.R

# Results will be saved in inst/benchmarks/
```

### Viewing Results

The Quarto vignette provides an interactive, comprehensive view:

```r
# Render the benchmark vignette
quarto::quarto_render("vignettes/query_benchmarks.qmd")

# Open the generated HTML
browseURL("vignettes/query_benchmarks.html")
```

## Customizing Benchmarks

Edit `benchmark_queries.R` to:

- Add new queries to test
- Change iteration counts
- Modify output formats
- Add custom visualizations

## Interpreting Results

### Speedup Factor

- **> 1.0**: Optimized implementation is faster
- **= 1.0**: Equal performance
- **< 1.0**: Standard emuR is faster

### Query Types Tested

1. **Simple**: Basic equality/inequality queries
2. **Regex**: Pattern matching queries
3. **Sequence**: Adjacent item queries (`->`)
4. **Dominance**: Hierarchical relationship queries (`^`)
5. **Boolean**: Conjunction/disjunction (`&`, `|`)
6. **Functions**: Position and count functions

## Requirements

- R packages: `emuR`, `bench`, `dplyr`, `tidyr`, `ggplot2`
- For vignette: `quarto`, `knitr`, `gt`

## Example Output

```
BENCHMARK SUMMARY
================================================================================
Query                                        emuR  Optimized    Speedup
--------------------------------------------------------------------------------
Phonetic == t                              12.5ms      3.2ms      3.91x
[Syllable == S ^ Phoneme == n]             45.2ms     15.3ms      2.95x
Num(Syllable, Phoneme) >= 3                28.7ms     12.1ms      2.37x
...
================================================================================

Overall median speedup: 2.54x
Mean speedup: 2.68x
Max speedup: 4.23x
Min speedup: 1.82x
```

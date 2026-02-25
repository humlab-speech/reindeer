#!/usr/bin/env Rscript

# Benchmark comparison of dspp_metadataParameters implementations
# Original (dplyr/tidyr) vs data.table optimized

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(bench)
  library(dplyr)
  library(ggplot2)
  library(tibble)
  library(data.table)
})

cat("\n")
cat("======================================================================\n")
cat("  BENCHMARKING: dspp_metadataParameters_dt()\n")
cat("======================================================================\n")
cat("\n")

# Note: original dplyr implementation (reindeeR_signalextensions.R) has been
# deleted. This benchmark now profiles the data.table version standalone.

cat("Running benchmarks...\n")
cat("This may take a few minutes.\n\n")

# Benchmark the data.table implementation
results <- bench::mark(
  data_table = dspp_metadataParameters_dt(recompute = TRUE, impute = TRUE),
  iterations = 10,
  time_unit = "ms"
)

cat("\n")
cat("──────────────────────────────────────────────────────────────────\n")
cat("Benchmark Results:\n")
cat("──────────────────────────────────────────────────────────────────\n")
print(results)

dt_median <- results$median[[1]]

cat("\n")
cat("──────────────────────────────────────────────────────────────────\n")
cat("Performance Summary:\n")
cat("──────────────────────────────────────────────────────────────────\n")
cat(sprintf("Median time (data.table): %.2f ms\n", as.numeric(dt_median)))

dt_result <- dspp_metadataParameters_dt(recompute = TRUE, impute = TRUE)
cat(sprintf("Result dimensions: %d x %d\n", nrow(dt_result), ncol(dt_result)))

# Save results
saveRDS(results, "benchmarking/dspp_benchmark_results.rds")
cat("Results saved to: benchmarking/dspp_benchmark_results.rds\n")

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    BENCHMARK COMPLETE                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")

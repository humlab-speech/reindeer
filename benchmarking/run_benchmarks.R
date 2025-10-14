#!/usr/bin/env Rscript
# Run EQL Query Benchmarks
# Usage: Rscript run_benchmarks.R [iterations]

args <- commandArgs(trailingOnly = TRUE)
iterations <- if (length(args) > 0) as.integer(args[1]) else 50

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║          EMU QUERY LANGUAGE BENCHMARK RUNNER                       ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("Iterations: %d\n", iterations))
cat("Loading packages...\n")

suppressPackageStartupMessages({
  library(emuR)
  library(bench)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

cat("Sourcing implementation...\n")
source("R/reindeer_query_optimized.r")
source("inst/benchmarks/benchmark_queries.R")

cat("\n")
cat("Running benchmarks (this may take a few minutes)...\n\n")

# Run benchmarks
results <- run_benchmark_suite(iterations = iterations)
summary <- summarize_benchmarks(results)

# Print summary
print_summary(summary)

# Create plots
cat("\nGenerating plots...\n")
plots <- create_benchmark_plots(results, summary)

# Save results
cat("Saving results...\n")
save_benchmark_results(results, summary, plots)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║  BENCHMARK COMPLETE                                                ║\n")
cat("║                                                                    ║\n")
cat("║  Results saved to: inst/benchmarks/                                ║\n")
cat("║  Render vignette with: quarto::quarto_render(...)                 ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

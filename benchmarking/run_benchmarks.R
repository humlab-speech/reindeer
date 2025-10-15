#!/usr/bin/env Rscript
# Run All Benchmarks (EQL Query + MOMEL/INTSINT)
# Usage: Rscript run_benchmarks.R [iterations] [momel]

args <- commandArgs(trailingOnly = TRUE)
iterations <- if (length(args) > 0) as.integer(args[1]) else 50
run_momel <- if (length(args) > 1) as.logical(args[2]) else TRUE

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║          REINDEER BENCHMARK RUNNER                                 ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("Query iterations: %d\n", iterations))
cat(sprintf("Run MOMEL/INTSINT benchmarks: %s\n", run_momel))
cat("Loading packages...\n")

suppressPackageStartupMessages({
  library(emuR)
  library(bench)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ==============================================================================
# EQL Query Benchmarks
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║          EMU QUERY LANGUAGE BENCHMARKS                             ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

cat("Sourcing implementation...\n")
source("R/reindeer_query_optimized.r")
source("benchmarking/benchmark_queries.R")

cat("\n")
cat("Running query benchmarks (this may take a few minutes)...\n\n")

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
cat("║  EQL QUERY BENCHMARKS COMPLETE                                     ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# MOMEL/INTSINT Benchmarks
# ==============================================================================

if (run_momel) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║          MOMEL/INTSINT BENCHMARKS                                  ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
  
  cat("Running MOMEL/INTSINT benchmarks...\n\n")
  
  tryCatch({
    source("benchmarking/benchmark_momel_intsint.R")
    cat("\n")
    cat("╔════════════════════════════════════════════════════════════════════╗\n")
    cat("║  MOMEL/INTSINT BENCHMARKS COMPLETE                                 ║\n")
    cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
  }, error = function(e) {
    cat("\n")
    cat("⚠️  MOMEL/INTSINT benchmarks failed:\n")
    cat(sprintf("   %s\n", e$message))
    cat("   (Continuing with query benchmarks only)\n\n")
  })
}

# ==============================================================================
# Final Summary
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║  ALL BENCHMARKS COMPLETE                                           ║\n")
cat("║                                                                    ║\n")
cat("║  Results saved to: benchmarking/                                   ║\n")
cat("║  Render vignette with:                                             ║\n")
cat("║    Rscript render_vignette.R                                       ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

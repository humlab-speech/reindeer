#!/usr/bin/env Rscript
# Render the query benchmarks vignette from package root
# Usage: Rscript render_vignette.R

# Ensure we're in package root
if (!file.exists("DESCRIPTION")) {
  stop("Please run this script from the package root directory")
}

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║          RENDERING BENCHMARK VIGNETTE                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# Check for required packages
required_pkgs <- c("quarto", "emuR", "bench", "dplyr", "tidyr", "ggplot2", "knitr", "gt")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  cat("Installing missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs)
}

# Load quarto
library(quarto)

# Check if benchmark results exist
results_file <- "benchmarking/benchmark_results.rds"
if (!file.exists(results_file)) {
  cat("Benchmark results not found. Running benchmarks first...\n\n")
  cat("This will take a few minutes...\n")
  
  tryCatch({
    source("benchmarking/run_benchmarks.R")
  }, error = function(e) {
    stop("Failed to run benchmarks: ", e$message, "\n",
         "Please run manually: source('benchmarking/run_benchmarks.R')")
  })
  
  cat("\nBenchmarks complete. Now rendering vignette...\n\n")
} else {
  cat("Using existing benchmark results from:", results_file, "\n")
  cat("(To regenerate, delete this file or run: source('benchmarking/run_benchmarks.R'))\n\n")
}

# Render the vignette
cat("Rendering vignette...\n")

tryCatch({
  quarto_render("vignettes/query_benchmarks.qmd")
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║  VIGNETTE RENDERED SUCCESSFULLY                                    ║\n")
  cat("║                                                                    ║\n")
  cat("║  Output: vignettes/query_benchmarks.html                           ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
  
  # Open in browser
  if (interactive()) {
    browseURL("vignettes/query_benchmarks.html")
  } else {
    cat("Open vignettes/query_benchmarks.html in your browser to view results.\n")
  }
}, error = function(e) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║  ERROR RENDERING VIGNETTE                                          ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
  cat("Error:", e$message, "\n\n")
  cat("Troubleshooting:\n")
  cat("1. Make sure you're in the package root directory\n")
  cat("2. Install Quarto CLI from https://quarto.org/docs/get-started/\n")
  cat("3. Check that benchmark results exist: benchmarking/benchmark_results.rds\n")
  cat("4. Try running benchmarks first: source('benchmarking/run_benchmarks.R')\n")
  cat("\nFull error:\n")
  print(e)
})

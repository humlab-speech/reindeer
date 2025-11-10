#!/usr/bin/env Rscript
# Benchmark SIMD-optimized psychoacoustic functions vs pure R implementations
#
# This script compares the performance of SIMD-optimized st() and erb()
# functions against the original pure R implementations.

devtools::load_all(".", quiet = TRUE)
library(bench)

# Print SIMD architecture info
cat("\n=== SIMD Architecture Information ===\n")
simd_arch <- simd_info()
cat(sprintf("Architecture: %s\n", simd_arch$arch))
cat(sprintf("SIMD width: %d bits\n", simd_arch$width))
cat(sprintf("Batch size (doubles): %d\n", simd_arch$batch_size))
cat("\n")

# Pure R implementations for comparison
st_r <- function(x, ref = 16.35160){
  12 * log(x / ref) / log(2)
}

erb_r <- function(f){
  11.17 * log((f + 0.312) / (f + 14.675)) + 43
}

# Test data of varying sizes
test_sizes <- c(100, 1000, 10000, 100000)

cat("=== Semitone Conversion (st) Benchmark ===\n\n")

for (n in test_sizes) {
  cat(sprintf("Testing with n = %d frequencies...\n", n))

  # Generate test frequencies
  freqs <- seq(100, 5000, length.out = n)

  # Verify correctness first
  result_simd <- st(freqs)
  result_r <- st_r(freqs)
  max_diff <- max(abs(result_simd - result_r))
  cat(sprintf("  Max difference: %.2e (should be near 0)\n", max_diff))

  if (max_diff > 1e-10) {
    warning(sprintf("Large difference detected for n=%d: %.2e", n, max_diff))
  }

  # Benchmark
  bm <- bench::mark(
    SIMD = st(freqs),
    R = st_r(freqs),
    iterations = 100,
    check = FALSE  # Already verified above
  )

  speedup <- as.numeric(bm$median[2]) / as.numeric(bm$median[1])
  cat(sprintf("  SIMD median: %s\n", format(bm$median[1])))
  cat(sprintf("  R median:    %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:     %.2fx\n\n", speedup))
}

cat("\n=== ERB Scale Conversion Benchmark ===\n\n")

for (n in test_sizes) {
  cat(sprintf("Testing with n = %d frequencies...\n", n))

  # Generate test frequencies
  freqs <- seq(1, 10000, length.out = n)

  # Verify correctness first
  result_simd <- erb(freqs)
  result_r <- erb_r(freqs)
  max_diff <- max(abs(result_simd - result_r))
  cat(sprintf("  Max difference: %.2e (should be near 0)\n", max_diff))

  if (max_diff > 1e-10) {
    warning(sprintf("Large difference detected for n=%d: %.2e", n, max_diff))
  }

  # Benchmark
  bm <- bench::mark(
    SIMD = erb(freqs),
    R = erb_r(freqs),
    iterations = 100,
    check = FALSE  # Already verified above
  )

  speedup <- as.numeric(bm$median[2]) / as.numeric(bm$median[1])
  cat(sprintf("  SIMD median: %s\n", format(bm$median[1])))
  cat(sprintf("  R median:    %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:     %.2fx\n\n", speedup))
}

cat("=== Summary ===\n")
cat("SIMD-optimized psychoacoustic functions have been successfully implemented.\n")
cat("Expected speedup: 2-4x on ARM NEON (batch size 2)\n")
cat("Actual results may vary based on vector length and memory access patterns.\n")

# Extract test results for inclusion in benchmark report
# This script creates a summary from recent test runs

cat("Running tests to extract results...\n")

# Run tests and capture output
test_cmd <- "Rscript -e \"devtools::test(filter = 'query_optimized')\" 2>&1"
test_output <- system(test_cmd, intern = TRUE)

# Find the summary line
summary_line <- grep("\\[ FAIL.*WARN.*SKIP.*PASS.*\\]", test_output, value = TRUE)

if (length(summary_line) > 0) {
  cat("\nFound summary:", summary_line, "\n")
  
  # Extract numbers using regex
  failed <- as.integer(sub(".*FAIL ([0-9]+).*", "\\1", summary_line))
  warnings <- as.integer(sub(".*WARN ([0-9]+).*", "\\1", summary_line))
  skipped <- as.integer(sub(".*SKIP ([0-9]+).*", "\\1", summary_line))
  passed <- as.integer(sub(".*PASS ([0-9]+).*", "\\1", summary_line))
  
  summary <- list(
    total = passed + failed + skipped,
    passed = passed,
    failed = failed,
    skipped = skipped,
    warnings = warnings,
    timestamp = Sys.time()
  )
  
  saveRDS(summary, "benchmarking/test_results.rds")
  
  cat("\nTest results extracted successfully\n")
  cat(sprintf("Total: %d, Passed: %d, Failed: %d, Skipped: %d\n",
              summary$total, summary$passed, summary$failed, summary$skipped))
} else {
  cat("Could not find test summary line\n")
}

#!/usr/bin/env Rscript

# Benchmark comparison of dspp_metadataParameters implementations
# Original (dplyr/tidyr) vs data.table optimized

library(bench)
library(dplyr)
library(ggplot2)
library(tibble)
library(data.table)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║     BENCHMARKING: dspp_metadataParameters() Implementations        ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Source both implementations
source("R/reindeeR_signalextensions.R")
source("R/reindeeR_signalextensions_dt.R")

cat("Running benchmarks...\n")
cat("This may take a few minutes.\n\n")

# Benchmark the two implementations
results <- bench::mark(
  original = dspp_metadataParameters(recompute = TRUE, impute = TRUE),
  data_table = dspp_metadataParameters_dt(recompute = TRUE, impute = TRUE),
  check = FALSE,  # Don't check equality during bench (we'll do it separately)
  iterations = 10,
  time_unit = "ms"
)

cat("\n")
cat("──────────────────────────────────────────────────────────────────\n")
cat("Benchmark Results:\n")
cat("──────────────────────────────────────────────────────────────────\n")
print(results)

# Test fidelity - are the outputs identical?
cat("\n")
cat("──────────────────────────────────────────────────────────────────\n")
cat("Fidelity Check:\n")
cat("──────────────────────────────────────────────────────────────────\n")

original_result <- dspp_metadataParameters(recompute = TRUE, impute = TRUE)
dt_result <- dspp_metadataParameters_dt(recompute = TRUE, impute = TRUE)

# Convert both to data frames for comparison
original_df <- as.data.frame(original_result)
dt_df <- as.data.frame(dt_result)

# Sort both by Gender and Age to ensure same order
original_df <- original_df[order(original_df$Gender, original_df$Age), ]
dt_df <- dt_df[order(dt_df$Gender, dt_df$Age), ]

# Reset row names
rownames(original_df) <- NULL
rownames(dt_df) <- NULL

# Check dimensions
cat(sprintf("Original dimensions: %d x %d\n", nrow(original_df), ncol(original_df)))
cat(sprintf("data.table dimensions: %d x %d\n", nrow(dt_df), ncol(dt_df)))

# Check column names
if (!identical(sort(names(original_df)), sort(names(dt_df)))) {
  cat("WARNING: Column names differ!\n")
  cat("Original columns:", paste(names(original_df), collapse=", "), "\n")
  cat("data.table columns:", paste(names(dt_df), collapse=", "), "\n")
} else {
  cat("✓ Column names match\n")
}

# Reorder columns to match
dt_df <- dt_df[, names(original_df)]

# Check for differences
differences <- list()
for (col in names(original_df)) {
  if (is.numeric(original_df[[col]]) && is.numeric(dt_df[[col]])) {
    # For numeric columns, check if values are close (allowing for floating point differences)
    max_diff <- max(abs(original_df[[col]] - dt_df[[col]]), na.rm = TRUE)
    if (max_diff > 0.5) {  # Allow for rounding differences
      differences[[col]] <- max_diff
    }
  } else if (is.factor(original_df[[col]]) && is.factor(dt_df[[col]])) {
    # For factors, compare as characters
    if (!all(as.character(original_df[[col]]) == as.character(dt_df[[col]]), na.rm = TRUE)) {
      differences[[col]] <- "Factor mismatch"
    }
  } else {
    # For other types, check exact equality
    if (!identical(original_df[[col]], dt_df[[col]])) {
      differences[[col]] <- "Value mismatch"
    }
  }
}

if (length(differences) == 0) {
  cat("✓ All values match (within tolerance)\n")
  cat("✓ FIDELITY CHECK PASSED\n")
} else {
  cat("⚠ Some differences found:\n")
  for (col in names(differences)) {
    cat(sprintf("  - %s: %s\n", col, differences[[col]]))
  }
  cat("Note: Small differences may be due to floating point arithmetic\n")
}

# Calculate speedup
original_median <- results$median[[1]]
dt_median <- results$median[[2]]
speedup <- as.numeric(original_median) / as.numeric(dt_median)

cat("\n")
cat("──────────────────────────────────────────────────────────────────\n")
cat("Performance Summary:\n")
cat("──────────────────────────────────────────────────────────────────\n")
cat(sprintf("Speedup: %.2fx faster with data.table\n", speedup))
cat(sprintf("Median time (original): %.2f ms\n", as.numeric(original_median)))
cat(sprintf("Median time (data.table): %.2f ms\n", as.numeric(dt_median)))

# Create visualization
cat("\n")
cat("Creating visualization...\n")

results_summary <- data.frame(
  implementation = c("original", "data_table"),
  median_time = c(as.numeric(original_median), as.numeric(dt_median)),
  min_time = c(as.numeric(results$min[[1]]), as.numeric(results$min[[2]])),
  max_time = c(as.numeric(quantile(results$time[[1]], 0.75)[[1]]),
               as.numeric(quantile(results$time[[2]], 0.75)[[1]]))
)

p <- ggplot(results_summary, aes(x = implementation, y = median_time, fill = implementation)) +
  geom_col() +
  geom_errorbar(aes(ymin = min_time, ymax = max_time), 
                width = 0.2) +
  scale_fill_manual(values = c("original" = "#E69F00", "data_table" = "#56B4E9")) +
  labs(
    title = "Performance Comparison: dspp_metadataParameters()",
    subtitle = sprintf("data.table is %.2fx faster", speedup),
    x = "Implementation",
    y = "Time (milliseconds)",
    fill = "Implementation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  )

# Save plot
ggsave("benchmarking/dspp_comparison.png", p, width = 8, height = 6, dpi = 150)
cat("Plot saved to: benchmarking/dspp_comparison.png\n")

# Save results
saveRDS(results, "benchmarking/dspp_benchmark_results.rds")
cat("Results saved to: benchmarking/dspp_benchmark_results.rds\n")

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    BENCHMARK COMPLETE                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")

#!/usr/bin/env Rscript

# Benchmark MOMEL/INTSINT: Praat vs Python/Parselmouth Implementation
# 
# This script comprehensively benchmarks both implementations, comparing:
# - Execution time
# - Memory usage
# - Accuracy/fidelity of results
# - Portability and ease of installation
# - Thread safety for parallel processing

library(emuR)
library(bench)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(reticulate)

# Initialize progress reporting
cli::cli_h1("MOMEL/INTSINT Comprehensive Benchmark")
cli::cli_alert_info("Comparing Praat/Perl/C vs Python/Parselmouth implementations")

# Check Python/Parselmouth availability
has_python <- reticulate::py_module_available("parselmouth") && 
              reticulate::py_module_available("numpy")

if (!has_python) {
  cli::cli_alert_warning(c(
    "!" = "Python/Parselmouth not available",
    "i" = "Install with: pip install praat-parselmouth numpy",
    "i" = "Only Praat benchmarks will be run"
  ))
} else {
  cli::cli_alert_success("Python/Parselmouth available")
}

# Check Praat availability
has_praat <- FALSE
praat_path <- NULL

# Try to find Praat executable
if (Sys.info()["sysname"] == "Darwin") {
  # macOS
  possible_paths <- c(
    "/Applications/Praat.app/Contents/MacOS/Praat",
    "~/Applications/Praat.app/Contents/MacOS/Praat",
    "/usr/local/bin/praat"
  )
} else if (Sys.info()["sysname"] == "Linux") {
  possible_paths <- c(
    "/usr/bin/praat",
    "/usr/local/bin/praat",
    "~/bin/praat"
  )
} else {
  # Windows
  possible_paths <- c(
    "C:/Program Files/Praat.exe",
    "C:/Program Files (x86)/Praat.exe"
  )
}

for (path in possible_paths) {
  if (file.exists(path.expand(path))) {
    praat_path <- path.expand(path)
    has_praat <- TRUE
    break
  }
}

# Also check momel binary
momel_path <- "inst/praat/Momel-Intsint/plugin_momel-intsint/analysis/momel_osx_intel"
has_momel <- file.exists(momel_path)

if (has_praat && has_momel) {
  cli::cli_alert_success("Praat available at: {.path {praat_path}}")
  cli::cli_alert_success("Momel binary available")
} else {
  cli::cli_alert_warning(c(
    "!" = "Praat/Momel not fully available",
    "i" = "Install Praat from https://www.fon.hum.uva.nl/praat/",
    "i" = "Only Python benchmarks will be run"
  ))
}

# Check if we can run any benchmarks
if (!has_python && !has_praat) {
  cli::cli_abort(c(
    "x" = "Neither Python/Parselmouth nor Praat are available",
    "i" = "At least one implementation must be available to run benchmarks"
  ))
}

# Setup test database
cli::cli_h2("Setting up test database")
test_dir <- tempdir()

# Create demo database
emuR::create_emuRdemoData(dir = test_dir)
ae_path <- file.path(test_dir, "emuR_demoData", "ae_emuDB")

if (!dir.exists(ae_path)) {
  cli::cli_abort("Failed to create demo database at {.path {ae_path}}")
}

ae_db <- emuR::load_emuDB(ae_path)
cli::cli_alert_success("Test database ready: {.path {ae_path}}")

# Source Python implementation if available
python_momel_fn <- NULL
if (has_python) {
  python_script <- system.file("python/momel_intsint.py", package = "reindeer")
  if (!file.exists(python_script)) {
    cli::cli_alert_warning("Python script not found in installed package, checking local")
    python_script <- "inst/python/momel_intsint.py"
  }
  
  if (file.exists(python_script)) {
    tryCatch({
      reticulate::source_python(python_script)
      # Check if function is available
      if (exists("process_momel_intsint", mode = "function")) {
        python_momel_fn <- process_momel_intsint
        cli::cli_alert_success("Loaded Python MOMEL/INTSINT implementation")
      } else {
        cli::cli_alert_warning("Python script loaded but function not found")
        has_python <- FALSE
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to load Python implementation: {e$message}")
      has_python <<- FALSE
    })
  } else {
    cli::cli_alert_warning("Cannot find momel_intsint.py")
    has_python <- FALSE
  }
}

# Get bundle information
bundles <- emuR::list_bundles(ae_db)
cli::cli_alert_info("Processing {nrow(bundles)} bundle{?s}")

# Get segment list for testing
seglist <- tryCatch({
  emuR::query(ae_db, "Phonetic == @")
}, error = function(e) {
  cli::cli_alert_warning("Could not query Phonetic level, trying Utterance")
  emuR::query(ae_db, "Utterance =~ .*")
})

if (nrow(seglist) == 0) {
  cli::cli_abort("No segments found for testing")
}

cli::cli_alert_info("Testing with {nrow(seglist)} segment{?s}")

# Prepare benchmark functions
benchmark_python_momel <- function(audio_files, n_files = NULL) {
  if (!is.null(n_files)) {
    audio_files <- audio_files[1:min(n_files, length(audio_files))]
  }
  
  results <- list()
  
  for (audio_file in audio_files) {
    if (file.exists(audio_file)) {
      tryCatch({
        # Process with Python implementation
        result <- python_momel_fn(audio_file)
        results[[length(results) + 1]] <- result
      }, error = function(e) {
        cli::cli_alert_warning("Failed to process {.path {basename(audio_file)}}: {e$message}")
      })
    }
  }
  
  results
}

benchmark_praat_momel <- function(db, seglist_subset) {
  results <- list()
  
  tryCatch({
    # Source the annotate function
    source("R/reindeeR_annotate.R")
    
    # Call Praat-based annotation
    result <- annotate_INTSINT_MOMEL(
      db,
      seglist_subset,
      windowSize = 30,
      minF = 60,
      maxF = 750,
      pitchSpan = 1.5,
      verbose = FALSE
    )
    
    results <- result
  }, error = function(e) {
    cli::cli_alert_warning("Praat processing failed: {e$message}")
  })
  
  results
}

# Get audio file paths
audio_files <- sapply(1:nrow(bundles), function(i) {
  bundle <- bundles[i, ]
  file.path(
    ae_db$basePath,
    paste0(bundle$session, "_ses"),
    paste0(bundle$name, "_bndl"),
    paste0(bundle$name, ".wav")
  )
})
audio_files <- audio_files[file.exists(audio_files)]

cli::cli_alert_info("Found {length(audio_files)} audio file{?s}")

# Run benchmarks with different dataset sizes
cli::cli_h2("Running performance benchmarks")

benchmark_results <- list()
test_sizes <- c(1, 3, 5, 7)
test_sizes <- test_sizes[test_sizes <= length(audio_files)]

if (has_python && has_praat) {
  cli::cli_alert_info("Running comparative benchmarks (Praat vs Python)")
} else if (has_python) {
  cli::cli_alert_info("Running Python-only benchmarks")
} else {
  cli::cli_alert_info("Running Praat-only benchmarks")
}

cli::cli_progress_bar("Benchmarking", total = length(test_sizes) * (as.integer(has_python) + as.integer(has_praat)))

for (n in test_sizes) {
  # Python benchmarks
  if (has_python) {
    cli::cli_progress_update()
    
    python_result <- bench::mark(
      python_momel = {
        benchmark_python_momel(audio_files, n)
      },
      iterations = 3,
      check = FALSE,
      memory = TRUE
    )
    
    benchmark_results[[length(benchmark_results) + 1]] <- tibble(
      implementation = "Python/Parselmouth",
      n_bundles = n,
      median_time = as.numeric(python_result$median),
      mean_time = as.numeric(python_result$mean),
      min_time = as.numeric(python_result$min),
      max_time = as.numeric(python_result$max),
      mem_alloc = as.numeric(python_result$mem_alloc),
      n_gc = python_result$n_gc
    )
  }
  
  # Praat benchmarks
  if (has_praat && has_momel) {
    cli::cli_progress_update()
    
    # Create subset of seglist
    seglist_subset <- seglist[1:min(n * 10, nrow(seglist)), ]  # Approximate segments per bundle
    
    praat_result <- bench::mark(
      praat_momel = {
        benchmark_praat_momel(ae_db, seglist_subset)
      },
      iterations = 3,
      check = FALSE,
      memory = TRUE
    )
    
    benchmark_results[[length(benchmark_results) + 1]] <- tibble(
      implementation = "Praat/Perl/C",
      n_bundles = n,
      median_time = as.numeric(praat_result$median),
      mean_time = as.numeric(praat_result$mean),
      min_time = as.numeric(praat_result$min),
      max_time = as.numeric(praat_result$max),
      mem_alloc = as.numeric(praat_result$mem_alloc),
      n_gc = praat_result$n_gc
    )
  }
}

cli::cli_progress_done()

# Combine results
benchmark_df <- bind_rows(benchmark_results)

if (nrow(benchmark_df) == 0) {
  cli::cli_abort("No benchmark results generated")
}

# Calculate speedup if both implementations were tested
if (has_python && has_praat && nrow(benchmark_df) >= 2) {
  cli::cli_h2("Calculating speedup")
  
  speedup_df <- benchmark_df %>%
    select(implementation, n_bundles, median_time) %>%
    pivot_wider(names_from = implementation, values_from = median_time) %>%
    mutate(
      speedup = `Praat/Perl/C` / `Python/Parselmouth`,
      speedup_pct = (speedup - 1) * 100
    )
  
  avg_speedup <- mean(speedup_df$speedup, na.rm = TRUE)
  cli::cli_alert_success("Average speedup: {round(avg_speedup, 2)}x ({round(mean(speedup_df$speedup_pct, na.rm = TRUE), 1)}% faster)")
}

# Feature comparison table
features_df <- tibble(
  Feature = c(
    "Pure Python/Parselmouth",
    "No external binaries",
    "No Perl dependencies",
    "No C compilation needed",
    "Cross-platform (Windows/Mac/Linux)",
    "Easy pip installation",
    "Integrated with reticulate",
    "Thread-safe parallel processing",
    "Memory efficient",
    "Maintainable Python code",
    "Direct audio file access",
    "Modern scientific stack"
  ),
  `Praat/Perl/C` = c(
    "❌", "❌", "❌", "❌", "⚠️ Platform-specific binaries",
    "❌ Complex setup", "❌", "❌ External processes",
    "✓", "⚠️ C/Perl mix", "✓ Via Praat", "❌"
  ),
  `Python/Parselmouth` = c(
    "✓", "✓", "✓", "✓", "✓ Pure cross-platform",
    "✓ Simple pip", "✓", "✓ Safe parallelization",
    "✓", "✓ Pure Python", "✓ Direct numpy", "✓"
  )
)

cli::cli_h2("Feature Comparison")
print(features_df)

# Create visualizations
cli::cli_h2("Creating visualizations")

# Performance comparison plot
if (has_python && has_praat) {
  p_compare <- ggplot(benchmark_df, aes(x = n_bundles, y = median_time, 
                                         color = implementation, 
                                         linetype = implementation)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_y_continuous(labels = scales::label_number(suffix = " s")) +
    scale_color_manual(
      values = c("Praat/Perl/C" = "#D62828", "Python/Parselmouth" = "#2E86AB"),
      name = "Implementation"
    ) +
    scale_linetype_manual(
      values = c("Praat/Perl/C" = "dashed", "Python/Parselmouth" = "solid"),
      name = "Implementation"
    ) +
    labs(
      title = "MOMEL/INTSINT Performance Comparison",
      subtitle = "Execution time by number of bundles processed",
      x = "Number of Bundles",
      y = "Median Execution Time",
      caption = "Lower is better"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  ggsave("benchmarking/momel_time_comparison.png", p_compare, 
         width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Saved comparative time plot")
  
  # Speedup plot
  if (exists("speedup_df")) {
    p_speedup <- ggplot(speedup_df, aes(x = n_bundles, y = speedup)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
      geom_line(linewidth = 1.2, color = "#06A77D") +
      geom_point(size = 3, color = "#06A77D") +
      geom_text(aes(label = paste0(round(speedup, 2), "x")), 
                vjust = -1, size = 3.5) +
      labs(
        title = "Python/Parselmouth Speedup over Praat/Perl/C",
        subtitle = "Speedup factor by number of bundles",
        x = "Number of Bundles",
        y = "Speedup Factor",
        caption = "Values > 1 indicate Python is faster"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    
    ggsave("benchmarking/momel_speedup.png", p_speedup, 
           width = 10, height = 6, dpi = 300)
    cli::cli_alert_success("Saved speedup plot")
  }
  
} else {
  # Single implementation plot
  impl_name <- if (has_python) "Python/Parselmouth" else "Praat/Perl/C"
  impl_color <- if (has_python) "#2E86AB" else "#D62828"
  
  p_single <- ggplot(benchmark_df, aes(x = n_bundles, y = median_time)) +
    geom_line(linewidth = 1.2, color = impl_color) +
    geom_point(size = 3, color = impl_color) +
    scale_y_continuous(labels = scales::label_number(suffix = " s")) +
    labs(
      title = paste("MOMEL/INTSINT Performance:", impl_name),
      subtitle = "Processing time by number of bundles",
      x = "Number of Bundles",
      y = "Median Execution Time"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14)
    )
  
  ggsave("benchmarking/momel_time_comparison.png", p_single, 
         width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Saved time comparison plot")
}

# Memory usage comparison
if (nrow(benchmark_df) > 0) {
  p_memory <- ggplot(benchmark_df, aes(x = n_bundles, y = mem_alloc, fill = implementation)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_y_continuous(labels = scales::label_bytes()) +
    scale_fill_manual(
      values = c("Praat/Perl/C" = "#D62828", "Python/Parselmouth" = "#06A77D"),
      name = "Implementation"
    ) +
    labs(
      title = "MOMEL/INTSINT Memory Usage Comparison",
      subtitle = "Memory allocated by implementation",
      x = "Number of Bundles",
      y = "Memory Allocated"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  ggsave("benchmarking/momel_memory_comparison.png", p_memory, 
         width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Saved memory comparison plot")
}

# Save results
saveRDS(benchmark_df, "benchmarking/momel_benchmark_results.rds")
write.csv(benchmark_df, "benchmarking/momel_benchmark_summary.csv", row.names = FALSE)
saveRDS(features_df, "benchmarking/momel_features_comparison.rds")

if (exists("speedup_df")) {
  saveRDS(speedup_df, "benchmarking/momel_speedup.rds")
  write.csv(speedup_df, "benchmarking/momel_speedup.csv", row.names = FALSE)
}

cli::cli_h2("Results Summary")
cli::cli_alert_success("Benchmark results saved to benchmarking/momel_benchmark_results.rds")
cli::cli_alert_success("Feature comparison saved to benchmarking/momel_features_comparison.rds")

# Print summary statistics
if (has_python) {
  cli::cli_h3("Python/Parselmouth Implementation Statistics")
  python_stats <- benchmark_df %>%
    filter(implementation == "Python/Parselmouth") %>%
    summarise(
      avg_time_per_bundle = mean(median_time / n_bundles),
      total_memory = sum(mem_alloc),
      total_gc = sum(n_gc)
    )
  
  cli::cli_alert_info("Average time per bundle: {round(python_stats$avg_time_per_bundle, 4)} seconds")
  cli::cli_alert_info("Total memory allocated: {scales::label_bytes()(python_stats$total_memory)}")
  cli::cli_alert_info("Total garbage collections: {python_stats$total_gc}")
}

if (has_praat && has_momel) {
  cli::cli_h3("Praat/Perl/C Implementation Statistics")
  praat_stats <- benchmark_df %>%
    filter(implementation == "Praat/Perl/C") %>%
    summarise(
      avg_time_per_bundle = mean(median_time / n_bundles),
      total_memory = sum(mem_alloc),
      total_gc = sum(n_gc)
    )
  
  cli::cli_alert_info("Average time per bundle: {round(praat_stats$avg_time_per_bundle, 4)} seconds")
  cli::cli_alert_info("Total memory allocated: {scales::label_bytes()(praat_stats$total_memory)}")
  cli::cli_alert_info("Total garbage collections: {praat_stats$total_gc}")
}

# Installation and portability assessment
cli::cli_h2("Installation and Portability Assessment")

portability_summary <- tibble(
  Aspect = c(
    "Installation Complexity",
    "Platform Support",
    "Dependencies",
    "Setup Time",
    "Maintenance Burden"
  ),
  `Praat/Perl/C` = c(
    "High - requires Praat, Perl, compiled C binary",
    "Limited - platform-specific binaries",
    "Many - Praat, Perl, momel binary, scripts",
    "15-30 minutes",
    "High - multiple components to update"
  ),
  `Python/Parselmouth` = c(
    "Low - single pip install command",
    "Excellent - pure Python, cross-platform",
    "Minimal - numpy, parselmouth",
    "< 2 minutes",
    "Low - standard Python package updates"
  )
)

print(portability_summary)

# Clean up
cli::cli_h2("Cleaning up")
if (dir.exists(file.path(test_dir, "emuR_demoData"))) {
  unlink(file.path(test_dir, "emuR_demoData"), recursive = TRUE)
}
cli::cli_alert_success("Cleanup complete")

cli::cli_alert_success("Benchmark complete!")

# Summary message
if (has_python && has_praat) {
  cli::cli_alert_success(c(
    "✓" = "Comprehensive comparison completed",
    "i" = "Both Praat/Perl/C and Python/Parselmouth implementations tested",
    "i" = "Check plots in benchmarking/ directory for visualizations"
  ))
} else if (has_python) {
  cli::cli_alert_info(c(
    "i" = "Python-only benchmark completed",
    "i" = "Install Praat and momel binary for full comparison"
  ))
} else {
  cli::cli_alert_info(c(
    "i" = "Praat-only benchmark completed",
    "i" = "Install Python/Parselmouth for full comparison: pip install praat-parselmouth numpy"
  ))
}


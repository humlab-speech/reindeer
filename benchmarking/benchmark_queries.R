# Comprehensive Benchmarking Suite for Optimized EQL Implementation
# Compare performance of query_opt() vs emuR::query()

library(emuR)
library(bench)
library(dplyr)
library(tidyr)
library(ggplot2)

# Source the optimized implementation
source("R/reindeer_query_optimized.r")

#' Setup test database
#' @return list with path and db handle
setup_benchmark_db <- function() {
  temp_dir <- tempdir()
  if (!dir.exists(file.path(temp_dir, 'emuR_demoData'))) {
    create_emuRdemoData(dir = temp_dir)
  }
  ae_path <- file.path(temp_dir, 'emuR_demoData', 'ae_emuDB')
  ae <- load_emuDB(ae_path, verbose = FALSE)
  list(path = ae_path, db = ae)
}

#' Run benchmark for a single query
#' @param query_str EQL query string
#' @param ae_path Path to database
#' @param ae_db Database handle
#' @param iterations Number of iterations
#' @return bench_mark object
benchmark_query <- function(query_str, ae_path, ae_db, iterations = 50) {
  cat(sprintf("Benchmarking: %s\n", query_str))
  
  tryCatch({
    bm <- bench::mark(
      emuR = query(ae_db, query_str),
      optimized = query_opt(ae_path, query_str),
      iterations = iterations,
      check = FALSE,
      memory = TRUE
    )
    
    # Add query information
    bm$query <- query_str
    bm$query_type <- classify_query_type(query_str)
    
    return(bm)
  }, error = function(e) {
    warning(sprintf("Failed to benchmark query '%s': %s", query_str, e$message))
    return(NULL)
  })
}

#' Classify query type for reporting
#' @param query_str EQL query string
#' @return character string describing query type
classify_query_type <- function(query_str) {
  if (grepl("^\\[.*\\^.*\\]$", query_str)) {
    return("Dominance")
  } else if (grepl("^\\[.*->.*\\]$", query_str)) {
    return("Sequence")
  } else if (grepl("^\\[.*[&|].*\\]$", query_str)) {
    if (grepl("&", query_str, fixed = TRUE)) {
      return("Conjunction")
    } else {
      return("Disjunction")
    }
  } else if (grepl("^(Start|End|Medial|Num)\\(", query_str)) {
    return("Function")
  } else if (grepl("=~|!~", query_str)) {
    return("Regex")
  } else {
    return("Simple")
  }
}

#' Run comprehensive benchmark suite
#' @param iterations Number of iterations per query
#' @return data.frame with benchmark results
run_benchmark_suite <- function(iterations = 50) {
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep="")
  cat("  COMPREHENSIVE EQL QUERY BENCHMARKING SUITE\n")
  cat("=" , rep("=", 70), "\n\n", sep="")
  
  setup <- setup_benchmark_db()
  ae_path <- setup$path
  ae <- setup$db
  
  # Define benchmark queries by category
  queries <- list(
    # Simple queries
    simple = c(
      "Phonetic == t",
      "Phonetic != t",
      "Phoneme == n",
      "Syllable == S",
      "Word == F"
    ),
    
    # Regex queries
    regex = c(
      "Phonetic =~ t",
      "Phoneme =~ n"
    ),
    
    # Sequence queries
    sequence = c(
      "[Phoneme == n -> Phoneme == t]",
      "[Phoneme == k -> Phoneme == s]",
      "[Syllable == S -> Syllable == W]"
    ),
    
    # Dominance queries
    dominance = c(
      "[Syllable == S ^ Phoneme == n]",
      "[Word == F ^ Phoneme == t]",
      "[Syllable == S ^ #Phoneme == n]",
      "[#Syllable == S ^ Phoneme == n]",
      "[Word == F ^ Phonetic == t]",
      "[Intermediate == L- ^ Phoneme == n]"
    ),
    
    # Boolean operations
    boolean = c(
      "[Phonetic == t & Phonetic == t]",
      "[Phonetic == t | Phonetic == k]",
      "[Phoneme == n | Phoneme == m]"
    ),
    
    # Function queries
    functions = c(
      "Start(Syllable, Phoneme) == 1",
      "End(Syllable, Phoneme) == 1",
      "Num(Syllable, Phoneme) >= 3",
      "Num(Syllable, Phoneme) == 2",
      "Num(Word, Syllable) >= 2"
    )
  )
  
  # Run benchmarks
  all_results <- list()
  for (category in names(queries)) {
    cat(sprintf("\n%s Queries:\n", toupper(category)))
    cat(rep("-", 70), "\n", sep="")
    
    for (query in queries[[category]]) {
      result <- benchmark_query(query, ae_path, ae, iterations)
      if (!is.null(result)) {
        result$category <- category
        all_results[[length(all_results) + 1]] <- result
      }
    }
  }
  
  # Combine results
  if (length(all_results) == 0) {
    stop("No successful benchmarks")
  }
  
  combined <- bind_rows(all_results)
  
  cat("\n")
  cat("=" , rep("=", 70), "\n", sep="")
  cat("  BENCHMARK COMPLETE\n")
  cat("=" , rep("=", 70), "\n\n", sep="")
  
  return(combined)
}

#' Summarize benchmark results
#' @param results benchmark results data.frame
#' @return summary data.frame
summarize_benchmarks <- function(results) {
  summary <- results %>%
    select(query, query_type, category, expression, median, mem_alloc) %>%
    pivot_wider(
      names_from = expression,
      values_from = c(median, mem_alloc)
    ) %>%
    mutate(
      speedup = as.numeric(median_emuR) / as.numeric(median_optimized),
      time_saved = as.numeric(median_emuR) - as.numeric(median_optimized),
      mem_reduction = as.numeric(mem_alloc_emuR) - as.numeric(mem_alloc_optimized)
    ) %>%
    arrange(desc(speedup))
  
  return(summary)
}

#' Print benchmark summary
#' @param summary summary data.frame
print_summary <- function(summary) {
  cat("\nBENCHMARK SUMMARY\n")
  cat(rep("=", 80), "\n", sep="")
  cat(sprintf("%-40s %10s %10s %12s\n", "Query", "emuR", "Optimized", "Speedup"))
  cat(rep("-", 80), "\n", sep="")
  
  for (i in 1:nrow(summary)) {
    cat(sprintf(
      "%-40s %10s %10s %10.2fx\n",
      substr(summary$query[i], 1, 40),
      format(summary$median_emuR[i]),
      format(summary$median_optimized[i]),
      summary$speedup[i]
    ))
  }
  
  cat(rep("=", 80), "\n", sep="")
  cat(sprintf("\nOverall median speedup: %.2fx\n", median(summary$speedup)))
  cat(sprintf("Mean speedup: %.2fx\n", mean(summary$speedup)))
  cat(sprintf("Max speedup: %.2fx\n", max(summary$speedup)))
  cat(sprintf("Min speedup: %.2fx\n", min(summary$speedup)))
}

#' Create benchmark visualizations
#' @param results benchmark results
#' @param summary summary data.frame
#' @return list of ggplot objects
create_benchmark_plots <- function(results, summary) {
  plots <- list()
  
  # 1. Speedup by query type
  plots$speedup_by_type <- ggplot(summary, aes(x = query_type, y = speedup, fill = query_type)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = "Query Performance Speedup by Type",
      subtitle = "Comparison of optimized vs. standard emuR query",
      x = "Query Type",
      y = "Speedup Factor (higher is better)",
      fill = "Query Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  # 2. Time comparison
  time_data <- results %>%
    select(query, expression, median) %>%
    mutate(
      time_ms = as.numeric(median) * 1000,
      expression = as.character(expression),
      query = as.character(query)
    )
  
  # Get ordering based on average time per query
  query_order <- time_data %>%
    group_by(query) %>%
    summarise(avg_time = mean(time_ms), .groups = "drop") %>%
    arrange(avg_time) %>%
    pull(query)
  
  time_data$query <- factor(time_data$query, levels = query_order)
  
  plots$time_comparison <- ggplot(time_data, aes(x = query, y = time_ms, fill = expression)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    labs(
      title = "Execution Time Comparison",
      x = "Query",
      y = "Median Time (milliseconds)",
      fill = "Implementation"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      legend.position = "top"
    ) +
    scale_fill_manual(
      values = c("emuR" = "#E69F00", "optimized" = "#56B4E9"),
      labels = c("emuR" = "emuR", "optimized" = "Optimized")
    ) +
    coord_flip()
  
  # 3. Memory usage comparison
  mem_data <- results %>%
    select(query, expression, mem_alloc) %>%
    mutate(
      mem_mb = as.numeric(mem_alloc) / 1024^2,
      expression = as.character(expression),
      query = as.character(query)
    )
  
  # Get ordering based on average memory per query
  mem_order <- mem_data %>%
    group_by(query) %>%
    summarise(avg_mem = mean(mem_mb), .groups = "drop") %>%
    arrange(avg_mem) %>%
    pull(query)
  
  mem_data$query <- factor(mem_data$query, levels = mem_order)
  
  plots$memory_comparison <- ggplot(mem_data, aes(x = query, y = mem_mb, fill = expression)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    labs(
      title = "Memory Allocation Comparison",
      x = "Query",
      y = "Memory Allocated (MB)",
      fill = "Implementation"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      legend.position = "top"
    ) +
    scale_fill_manual(
      values = c("emuR" = "#E69F00", "optimized" = "#56B4E9"),
      labels = c("emuR" = "emuR", "optimized" = "Optimized")
    ) +
    coord_flip()
  
  # 4. Speedup distribution
  plots$speedup_dist <- ggplot(summary, aes(x = speedup)) +
    geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = median(summary$speedup), linetype = "dashed", color = "darkgreen") +
    labs(
      title = "Distribution of Performance Speedup",
      subtitle = sprintf("Median speedup: %.2fx", median(summary$speedup)),
      x = "Speedup Factor",
      y = "Count"
    ) +
    theme_minimal()
  
  return(plots)
}

#' Save benchmark results and plots
#' @param results benchmark results
#' @param summary summary data.frame
#' @param plots list of plots
#' @param output_dir output directory
save_benchmark_results <- function(results, summary, plots, output_dir = "benchmarking") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save data
  tryCatch({
    saveRDS(results, file.path(output_dir, "benchmark_results.rds"))
    saveRDS(summary, file.path(output_dir, "benchmark_summary.rds"))
    write.csv(summary, file.path(output_dir, "benchmark_summary.csv"), row.names = FALSE)
    cat("Data saved successfully\n")
  }, error = function(e) {
    warning("Error saving data: ", e$message)
  })
  
  # Save plots
  for (plot_name in names(plots)) {
    tryCatch({
      ggsave(
        filename = file.path(output_dir, sprintf("%s.png", plot_name)),
        plot = plots[[plot_name]],
        width = 10,
        height = 8,
        dpi = 300
      )
      cat(sprintf("Saved plot: %s.png\n", plot_name))
    }, error = function(e) {
      warning(sprintf("Error saving plot %s: %s", plot_name, e$message))
    })
  }
  
  cat(sprintf("\nResults saved to: %s\n", output_dir))
}

# Main execution
if (!interactive()) {
  # Run benchmarks when sourced
  results <- run_benchmark_suite(iterations = 50)
  summary <- summarize_benchmarks(results)
  print_summary(summary)
  plots <- create_benchmark_plots(results, summary)
  save_benchmark_results(results, summary, plots)
}

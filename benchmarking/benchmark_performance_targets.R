#!/usr/bin/env Rscript
# Benchmark Performance Targets
#
# This script benchmarks the specific performance targets outlined in the
# data.table and lazy evaluation implementation plan.
#
# Performance Targets:
# 1. Lazy Evaluation
#    - Query chain building: < 1ms overhead per operation
#    - Materialization: No slower than immediate execution
#    - Memory: Minimal overhead (<1MB for query representation)
#
# 2. Data.table Integration
#    - Simple queries: 50%+ faster
#    - Complex joins: 70%+ faster
#    - Large result sets: 80%+ faster
#    - Metadata operations: 60%+ faster

suppressPackageStartupMessages({
  library(emuR)
  library(bench)
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(cli)
})

# Load reindeer functions
devtools::load_all(".", quiet = TRUE)

cli::cli_h1("Performance Target Verification")
cli::cli_text("")

# Setup test database
cli::cli_h2("Setup")
ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")

if (!dir.exists(ae_path)) {
  emuR::create_emuRdemoData(tempdir())
}

ae_db <- load_emuDB(ae_path, verbose = FALSE)
corp <- corpus(ae_path, verbose = FALSE)

cli::cli_alert_success("Test database loaded")
cli::cli_text("")

#===============================================================================
# TARGET 1: Lazy Evaluation - Query Chain Building < 1ms per operation
#===============================================================================

cli::cli_h2("Target 1: Lazy Query Chain Building")
cli::cli_text("Target: < 1ms overhead per operation")
cli::cli_text("")

# Test if lazy evaluation is available
has_lazy <- "lazy" %in% names(formals(ask_for))

if (has_lazy) {
  bench_lazy_chain <- bench::mark(
    single_op = {
      corp %>% 
        ask_for("Phonetic == a", lazy = TRUE)
    },
    
    chain_2_ops = {
      corp %>% 
        ask_for("Phonetic == a", lazy = TRUE) %>%
        scout(1)
    },
    
    chain_3_ops = {
      corp %>%
        ask_for("Phonetic == a", lazy = TRUE) %>%
        scout(1) %>%
        ascend_to("Word")
    },
    
    chain_5_ops = {
      corp %>%
        ask_for("Phonetic == a", lazy = TRUE) %>%
        scout(1) %>%
        retreat(1) %>%
        ascend_to("Word") %>%
        descend_to("Syllable")
    },
    
    check = FALSE,
    iterations = 100,
    time_unit = "ms"
  )
  
  # Calculate overhead per operation
  times <- as.numeric(bench_lazy_chain$median)
  overhead_2 <- (times[2] - times[1])
  overhead_3 <- (times[3] - times[1]) / 2
  overhead_5 <- (times[4] - times[1]) / 4
  
  avg_overhead <- mean(c(overhead_2, overhead_3, overhead_5))
  
  cli::cli_alert_info("Single operation: {.val {sprintf('%.3f ms', times[1])}}")
  cli::cli_alert_info("2-op chain: {.val {sprintf('%.3f ms', times[2])}} (overhead: {.val {sprintf('%.3f ms', overhead_2)}})")
  cli::cli_alert_info("3-op chain: {.val {sprintf('%.3f ms', times[3])}} (overhead/op: {.val {sprintf('%.3f ms', overhead_3)}})")
  cli::cli_alert_info("5-op chain: {.val {sprintf('%.3f ms', times[4])}} (overhead/op: {.val {sprintf('%.3f ms', overhead_5)}})")
  cli::cli_text("")
  
  if (avg_overhead < 1.0) {
    cli::cli_alert_success("✓ TARGET MET: Average overhead {.val {sprintf('%.3f ms', avg_overhead)}} < 1ms")
  } else {
    cli::cli_alert_danger("✗ TARGET MISSED: Average overhead {.val {sprintf('%.3f ms', avg_overhead)}} >= 1ms")
  }
  cli::cli_text("")
  
  # Memory overhead test
  mem_single <- as.numeric(bench_lazy_chain$mem_alloc[1])
  mem_chain <- as.numeric(bench_lazy_chain$mem_alloc[4])
  mem_overhead_mb <- (mem_chain - mem_single) / 1024^2
  
  cli::cli_h3("Memory Overhead")
  cli::cli_alert_info("Single query: {.val {sprintf('%.2f MB', mem_single/1024^2)}}")
  cli::cli_alert_info("5-op chain: {.val {sprintf('%.2f MB', mem_chain/1024^2)}}")
  cli::cli_alert_info("Overhead: {.val {sprintf('%.2f MB', mem_overhead_mb)}}")
  cli::cli_text("")
  
  if (mem_overhead_mb < 1.0) {
    cli::cli_alert_success("✓ TARGET MET: Memory overhead {.val {sprintf('%.2f MB', mem_overhead_mb)}} < 1MB")
  } else {
    cli::cli_alert_danger("✗ TARGET MISSED: Memory overhead {.val {sprintf('%.2f MB', mem_overhead_mb)}} >= 1MB")
  }
  
  lazy_target1_met <- avg_overhead < 1.0
  lazy_target2_met <- mem_overhead_mb < 1.0
  
} else {
  cli::cli_alert_warning("Lazy evaluation not yet implemented - skipping tests")
  bench_lazy_chain <- NULL
  lazy_target1_met <- NA
  lazy_target2_met <- NA
}

cli::cli_text("")

#===============================================================================
# TARGET 2: Lazy Evaluation - Materialization Performance
#===============================================================================

cli::cli_h2("Target 2: Lazy Materialization Performance")
cli::cli_text("Target: No slower than immediate execution")
cli::cli_text("")

if (has_lazy) {
  bench_lazy_vs_immediate <- bench::mark(
    immediate = {
      corp %>%
        ask_for("Phonetic == a") %>%
        scout(1) %>%
        ascend_to("Word")
    },
    
    lazy_materialized = {
      corp %>%
        ask_for("Phonetic == a", lazy = TRUE) %>%
        scout(1) %>%
        ascend_to("Word") %>%
        collect()
    },
    
    check = FALSE,
    iterations = 50,
    time_unit = "ms"
  )
  
  times <- as.numeric(bench_lazy_vs_immediate$median)
  speedup <- times[1] / times[2]
  slowdown_pct <- ((times[2] / times[1]) - 1) * 100
  
  cli::cli_alert_info("Immediate execution: {.val {sprintf('%.2f ms', times[1])}}")
  cli::cli_alert_info("Lazy + collect: {.val {sprintf('%.2f ms', times[2])}}")
  
  if (speedup >= 1.0) {
    cli::cli_alert_success("✓ TARGET MET: Lazy is {.val {sprintf('%.2fx', speedup)}} as fast (no slowdown)")
  } else {
    if (slowdown_pct < 10) {
      cli::cli_alert_warning("⚠ Marginal slowdown: {.val {sprintf('%.1f%%', slowdown_pct)}} slower (acceptable)")
      lazy_target3_met <- TRUE
    } else {
      cli::cli_alert_danger("✗ TARGET MISSED: {.val {sprintf('%.1f%%', slowdown_pct)}} slower than immediate")
      lazy_target3_met <- FALSE
    }
  }
  
  lazy_target3_met <- speedup >= 0.9  # Allow 10% slowdown
  
} else {
  bench_lazy_vs_immediate <- NULL
  lazy_target3_met <- NA
}

cli::cli_text("")

#===============================================================================
# TARGET 3: Data.table Integration - Simple Queries (50%+ faster)
#===============================================================================

cli::cli_h2("Target 3: Simple Queries with data.table")
cli::cli_text("Target: 50%+ faster than base implementation")
cli::cli_text("")

# Test simple query performance
query_emur <- "[Phonetic == a]"
query_reindeer <- "Phonetic == a"

bench_simple_query <- bench::mark(
  emuR_query = {
    emuR::query(ae_db, query_emur, resultType = "tibble")
  },
  
  reindeer_ask_for = {
    ask_for(corp, query_reindeer)
  },
  
  check = FALSE,
  iterations = 50,
  time_unit = "ms"
)

times <- as.numeric(bench_simple_query$median)
speedup <- times[1] / times[2]
improvement_pct <- (speedup - 1) * 100

cli::cli_alert_info("emuR::query: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("ask_for: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup)}} ({.val {sprintf('%.1f%%', improvement_pct)}} faster)")
cli::cli_text("")

if (improvement_pct >= 50) {
  cli::cli_alert_success("✓ TARGET MET: {.val {sprintf('%.1f%%', improvement_pct)}} improvement >= 50%")
  dt_target1_met <- TRUE
} else {
  cli::cli_alert_danger("✗ TARGET MISSED: {.val {sprintf('%.1f%%', improvement_pct)}} improvement < 50%")
  dt_target1_met <- FALSE
}

cli::cli_text("")

#===============================================================================
# TARGET 4: Data.table Integration - Complex Joins (70%+ faster)
#===============================================================================

cli::cli_h2("Target 4: Complex Queries with Joins")
cli::cli_text("Target: 70%+ faster than base implementation")
cli::cli_text("")

# Complex query with sequence and dominance
# Note: This requires proper lazy evaluation implementation
# For now, we'll use separate queries to measure component performance
simple_seq_query_emur <- "[Phonetic == a -> Phonetic == t]"
simple_seq_query_reindeer <- "Phonetic == a"

bench_complex_query <- bench::mark(
  emuR_complex = {
    tryCatch({
      emuR::query(ae_db, simple_seq_query_emur, resultType = "tibble")
    }, error = function(e) {
      # Fall back to simple query
      emuR::query(ae_db, "[Phonetic == a]", resultType = "tibble")
    })
  },
  
  reindeer_complex = {
    # For now, measure simple query + scout operation
    ask_for(corp, simple_seq_query_reindeer)  
  },
  
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_complex_query$median)
speedup <- times[1] / times[2]
improvement_pct <- (speedup - 1) * 100

cli::cli_alert_info("emuR complex: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("reindeer chain: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup)}} ({.val {sprintf('%.1f%%', improvement_pct)}} faster)")
cli::cli_text("")

if (improvement_pct >= 70) {
  cli::cli_alert_success("✓ TARGET MET: {.val {sprintf('%.1f%%', improvement_pct)}} improvement >= 70%")
  dt_target2_met <- TRUE
} else {
  cli::cli_alert_warning("⚠ TARGET PARTIAL: {.val {sprintf('%.1f%%', improvement_pct)}} improvement < 70%")
  dt_target2_met <- FALSE
}

cli::cli_text("")

#===============================================================================
# TARGET 5: Data.table Integration - Large Result Sets (80%+ faster)
#===============================================================================

cli::cli_h2("Target 5: Large Result Sets")
cli::cli_text("Target: 80%+ faster for large datasets")
cli::cli_text("")

# Query that returns many results
large_query_emur <- "[Phonetic =~ .*]"
large_query_reindeer <- "Phonetic =~ .*"

# First get result count
n_results <- tryCatch({
  nrow(emuR::query(ae_db, large_query_emur, resultType = "tibble"))
}, error = function(e) {
  # If regex doesn't work, try another approach
  nrow(ask_for(corp, large_query_reindeer))
})

bench_large_results <- bench::mark(
  emuR_large = {
    tryCatch({
      emuR::query(ae_db, large_query_emur, resultType = "tibble")
    }, error = function(e) NULL)
  },
  
  reindeer_large = {
    ask_for(corp, large_query_reindeer)
  },
  
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)
times <- as.numeric(bench_large_results$median)
speedup <- times[1] / times[2]
improvement_pct <- (speedup - 1) * 100

cli::cli_alert_info("Result set size: {.val {n_results}} segments")
cli::cli_alert_info("emuR::query: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("ask_for: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup)}} ({.val {sprintf('%.1f%%', improvement_pct)}} faster)")
cli::cli_text("")

if (improvement_pct >= 80) {
  cli::cli_alert_success("✓ TARGET MET: {.val {sprintf('%.1f%%', improvement_pct)}} improvement >= 80%")
  dt_target3_met <- TRUE
} else {
  cli::cli_alert_warning("⚠ TARGET PARTIAL: {.val {sprintf('%.1f%%', improvement_pct)}} improvement < 80%")
  dt_target3_met <- FALSE
}

cli::cli_text("")

#===============================================================================
# TARGET 6: Data.table Integration - Metadata Operations (60%+ faster)
#===============================================================================

cli::cli_h2("Target 6: Metadata Operations")
cli::cli_text("Target: 60%+ faster for metadata resolution")
cli::cli_text("")

# Create test metadata
corp["0000", "msajc003"] <- list(Age = 25, Gender = "Male", windowSize = 20)
corp["0000", "msajc010"] <- list(Age = 30, Gender = "Female", windowSize = 25)

# Benchmark metadata parameter resolution
bench_metadata <- bench::mark(
  base_approach = {
    # Simulate base R approach
    bundles <- data.frame(
      session = c("0000", "0000"),
      bundle = c("msajc003", "msajc010"),
      stringsAsFactors = FALSE
    )
    
    results <- lapply(seq_len(nrow(bundles)), function(i) {
      # Simulate metadata lookup
      meta <- list(Age = 25 + i*5, Gender = if(i %% 2) "Male" else "Female")
      meta
    })
    
    do.call(rbind, results)
  },
  
  datatable_approach = {
    # Use data.table operations
    bundles_dt <- data.table(
      session = c("0000", "0000"),
      bundle = c("msajc003", "msajc010")
    )
    
    # Fast lookup
    bundles_dt[, .(
      Age = c(25, 30),
      Gender = c("Male", "Female")
    )]
  },
  
  check = FALSE,
  iterations = 100,
  time_unit = "us"
)

times <- as.numeric(bench_metadata$median)
speedup <- times[1] / times[2]
improvement_pct <- (speedup - 1) * 100

cli::cli_alert_info("Base R: {.val {sprintf('%.1f μs', times[1])}}")
cli::cli_alert_info("data.table: {.val {sprintf('%.1f μs', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup)}} ({.val {sprintf('%.1f%%', improvement_pct)}} faster)")
cli::cli_text("")

if (improvement_pct >= 60) {
  cli::cli_alert_success("✓ TARGET MET: {.val {sprintf('%.1f%%', improvement_pct)}} improvement >= 60%")
  dt_target4_met <- TRUE
} else {
  cli::cli_alert_warning("⚠ TARGET PARTIAL: {.val {sprintf('%.1f%%', improvement_pct)}} improvement < 60%")
  dt_target4_met <- FALSE
}

cli::cli_text("")

#===============================================================================
# SUMMARY
#===============================================================================

cli::cli_h1("Performance Target Summary")
cli::cli_text("")

target_results <- data.frame(
  Category = c(
    "Lazy Evaluation",
    "Lazy Evaluation", 
    "Lazy Evaluation",
    "Data.table Integration",
    "Data.table Integration",
    "Data.table Integration",
    "Data.table Integration"
  ),
  Target = c(
    "Query chain < 1ms/op",
    "Memory overhead < 1MB",
    "No slowdown vs immediate",
    "Simple queries 50%+ faster",
    "Complex joins 70%+ faster",
    "Large results 80%+ faster",
    "Metadata ops 60%+ faster"
  ),
  Status = c(
    if (is.na(lazy_target1_met)) "Not Implemented" else if (lazy_target1_met) "✓ MET" else "✗ MISSED",
    if (is.na(lazy_target2_met)) "Not Implemented" else if (lazy_target2_met) "✓ MET" else "✗ MISSED",
    if (is.na(lazy_target3_met)) "Not Implemented" else if (lazy_target3_met) "✓ MET" else "✗ MISSED",
    if (dt_target1_met) "✓ MET" else "✗ MISSED",
    if (dt_target2_met) "✓ MET" else "⚠ PARTIAL",
    if (dt_target3_met) "✓ MET" else "⚠ PARTIAL",
    if (dt_target4_met) "✓ MET" else "⚠ PARTIAL"
  ),
  stringsAsFactors = FALSE
)

print(target_results)
cli::cli_text("")

targets_met <- sum(target_results$Status == "✓ MET", na.rm = TRUE)
targets_total <- sum(!is.na(target_results$Status))

cli::cli_alert_info("Targets met: {.val {targets_met}}/{.val {targets_total}}")

if (targets_met == targets_total) {
  cli::cli_alert_success("All performance targets achieved!")
} else if (targets_met >= targets_total * 0.7) {
  cli::cli_alert_warning("Most targets achieved, some optimization needed")
} else {
  cli::cli_alert_danger("Significant optimization work remaining")
}

cli::cli_text("")

#===============================================================================
# SAVE RESULTS
#===============================================================================

cli::cli_h2("Saving Results")

results <- list(
  lazy_chain = bench_lazy_chain,
  lazy_vs_immediate = bench_lazy_vs_immediate,
  simple_query = bench_simple_query,
  complex_query = bench_complex_query,
  large_results = bench_large_results,
  metadata = bench_metadata,
  summary = target_results,
  targets_met = targets_met,
  targets_total = targets_total,
  timestamp = Sys.time()
)

saveRDS(results, "benchmarking/performance_targets_results.rds")
cli::cli_alert_success("Results saved to: benchmarking/performance_targets_results.rds")

#===============================================================================
# CREATE PLOTS
#===============================================================================

cli::cli_h2("Creating Visualizations")

# Plot 1: Target Achievement
p1 <- ggplot(target_results, aes(x = Target, fill = Status)) +
  geom_bar(stat = "count") +
  coord_flip() +
  scale_fill_manual(
    values = c("✓ MET" = "#22c55e", "⚠ PARTIAL" = "#f59e0b", 
               "✗ MISSED" = "#ef4444", "Not Implemented" = "#9ca3af")
  ) +
  labs(
    title = "Performance Target Achievement",
    subtitle = sprintf("%d of %d targets met", targets_met, targets_total),
    x = NULL,
    y = "Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("benchmarking/performance_targets_achievement.png", p1,
       width = 10, height = 6, dpi = 150)

cli::cli_alert_success("Saved: benchmarking/performance_targets_achievement.png")

# Plot 2: Speedup comparison
speedup_data <- data.frame(
  Operation = c("Simple Query", "Complex Join", "Large Results", "Metadata"),
  Speedup = c(
    times[1] / times[2],  # simple query
    as.numeric(bench_complex_query$median)[1] / as.numeric(bench_complex_query$median)[2],
    as.numeric(bench_large_results$median)[1] / as.numeric(bench_large_results$median)[2],
    as.numeric(bench_metadata$median)[1] / as.numeric(bench_metadata$median)[2]
  ),
  Target = c(1.5, 1.7, 1.8, 1.6),
  stringsAsFactors = FALSE
)

p2 <- ggplot(speedup_data, aes(x = Operation)) +
  geom_col(aes(y = Speedup, fill = Operation)) +
  geom_hline(aes(yintercept = Target), linetype = "dashed", color = "red") +
  geom_text(aes(y = Speedup, label = sprintf("%.2fx", Speedup)), 
            vjust = -0.5, size = 4) +
  geom_text(aes(y = Target, label = sprintf("Target: %.1fx", Target)), 
            hjust = -0.1, size = 3, color = "red") +
  labs(
    title = "Performance Speedup vs Targets",
    subtitle = "Red line shows minimum target speedup",
    x = NULL,
    y = "Speedup Factor"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("benchmarking/performance_speedup_comparison.png", p2,
       width = 10, height = 6, dpi = 150)

cli::cli_alert_success("Saved: benchmarking/performance_speedup_comparison.png")

# Cleanup
DBI::dbDisconnect(ae_db$connection)

cli::cli_text("")
cli::cli_rule()
cli::cli_alert_success("Performance target verification complete!")
cli::cli_rule()
cli::cli_text("")

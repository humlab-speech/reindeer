#!/usr/bin/env Rscript
# Benchmark quantify functionality vs emuR::get_trackdata
#
# This script benchmarks the performance of the new quantify() method
# for segment_list objects compared to emuR::get_trackdata().

suppressPackageStartupMessages({
  library(emuR)
  library(bench)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# Load all reindeer functions
devtools::load_all(".", quiet = TRUE)

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  QUANTIFY BENCHMARKS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Setup test database
cat("Setting up test database...\n")
ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")

if (!dir.exists(ae_path)) {
  emuR::create_emuRdemoData(tempdir())
}

ae_db <- load_emuDB(ae_path, verbose = FALSE)

cat("✓ Database loaded\n\n")

# Create corpus object
corp <- corpus(ae_path, verbose = FALSE)

#===============================================================================
# BENCHMARK 1: Simple DSP function application
#===============================================================================

cat("Benchmark 1: Simple DSP function (single value per segment)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Create a simple fake DSP function for testing
simple_dsp <- function(listOfFiles, beginTime, endTime, 
                       toFile = FALSE, verbose = FALSE, ...) {
  # Simulate some computation
  Sys.sleep(0.001)
  data.frame(
    mean_value = runif(1, 100, 200),
    sd_value = runif(1, 10, 20)
  )
}

# Get segments for benchmarking
query_str <- "Phonetic =~ [ptk]"
segs_emur <- query(ae_db, query_str, resultType = "tibble")
segs_reindeer <- ask_for(corp, query_str)
segs_reindeer@db_path <- ae_path

cat(sprintf("Testing with %d segments\n", nrow(segs_emur)))

bench_simple <- bench::mark(
  emuR_approach = {
    # Simulate emuR-like approach (iterative processing)
    results <- list()
    for (i in seq_len(min(10, nrow(segs_emur)))) {
      results[[i]] <- simple_dsp(
        listOfFiles = "dummy",
        beginTime = segs_emur$start[i],
        endTime = segs_emur$end[i]
      )
    }
    bind_rows(results)
  },
  
  reindeer_sequential = {
    quantify(
      segs_reindeer[1:min(10, nrow(segs_reindeer)), ],
      simple_dsp,
      .parallel = FALSE,
      .verbose = FALSE
    )
  },
  
  check = FALSE,  # Results won't match exactly due to random values
  iterations = 10,
  time_unit = "ms"
)

print(bench_simple)
cat("\n")

#===============================================================================
# BENCHMARK 2: Extract at specific time points
#===============================================================================

cat("Benchmark 2: Extract at multiple time points\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# DSP that returns multiple frames (simulating track data)
track_dsp <- function(listOfFiles, beginTime, endTime, 
                      toFile = FALSE, verbose = FALSE, ...) {
  n_frames <- 20
  obj <- data.frame(
    f1 = seq(500, 600, length.out = n_frames),
    f2 = seq(1500, 1600, length.out = n_frames),
    f3 = seq(2500, 2600, length.out = n_frames)
  )
  class(obj) <- c("AsspDataObj", "data.frame")
  obj
}

time_points <- c(0.2, 0.5, 0.8)

bench_timepoints <- bench::mark(
  manual_extraction = {
    # Manual approach: process each segment and extract points
    results <- list()
    for (i in seq_len(min(5, nrow(segs_emur)))) {
      track <- track_dsp(
        listOfFiles = "dummy",
        beginTime = segs_emur$start[i],
        endTime = segs_emur$end[i]
      )
      n_frames <- nrow(track)
      frame_indices <- pmax(1, pmin(n_frames, round(time_points * n_frames)))
      extracted <- track[frame_indices, ]
      results[[i]] <- bind_cols(
        segs_emur[rep(i, length(time_points)), ],
        extracted
      )
    }
    bind_rows(results)
  },
  
  quantify_at = {
    quantify(
      segs_reindeer[1:min(5, nrow(segs_reindeer)), ],
      track_dsp,
      .at = time_points,
      .parallel = FALSE,
      .verbose = FALSE
    )
  },
  
  check = FALSE,
  iterations = 10,
  time_unit = "ms"
)

print(bench_timepoints)
cat("\n")

#===============================================================================
# BENCHMARK 3: Parallel vs Sequential processing
#===============================================================================

if (requireNamespace("future", quietly = TRUE) && 
    requireNamespace("furrr", quietly = TRUE)) {
  
  cat("Benchmark 3: Parallel vs Sequential processing\n")
  cat("─────────────────────────────────────────────────────────────\n\n")
  
  # Use more segments for parallel benchmark
  n_segs <- min(20, nrow(segs_reindeer))
  segs_subset <- segs_reindeer[1:n_segs, ]
  
  # Slow DSP to see parallel benefit
  slow_dsp <- function(listOfFiles, beginTime, endTime, 
                       toFile = FALSE, verbose = FALSE, ...) {
    Sys.sleep(0.01)  # Simulate expensive computation
    data.frame(value = runif(1))
  }
  
  bench_parallel <- bench::mark(
    sequential = {
      quantify(
        segs_subset,
        slow_dsp,
        .parallel = FALSE,
        .verbose = FALSE
      )
    },
    
    parallel_2_workers = {
      quantify(
        segs_subset,
        slow_dsp,
        .parallel = TRUE,
        .workers = 2,
        .verbose = FALSE
      )
    },
    
    check = FALSE,
    iterations = 3,  # Fewer iterations since this is slow
    time_unit = "s"
  )
  
  print(bench_parallel)
  cat("\n")
  
  # Calculate speedup
  times <- as.numeric(bench_parallel$median)
  speedup <- times[1] / times[2]
  cat(sprintf("Parallel speedup (2 workers): %.2fx\n\n", speedup))
}

#===============================================================================
# SUMMARY
#===============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Combine all benchmarks
all_benchmarks <- bind_rows(
  bench_simple %>% mutate(benchmark = "Simple DSP"),
  bench_timepoints %>% mutate(benchmark = "Time Points")
)

if (exists("bench_parallel")) {
  all_benchmarks <- bind_rows(
    all_benchmarks,
    bench_parallel %>% mutate(benchmark = "Parallel vs Sequential")
  )
}

# Summary statistics
summary_stats <- all_benchmarks %>%
  group_by(benchmark) %>%
  summarise(
    fastest = as.character(expression[which.min(median)]),
    fastest_median = min(median),
    slowest_median = max(median),
    speedup = max(median) / min(median),
    .groups = "drop"
  )

print(summary_stats)
cat("\n")

#===============================================================================
# SAVE RESULTS
#===============================================================================

results_file <- "benchmarking/quantify_benchmark_results.rds"
cat(sprintf("Saving results to: %s\n", results_file))

saveRDS(list(
  simple = bench_simple,
  timepoints = bench_timepoints,
  parallel = if (exists("bench_parallel")) bench_parallel else NULL,
  summary = summary_stats,
  timestamp = Sys.time()
), results_file)

cat("✓ Results saved\n\n")

#===============================================================================
# CREATE PLOTS
#===============================================================================

cat("Creating plots...\n")

# Plot 1: Comparison of methods
p1 <- all_benchmarks %>%
  mutate(
    method = as.character(expression),
    time_ms = as.numeric(median) * 1000
  ) %>%
  ggplot(aes(x = method, y = time_ms, fill = method)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f ms", time_ms)), 
            vjust = -0.5, size = 3) +
  facet_wrap(~benchmark, scales = "free") +
  labs(
    title = "Quantify Performance Comparison",
    subtitle = "Lower is better",
    x = "Method",
    y = "Median Time (ms)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("benchmarking/quantify_performance.png", p1, 
       width = 12, height = 8, dpi = 150)

cat("✓ Plot saved: benchmarking/quantify_performance.png\n")

# Plot 2: Memory usage
p2 <- all_benchmarks %>%
  mutate(
    method = as.character(expression),
    mem_mb = as.numeric(mem_alloc) / 1024^2
  ) %>%
  ggplot(aes(x = method, y = mem_mb, fill = method)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f MB", mem_mb)), 
            vjust = -0.5, size = 3) +
  facet_wrap(~benchmark, scales = "free") +
  labs(
    title = "Quantify Memory Usage",
    subtitle = "Memory allocated per operation",
    x = "Method",
    y = "Memory (MB)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("benchmarking/quantify_memory.png", p2, 
       width = 12, height = 8, dpi = 150)

cat("✓ Plot saved: benchmarking/quantify_memory.png\n\n")

# Cleanup
DBI::dbDisconnect(ae_db$connection)

cat("═══════════════════════════════════════════════════════════════\n")
cat("  QUANTIFY BENCHMARKS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

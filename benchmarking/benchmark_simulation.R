# ==============================================================================
# BENCHMARK SIMULATION INFRASTRUCTURE
# ==============================================================================
# 
# This script benchmarks the performance of the simulation caching system
# for both quantify (segment-level) and enrich (track-level) operations.
#
# Author: Automated benchmarking system
# Date: 2025-10-16
#
# ==============================================================================

library(reindeer)
library(bench)
library(data.table)
library(ggplot2)

# Setup --------------------------------------------------------------------

cat("\n=== Simulation Infrastructure Benchmarks ===\n\n")

# Create test database
emuR::create_emuRdemoData(dir = tempdir())
ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
corp <- corpus(ae_path)

# Create cache directory
cache_dir <- file.path(tempdir(), ".sim_benchmark")
if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
dir.create(cache_dir, recursive = TRUE)

# Test DSP functions -------------------------------------------------------

# Simple DSP function for quantify benchmarks
simple_dsp <- function(signal, sample_rate, multiplier = 1.0, offset = 0) {
  n <- length(signal)
  list(
    result1 = signal * multiplier + offset,
    result2 = rep(sample_rate, n)
  )
}

# Track-generating DSP function for enrich benchmarks
track_dsp <- function(signal, sample_rate, window_size = 512, hop_size = 256) {
  n_samples <- length(signal)
  n_frames <- max(1, floor((n_samples - window_size) / hop_size) + 1)
  
  # Simulate track generation
  result <- matrix(
    rnorm(n_frames * 2),
    nrow = n_frames,
    ncol = 2,
    dimnames = list(NULL, c("value1", "value2"))
  )
  
  # Add SSFF-like attributes
  attr(result, "startTime") <- 0
  attr(result, "origFreq") <- sample_rate
  attr(result, "sampleRate") <- sample_rate / hop_size
  
  result
}

# Benchmark 1: Parameter Grid Creation ------------------------------------

cat("Benchmarking parameter grid creation...\n")

grid_benchmarks <- bench::mark(
  small_grid = create_parameter_grid(list(
    p1 = 1:5,
    p2 = c("a", "b")
  )),
  
  medium_grid = create_parameter_grid(list(
    p1 = 1:10,
    p2 = 1:10,
    p3 = c(TRUE, FALSE)
  )),
  
  large_grid = create_parameter_grid(list(
    p1 = seq(100, 1000, by = 100),
    p2 = seq(0.5, 2.0, by = 0.1),
    p3 = c("a", "b", "c")
  )),
  
  check = FALSE,
  iterations = 100
)

cat("\nParameter Grid Creation Results:\n")
print(grid_benchmarks[, c("expression", "min", "median", "mem_alloc")])

# Benchmark 2: Cache Initialization ----------------------------------------

cat("\nBenchmarking cache initialization...\n")

cache_init_benchmarks <- bench::mark(
  quantify_cache = {
    f <- initialize_simulation_cache(
      cache_dir,
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "test_dsp"
    )
    unlink(f)
  },
  
  enrich_cache = {
    f <- initialize_track_simulation_cache(
      cache_dir,
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "test_track"
    )
    unlink(f)
  },
  
  check = FALSE,
  iterations = 50
)

cat("\nCache Initialization Results:\n")
print(cache_init_benchmarks[, c("expression", "min", "median", "mem_alloc")])

# Benchmark 3: Simulation Execution ----------------------------------------

cat("\nBenchmarking simulation execution...\n")

# Get test segment list
segs <- ask_for(corp, "[Phonetic = a]")

# Small parameter space
small_sim_bench <- bench::mark(
  small_space = {
    cache_subdir <- file.path(cache_dir, "small")
    dir.create(cache_subdir, showWarnings = FALSE)
    quantify_simulate(
      segs,
      .using = simple_dsp,
      .simulate = list(
        multiplier = c(1.0, 2.0),
        offset = c(0, 10)
      ),
      .simulation_store = cache_subdir,
      .verbose = FALSE
    )
    unlink(cache_subdir, recursive = TRUE)
  },
  
  check = FALSE,
  iterations = 5
)

# Medium parameter space
medium_sim_bench <- bench::mark(
  medium_space = {
    cache_subdir <- file.path(cache_dir, "medium")
    dir.create(cache_subdir, showWarnings = FALSE)
    quantify_simulate(
      segs,
      .using = simple_dsp,
      .simulate = list(
        multiplier = seq(0.5, 2.0, by = 0.5),
        offset = seq(0, 20, by = 5)
      ),
      .simulation_store = cache_subdir,
      .verbose = FALSE
    )
    unlink(cache_subdir, recursive = TRUE)
  },
  
  check = FALSE,
  iterations = 3
)

cat("\nSimulation Execution Results:\n")
cat("Small parameter space (4 combinations):\n")
print(small_sim_bench[, c("expression", "min", "median", "mem_alloc")])
cat("\nMedium parameter space (20 combinations):\n")
print(medium_sim_bench[, c("expression", "min", "median", "mem_alloc")])

# Benchmark 4: Reminisce Performance ---------------------------------------

cat("\nBenchmarking reminisce operations...\n")

# First, create a simulation to reminisce from
setup_cache <- file.path(cache_dir, "reminisce_test")
dir.create(setup_cache, showWarnings = FALSE)

sim_results <- quantify_simulate(
  segs,
  .using = simple_dsp,
  .simulate = list(
    multiplier = seq(0.5, 2.0, by = 0.25),
    offset = seq(0, 30, by = 10)
  ),
  .simulation_store = setup_cache,
  .verbose = FALSE
)

cache_file <- attr(sim_results, "cache_file")

reminisce_benchmarks <- bench::mark(
  first_params = reminisce(
    segs,
    parameters = list(multiplier = 0.5, offset = 0),
    cache_path = cache_file
  ),
  
  middle_params = reminisce(
    segs,
    parameters = list(multiplier = 1.25, offset = 15),
    cache_path = cache_file
  ),
  
  last_params = reminisce(
    segs,
    parameters = list(multiplier = 2.0, offset = 30),
    cache_path = cache_file
  ),
  
  check = FALSE,
  iterations = 20
)

cat("\nReminisce Performance Results:\n")
print(reminisce_benchmarks[, c("expression", "min", "median", "mem_alloc")])

# Benchmark 5: Cache Listing -----------------------------------------------

cat("\nBenchmarking cache listing operations...\n")

# Create multiple caches
for (i in 1:10) {
  cache_file_i <- initialize_simulation_cache(
    cache_dir,
    sprintf("2023101%d_12%02d00", i %% 10, i),
    sprintf("dsp_%d", i)
  )
  
  # Add minimal metadata
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_file_i)
  DBI::dbExecute(con, "
    INSERT INTO simulation_metadata 
      (timestamp, dsp_function, created_at, corpus_path, corpus_uuid,
       n_segments, n_parameter_combinations, parameter_names)
    VALUES (?, ?, datetime('now'), '/test', 'uuid', 10, 5, '[]')",
    params = list(
      sprintf("2023101%d_12%02d00", i %% 10, i),
      sprintf("dsp_%d", i)
    )
  )
  DBI::dbDisconnect(con)
}

listing_benchmarks <- bench::mark(
  list_all = list_simulations(cache_dir),
  
  check = FALSE,
  iterations = 50
)

cat("\nCache Listing Results (10 caches):\n")
print(listing_benchmarks[, c("expression", "min", "median", "mem_alloc")])

# Benchmark 6: Enrich Simulation -------------------------------------------

cat("\nBenchmarking enrich simulation...\n")

enrich_sim_bench <- bench::mark(
  enrich_small = {
    cache_subdir <- file.path(cache_dir, "enrich_small")
    dir.create(cache_subdir, showWarnings = FALSE)
    enrich_simulate(
      corp,
      .using = track_dsp,
      .simulate = list(
        window_size = c(256, 512),
        hop_size = c(128, 256)
      ),
      .simulation_store = cache_subdir,
      .verbose = FALSE
    )
    unlink(cache_subdir, recursive = TRUE)
  },
  
  check = FALSE,
  iterations = 3
)

cat("\nEnrich Simulation Results:\n")
print(enrich_sim_bench[, c("expression", "min", "median", "mem_alloc")])

# Benchmark 7: Signal Hash Computation -------------------------------------

cat("\nBenchmarking signal hash computation...\n")

# Get paths to actual signal files
signal_files <- peek_signals(corp)
test_signals <- head(signal_files$full_path, 5)

hash_benchmarks <- bench::mark(
  single_hash = compute_signal_hash(test_signals[1]),
  
  five_hashes = lapply(test_signals, compute_signal_hash),
  
  check = FALSE,
  iterations = 20
)

cat("\nSignal Hash Computation Results:\n")
print(hash_benchmarks[, c("expression", "min", "median", "mem_alloc")])

# Summary Statistics -------------------------------------------------------

cat("\n=== Summary of Simulation Benchmarks ===\n\n")

all_benchmarks <- list(
  grid_creation = grid_benchmarks,
  cache_init = cache_init_benchmarks,
  simulation_small = small_sim_bench,
  simulation_medium = medium_sim_bench,
  reminisce = reminisce_benchmarks,
  cache_listing = listing_benchmarks,
  enrich_sim = enrich_sim_bench,
  signal_hash = hash_benchmarks
)

# Extract summary statistics
summary_stats <- lapply(names(all_benchmarks), function(name) {
  bm <- all_benchmarks[[name]]
  data.table(
    benchmark_category = name,
    operation = as.character(bm$expression),
    median_time_ms = as.numeric(bm$median) * 1000,
    min_time_ms = as.numeric(bm$min) * 1000,
    max_time_ms = as.numeric(bm$max) * 1000,
    mem_alloc_mb = as.numeric(bm$mem_alloc) / 1024^2
  )
}) %>% rbindlist()

print(summary_stats)

# Visualization ------------------------------------------------------------

cat("\nGenerating visualizations...\n")

# Plot 1: Median times by category
p1 <- ggplot(summary_stats, aes(x = operation, y = median_time_ms, fill = benchmark_category)) +
  geom_col() +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Simulation Benchmark: Median Execution Times",
    x = "Operation",
    y = "Median Time (ms, log scale)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Memory allocation
p2 <- ggplot(summary_stats, aes(x = operation, y = mem_alloc_mb, fill = benchmark_category)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Simulation Benchmark: Memory Allocation",
    x = "Operation",
    y = "Memory Allocated (MB)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plots
ggsave(
  filename = "benchmarking/plots/simulation_times.png",
  plot = p1,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "benchmarking/plots/simulation_memory.png",
  plot = p2,
  width = 10,
  height = 8,
  dpi = 300
)

# Save Results -------------------------------------------------------------

cat("\nSaving results...\n")

simulation_benchmark_results <- list(
  timestamp = Sys.time(),
  r_version = R.version.string,
  package_version = packageVersion("reindeer"),
  benchmarks = all_benchmarks,
  summary = summary_stats,
  plots = list(times = p1, memory = p2)
)

saveRDS(
  simulation_benchmark_results,
  file = "benchmarking/simulation_benchmark_results.rds"
)

# Cleanup ------------------------------------------------------------------

unlink(cache_dir, recursive = TRUE)

cat("\n=== Simulation Benchmarks Complete ===\n")
cat("Results saved to: benchmarking/simulation_benchmark_results.rds\n")
cat("Plots saved to: benchmarking/plots/\n\n")

# Performance Insights -----------------------------------------------------

cat("=== Key Performance Insights ===\n\n")

cat("1. Parameter Grid Creation:\n")
grid_summary <- summary_stats[benchmark_category == "grid_creation"]
cat(sprintf("   - Small grid (10 combos): %.2f ms\n", 
            grid_summary[operation == "small_grid", median_time_ms]))
cat(sprintf("   - Medium grid (200 combos): %.2f ms\n",
            grid_summary[operation == "medium_grid", median_time_ms]))
cat(sprintf("   - Large grid (660 combos): %.2f ms\n",
            grid_summary[operation == "large_grid", median_time_ms]))

cat("\n2. Simulation Execution:\n")
sim_summary <- summary_stats[grepl("simulation", benchmark_category)]
cat(sprintf("   - 4 parameter combinations: %.2f ms\n",
            sim_summary[operation == "small_space", median_time_ms]))
cat(sprintf("   - 20 parameter combinations: %.2f ms\n",
            sim_summary[operation == "medium_space", median_time_ms]))
cat(sprintf("   - Time per combination (small): %.2f ms\n",
            sim_summary[operation == "small_space", median_time_ms] / 4))
cat(sprintf("   - Time per combination (medium): %.2f ms\n",
            sim_summary[operation == "medium_space", median_time_ms] / 20))

cat("\n3. Cache Retrieval (Reminisce):\n")
rem_summary <- summary_stats[benchmark_category == "reminisce"]
cat(sprintf("   - Average retrieval time: %.2f ms\n",
            mean(rem_summary$median_time_ms)))
cat(sprintf("   - Min: %.2f ms, Max: %.2f ms\n",
            min(rem_summary$median_time_ms),
            max(rem_summary$median_time_ms)))

cat("\n4. Memory Efficiency:\n")
cat(sprintf("   - Average memory per operation: %.2f MB\n",
            mean(summary_stats$mem_alloc_mb)))
cat(sprintf("   - Peak memory usage: %.2f MB (in %s)\n",
            max(summary_stats$mem_alloc_mb),
            summary_stats[which.max(mem_alloc_mb), operation]))

cat("\n==============================================\n\n")

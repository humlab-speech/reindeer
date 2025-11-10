# Benchmark Serialization Methods for Cache
# Compares: base R serialize, qs, fst, and RDS variants

library(microbenchmark)
library(tibble)
library(dplyr)
library(ggplot2)

# Install required packages if not present
if (!requireNamespace("qs", quietly = TRUE)) {
  message("qs package not installed. Install with: install.packages('qs')")
}
if (!requireNamespace("fst", quietly = TRUE)) {
  message("fst package not installed. Install with: install.packages('fst')")
}

# ============================================================================
# Generate Test Data
# ============================================================================

#' Generate test data representative of quantify() results
generate_test_data <- function(n_rows, n_cols) {
  # Simulate DSP output (formants, f0, etc.)
  data <- tibble(
    labels = sample(c("a", "e", "i", "o", "u"), n_rows, replace = TRUE),
    start = runif(n_rows, 0, 1000),
    end = runif(n_rows, 1000, 2000),
    db_uuid = rep("test-uuid", n_rows),
    session = sample(paste0("session", 1:5), n_rows, replace = TRUE),
    bundle = sample(paste0("bundle", 1:20), n_rows, replace = TRUE),
    start_item_id = seq_len(n_rows),
    end_item_id = seq_len(n_rows),
    level = "Phonetic",
    attribute = "Phonetic",
    start_item_seq_idx = 1,
    end_item_seq_idx = 1,
    type = "SEGMENT",
    sample_start = as.integer(runif(n_rows, 0, 16000)),
    sample_end = as.integer(runif(n_rows, 16000, 32000)),
    sample_rate = 16000
  )
  
  # Add DSP columns
  for (i in seq_len(n_cols - ncol(data))) {
    col_name <- paste0("dsp_col_", i)
    data[[col_name]] <- rnorm(n_rows, mean = 1000, sd = 200)
  }
  
  data
}

# Create test datasets of different sizes
cat("Generating test datasets...\n")
test_data <- list(
  small = generate_test_data(100, 5),      # 100 segments, 5 DSP columns
  medium = generate_test_data(1000, 10),   # 1000 segments, 10 DSP columns
  large = generate_test_data(10000, 20)    # 10000 segments, 20 DSP columns
)

cat(sprintf("Small:  %d rows x %d cols (%.1f KB)\n", 
            nrow(test_data$small), ncol(test_data$small),
            object.size(test_data$small) / 1024))
cat(sprintf("Medium: %d rows x %d cols (%.1f KB)\n", 
            nrow(test_data$medium), ncol(test_data$medium),
            object.size(test_data$medium) / 1024))
cat(sprintf("Large:  %d rows x %d cols (%.1f KB)\n", 
            nrow(test_data$large), ncol(test_data$large),
            object.size(test_data$large) / 1024))

# ============================================================================
# Serialization Functions
# ============================================================================

#' Base R serialize
serialize_base <- function(obj) {
  serialize(obj, NULL)
}

deserialize_base <- function(blob) {
  unserialize(blob)
}

#' qs - fast preset
serialize_qs_fast <- function(obj) {
  if (!requireNamespace("qs", quietly = TRUE)) return(NULL)
  qs::qserialize(obj, preset = "fast")
}

deserialize_qs_fast <- function(blob) {
  if (!requireNamespace("qs", quietly = TRUE)) return(NULL)
  qs::qdeserialize(blob)
}

#' qs - balanced preset
serialize_qs_balanced <- function(obj) {
  if (!requireNamespace("qs", quietly = TRUE)) return(NULL)
  qs::qserialize(obj, preset = "balanced")
}

deserialize_qs_balanced <- function(blob) {
  if (!requireNamespace("qs", quietly = TRUE)) return(NULL)
  qs::qdeserialize(blob)
}

#' qs - high compression preset
serialize_qs_high <- function(obj) {
  if (!requireNamespace("qs", quietly = TRUE)) return(NULL)
  qs::qserialize(obj, preset = "high")
}

deserialize_qs_high <- function(blob) {
  if (!requireNamespace("qs", quietly = TRUE)) return(NULL)
  qs::qdeserialize(blob)
}

#' saveRDS with different compression
serialize_rds_gzip <- function(obj) {
  f <- tempfile()
  saveRDS(obj, f, compress = "gzip")
  blob <- readBin(f, "raw", file.size(f))
  unlink(f)
  blob
}

deserialize_rds_gzip <- function(blob) {
  f <- tempfile()
  writeBin(blob, f)
  obj <- readRDS(f)
  unlink(f)
  obj
}

#' fst (data frames only)
serialize_fst <- function(obj) {
  if (!requireNamespace("fst", quietly = TRUE)) return(NULL)
  if (!is.data.frame(obj)) return(NULL)
  f <- tempfile(fileext = ".fst")
  fst::write_fst(as.data.frame(obj), f, compress = 50)
  blob <- readBin(f, "raw", file.size(f))
  unlink(f)
  blob
}

deserialize_fst <- function(blob) {
  if (!requireNamespace("fst", quietly = TRUE)) return(NULL)
  f <- tempfile(fileext = ".fst")
  writeBin(blob, f)
  obj <- fst::read_fst(f)
  unlink(f)
  obj
}

# ============================================================================
# Benchmark Serialization
# ============================================================================

benchmark_serialize <- function(data, label) {
  cat(sprintf("\n=== Benchmarking serialization: %s ===\n", label))
  
  results <- list()
  
  # Base R serialize
  cat("Testing base serialize...\n")
  results$base <- microbenchmark(
    serialize_base(data),
    times = 100
  )
  blob_base <- serialize_base(data)
  
  # qs variants
  if (requireNamespace("qs", quietly = TRUE)) {
    cat("Testing qs (fast)...\n")
    results$qs_fast <- microbenchmark(
      serialize_qs_fast(data),
      times = 100
    )
    blob_qs_fast <- serialize_qs_fast(data)
    
    cat("Testing qs (balanced)...\n")
    results$qs_balanced <- microbenchmark(
      serialize_qs_balanced(data),
      times = 100
    )
    blob_qs_balanced <- serialize_qs_balanced(data)
    
    cat("Testing qs (high)...\n")
    results$qs_high <- microbenchmark(
      serialize_qs_high(data),
      times = 100
    )
    blob_qs_high <- serialize_qs_high(data)
  } else {
    cat("qs package not available\n")
    blob_qs_fast <- blob_qs_balanced <- blob_qs_high <- NULL
  }
  
  # RDS with compression
  cat("Testing RDS (gzip)...\n")
  results$rds_gzip <- microbenchmark(
    serialize_rds_gzip(data),
    times = 50  # Slower, fewer iterations
  )
  blob_rds_gzip <- serialize_rds_gzip(data)
  
  # fst (if applicable)
  if (requireNamespace("fst", quietly = TRUE) && is.data.frame(data)) {
    cat("Testing fst...\n")
    results$fst <- microbenchmark(
      serialize_fst(data),
      times = 100
    )
    blob_fst <- serialize_fst(data)
  } else {
    cat("fst not available or data not data.frame\n")
    blob_fst <- NULL
  }
  
  # Compile results
  times <- tibble(
    method = character(),
    median_ms = numeric(),
    mean_ms = numeric(),
    size_bytes = integer(),
    size_ratio = numeric()
  )
  
  base_size <- length(blob_base)
  
  times <- bind_rows(times, tibble(
    method = "base_serialize",
    median_ms = median(results$base$time) / 1e6,
    mean_ms = mean(results$base$time) / 1e6,
    size_bytes = base_size,
    size_ratio = 1.0
  ))
  
  if (!is.null(blob_qs_fast)) {
    times <- bind_rows(times, tibble(
      method = "qs_fast",
      median_ms = median(results$qs_fast$time) / 1e6,
      mean_ms = mean(results$qs_fast$time) / 1e6,
      size_bytes = length(blob_qs_fast),
      size_ratio = length(blob_qs_fast) / base_size
    ))
  }
  
  if (!is.null(blob_qs_balanced)) {
    times <- bind_rows(times, tibble(
      method = "qs_balanced",
      median_ms = median(results$qs_balanced$time) / 1e6,
      mean_ms = mean(results$qs_balanced$time) / 1e6,
      size_bytes = length(blob_qs_balanced),
      size_ratio = length(blob_qs_balanced) / base_size
    ))
  }
  
  if (!is.null(blob_qs_high)) {
    times <- bind_rows(times, tibble(
      method = "qs_high",
      median_ms = median(results$qs_high$time) / 1e6,
      mean_ms = mean(results$qs_high$time) / 1e6,
      size_bytes = length(blob_qs_high),
      size_ratio = length(blob_qs_high) / base_size
    ))
  }
  
  times <- bind_rows(times, tibble(
    method = "rds_gzip",
    median_ms = median(results$rds_gzip$time) / 1e6,
    mean_ms = mean(results$rds_gzip$time) / 1e6,
    size_bytes = length(blob_rds_gzip),
    size_ratio = length(blob_rds_gzip) / base_size
  ))
  
  if (!is.null(blob_fst)) {
    times <- bind_rows(times, tibble(
      method = "fst",
      median_ms = median(results$fst$time) / 1e6,
      mean_ms = mean(results$fst$time) / 1e6,
      size_bytes = length(blob_fst),
      size_ratio = length(blob_fst) / base_size
    ))
  }
  
  list(
    times = times,
    blobs = list(
      base = blob_base,
      qs_fast = blob_qs_fast,
      qs_balanced = blob_qs_balanced,
      qs_high = blob_qs_high,
      rds_gzip = blob_rds_gzip,
      fst = blob_fst
    )
  )
}

# ============================================================================
# Benchmark Deserialization
# ============================================================================

benchmark_deserialize <- function(blobs, label) {
  cat(sprintf("\n=== Benchmarking deserialization: %s ===\n", label))
  
  results <- list()
  
  # Base R
  cat("Testing base unserialize...\n")
  results$base <- microbenchmark(
    deserialize_base(blobs$base),
    times = 100
  )
  
  # qs variants
  if (!is.null(blobs$qs_fast)) {
    cat("Testing qs (fast)...\n")
    results$qs_fast <- microbenchmark(
      deserialize_qs_fast(blobs$qs_fast),
      times = 100
    )
  }
  
  if (!is.null(blobs$qs_balanced)) {
    cat("Testing qs (balanced)...\n")
    results$qs_balanced <- microbenchmark(
      deserialize_qs_balanced(blobs$qs_balanced),
      times = 100
    )
  }
  
  if (!is.null(blobs$qs_high)) {
    cat("Testing qs (high)...\n")
    results$qs_high <- microbenchmark(
      deserialize_qs_high(blobs$qs_high),
      times = 100
    )
  }
  
  # RDS
  cat("Testing RDS (gzip)...\n")
  results$rds_gzip <- microbenchmark(
    deserialize_rds_gzip(blobs$rds_gzip),
    times = 50
  )
  
  # fst
  if (!is.null(blobs$fst)) {
    cat("Testing fst...\n")
    results$fst <- microbenchmark(
      deserialize_fst(blobs$fst),
      times = 100
    )
  }
  
  # Compile results
  times <- tibble(
    method = character(),
    median_ms = numeric(),
    mean_ms = numeric()
  )
  
  times <- bind_rows(times, tibble(
    method = "base_serialize",
    median_ms = median(results$base$time) / 1e6,
    mean_ms = mean(results$base$time) / 1e6
  ))
  
  if (!is.null(blobs$qs_fast)) {
    times <- bind_rows(times, tibble(
      method = "qs_fast",
      median_ms = median(results$qs_fast$time) / 1e6,
      mean_ms = mean(results$qs_fast$time) / 1e6
    ))
  }
  
  if (!is.null(blobs$qs_balanced)) {
    times <- bind_rows(times, tibble(
      method = "qs_balanced",
      median_ms = median(results$qs_balanced$time) / 1e6,
      mean_ms = mean(results$qs_balanced$time) / 1e6
    ))
  }
  
  if (!is.null(blobs$qs_high)) {
    times <- bind_rows(times, tibble(
      method = "qs_high",
      median_ms = median(results$qs_high$time) / 1e6,
      mean_ms = mean(results$qs_high$time) / 1e6
    ))
  }
  
  times <- bind_rows(times, tibble(
    method = "rds_gzip",
    median_ms = median(results$rds_gzip$time) / 1e6,
    mean_ms = mean(results$rds_gzip$time) / 1e6
  ))
  
  if (!is.null(blobs$fst)) {
    times <- bind_rows(times, tibble(
      method = "fst",
      median_ms = median(results$fst$time) / 1e6,
      mean_ms = mean(results$fst$time) / 1e6
    ))
  }
  
  times
}

# ============================================================================
# Run Benchmarks
# ============================================================================

cat("\n" , strrep("=", 70), "\n")
cat("SERIALIZATION BENCHMARKS\n")
cat(strrep("=", 70), "\n")

all_results <- list()

for (size_name in names(test_data)) {
  # Serialize
  serialize_results <- benchmark_serialize(test_data[[size_name]], size_name)
  
  # Deserialize
  deserialize_results <- benchmark_deserialize(serialize_results$blobs, size_name)
  
  # Combine
  combined <- serialize_results$times %>%
    left_join(deserialize_results, by = "method", suffix = c("_serialize", "_deserialize")) %>%
    mutate(
      total_ms = median_ms_serialize + median_ms_deserialize,
      dataset = size_name
    )
  
  all_results[[size_name]] <- combined
  
  # Print summary
  cat("\n")
  cat(sprintf("--- %s dataset summary ---\n", toupper(size_name)))
  print(combined %>% 
    select(method, 
           ser_ms = median_ms_serialize, 
           deser_ms = median_ms_deserialize,
           total_ms,
           size_kb = size_bytes,
           ratio = size_ratio) %>%
    mutate(
      size_kb = size_kb / 1024,
      speedup_ser = combined$median_ms_serialize[1] / ser_ms,
      speedup_deser = combined$median_ms_deserialize[1] / deser_ms,
      speedup_total = combined$total_ms[1] / total_ms
    ) %>%
    arrange(total_ms))
}

# ============================================================================
# Create Summary Report
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY REPORT\n")
cat(strrep("=", 70), "\n\n")

final_results <- bind_rows(all_results)

# Overall best performers
cat("=== Overall Best Performers ===\n\n")

cat("Fastest Serialization:\n")
print(final_results %>%
  group_by(dataset) %>%
  slice_min(median_ms_serialize, n = 1) %>%
  select(dataset, method, median_ms_serialize, size_ratio))

cat("\nFastest Deserialization:\n")
print(final_results %>%
  group_by(dataset) %>%
  slice_min(median_ms_deserialize, n = 1) %>%
  select(dataset, method, median_ms_deserialize))

cat("\nBest Compression:\n")
print(final_results %>%
  group_by(dataset) %>%
  slice_min(size_ratio, n = 1) %>%
  select(dataset, method, size_ratio, median_ms_serialize))

cat("\nBest Overall (total time):\n")
print(final_results %>%
  group_by(dataset) %>%
  slice_min(total_ms, n = 1) %>%
  select(dataset, method, total_ms, size_ratio))

# Speedup relative to base serialize
cat("\n=== Speedup vs. base serialize() ===\n\n")
speedup_summary <- final_results %>%
  group_by(dataset) %>%
  mutate(
    base_serialize = median_ms_serialize[method == "base_serialize"],
    base_deserialize = median_ms_deserialize[method == "base_serialize"],
    base_total = total_ms[method == "base_serialize"],
    speedup_serialize = base_serialize / median_ms_serialize,
    speedup_deserialize = base_deserialize / median_ms_deserialize,
    speedup_total = base_total / total_ms
  ) %>%
  filter(method != "base_serialize") %>%
  select(dataset, method, speedup_serialize, speedup_deserialize, speedup_total, size_ratio)

print(speedup_summary)

# ============================================================================
# Generate Plots
# ============================================================================

cat("\n=== Generating plots ===\n")

# Plot 1: Serialization time comparison
p1 <- ggplot(final_results, aes(x = method, y = median_ms_serialize, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Serialization Time Comparison",
    x = "Method",
    y = "Median Time (ms)",
    fill = "Dataset"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("benchmarking/serialization_time.png", p1, width = 10, height = 6, dpi = 150)

# Plot 2: Size comparison
p2 <- ggplot(final_results, aes(x = method, y = size_ratio, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Serialized Size Comparison (vs. base serialize)",
    x = "Method",
    y = "Size Ratio (1.0 = same as base)",
    fill = "Dataset"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("benchmarking/serialization_size.png", p2, width = 10, height = 6, dpi = 150)

# Plot 3: Total time (serialize + deserialize)
p3 <- ggplot(final_results, aes(x = method, y = total_ms, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Round-Trip Time (Serialize + Deserialize)",
    x = "Method",
    y = "Total Time (ms)",
    fill = "Dataset"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("benchmarking/serialization_total_time.png", p3, width = 10, height = 6, dpi = 150)

# Plot 4: Speedup heatmap
speedup_long <- speedup_summary %>%
  select(dataset, method, speedup_serialize, speedup_deserialize, speedup_total) %>%
  tidyr::pivot_longer(
    cols = starts_with("speedup_"),
    names_to = "operation",
    values_to = "speedup"
  ) %>%
  mutate(
    operation = gsub("speedup_", "", operation),
    operation = factor(operation, levels = c("serialize", "deserialize", "total"))
  )

p4 <- ggplot(speedup_long, aes(x = method, y = operation, fill = speedup)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2fx", speedup)), color = "white", size = 3) +
  facet_wrap(~ dataset, ncol = 1) +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "green",
    midpoint = 1, limits = c(0, NA)
  ) +
  labs(
    title = "Speedup vs. base serialize()",
    x = "Method",
    y = "Operation",
    fill = "Speedup"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("benchmarking/serialization_speedup_heatmap.png", p4, width = 10, height = 8, dpi = 150)

# ============================================================================
# Save Results
# ============================================================================

cat("\n=== Saving results ===\n")
saveRDS(list(
  results = final_results,
  speedup = speedup_summary,
  test_data_info = lapply(test_data, function(d) {
    list(
      nrow = nrow(d),
      ncol = ncol(d),
      size_kb = as.numeric(object.size(d)) / 1024
    )
  })
), "benchmarking/serialization_benchmark_results.rds")

write.csv(final_results, "benchmarking/serialization_results.csv", row.names = FALSE)
write.csv(speedup_summary, "benchmarking/serialization_speedup.csv", row.names = FALSE)

cat("\n", strrep("=", 70), "\n")
cat("BENCHMARK COMPLETE\n")
cat("Results saved to benchmarking/serialization_benchmark_results.rds\n")
cat("Plots saved to benchmarking/serialization_*.png\n")
cat(strrep("=", 70), "\n")

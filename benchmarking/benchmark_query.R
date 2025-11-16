# Query Performance Benchmarking
#
# Compares optimized EQL implementation with emuR::query() across
# various query types and complexity levels.
#
# Usage:
#   Rscript benchmarking/benchmark_query.R

library(emuR)
library(bench)

# Load package
suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(reindeer)
  }
})

# Setup test database
cat("Setting up test database...\n")
temp_dir <- tempdir()
if (!dir.exists(file.path(temp_dir, 'emuR_demoData'))) {
  create_emuRdemoData(dir = temp_dir)
}
ae_path <- file.path(temp_dir, 'emuR_demoData', 'ae_emuDB')
ae <- load_emuDB(ae_path, verbose = FALSE)

# Ensure cache exists
suppressMessages(emuR::query(ae, "Phonetic == t"))

cat("\n")
cat("="[rep(1,70)], "\n")
cat("QUERY PERFORMANCE BENCHMARKS\n")
cat("="[rep(1,70)], "\n")
cat("\n")

# ============================================================================
# SIMPLE QUERIES
# ============================================================================

cat("1. SIMPLE QUERIES\n")
cat("-"[rep(1,70)], "\n")

simple_queries <- c(
  "Phonetic == t",
  "Phoneme == n",
  "Syllable == S",
  "Word =~ .*"
)

for (query in simple_queries) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    emuR = emuR::query(ae, query),
    optimized = ask_for(ae_path, query),
    iterations = 20,
    check = FALSE
  )

  speedup <- as.numeric(bm$median[1] / bm$median[2])

  cat(sprintf("  emuR median:      %s\n", format(bm$median[1])))
  cat(sprintf("  Optimized median: %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:          %.2fx\n", speedup))
}

# ============================================================================
# REGEX QUERIES
# ============================================================================

cat("\n")
cat("2. REGEX QUERIES\n")
cat("-"[rep(1,70)], "\n")

regex_queries <- c(
  "Phonetic =~ .*",
  "Word =~ .*"
)

for (query in regex_queries) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    emuR = emuR::query(ae, query),
    optimized = ask_for(ae_path, query),
    iterations = 20,
    check = FALSE
  )

  speedup <- as.numeric(bm$median[1] / bm$median[2])

  cat(sprintf("  emuR median:      %s\n", format(bm$median[1])))
  cat(sprintf("  Optimized median: %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:          %.2fx\n", speedup))
}

# Test optimized-only queries (not supported by emuR)
cat("\n")
cat("2b. OPTIMIZED-ONLY REGEX QUERIES\n")
cat("-"[rep(1,70)], "\n")

optimized_only_regex <- c(
  "Phonetic =~ [tkp]",
  "Phonetic =~ ^[AIOUEV]$",
  "Phonetic !~ [tkp]"
)

for (query in optimized_only_regex) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    optimized = ask_for(ae_path, query),
    iterations = 20
  )

  cat(sprintf("  Optimized median: %s\n", format(bm$median)))
  cat("  (emuR doesn't support this syntax)\n")
}

# ============================================================================
# SEQUENCE QUERIES
# ============================================================================

cat("\n")
cat("3. SEQUENCE QUERIES\n")
cat("-"[rep(1,70)], "\n")

sequence_queries <- c(
  "[Phoneme == n -> Phoneme == t]",
  "[#Phoneme == n -> Phoneme == t]",
  "[Phoneme == n -> #Phoneme == t]"
)

for (query in sequence_queries) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    emuR = emuR::query(ae, query),
    optimized = ask_for(ae_path, query),
    iterations = 15,
    check = FALSE
  )

  speedup <- as.numeric(bm$median[1] / bm$median[2])

  cat(sprintf("  emuR median:      %s\n", format(bm$median[1])))
  cat(sprintf("  Optimized median: %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:          %.2fx\n", speedup))
}

# ============================================================================
# DOMINANCE QUERIES
# ============================================================================

cat("\n")
cat("4. DOMINANCE QUERIES\n")
cat("-"[rep(1,70)], "\n")

dominance_queries <- c(
  "[Syllable == S ^ Phoneme == n]",
  "[#Syllable == S ^ Phoneme == n]",
  "[Word == F ^ Phonetic == t]"
)

for (query in dominance_queries) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    emuR = emuR::query(ae, query),
    optimized = ask_for(ae_path, query),
    iterations = 15,
    check = FALSE
  )

  speedup <- as.numeric(bm$median[1] / bm$median[2])

  cat(sprintf("  emuR median:      %s\n", format(bm$median[1])))
  cat(sprintf("  Optimized median: %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:          %.2fx\n", speedup))
}

# ============================================================================
# FUNCTION QUERIES
# ============================================================================

cat("\n")
cat("5. FUNCTION QUERIES\n")
cat("-"[rep(1,70)], "\n")

function_queries <- c(
  "Start(Syllable, Phoneme) == 1",
  "End(Syllable, Phoneme) == 1",
  "Medial(Syllable, Phoneme) == 1",
  "Num(Syllable, Phoneme) >= 3"
)

for (query in function_queries) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    emuR = emuR::query(ae, query),
    optimized = ask_for(ae_path, query),
    iterations = 15,
    check = FALSE
  )

  speedup <- as.numeric(bm$median[1] / bm$median[2])

  cat(sprintf("  emuR median:      %s\n", format(bm$median[1])))
  cat(sprintf("  Optimized median: %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:          %.2fx\n", speedup))
}

# ============================================================================
# COMPLEX QUERIES
# ============================================================================

cat("\n")
cat("6. COMPLEX QUERIES\n")
cat("-"[rep(1,70)], "\n")

complex_queries <- c(
  "[Start(Syllable, Phoneme) == 1 & Phoneme == n]"
)

for (query in complex_queries) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    emuR = emuR::query(ae, query),
    optimized = ask_for(ae_path, query),
    iterations = 10,
    check = FALSE
  )

  speedup <- as.numeric(bm$median[1] / bm$median[2])

  cat(sprintf("  emuR median:      %s\n", format(bm$median[1])))
  cat(sprintf("  Optimized median: %s\n", format(bm$median[2])))
  cat(sprintf("  Speedup:          %.2fx\n", speedup))
}

# Test optimized-only complex queries
cat("\n")
cat("6b. OPTIMIZED-ONLY COMPLEX QUERIES\n")
cat("-"[rep(1,70)], "\n")

optimized_only_complex <- c(
  "[[Syllable == S ^ Phoneme == n] -> Phoneme == t]",
  "[Syllable == S ^ [Phoneme == n -> Phoneme == t]]",
  "[Start(Syllable, Phoneme) == 1 & Phoneme =~ [tkp]]"
)

for (query in optimized_only_complex) {
  cat(sprintf("\nQuery: %s\n", query))

  bm <- bench::mark(
    optimized = ask_for(ae_path, query),
    iterations = 10
  )

  cat(sprintf("  Optimized median: %s\n", format(bm$median)))
  cat("  (emuR doesn't support this syntax)\n")
}

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n")
cat("="[rep(1,70)], "\n")
cat("SUMMARY\n")
cat("="[rep(1,70)], "\n")
cat("\n")

all_queries <- c(simple_queries, regex_queries, sequence_queries,
                dominance_queries, function_queries, complex_queries)

cat(sprintf("Total queries benchmarked: %d\n", length(all_queries)))
cat("\nBenchmarks complete!\n")
cat("\nNOTE: Speedup values indicate how many times faster the optimized\n")
cat("      implementation is compared to emuR::query()\n")
cat("\n")

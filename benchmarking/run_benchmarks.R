#!/usr/bin/env Rscript
# Run All Benchmarks
# Usage: Rscript benchmarking/run_benchmarks.R [iterations]

args <- commandArgs(trailingOnly = TRUE)
iterations <- if (length(args) > 0) as.integer(args[1]) else 50

cat("\n")
cat("======================================================================\n")
cat("  REINDEER BENCHMARK RUNNER\n")
cat("======================================================================\n\n")

cat(sprintf("Query iterations: %d\n", iterations))
cat("Loading package...\n")

suppressWarnings(suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(emuR)
  library(bench)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cli)
}))

# Print warnings immediately so withCallingHandlers can catch them
# (default warn=0 defers warnings, which escape handlers)
options(warn = 1)

# Patterns for warnings that are expected benchmark noise
.bench_noise <- paste(
  "=~ now requires",
  "GC in every iteration",
  "missing items in resulting segment",
  "was built under R version",
  "nanosecond times",
  "dbDisconnect|connection_release",
  "uninitialised column",
  "no non-missing arguments",
  sep = "|"
)

# Helper: run a benchmark script with error handling
run_section <- function(label, script_path) {
  cat("\n")
  cat("======================================================================\n")
  cat(sprintf("  %s\n", label))
  cat("======================================================================\n\n")

  tryCatch(
    withCallingHandlers({
      env <- new.env(parent = globalenv())
      source(script_path, local = env)
      # Close emuDB/corpus connections before env is GC'd
      for (nm in ls(env)) {
        obj <- tryCatch(env[[nm]], error = function(e) NULL)
        if (is.null(obj)) next
        if (inherits(obj, "emuDBhandle") && !is.null(obj$connection))
          try(suppressWarnings(DBI::dbDisconnect(obj$connection)), silent = TRUE)
        if (inherits(obj, "reindeer::corpus") && !is.null(obj@.connection))
          try(suppressWarnings(DBI::dbDisconnect(obj@.connection$con)), silent = TRUE)
        if (is.list(obj)) {
          for (el in obj) {
            if (inherits(el, "emuDBhandle") && !is.null(el$connection))
              try(suppressWarnings(DBI::dbDisconnect(el$connection)), silent = TRUE)
            if (inherits(el, "reindeer::corpus") && !is.null(el@.connection))
              try(suppressWarnings(DBI::dbDisconnect(el@.connection$con)), silent = TRUE)
          }
        }
      }
      rm(env)
      suppressWarnings(gc())
      cat(sprintf("\n  [OK] %s complete\n", label))
    }, warning = function(w) {
      if (grepl(.bench_noise, conditionMessage(w)))
        invokeRestart("muffleWarning")
    }),
    error = function(e) {
      cat(sprintf("\n  [FAIL] %s: %s\n", label, e$message))
    }
  )
}

# ==============================================================================
# Head-to-head vs emuR benchmarks
# ==============================================================================

run_section("EQL QUERY BENCHMARKS", "benchmarking/benchmark_queries.R")
run_section("HIERARCHY BENCHMARKS (requery_hier vs ascend/descend)", "benchmarking/benchmark_hierarchy.R")
run_section("SEQUENCE BENCHMARKS (requery_seq vs scout/retreat)", "benchmarking/benchmark_requery_seq.R")
run_section("TRACK DATA BENCHMARKS (get_trackdata vs quantify)", "benchmarking/benchmark_trackdata.R")
run_section("METADATA BENCHMARKS", "benchmarking/benchmark_metadata.R")
run_section("PERFORMANCE TARGETS", "benchmarking/benchmark_performance_targets.R")

# ==============================================================================
# Internal optimization benchmarks (no emuR comparison)
# ==============================================================================

run_section("DSPP: dplyr vs data.table", "benchmarking/benchmark_dspp.R")
run_section("SERIALIZATION: base vs qs", "benchmarking/benchmark_serialization.R")
run_section("JSON: jsonlite vs RcppSimdJson", "benchmarking/benchmark_json.R")
run_section("SIMULATION INFRASTRUCTURE", "benchmarking/benchmark_simulation.R")
run_section("QUANTIFY INTERNALS", "benchmarking/benchmark_quantify.R")

# ==============================================================================
# Final Summary
# ==============================================================================

cat("\n")
cat("======================================================================\n")
cat("  ALL BENCHMARKS COMPLETE\n")
cat("  Results saved to: benchmarking/\n")
cat("======================================================================\n\n")

# Force GC with warnings suppressed to clean up RSQLite connection finalizers
options(warn = -1)
invisible(gc())

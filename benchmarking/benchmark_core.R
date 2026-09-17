#!/usr/bin/env Rscript
# ==============================================================================
# CORE BENCHMARK HARNESS
#
# Measures the operations that the remediation plan touches, so that energy and
# performance claims are recorded before and after each change instead of being
# asserted.
#
# Usage:
#   Rscript benchmarking/benchmark_core.R [output.csv]
#
# Default output: benchmarking/core_baseline.csv
#
# Notes
#   * The multisession measurements need reindeer visible to worker processes,
#     which means an installed copy. If no installed copy is found, the script
#     installs the working tree into a temporary library and points R_LIBS_USER
#     at it. Your default library is not touched.
#   * Every workload is wrapped in tryCatch: a workload that errors is recorded
#     with its message rather than aborting the run.
# ==============================================================================

suppressMessages({
  library(future)
  library(future.apply)
})

args <- commandArgs(trailingOnly = TRUE)
out_csv <- if (length(args) >= 1) args[1] else "benchmarking/core_baseline.csv"

# --- package under test -------------------------------------------------------
lib <- file.path(tempdir(), "reindeer_bench_lib")
installed <- nzchar(system.file(package = "reindeer"))
if (!installed) {
  message("No installed reindeer found; installing into ", lib)
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  withr::with_libpaths(lib, action = "replace", {
    devtools::install(quiet = TRUE, upgrade = FALSE,
                      dependencies = FALSE, build_vignettes = FALSE,
                      reload = FALSE)
  })
  Sys.setenv(R_LIBS_USER = lib)
  .libPaths(c(lib, .libPaths()))
} else {
  message("Using installed reindeer at ", system.file(package = "reindeer"))
}
suppressMessages(library(reindeer))

# --- worker spawn counter -----------------------------------------------------
plan_calls <- 0L
trace("plan", where = asNamespace("future"), print = FALSE,
      tracer = quote(plan_calls <<- plan_calls + 1L))
reset_plan_calls <- function() plan_calls <<- 0L

# --- measurement helpers ------------------------------------------------------
rows <- list()

record <- function(workload, n = NA_integer_, cache = NA_character_,
                   elapsed = NA_real_, plan_calls = NA_integer_,
                   note = "") {
  rows[[length(rows) + 1L]] <<- data.frame(
    workload = workload, n = n, cache = cache,
    elapsed_s = round(elapsed, 4), plan_calls = plan_calls,
    note = note, stringsAsFactors = FALSE
  )
}

measure <- function(workload, expr, n = NA_integer_, cache = NA_character_) {
  reset_plan_calls()
  res <- tryCatch({
    t0 <- Sys.time()
    force(expr)
    as.numeric(Sys.time() - t0, units = "secs")
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  if (is.character(res)) {
    record(workload, n, cache, NA_real_, NA_integer_, res)
    message(sprintf("  %-28s ERROR: %s", workload, substr(res, 1, 90)))
  } else {
    record(workload, n, cache, res, plan_calls)
    message(sprintf("  %-28s %7.2fs  plan_calls=%d", workload, res, plan_calls))
  }
  invisible(res)
}

qdsp <- function() superassp::trk_rms

# --- corpus -------------------------------------------------------------------
message("\n== corpus ==")
bench_dir <- file.path(tempdir(), "reindeer_bench_data")
if (!dir.exists(file.path(bench_dir, "ae_emuDB"))) {
  dir.create(bench_dir, recursive = TRUE, showWarnings = FALSE)
  utils::untar(system.file("extdata", "ae.tar.xz", package = "reindeer"),
               exdir = bench_dir)
}
ae <- file.path(bench_dir, "ae_emuDB")

measure("corpus_open_quick", { corp <- reindeer::corpus(ae) })
measure("corpus_open_rebuild", { corp <- reindeer::corpus(ae, quick = FALSE) })

corp <- reindeer::corpus(ae)

# --- query --------------------------------------------------------------------
message("\n== query ==")
segs <- collect(query(corp, "Phoneme =~ .+"))
measure("query_collect_223", { collect(query(corp, "Phoneme =~ .+")) }, n = nrow(segs))
measure("query_eager_223", { query(corp, "Phoneme =~ .+", lazy = FALSE) }, n = nrow(segs))
measure("scout_223", { scout(segs, 1) }, n = nrow(segs))

# --- quantify -----------------------------------------------------------------
message("\n== quantify (superassp::trk_rms) ==")
quant_workload <- function(n, cache, parallel) {
  s <- segs[seq_len(min(n, nrow(segs))), ]
  quantify(s, qdsp(), .at = 0.5, .use_cache = cache,
           .parallel = parallel, .verbose = FALSE)
}
for (n in c(5L, 50L, 150L)) {
  measure(sprintf("quantify_seq_%d_cold", n), quant_workload(n, FALSE, FALSE), n = n, cache = "off")
}
for (n in c(5L, 50L, 150L)) {
  measure(sprintf("quantify_cached_%d_cold", n), quant_workload(n, TRUE, FALSE), n = n, cache = "cold")
  measure(sprintf("quantify_cached_%d_warm", n), quant_workload(n, TRUE, FALSE), n = n, cache = "warm")
}
for (n in c(50L, 150L)) {
  measure(sprintf("quantify_par_%d", n), quant_workload(n, FALSE, TRUE), n = n, cache = "off")
}

plan(sequential)
untrace("plan", where = asNamespace("future"))

# --- write --------------------------------------------------------------------
result <- do.call(rbind, rows)
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(result, out_csv, row.names = FALSE)
message("\nWrote ", nrow(result), " measurements to ", out_csv)
print(result)

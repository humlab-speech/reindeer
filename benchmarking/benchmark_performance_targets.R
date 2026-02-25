#!/usr/bin/env Rscript
# Performance Target Verification
#
# Head-to-head comparisons of reindeer vs emuR for core operations.
# Targets:
#   3. Simple queries: 50%+ faster
#   4. Sequence queries (complex joins): 70%+ faster
#   5. Large result sets (regex): 80%+ faster
#   6. Bundle listing: 50%+ faster

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(emuR)
  library(bench)
  library(data.table)
  library(cli)
})

cli::cli_h1("Performance Target Verification")

# Setup test database
cli::cli_h2("Setup")
ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")

if (!dir.exists(ae_path)) {
  emuR::create_emuRdemoData(tempdir())
}

ae_db <- load_emuDB(ae_path, verbose = FALSE)
corp <- corpus(ae_path, verbose = FALSE)

cli::cli_alert_success("Test database loaded")

#===============================================================================
# TARGET 3: Simple Queries (50%+ faster)
#===============================================================================

cli::cli_h2("Target 3: Simple Queries")
cli::cli_text("Target: 50%+ faster than emuR::query()")

bench_simple <- bench::mark(
  emuR = emuR::query(ae_db, "[Phonetic == a]", resultType = "tibble"),
  reindeer = ask_for(corp, "Phonetic == a"),
  check = FALSE,
  iterations = 50,
  time_unit = "ms"
)

times <- as.numeric(bench_simple$median)
speedup_simple <- times[1] / times[2]
pct_simple <- (speedup_simple - 1) * 100

cli::cli_alert_info("emuR: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("reindeer: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup_simple)}} ({.val {sprintf('%.1f%%', pct_simple)}} faster)")

target3_met <- pct_simple >= 50
if (target3_met) {
  cli::cli_alert_success("TARGET MET: {.val {sprintf('%.1f%%', pct_simple)}} >= 50%")
} else {
  cli::cli_alert_danger("TARGET MISSED: {.val {sprintf('%.1f%%', pct_simple)}} < 50%")
}

#===============================================================================
# TARGET 4: Sequence Queries / Complex Joins (70%+ faster)
#===============================================================================

cli::cli_h2("Target 4: Sequence Queries (Complex Joins)")
cli::cli_text("Target: 70%+ faster than emuR::query()")

seq_query <- "[Phonetic == a -> Phonetic == t]"

bench_seq <- bench::mark(
  emuR = emuR::query(ae_db, seq_query, resultType = "tibble"),
  reindeer = ask_for(corp, seq_query),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_seq$median)
speedup_seq <- times[1] / times[2]
pct_seq <- (speedup_seq - 1) * 100

cli::cli_alert_info("emuR: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("reindeer: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup_seq)}} ({.val {sprintf('%.1f%%', pct_seq)}} faster)")

target4_met <- pct_seq >= 70
if (target4_met) {
  cli::cli_alert_success("TARGET MET: {.val {sprintf('%.1f%%', pct_seq)}} >= 70%")
} else {
  cli::cli_alert_warning("TARGET PARTIAL: {.val {sprintf('%.1f%%', pct_seq)}} < 70%")
}

#===============================================================================
# TARGET 5: Large Result Sets / Regex (80%+ faster)
#===============================================================================

cli::cli_h2("Target 5: Large Result Sets (Regex)")
cli::cli_text("Target: 80%+ faster for large datasets")

regex_query <- "Phonetic =~ .*"

bench_large <- bench::mark(
  emuR = suppressWarnings(emuR::query(ae_db, sprintf("[%s]", regex_query), resultType = "tibble")),
  reindeer = ask_for(corp, regex_query),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_large$median)
speedup_large <- times[1] / times[2]
pct_large <- (speedup_large - 1) * 100

n_results <- nrow(ask_for(corp, regex_query))
cli::cli_alert_info("Result set size: {.val {n_results}} segments")
cli::cli_alert_info("emuR: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("reindeer: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup_large)}} ({.val {sprintf('%.1f%%', pct_large)}} faster)")

target5_met <- pct_large >= 80
if (target5_met) {
  cli::cli_alert_success("TARGET MET: {.val {sprintf('%.1f%%', pct_large)}} >= 80%")
} else {
  cli::cli_alert_warning("TARGET PARTIAL: {.val {sprintf('%.1f%%', pct_large)}} < 80%")
}

#===============================================================================
# TARGET 6: Bundle Listing (50%+ faster)
#===============================================================================

cli::cli_h2("Target 6: Bundle Listing")
cli::cli_text("Target: 50%+ faster than emuR bundle listing")

bench_meta <- bench::mark(
  emuR = emuR::list_bundles(ae_db),
  reindeer = reindeer:::.list_bundles(corp),
  check = FALSE,
  iterations = 50,
  time_unit = "ms"
)

times <- as.numeric(bench_meta$median)
speedup_meta <- times[1] / times[2]
pct_meta <- (speedup_meta - 1) * 100

cli::cli_alert_info("emuR::list_bundles: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info(".list_bundles: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', speedup_meta)}} ({.val {sprintf('%.1f%%', pct_meta)}} faster)")

target6_met <- pct_meta >= 50
if (target6_met) {
  cli::cli_alert_success("TARGET MET: {.val {sprintf('%.1f%%', pct_meta)}} >= 50%")
} else {
  cli::cli_alert_warning("TARGET PARTIAL: {.val {sprintf('%.1f%%', pct_meta)}} < 50%")
}

#===============================================================================
# SUMMARY
#===============================================================================

cli::cli_h1("Performance Target Summary")

target_results <- data.frame(
  Operation = c("Simple queries", "Sequence queries", "Large result sets", "Bundle listing"),
  Target = c("50%+ faster", "70%+ faster", "80%+ faster", "50%+ faster"),
  Speedup = sprintf("%.2fx", c(speedup_simple, speedup_seq, speedup_large, speedup_meta)),
  Status = c(
    if (target3_met) "MET" else "MISSED",
    if (target4_met) "MET" else "PARTIAL",
    if (target5_met) "MET" else "PARTIAL",
    if (target6_met) "MET" else "PARTIAL"
  ),
  stringsAsFactors = FALSE
)

print(target_results)

targets_met <- sum(target_results$Status == "MET")
cli::cli_alert_info("Targets met: {.val {targets_met}}/{.val {nrow(target_results)}}")

# Save results
results <- list(
  simple = bench_simple,
  sequence = bench_seq,
  large = bench_large,
  metadata = bench_meta,
  summary = target_results,
  timestamp = Sys.time()
)

saveRDS(results, "benchmarking/performance_targets_results.rds")
cli::cli_alert_success("Results saved to benchmarking/performance_targets_results.rds")

# Cleanup
DBI::dbDisconnect(ae_db$connection)

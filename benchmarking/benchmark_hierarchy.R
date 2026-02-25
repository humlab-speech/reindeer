#!/usr/bin/env Rscript
# Hierarchy Navigation Benchmarks
# Compare emuR::requery_hier() vs reindeer ascend_to()/descend_to()

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(emuR)
  library(bench)
  library(cli)
})

cli::cli_h1("Hierarchy Navigation: requery_hier vs ascend/descend")

# Setup
ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
if (!dir.exists(ae_path)) emuR::create_emuRdemoData(tempdir())

ae_db <- load_emuDB(ae_path, verbose = FALSE)
corp <- corpus(ae_path, verbose = FALSE)

# Pre-compute segments for both systems
segs_emur <- emuR::query(ae_db, "[Phonetic == t]")
segs_rein <- ask_for(corp, "Phonetic == t")

word_segs_emur <- suppressWarnings(emuR::query(ae_db, "Word =~ .*"))
word_segs_rein <- ask_for(corp, "Word =~ .*")

#===============================================================================
# 1. Ascend: Phonetic -> Syllable (one level up)
#===============================================================================

cli::cli_h2("Ascend: Phonetic -> Syllable")

bench_ascend_syl <- bench::mark(
  emuR = emuR::requery_hier(ae_db, segs_emur, level = "Syllable"),
  reindeer = ascend_to(segs_rein, "Syllable"),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_ascend_syl$median)
cli::cli_alert_info("emuR::requery_hier: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("ascend_to: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 2. Ascend: Phonetic -> Word (two levels up)
#===============================================================================

cli::cli_h2("Ascend: Phonetic -> Word")

bench_ascend_word <- bench::mark(
  emuR = emuR::requery_hier(ae_db, segs_emur, level = "Word"),
  reindeer = ascend_to(segs_rein, "Word"),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_ascend_word$median)
cli::cli_alert_info("emuR::requery_hier: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("ascend_to: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 3. Descend: Word -> Phonetic
#===============================================================================

cli::cli_h2("Descend: Word -> Phonetic")

bench_descend <- bench::mark(
  emuR = emuR::requery_hier(ae_db, word_segs_emur, level = "Phonetic"),
  reindeer = descend_to(word_segs_rein, "Phonetic"),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_descend$median)
cli::cli_alert_info("emuR::requery_hier: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("descend_to: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# SUMMARY
#===============================================================================

cli::cli_h1("Hierarchy Benchmark Summary")

summary_df <- data.frame(
  Operation = c("Ascend Phonetic->Syllable", "Ascend Phonetic->Word", "Descend Word->Phonetic"),
  emuR_ms = sprintf("%.2f", c(
    as.numeric(bench_ascend_syl$median)[1],
    as.numeric(bench_ascend_word$median)[1],
    as.numeric(bench_descend$median)[1]
  )),
  reindeer_ms = sprintf("%.2f", c(
    as.numeric(bench_ascend_syl$median)[2],
    as.numeric(bench_ascend_word$median)[2],
    as.numeric(bench_descend$median)[2]
  )),
  Speedup = sprintf("%.2fx", c(
    as.numeric(bench_ascend_syl$median)[1] / as.numeric(bench_ascend_syl$median)[2],
    as.numeric(bench_ascend_word$median)[1] / as.numeric(bench_ascend_word$median)[2],
    as.numeric(bench_descend$median)[1] / as.numeric(bench_descend$median)[2]
  )),
  stringsAsFactors = FALSE
)

print(summary_df)

# Save
results <- list(
  ascend_syllable = bench_ascend_syl,
  ascend_word = bench_ascend_word,
  descend_phonetic = bench_descend,
  summary = summary_df,
  timestamp = Sys.time()
)

saveRDS(results, "benchmarking/hierarchy_benchmark_results.rds")
cli::cli_alert_success("Saved to benchmarking/hierarchy_benchmark_results.rds")

DBI::dbDisconnect(ae_db$connection)

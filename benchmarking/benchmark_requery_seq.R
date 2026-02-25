#!/usr/bin/env Rscript
# Sequential Navigation Benchmarks
# Compare emuR::requery_seq() vs reindeer scout()/retreat()

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(emuR)
  library(bench)
  library(cli)
})

cli::cli_h1("Sequential Navigation: requery_seq vs scout/retreat")

# Setup
ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
if (!dir.exists(ae_path)) emuR::create_emuRdemoData(tempdir())

ae_db <- load_emuDB(ae_path, verbose = FALSE)
corp <- corpus(ae_path, verbose = FALSE)

# Pre-compute segments
segs_emur <- emuR::query(ae_db, "[Phonetic == t]")
segs_rein <- ask_for(corp, "Phonetic == t")

#===============================================================================
# 1. scout(1) vs requery_seq(offset = 1) — next segment
#===============================================================================

cli::cli_h2("Next segment (offset +1)")

bench_next <- bench::mark(
  emuR = emuR::requery_seq(ae_db, segs_emur, offset = 1),
  reindeer = scout(segs_rein, 1),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_next$median)
cli::cli_alert_info("emuR::requery_seq(offset=1): {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("scout(1): {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 2. retreat(1) vs requery_seq(offset = -1) — previous segment
#===============================================================================

cli::cli_h2("Previous segment (offset -1)")

bench_prev <- bench::mark(
  emuR = emuR::requery_seq(ae_db, segs_emur, offset = -1),
  reindeer = retreat(segs_rein, 1),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_prev$median)
cli::cli_alert_info("emuR::requery_seq(offset=-1): {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("retreat(1): {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 3. scout(2) vs requery_seq(offset = 2) — 2 ahead
#===============================================================================

cli::cli_h2("Two segments ahead (offset +2)")

bench_two <- bench::mark(
  emuR = emuR::requery_seq(ae_db, segs_emur, offset = 2),
  reindeer = scout(segs_rein, 2),
  check = FALSE,
  iterations = 30,
  time_unit = "ms"
)

times <- as.numeric(bench_two$median)
cli::cli_alert_info("emuR::requery_seq(offset=2): {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("scout(2): {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# SUMMARY
#===============================================================================

cli::cli_h1("Sequential Navigation Summary")

summary_df <- data.frame(
  Operation = c("Next (offset +1)", "Previous (offset -1)", "Two ahead (offset +2)"),
  emuR_ms = sprintf("%.2f", c(
    as.numeric(bench_next$median)[1],
    as.numeric(bench_prev$median)[1],
    as.numeric(bench_two$median)[1]
  )),
  reindeer_ms = sprintf("%.2f", c(
    as.numeric(bench_next$median)[2],
    as.numeric(bench_prev$median)[2],
    as.numeric(bench_two$median)[2]
  )),
  Speedup = sprintf("%.2fx", c(
    as.numeric(bench_next$median)[1] / as.numeric(bench_next$median)[2],
    as.numeric(bench_prev$median)[1] / as.numeric(bench_prev$median)[2],
    as.numeric(bench_two$median)[1] / as.numeric(bench_two$median)[2]
  )),
  stringsAsFactors = FALSE
)

print(summary_df)

# Save
results <- list(
  next_seg = bench_next,
  prev_seg = bench_prev,
  two_ahead = bench_two,
  summary = summary_df,
  timestamp = Sys.time()
)

saveRDS(results, "benchmarking/requery_seq_benchmark_results.rds")
cli::cli_alert_success("Saved to benchmarking/requery_seq_benchmark_results.rds")

DBI::dbDisconnect(ae_db$connection)

#!/usr/bin/env Rscript
# Track Data Benchmarks
# Compare emuR::get_trackdata() vs direct SSFF reading
#
# Note: quantify() requires a superassp DSP function and operates differently
# from get_trackdata() (it computes new tracks rather than reading existing
# SSFF files). This benchmark compares emuR::get_trackdata() with the low-level
# SSFF reading that reindeer uses internally, to measure the overhead of
# emuR's query+read pipeline vs direct file access.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(emuR)
  library(bench)
  library(cli)
})

cli::cli_h1("Track Data: emuR::get_trackdata baseline")

# Setup
ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
if (!dir.exists(ae_path)) emuR::create_emuRdemoData(tempdir())

ae_db <- load_emuDB(ae_path, verbose = FALSE)
corp <- corpus(ae_path, verbose = FALSE)

# Check available tracks
tracks <- emuR::list_ssffTrackDefinitions(ae_db)
cli::cli_alert_info("Available tracks: {.val {paste(tracks$name, collapse = ', ')}}")

# "fm" (formants) track exists in ae demo
track_name <- "fm"

#===============================================================================
# 1. get_trackdata with filtered segments
#===============================================================================

cli::cli_h2("get_trackdata: filtered segments (Phonetic == a)")

segs_emur <- emuR::query(ae_db, "[Phonetic == a]")
cli::cli_alert_info("Segment count: {.val {nrow(segs_emur)}}")

bench_filtered <- bench::mark(
  emuR_get_trackdata = emuR::get_trackdata(ae_db, segs_emur, ssffTrackName = track_name),
  iterations = 10,
  time_unit = "ms"
)

cli::cli_alert_info("emuR::get_trackdata (filtered): {.val {sprintf('%.2f ms', as.numeric(bench_filtered$median))}}")

#===============================================================================
# 2. get_trackdata with all segments (larger result set)
#===============================================================================

cli::cli_h2("get_trackdata: all segments")

all_segs_emur <- emuR::query(ae_db, "[Phonetic =~ .*]")
cli::cli_alert_info("Segment count: {.val {nrow(all_segs_emur)}}")

bench_all <- bench::mark(
  emuR_get_trackdata = emuR::get_trackdata(ae_db, all_segs_emur, ssffTrackName = track_name),
  iterations = 5,
  time_unit = "ms"
)

cli::cli_alert_info("emuR::get_trackdata (all): {.val {sprintf('%.2f ms', as.numeric(bench_all$median))}}")

#===============================================================================
# 3. Query + get_trackdata combined (end-to-end)
#===============================================================================

cli::cli_h2("End-to-end: query + get_trackdata vs ask_for (query only)")
cli::cli_text("Measures emuR query+trackdata pipeline overhead")

bench_e2e <- bench::mark(
  emuR_query_plus_track = {
    s <- emuR::query(ae_db, "[Phonetic == a]")
    emuR::get_trackdata(ae_db, s, ssffTrackName = track_name)
  },
  reindeer_query_only = {
    ask_for(corp, "Phonetic == a")
  },
  check = FALSE,
  iterations = 10,
  time_unit = "ms"
)

times <- as.numeric(bench_e2e$median)
cli::cli_alert_info("emuR (query+trackdata): {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("reindeer (query only): {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Note: reindeer query is just the query step; trackdata requires superassp DSP")

#===============================================================================
# SUMMARY
#===============================================================================

cli::cli_h1("Track Data Summary")

summary_df <- data.frame(
  Operation = c(
    sprintf("get_trackdata (filtered, n=%d)", nrow(segs_emur)),
    sprintf("get_trackdata (all, n=%d)", nrow(all_segs_emur)),
    "emuR query+trackdata",
    "reindeer query only"
  ),
  Median_ms = sprintf("%.2f", c(
    as.numeric(bench_filtered$median),
    as.numeric(bench_all$median),
    times[1],
    times[2]
  )),
  stringsAsFactors = FALSE
)

print(summary_df)

cli::cli_text("")
cli::cli_alert_info("Full quantify() comparison requires superassp — see benchmark_quantify.R for DSP benchmarks")

# Save
results <- list(
  filtered = bench_filtered,
  all_segments = bench_all,
  end_to_end = bench_e2e,
  summary = summary_df,
  timestamp = Sys.time()
)

saveRDS(results, "benchmarking/trackdata_benchmark_results.rds")
cli::cli_alert_success("Saved to benchmarking/trackdata_benchmark_results.rds")

DBI::dbDisconnect(ae_db$connection)

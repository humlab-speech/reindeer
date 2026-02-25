# ==============================================================================
# METADATA OPERATIONS BENCHMARKING
# ==============================================================================
# Benchmarks metadata operations, including comparison with emuR equivalents.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(emuR)
  library(bench)
  library(ggplot2)
  library(dplyr)
  library(cli)
})

# Setup test database
setup_test_db <- function() {
  if (!dir.exists(file.path(tempdir(), "emuR_demoData"))) {
    emuR::create_emuRdemoData(tempdir())
  }

  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  ae_handle <- emuR::load_emuDB(ae_path, verbose = FALSE)

  # Add bundle-level metadata
  bundles <- emuR::list_bundles(ae_handle)
  corp <- corpus(ae_path, verbose = FALSE)

  for (i in seq_len(nrow(bundles))) {
    add_metadata(
      corp,
      list(Age = sample(20:60, 1), Gender = sample(c("Male", "Female"), 1)),
      session = bundles$session[i],
      bundle = bundles$name[i]
    )
  }

  gather_metadata(corp, verbose = FALSE)

  list(path = ae_path, handle = ae_handle, corpus = corp)
}

cli::cli_h1("Metadata Operations Benchmarking")

cli::cli_alert_info("Setting up test database...")
setup <- setup_test_db()
corp <- setup$corpus
ae_db <- setup$handle

#===============================================================================
# 1. emuR::list_bundles vs get_metadata — head-to-head
#===============================================================================

cli::cli_h2("emuR::list_bundles vs get_metadata")

bench_vs_emur <- bench::mark(
  emuR = emuR::list_bundles(ae_db),
  reindeer = get_metadata(corp),
  check = FALSE,
  iterations = 50,
  time_unit = "ms"
)

times <- as.numeric(bench_vs_emur$median)
cli::cli_alert_info("emuR::list_bundles: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("get_metadata: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 2. Metadata gathering (reindeer-only, no emuR equivalent)
#===============================================================================

cli::cli_h2("Metadata Gathering (reindeer-only)")

bench_gather <- bench::mark(
  gather = gather_metadata(corp, verbose = FALSE),
  iterations = 10,
  check = FALSE
)

cli::cli_alert_info("gather_metadata: {.val {format(bench_gather$median)}}")

#===============================================================================
# 3. Filtered retrieval
#===============================================================================

cli::cli_h2("Filtered Metadata Retrieval")

bench_filtered <- bench::mark(
  get_all = get_metadata(corp),
  get_filtered = get_metadata(corp, bundle_pattern = "msajc.*"),
  iterations = 50,
  check = FALSE
)

cli::cli_alert_success("Retrieval benchmark complete")
print(bench_filtered)

#===============================================================================
# 4. biographize (reindeer-only)
#===============================================================================

cli::cli_h2("biographize (reindeer-only)")

segs <- ask_for(corp, "Phonetic == t")

bench_bio <- bench::mark(
  biographize = biographize(segs, corp),
  iterations = 30,
  check = FALSE
)

cli::cli_alert_info("biographize: {.val {format(bench_bio$median)}}")

#===============================================================================
# 5. Excel export (reindeer-only)
#===============================================================================

cli::cli_h2("Excel Export")

temp_excel <- tempfile(fileext = ".xlsx")

bench_export <- bench::mark(
  export = export_metadata(corp, temp_excel, overwrite = TRUE),
  iterations = 5,
  check = FALSE
)

cli::cli_alert_info("export_metadata: {.val {format(bench_export$median)}}")
unlink(temp_excel)

#===============================================================================
# SUMMARY
#===============================================================================

cli::cli_h1("Summary")

all_results <- bind_rows(
  bench_vs_emur %>%
    select(expression, median, mem_alloc) %>%
    mutate(category = "vs_emuR"),
  bench_gather %>%
    select(expression, median, mem_alloc) %>%
    mutate(category = "gather"),
  bench_filtered %>%
    select(expression, median, mem_alloc) %>%
    mutate(category = "retrieve"),
  bench_bio %>%
    select(expression, median, mem_alloc) %>%
    mutate(category = "biographize"),
  bench_export %>%
    select(expression, median, mem_alloc) %>%
    mutate(category = "export")
)

print(all_results)

# Save
saveRDS(
  list(
    vs_emuR = bench_vs_emur,
    gather = bench_gather,
    filtered = bench_filtered,
    biographize = bench_bio,
    export = bench_export,
    summary = all_results
  ),
  file = "benchmarking/metadata_benchmark_results.rds"
)

cli::cli_alert_success("Saved to benchmarking/metadata_benchmark_results.rds")

DBI::dbDisconnect(ae_db$connection)

# ==============================================================================
# LISTING & CACHE-BUILD BENCHMARKING
# ==============================================================================
# Benchmarks session/bundle listing and cache build speed vs emuR equivalents,
# plus reindeer-only metadata operations.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(emuR)
  library(bench)
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
  corp <- corpus(ae_path, verbose = FALSE)

  # Add bundle-level metadata for biographize test
  bundles <- emuR::list_bundles(ae_handle)
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

cli::cli_h1("Listing & Cache-Build Benchmarking")

cli::cli_alert_info("Setting up test database...")
setup <- setup_test_db()
corp <- setup$corpus
ae_db <- setup$handle
ae_path <- setup$path

#===============================================================================
# 1. Session listing — emuR vs reindeer
#===============================================================================

cli::cli_h2("Session Listing: emuR vs reindeer")

bench_sessions <- bench::mark(
  emuR = emuR::list_sessions(ae_db),
  reindeer = reindeer:::.list_sessions(corp),
  check = FALSE,
  iterations = 50,
  time_unit = "ms"
)

times <- as.numeric(bench_sessions$median)
cli::cli_alert_info("emuR::list_sessions: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info(".list_sessions: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 2. Bundle listing — emuR vs reindeer
#===============================================================================

cli::cli_h2("Bundle Listing: emuR vs reindeer")

bench_bundles <- bench::mark(
  emuR = emuR::list_bundles(ae_db),
  reindeer = reindeer:::.list_bundles(corp),
  check = FALSE,
  iterations = 50,
  time_unit = "ms"
)

times <- as.numeric(bench_bundles$median)
cli::cli_alert_info("emuR::list_bundles: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info(".list_bundles: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 3. Cache build — emuR::load_emuDB vs build_emuDB_cache
#===============================================================================

cli::cli_h2("Cache Build: emuR::load_emuDB vs build_emuDB_cache")

cache_file <- file.path(ae_path, paste0("ae", "_emuDBcache.sqlite"))

bench_cache <- bench::mark(
  emuR = {
    unlink(cache_file)
    emuR::load_emuDB(ae_path, verbose = FALSE)
  },
  reindeer = {
    unlink(cache_file)
    build_emuDB_cache(ae_path)
  },
  check = FALSE,
  iterations = 10,
  time_unit = "ms"
)

# Reload after benchmark so subsequent tests work
ae_db <- emuR::load_emuDB(ae_path, verbose = FALSE)
corp <- corpus(ae_path, verbose = FALSE)

times <- as.numeric(bench_cache$median)
cli::cli_alert_info("emuR::load_emuDB: {.val {sprintf('%.2f ms', times[1])}}")
cli::cli_alert_info("build_emuDB_cache: {.val {sprintf('%.2f ms', times[2])}}")
cli::cli_alert_info("Speedup: {.val {sprintf('%.2fx', times[1]/times[2])}}")

#===============================================================================
# 4. Metadata gathering (reindeer-only)
#===============================================================================

cli::cli_h2("Metadata Gathering (reindeer-only)")

bench_gather <- bench::mark(
  gather = gather_metadata(corp, verbose = FALSE),
  iterations = 10,
  check = FALSE
)

cli::cli_alert_info("gather_metadata: {.val {format(bench_gather$median)}}")

#===============================================================================
# 5. biographize (reindeer-only)
#===============================================================================

cli::cli_h2("biographize (reindeer-only)")

# .meta_json files persist through cache rebuild, just re-gather
gather_metadata(corp, verbose = FALSE)

segs <- ask_for(corp, "Phonetic == t")

bench_bio <- bench::mark(
  biographize = biographize(segs, corp),
  iterations = 30,
  check = FALSE
)

cli::cli_alert_info("biographize: {.val {format(bench_bio$median)}}")

#===============================================================================
# SUMMARY
#===============================================================================

cli::cli_h1("Summary")

coerce_bench <- function(b, cat) {
  med <- b$median
  # bench::mark with time_unit="ms" returns plain numeric (already ms);
  # without time_unit returns bench_time (seconds)
  if (inherits(med, "bench_time")) {
    ms <- as.numeric(med) * 1000
  } else {
    ms <- as.numeric(med)
  }
  tibble::tibble(
    expression = b$expression,
    median_ms = ms,
    mem_alloc = b$mem_alloc,
    category = cat
  )
}

all_results <- bind_rows(
  coerce_bench(bench_sessions, "session_listing"),
  coerce_bench(bench_bundles, "bundle_listing"),
  coerce_bench(bench_cache, "cache_build"),
  coerce_bench(bench_gather, "gather"),
  coerce_bench(bench_bio, "biographize")
)

print(all_results)

# Save
saveRDS(
  list(
    session_listing = bench_sessions,
    bundle_listing = bench_bundles,
    cache_build = bench_cache,
    gather = bench_gather,
    biographize = bench_bio,
    summary = all_results
  ),
  file = "benchmarking/metadata_benchmark_results.rds"
)

cli::cli_alert_success("Saved to benchmarking/metadata_benchmark_results.rds")

DBI::dbDisconnect(ae_db$connection)

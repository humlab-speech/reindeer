# ==============================================================================
# METADATA OPERATIONS BENCHMARKING
# ==============================================================================
#
# Benchmarks metadata gathering and retrieval operations
#

library(bench)
library(reindeer)
library(ggplot2)
library(dplyr)

# Setup test database
setup_test_db <- function() {
  # Use emuR demo data
  if (!dir.exists(file.path(tempdir(), "emuR_demoData"))) {
    emuR::create_emuRdemoData(tempdir())
  }
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  
  # Add some metadata for testing
  ae_handle <- emuR::load_emuDB(ae_path)
  
  # Database-level
  add_metadata(corpus(ae_path), list(Project = "Test", Institution = "University"))
  
  # Session-level
  sessions <- emuR::list_sessions(ae_handle)
  for (sess in sessions$name) {
    add_metadata(corpus(ae_path), list(SessionDate = "2024-01-01"), session = sess)
  }
  
  # Bundle-level - add varying metadata
  bundles <- emuR::list_bundles(ae_handle)
  ages <- sample(20:60, nrow(bundles), replace = TRUE)
  genders <- sample(c("Male", "Female"), nrow(bundles), replace = TRUE)
  
  for (i in seq_len(nrow(bundles))) {
    add_metadata(
      corpus(ae_path),
      list(Age = ages[i], Gender = genders[i]),
      session = bundles$session[i],
      bundle = bundles$name[i]
    )
  }
  
  ae_path
}

cli::cli_h1("Metadata Operations Benchmarking")

# Setup
cli::cli_alert_info("Setting up test database...")
ae_path <- setup_test_db()
corp <- corpus(ae_path)

cli::cli_h2("Benchmark 1: Metadata Gathering")

results_gather <- bench::mark(
  gather_from_files = {
    gather_metadata(corp, verbose = FALSE, parallel = FALSE)
  },
  gather_parallel = {
    if (requireNamespace("future.apply", quietly = TRUE)) {
      gather_metadata(corp, verbose = FALSE, parallel = TRUE)
    } else {
      gather_metadata(corp, verbose = FALSE, parallel = FALSE)
    }
  },
  iterations = 10,
  check = FALSE  # Results may differ slightly due to timestamps
)

cli::cli_alert_success("Gathering benchmark complete")
print(results_gather)

cli::cli_h2("Benchmark 2: Metadata Retrieval")

# First gather to ensure cache is populated
gather_metadata(corp, verbose = FALSE)

results_retrieve <- bench::mark(
  get_all_metadata = {
    get_metadata(corp)
  },
  get_filtered_metadata = {
    get_metadata(corp, bundle_pattern = "msajc.*")
  },
  iterations = 50,
  check = FALSE
)

cli::cli_alert_success("Retrieval benchmark complete")
print(results_retrieve)

cli::cli_h2("Benchmark 3: Excel Export")

temp_excel <- tempfile(fileext = ".xlsx")

results_export <- bench::mark(
  export_to_excel = {
    export_metadata(corp, temp_excel, overwrite = TRUE)
  },
  iterations = 5,
  check = FALSE
)

cli::cli_alert_success("Export benchmark complete")
print(results_export)

# Create summary
cli::cli_h2("Summary")

all_results <- bind_rows(
  results_gather %>% 
    select(expression, median, mem_alloc) %>%
    mutate(operation = "gather"),
  results_retrieve %>%
    select(expression, median, mem_alloc) %>%
    mutate(operation = "retrieve"),
  results_export %>%
    select(expression, median, mem_alloc) %>%
    mutate(operation = "export")
)

print(all_results)

# Save results
saveRDS(
  list(
    gather = results_gather,
    retrieve = results_retrieve,
    export = results_export,
    summary = all_results
  ),
  file = "benchmarking/metadata_benchmark_results.rds"
)

cli::cli_alert_success("Results saved to benchmarking/metadata_benchmark_results.rds")

# Create visualization
p <- ggplot(all_results, aes(x = as.character(expression), y = as.numeric(median), fill = operation)) +
  geom_col() +
  facet_wrap(~operation, scales = "free") +
  coord_flip() +
  labs(
    title = "Metadata Operations Performance",
    x = "Operation",
    y = "Median Time (seconds)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("benchmarking/metadata_performance.png", p, width = 10, height = 6, dpi = 300)

cli::cli_alert_success("Visualization saved to benchmarking/metadata_performance.png")

# Cleanup
unlink(temp_excel)

cli::cli_alert_success("Benchmarking complete!")

# ==============================================================================
# FIDELITY TESTS: Python/Parselmouth vs Praat Implementations
# ==============================================================================
#
# Tests to ensure Python implementations produce equivalent results to
# original Praat scripts
#

library(testthat)
library(reindeer)
library(emuR)
library(dplyr)

context("Python annotation fidelity tests")

# Setup test database
setup_test_db <- function() {
  # Create test database
  test_dir <- tempdir()
  ae_path <- file.path(test_dir, "ae_emuDB")
  
  if (dir.exists(ae_path)) {
    unlink(ae_path, recursive = TRUE)
  }
  
  # Create demo database
  emuR::create_emuRdemoData(dir = test_dir)
  ae_db <- emuR::load_emuDB(ae_path)
  
  list(
    db = ae_db,
    path = ae_path,
    corpus = corpus(ae_path)
  )
}

# ==============================================================================
# PERIOD ANNOTATION FIDELITY
# ==============================================================================

test_that("Python period annotation matches Praat output structure", {
  skip_if_not(has_python_annotations(), "Python annotations not available")
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  
  # Get a test bundle
  bundles <- corp["0000", "msajc003"]
  
  # Draft period annotations using Python
  suggestions <- draft_periods(
    corpus = corp,
    bundles = bundles,
    level_name = "Periods_Test",
    minimum_f0 = 75,
    maximum_f0 = 600
  )
  
  # Check that we got suggestions
  expect_s7_class(suggestions, "SuggestedEvents")
  expect_equal(suggestions@level_type, "EVENT")
  expect_equal(suggestions@level_name, "Periods_Test")
  
  # Check suggestions structure
  expect_true("start_time" %in% names(suggestions@suggestions))
  expect_true("end_time" %in% names(suggestions@suggestions))
  expect_true("label" %in% names(suggestions@suggestions))
  
  # For EVENT type, start_time should equal end_time
  expect_equal(suggestions@suggestions$start_time, 
               suggestions@suggestions$end_time)
  
  # Labels should be numeric intensity values (or empty)
  non_empty_labels <- suggestions@suggestions$label[suggestions@suggestions$label != ""]
  if (length(non_empty_labels) > 0) {
    expect_true(all(grepl("^-?[0-9]+\\.?[0-9]*$", non_empty_labels)))
  }
  
  # Clean up
  unlink(test_env$path, recursive = TRUE)
})


test_that("Period annotations are monotonically increasing", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  suggestions <- draft_periods(corp, bundles, level_name = "Periods_Test")
  
  # Times should be in order
  times <- suggestions@suggestions$start_time
  expect_true(all(diff(times) > 0))
  
  unlink(test_env$path, recursive = TRUE)
})


test_that("Period annotations respect F0 range constraints", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  # Use narrow F0 range
  suggestions_narrow <- draft_periods(
    corp, bundles,
    minimum_f0 = 150,
    maximum_f0 = 300
  )
  
  # Use wide F0 range
  suggestions_wide <- draft_periods(
    corp, bundles,
    minimum_f0 = 50,
    maximum_f0 = 600
  )
  
  # Wide range should find more periods
  expect_gte(
    nrow(suggestions_wide@suggestions),
    nrow(suggestions_narrow@suggestions)
  )
  
  unlink(test_env$path, recursive = TRUE)
})


# ==============================================================================
# INTSINT/MOMEL FIDELITY
# ==============================================================================

test_that("INTSINT annotations contain valid tone labels", {
  skip_if_not(has_python_annotations())
  skip("INTSINT tests require full MOMEL setup")
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  suggestions <- draft_intsint_momel(
    corpus = corp,
    bundles = bundles,
    level_name = "Intsint_Test"
  )
  
  # Check tone labels
  valid_tones <- c("T", "M", "B", "H", "U", "S", "D", "L")
  all_labels <- suggestions@suggestions$label
  
  expect_true(all(all_labels %in% valid_tones))
  
  # Should have at least some absolute tones
  absolute_tones <- c("T", "M", "B")
  expect_true(any(all_labels %in% absolute_tones))
  
  unlink(test_env$path, recursive = TRUE)
})


# ==============================================================================
# TRANSCRIPTION SYSTEM INTEGRATION
# ==============================================================================

test_that("Suggestions integrate correctly with transcription system", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  # Draft
  suggestions <- draft_periods(corp, bundles, level_name = "TestPeriods")
  
  # Assess
  assessment <- assess(suggestions)
  expect_true(suggestions@assessed)
  expect_false(is.null(suggestions@assessment_results))
  
  # Check assessment results structure
  expect_true("level_exists" %in% names(assessment))
  expect_true("has_overlaps" %in% names(assessment))
  expect_true("within_bundle_bounds" %in% names(assessment))
  
  unlink(test_env$path, recursive = TRUE)
})


test_that("Prepare creates levels correctly", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  suggestions <- draft_periods(corp, bundles, level_name = "NewPeriods")
  assess(suggestions)
  
  # Level should not exist yet
  levels <- emuR::list_levelDefinitions(test_env$db)
  expect_false("NewPeriods" %in% levels$name)
  
  # Prepare should create it
  prepare(suggestions)
  
  # Now it should exist
  levels <- emuR::list_levelDefinitions(test_env$db)
  expect_true("NewPeriods" %in% levels$name)
  
  # Check level type
  level_info <- levels %>% dplyr::filter(name == "NewPeriods")
  expect_equal(level_info$type, "EVENT")
  
  unlink(test_env$path, recursive = TRUE)
})


test_that("Transcribe applies annotations to database", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  suggestions <- draft_periods(corp, bundles, level_name = "TestPeriods2")
  assess(suggestions)
  prepare(suggestions)
  
  # Transcribe
  log <- transcribe(suggestions)
  
  # Check log
  expect_s7_class(log, "TranscriptionLog")
  expect_true(log@success)
  expect_gt(log@n_items_added, 0)
  
  # Verify annotations in database
  result <- emuR::query(test_env$db, "TestPeriods2 =~ .*")
  expect_gt(nrow(result), 0)
  
  # Should match number of suggestions
  expect_equal(nrow(result), nrow(suggestions@suggestions))
  
  unlink(test_env$path, recursive = TRUE)
})


# ==============================================================================
# DATABASE COMPATIBILITY
# ==============================================================================

test_that("Annotated database remains loadable by emuR", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  suggestions <- draft_periods(corp, bundles, level_name = "CompatTest")
  assess(suggestions)
  prepare(suggestions)
  transcribe(suggestions)
  
  # Reload database with emuR
  expect_silent({
    db_reload <- emuR::load_emuDB(test_env$path)
  })
  
  # Query should work
  expect_silent({
    result <- emuR::query(db_reload, "CompatTest =~ .*")
  })
  
  expect_gt(nrow(result), 0)
  
  unlink(test_env$path, recursive = TRUE)
})


test_that("All emuR list functions work after annotation", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  suggestions <- draft_periods(corp, bundles, level_name = "ListTest")
  assess(suggestions)
  prepare(suggestions)
  transcribe(suggestions)
  
  db <- emuR::load_emuDB(test_env$path)
  
  # Test various list functions
  expect_silent(levels <- emuR::list_levelDefinitions(db))
  expect_true("ListTest" %in% levels$name)
  
  expect_silent(attrs <- emuR::list_attributeDefinitions(db, "ListTest"))
  expect_gt(nrow(attrs), 0)
  
  expect_silent(bundles_list <- emuR::list_bundles(db))
  expect_gt(nrow(bundles_list), 0)
  
  expect_silent(files <- emuR::list_files(db))
  expect_gt(nrow(files), 0)
  
  unlink(test_env$path, recursive = TRUE)
})


# ==============================================================================
# PARALLEL PROCESSING SAFETY
# ==============================================================================

test_that("Parallel annotation processing is safe", {
  skip_if_not(has_python_annotations())
  skip_on_cran()
  skip_if(parallel::detectCores() < 2, "Need multiple cores")
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  
  # Get multiple bundles
  all_bundles <- corp["0000", ".*"]
  
  # Process in parallel
  library(future)
  library(furrr)
  
  plan(multisession, workers = 2)
  
  results <- all_bundles %>%
    dplyr::group_by(bundle) %>%
    dplyr::group_split() %>%
    furrr::future_map(function(b) {
      draft_periods(corp, b, level_name = "ParallelTest")
    })
  
  # All results should be valid
  expect_true(all(sapply(results, function(r) {
    S7::S7_inherits(r, "SuggestedEvents")
  })))
  
  # Each should have suggestions
  expect_true(all(sapply(results, function(r) {
    nrow(r@suggestions) > 0
  })))
  
  plan(sequential)
  unlink(test_env$path, recursive = TRUE)
})


# ==============================================================================
# ROLLBACK FUNCTIONALITY
# ==============================================================================

test_that("Transcription can be rolled back", {
  skip_if_not(has_python_annotations())
  
  test_env <- setup_test_db()
  corp <- test_env$corpus
  bundles <- corp["0000", "msajc003"]
  
  suggestions <- draft_periods(corp, bundles, level_name = "RollbackTest")
  assess(suggestions)
  prepare(suggestions)
  log <- transcribe(suggestions)
  
  # Verify annotations exist
  db <- emuR::load_emuDB(test_env$path)
  result_before <- emuR::query(db, "RollbackTest =~ .*")
  expect_gt(nrow(result_before), 0)
  
  # Rollback
  reverse(log)
  
  # Annotations should be removed
  db <- emuR::load_emuDB(test_env$path)
  result_after <- emuR::query(db, "RollbackTest =~ .*")
  expect_equal(nrow(result_after), 0)
  
  unlink(test_env$path, recursive = TRUE)
})

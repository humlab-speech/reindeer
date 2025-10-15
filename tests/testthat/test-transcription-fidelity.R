# ==============================================================================
# COMPREHENSIVE FIDELITY TESTS FOR TRANSCRIPTION SYSTEM
# ==============================================================================
# This test suite ensures that:
# 1. Transcriptions are applied correctly to annotation levels
# 2. Databases remain loadable by emuR::load_emuDB after transcriptions
# 3. Databases remain queryable by emuR::query after transcriptions
# 4. All emuR::list_* functions work correctly and report consistent results
# ==============================================================================

# Helper function to create test database
create_test_db <- function(suffix = "test") {
  tmp_dir <- tempdir()
  demo_dir <- file.path(tmp_dir, "emuR_demoData")
  
  # Create demo data only once
  if (!dir.exists(demo_dir)) {
    suppressMessages({
      emuR::create_emuRdemoData(dir = tmp_dir)
    })
  }
  
  ae_path <- file.path(demo_dir, "ae_emuDB")
  test_db_name <- paste0("ae_", suffix)
  test_path <- file.path(tmp_dir, paste0(test_db_name, "_emuDB"))
  
  # Copy to test path
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(ae_path, full.names = TRUE), test_path, recursive = TRUE)
  
  # Rename config file to match new database name
  old_config <- file.path(test_path, "ae_DBconfig.json")
  new_config <- file.path(test_path, paste0(test_db_name, "_DBconfig.json"))
  if (file.exists(old_config)) {
    # Read config, update name, write back
    config <- jsonlite::fromJSON(old_config, simplifyVector = FALSE)
    config$name <- test_db_name
    jsonlite::write_json(config, new_config, pretty = TRUE, auto_unbox = TRUE)
    file.remove(old_config)
  }
  
  # Rename cache file if it exists
  old_cache <- file.path(test_path, "ae_emuDBcache.sqlite")
  new_cache <- file.path(test_path, paste0(test_db_name, "_emuDBcache.sqlite"))
  if (file.exists(old_cache)) {
    file.rename(old_cache, new_cache)
  }
  
  return(test_path)
}

test_that("Transcription to existing SEGMENT level maintains emuR compatibility", {
  skip_if_not_installed("emuR")
  
  # Create test database
  test_path <- create_test_db("segment_test")
  
  # Create corpus
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Get original state from emuR
  ae_emur <- emuR::load_emuDB(test_path, verbose = FALSE)
  original_phonetic <- emuR::query(ae_emur, "Phonetic =~ .*")
  original_item_count <- nrow(original_phonetic)
  
  # Create suggestions for new segments (non-overlapping with existing)
  suggestions_df <- data.frame(
    start_time = c(2.5, 2.7, 2.9),
    end_time = c(2.6, 2.8, 3.0),
    label = c("test_a", "test_b", "test_c"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedSegments(
    corpus = ae,
    session = "0000",
    bundle = "msajc003",
    level_name = "Phonetic",
    suggestions = suggestions_df
  )
  
  # Assess and prepare
  sugg <- assess(sugg, verbose = FALSE)
  expect_true(sugg@assessment_results$info$level_exists)
  
  # Apply transcription
  log <- transcribe(sugg, force = TRUE, verbose = FALSE)
  
  # Verify TranscriptionLog
  expect_s3_class(log, "TranscriptionLog")
  expect_true(log@success)
  expect_equal(log@n_items_added, 3)
  expect_equal(log@n_items_modified, 0)
  
  # Test 1: Database should still be loadable by emuR
  ae_emur_after <- NULL
  expect_no_error({
    ae_emur_after <- emuR::load_emuDB(test_path, verbose = FALSE)
  })
  expect_true(!is.null(ae_emur_after))
  
  # Test 2: Query should work and return more items
  new_phonetic <- NULL
  expect_no_error({
    new_phonetic <- emuR::query(ae_emur_after, "Phonetic =~ .*")
  })
  expect_equal(nrow(new_phonetic), original_item_count + 3)
  
  # Test 3: New labels should be queryable
  test_labels <- NULL
  expect_no_error({
    test_labels <- emuR::query(ae_emur_after, "Phonetic =~ test_.*")
  })
  expect_equal(nrow(test_labels), 3)
  expect_true(all(c("test_a", "test_b", "test_c") %in% test_labels$labels))
  
  # Test 4: emuR::list_* functions should work
  expect_no_error({
    levels <- emuR::list_levelDefinitions(ae_emur_after)
    bundles <- emuR::list_bundles(ae_emur_after)
    sessions <- emuR::list_sessions(ae_emur_after)
  })
  
  # Test 5: Corpus object should be consistent
  suppressMessages({
    ae_reload <- corpus(test_path, verbose = FALSE)
  })
  
  # Query using query_opt should also work
  query_result <- query_opt(test_path, "Phonetic =~ test_.*")
  expect_equal(nrow(query_result), 3)
  
  # Clean up
  DBI::dbDisconnect(ae_emur_after$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("Transcription creates new SEGMENT level correctly", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("newseg_test")
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Create suggestions for new level
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.4, 0.7),
    end_time = c(0.3, 0.6, 0.9),
    label = c("seg1", "seg2", "seg3"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedSegments(
    corpus = ae,
    session = "0000",
    bundle = "msajc003",
    level_name = "TestSegmentLevel",
    suggestions = suggestions_df
  )
  
  sugg <- assess(sugg, verbose = FALSE)
  expect_true(sugg@assessment_results$info$level_needs_creation)
  
  # Prepare level
  updated_config <- prepare(sugg, force = TRUE, verbose = FALSE)
  expect_true("TestSegmentLevel" %in% sapply(updated_config$levelDefinitions, function(x) x$name))
  
  # Apply transcription
  log <- transcribe(sugg, force = TRUE, verbose = FALSE)
  expect_true(log@success)
  expect_equal(log@n_items_added, 3)
  
  # Test with emuR
  ae_emur <- emuR::load_emuDB(test_path, verbose = FALSE)
  
  # Level should exist
  levels <- emuR::list_levelDefinitions(ae_emur)
  expect_true("TestSegmentLevel" %in% levels$name)
  
  # Should be queryable
  results <- emuR::query(ae_emur, "TestSegmentLevel =~ .*")
  expect_equal(nrow(results), 3)
  expect_true(all(c("seg1", "seg2", "seg3") %in% results$labels))
  
  # Clean up
  DBI::dbDisconnect(ae_emur$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("Transcription to ITEM level works with emuR", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("item_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Create suggestions for new ITEM level
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5, 1.0),
    end_time = c(0.4, 0.8, 1.3),
    label = c("item1", "item2", "item3"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedItems(
    corpus = ae,
    session = "0000",
    bundle = "msajc003",
    level_name = "TestItemLevel",
    suggestions = suggestions_df
  )
  
  sugg <- assess(sugg, verbose = FALSE)
  
  # Prepare and transcribe
  prepare(sugg, force = TRUE, verbose = FALSE)
  log <- transcribe(sugg, force = TRUE, verbose = FALSE)
  
  expect_true(log@success)
  expect_equal(log@n_items_added, 3)
  
  # Verify with emuR
  ae_emur <- emuR::load_emuDB(test_path, verbose = FALSE)
  
  levels <- emuR::list_levelDefinitions(ae_emur)
  expect_true("TestItemLevel" %in% levels$name)
  
  level_def <- levels[levels$name == "TestItemLevel", ]
  expect_equal(level_def$type, "ITEM")
  
  # Query should work
  results <- emuR::query(ae_emur, "TestItemLevel =~ .*")
  expect_equal(nrow(results), 3)
  
  # Clean up
  DBI::dbDisconnect(ae_emur$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("Transcription to EVENT level works with emuR", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("event_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Create suggestions for new EVENT level
  suggestions_df <- data.frame(
    start_time = c(0.2, 0.6, 1.1),
    end_time = c(0.2, 0.6, 1.1),  # Events have same start/end
    label = c("event1", "event2", "event3"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedEvents(
    corpus = ae,
    session = "0000",
    bundle = "msajc003",
    level_name = "TestEventLevel",
    suggestions = suggestions_df
  )
  
  sugg <- assess(sugg, verbose = FALSE)
  
  # Prepare and transcribe
  prepare(sugg, force = TRUE, verbose = FALSE)
  log <- transcribe(sugg, force = TRUE, verbose = FALSE)
  
  expect_true(log@success)
  expect_equal(log@n_items_added, 3)
  
  # Verify with emuR
  ae_emur <- emuR::load_emuDB(test_path, verbose = FALSE)
  
  levels <- emuR::list_levelDefinitions(ae_emur)
  expect_true("TestEventLevel" %in% levels$name)
  
  level_def <- levels[levels$name == "TestEventLevel", ]
  expect_equal(level_def$type, "EVENT")
  
  # Query should work
  results <- emuR::query(ae_emur, "TestEventLevel =~ .*")
  expect_equal(nrow(results), 3)
  
  # Clean up
  DBI::dbDisconnect(ae_emur$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("Multiple transcription operations maintain consistency", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("multi_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # First transcription: Add SEGMENT level
  seg_df <- data.frame(
    start_time = c(0.1, 0.3),
    end_time = c(0.2, 0.4),
    label = c("s1", "s2"),
    stringsAsFactors = FALSE
  )
  
  seg_sugg <- SuggestedSegments(ae, "0000", "msajc003", "MultiTestSeg", seg_df)
  seg_sugg <- assess(seg_sugg, verbose = FALSE)
  prepare(seg_sugg, force = TRUE, verbose = FALSE)
  log1 <- transcribe(seg_sugg, force = TRUE, verbose = FALSE)
  expect_true(log1@success)
  
  # Reload corpus to get updated config
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Second transcription: Add ITEM level
  item_df <- data.frame(
    start_time = c(0.5, 0.9),
    end_time = c(0.7, 1.1),
    label = c("i1", "i2"),
    stringsAsFactors = FALSE
  )
  
  item_sugg <- SuggestedItems(ae, "0000", "msajc003", "MultiTestItem", item_df)
  item_sugg <- assess(item_sugg, verbose = FALSE)
  prepare(item_sugg, force = TRUE, verbose = FALSE)
  log2 <- transcribe(item_sugg, force = TRUE, verbose = FALSE)
  expect_true(log2@success)
  
  # Reload corpus again
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Third transcription: Add EVENT level
  event_df <- data.frame(
    start_time = c(0.15, 0.45),
    end_time = c(0.15, 0.45),
    label = c("e1", "e2"),
    stringsAsFactors = FALSE
  )
  
  event_sugg <- SuggestedEvents(ae, "0000", "msajc003", "MultiTestEvent", event_df)
  event_sugg <- assess(event_sugg, verbose = FALSE)
  prepare(event_sugg, force = TRUE, verbose = FALSE)
  log3 <- transcribe(event_sugg, force = TRUE, verbose = FALSE)
  expect_true(log3@success)
  
  # Verify all levels with emuR
  ae_emur <- emuR::load_emuDB(test_path, verbose = FALSE)
  
  levels <- emuR::list_levelDefinitions(ae_emur)
  expect_true(all(c("MultiTestSeg", "MultiTestItem", "MultiTestEvent") %in% levels$name))
  
  # All should be queryable
  seg_results <- emuR::query(ae_emur, "MultiTestSeg =~ .*")
  expect_equal(nrow(seg_results), 2)
  
  item_results <- emuR::query(ae_emur, "MultiTestItem =~ .*")
  expect_equal(nrow(item_results), 2)
  
  event_results <- emuR::query(ae_emur, "MultiTestEvent =~ .*")
  expect_equal(nrow(event_results), 2)
  
  # Clean up
  DBI::dbDisconnect(ae_emur$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("TranscriptionLog reverse operation works correctly", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("reverse_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Get initial state
  ae_emur_before <- emuR::load_emuDB(test_path, verbose = FALSE)
  initial_phonetic <- emuR::query(ae_emur_before, "Phonetic =~ .*")
  initial_count <- nrow(initial_phonetic)
  DBI::dbDisconnect(ae_emur_before$connection)
  
  # Apply transcription
  suggestions_df <- data.frame(
    start_time = c(2.1, 2.3),
    end_time = c(2.2, 2.4),
    label = c("rev1", "rev2"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "Phonetic", suggestions_df)
  sugg <- assess(sugg, verbose = FALSE)
  log <- transcribe(sugg, force = TRUE, verbose = FALSE)
  
  expect_true(log@success)
  expect_equal(log@n_items_added, 2)
  
  # Verify additions
  ae_emur_after <- emuR::load_emuDB(test_path, verbose = FALSE)
  after_phonetic <- emuR::query(ae_emur_after, "Phonetic =~ .*")
  expect_equal(nrow(after_phonetic), initial_count + 2)
  DBI::dbDisconnect(ae_emur_after$connection)
  
  # Reverse the operation
  reverse(log, force = TRUE, verbose = FALSE)
  
  # Verify reversal
  ae_emur_reversed <- emuR::load_emuDB(test_path, verbose = FALSE)
  reversed_phonetic <- emuR::query(ae_emur_reversed, "Phonetic =~ .*")
  expect_equal(nrow(reversed_phonetic), initial_count)
  
  # Should not be able to query the reversed items
  rev_items <- emuR::query(ae_emur_reversed, "Phonetic =~ rev.*")
  expect_equal(nrow(rev_items), 0)
  
  # Clean up
  DBI::dbDisconnect(ae_emur_reversed$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("emuR list functions consistency after transcription", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("list_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Get initial state from emuR
  ae_emur_before <- emuR::load_emuDB(test_path, verbose = FALSE)
  levels_before <- emuR::list_levelDefinitions(ae_emur_before)
  bundles_before <- emuR::list_bundles(ae_emur_before)
  sessions_before <- emuR::list_sessions(ae_emur_before)
  DBI::dbDisconnect(ae_emur_before$connection)
  
  # Add new level via transcription
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5),
    end_time = c(0.3, 0.7),
    label = c("test1", "test2"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "ConsistencyTest", suggestions_df)
  sugg <- assess(sugg, verbose = FALSE)
  prepare(sugg, force = TRUE, verbose = FALSE)
  transcribe(sugg, force = TRUE, verbose = FALSE)
  
  # Check consistency with emuR after transcription
  ae_emur_after <- emuR::load_emuDB(test_path, verbose = FALSE)
  
  # list_levelDefinitions should have one more level
  levels_after <- emuR::list_levelDefinitions(ae_emur_after)
  expect_equal(nrow(levels_after), nrow(levels_before) + 1)
  expect_true("ConsistencyTest" %in% levels_after$name)
  
  # list_bundles should be unchanged
  bundles_after <- emuR::list_bundles(ae_emur_after)
  expect_equal(nrow(bundles_after), nrow(bundles_before))
  expect_equal(bundles_after$session, bundles_before$session)
  expect_equal(bundles_after$name, bundles_before$name)
  
  # list_sessions should be unchanged
  sessions_after <- emuR::list_sessions(ae_emur_after)
  expect_equal(nrow(sessions_after), nrow(sessions_before))
  expect_equal(sessions_after$name, sessions_before$name)
  
  # list_ssffTrackDefinitions should be unchanged
  if ("list_ssffTrackDefinitions" %in% ls(getNamespace("emuR"))) {
    tracks_before <- emuR::list_ssffTrackDefinitions(ae_emur_before)
    tracks_after <- emuR::list_ssffTrackDefinitions(ae_emur_after)
    expect_equal(nrow(tracks_after), nrow(tracks_before))
  }
  
  # Clean up
  DBI::dbDisconnect(ae_emur_after$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("Corpus object reflects same state as emuR after transcription", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("corpus_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Apply transcription
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.4),
    end_time = c(0.3, 0.6),
    label = c("c1", "c2"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "CorpusTestLevel", suggestions_df)
  sugg <- assess(sugg, verbose = FALSE)
  prepare(sugg, force = TRUE, verbose = FALSE)
  transcribe(sugg, force = TRUE, verbose = FALSE)
  
  # Reload corpus
  suppressMessages({
    ae_reload <- corpus(test_path, verbose = FALSE)
  })
  
  # Load with emuR
  ae_emur <- emuR::load_emuDB(test_path, verbose = FALSE)
  
  # Compare level definitions
  emur_levels <- emuR::list_levelDefinitions(ae_emur)
  corpus_levels <- ae_reload@config$levelDefinitions
  
  expect_equal(length(corpus_levels), nrow(emur_levels))
  
  corpus_level_names <- sapply(corpus_levels, function(x) x$name)
  expect_true(all(emur_levels$name %in% corpus_level_names))
  expect_true("CorpusTestLevel" %in% corpus_level_names)
  
  # Query results should match
  emur_query <- emuR::query(ae_emur, "CorpusTestLevel =~ .*")
  corpus_query <- query_opt(test_path, "CorpusTestLevel =~ .*")
  
  expect_equal(nrow(emur_query), nrow(corpus_query))
  expect_equal(nrow(emur_query), 2)
  
  # Clean up
  DBI::dbDisconnect(ae_emur$connection)
  unlink(test_path, recursive = TRUE)
})

test_that("Annotation JSON files are valid after transcription", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("json_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Apply transcription
  suggestions_df <- data.frame(
    start_time = c(0.2, 0.6),
    end_time = c(0.4, 0.8),
    label = c("json1", "json2"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "Phonetic", suggestions_df)
  sugg <- assess(sugg, verbose = FALSE)
  transcribe(sugg, force = TRUE, verbose = FALSE)
  
  # Read annotation JSON directly
  annot_path <- file.path(test_path, "0000_ses", "msajc003_bndl", "msajc003_annot.json")
  expect_true(file.exists(annot_path))
  
  # Parse JSON
  annot_json <- NULL
  expect_no_error({
    annot_json <- jsonlite::fromJSON(annot_path, simplifyVector = FALSE)
  })
  
  expect_true(!is.null(annot_json))
  expect_true("levels" %in% names(annot_json))
  
  # Find Phonetic level
  phonetic_level <- NULL
  for (level in annot_json$levels) {
    if (level$name == "Phonetic") {
      phonetic_level <- level
      break
    }
  }
  
  expect_true(!is.null(phonetic_level))
  expect_true("items" %in% names(phonetic_level))
  
  # Check that new items exist
  labels <- sapply(phonetic_level$items, function(x) x$labels[[1]]$value)
  expect_true(all(c("json1", "json2") %in% labels))
  
  # Verify emuR can still load it
  ae_emur <- NULL
  expect_no_error({
    ae_emur <- emuR::load_emuDB(test_path, verbose = FALSE)
  })
  
  expect_true(!is.null(ae_emur))
  
  # Clean up
  if (!is.null(ae_emur)) {
    DBI::dbDisconnect(ae_emur$connection)
  }
  unlink(test_path, recursive = TRUE)
})

test_that("Cache file consistency after transcription", {
  skip_if_not_installed("emuR")
  
  test_path <- create_test_db("cache_test")
  
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path, recursive = TRUE)
  
  suppressMessages({
    ae <- corpus(test_path, verbose = FALSE)
  })
  
  # Get cache path
  db_name <- sub("_emuDB$", "", basename(test_path))
  cache_path <- file.path(test_path, paste0(db_name, "_emuDBcache.sqlite"))
  expect_true(file.exists(cache_path))
  
  # Count items before
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  items_before <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM items WHERE level = 'Phonetic'")$count
  DBI::dbDisconnect(con)
  
  # Apply transcription
  suggestions_df <- data.frame(
    start_time = c(2.2, 2.5),
    end_time = c(2.3, 2.6),
    label = c("cache1", "cache2"),
    stringsAsFactors = FALSE
  )
  
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "Phonetic", suggestions_df)
  sugg <- assess(sugg, verbose = FALSE)
  log <- transcribe(sugg, force = TRUE, verbose = FALSE)
  
  expect_true(log@success)
  
  # Reload corpus (should rebuild cache)
  suppressMessages({
    ae_reload <- corpus(test_path, verbose = FALSE)
  })
  
  # Count items after
  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)
  items_after <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM items WHERE level = 'Phonetic'")$count
  
  expect_equal(items_after, items_before + 2)
  
  # Check that new items are in cache
  new_items <- DBI::dbGetQuery(con, 
    "SELECT l.label FROM labels l 
     JOIN items i ON l.item_id = i.item_id AND l.session = i.session AND l.bundle = i.bundle
     WHERE i.level = 'Phonetic' AND l.label IN ('cache1', 'cache2')")
  
  expect_equal(nrow(new_items), 2)
  expect_true(all(c("cache1", "cache2") %in% new_items$label))
  
  DBI::dbDisconnect(con)
  
  # Clean up
  unlink(test_path, recursive = TRUE)
})

# ==============================================================================
# TEST TRANSCRIPTION SYSTEM
# ==============================================================================

test_that("Suggestion classes can be created", {
  skip_if_not_installed("emuR")
  
  # Create test corpus
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  # Load database
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  # Create simple suggestions data
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5, 1.0),
    end_time = c(0.4, 0.8, 1.3),
    label = c("test1", "test2", "test3")
  )
  
  # Test SuggestedSegments
  seg_sugg <- SuggestedSegments(
    corpus = ae,
    session = "0000",
    bundle = "msajc003",
    level_name = "Phonetic",
    suggestions = suggestions_df
  )
  
  expect_s3_class(seg_sugg, "Suggestion")
  expect_equal(seg_sugg@level_type, "SEGMENT")
  expect_equal(nrow(seg_sugg@suggestions), 3)
  
  # Test SuggestedItems
  item_sugg <- SuggestedItems(
    corpus = ae,
    session = "0000",
    bundle = "msajc003",
    level_name = "Word",
    suggestions = suggestions_df
  )
  
  expect_s3_class(item_sugg, "Suggestion")
  expect_equal(item_sugg@level_type, "ITEM")
  
  # Test SuggestedEvents
  event_sugg <- SuggestedEvents(
    corpus = ae,
    session = "0000",
    bundle = "msajc003",
    level_name = "Tone",
    suggestions = suggestions_df
  )
  
  expect_s3_class(event_sugg, "Suggestion")
  expect_equal(event_sugg@level_type, "EVENT")
})

test_that("Suggestion validation detects errors", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  # Test overlapping suggestions (should error in constructor)
  overlapping_df <- data.frame(
    start_time = c(0.1, 0.3, 0.6),
    end_time = c(0.4, 0.5, 0.9),  # 0.3-0.5 overlaps with 0.1-0.4
    label = c("a", "b", "c")
  )
  
  expect_error(
    SuggestedSegments(ae, "0000", "msajc003", "Phonetic", overlapping_df),
    "Overlapping"
  )
  
  # Test negative duration (should error)
  negative_df <- data.frame(
    start_time = c(0.5),
    end_time = c(0.3),  # end before start
    label = c("a")
  )
  
  expect_error(
    SuggestedSegments(ae, "0000", "msajc003", "Phonetic", negative_df)
  )
})

test_that("assess() performs comprehensive validation", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  # Valid suggestions for existing level
  valid_df <- data.frame(
    start_time = c(0.1, 0.4, 0.7),
    end_time = c(0.3, 0.6, 0.9),
    label = c("test1", "test2", "test3")
  )
  
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "Phonetic", valid_df)
  
  # Run assessment
  sugg <- assess(sugg, verbose = FALSE)
  
  expect_true(sugg@assessed)
  expect_type(sugg@assessment_results, "list")
  expect_true("errors" %in% names(sugg@assessment_results))
  expect_true("warnings" %in% names(sugg@assessment_results))
  expect_true("info" %in% names(sugg@assessment_results))
  
  # Check that existing level is detected
  expect_true(sugg@assessment_results$info$level_exists)
  expect_false(sugg@assessment_results$info$level_needs_creation)
})

test_that("assess() detects non-existent levels", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  valid_df <- data.frame(
    start_time = c(0.1, 0.4),
    end_time = c(0.3, 0.6),
    label = c("a", "b")
  )
  
  # Create suggestion for non-existent level
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "NewLevel", valid_df)
  sugg <- assess(sugg, verbose = FALSE)
  
  expect_true(sugg@assessed)
  expect_true(length(sugg@assessment_results$warnings) > 0)
  expect_true(sugg@assessment_results$info$level_needs_creation)
})

test_that("correct() modifies suggestions", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5, 1.0),
    end_time = c(0.4, 0.8, 1.3),
    label = c("original1", "original2", "original3")
  )
  
  sugg <- SuggestedSegments(ae, "0000", "msajc003", "Phonetic", suggestions_df)
  
  # Correct first suggestion
  sugg_corrected <- correct(sugg, 1, start_time = 0.15, label = "corrected")
  
  expect_equal(sugg_corrected@suggestions$start_time[1], 0.15)
  expect_equal(sugg_corrected@suggestions$label[1], "corrected")
  expect_false(sugg_corrected@assessed)  # Should need reassessment
  
  # Correct multiple suggestions
  sugg_multi <- correct(sugg, c(1, 3), label = c("new1", "new3"))
  
  expect_equal(sugg_multi@suggestions$label[1], "new1")
  expect_equal(sugg_multi@suggestions$label[3], "new3")
  expect_equal(sugg_multi@suggestions$label[2], "original2")  # Unchanged
})

test_that("draft() creates suggestions from annotation function", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  # Mock annotation function
  mock_annotator <- function(corpus, session, bundle, ...) {
    data.frame(
      start_time = c(0.1, 0.5),
      end_time = c(0.3, 0.7),
      label = c("auto1", "auto2")
    )
  }
  
  # Create draft
  sugg <- draft(
    corpus = ae,
    annotation_func = mock_annotator,
    session = "0000",
    bundle = "msajc003",
    level_name = "TestLevel",
    level_type = "SEGMENT"
  )
  
  expect_s3_class(sugg, "Suggestion")
  expect_equal(sugg@session, "0000")
  expect_equal(sugg@bundle, "msajc003")
  expect_equal(sugg@level_name, "TestLevel")
  expect_equal(nrow(sugg@suggestions), 2)
})

test_that("TranscriptionLog tracks operations", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  # Create log
  log <- TranscriptionLog(ae, "0000", "msajc003", "Phonetic")
  
  expect_s3_class(log, "TranscriptionLog")
  expect_equal(log@session, "0000")
  expect_equal(log@bundle, "msajc003")
  expect_equal(log@level_name, "Phonetic")
  expect_false(log@success)  # Initially not successful
  
  # Check timestamp
  expect_true(inherits(log@timestamp, "POSIXct"))
})

test_that("prepare() updates configuration for new levels", {
  skip_if_not_installed("emuR")
  
  # Use temporary directory for this test
  tmp_dir <- tempdir()
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  test_path <- file.path(tmp_dir, "ae_test_emuDB")
  
  # Copy database
  if (dir.exists(test_path)) {
    unlink(test_path, recursive = TRUE)
  }
  dir.create(test_path)
  file.copy(list.files(ae_path, full.names = TRUE), test_path, recursive = TRUE)
  
  suppressMessages({
    ae_test <- corpus(test_path, verbose = FALSE)
  })
  
  # Create suggestion for new level
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5),
    end_time = c(0.3, 0.7),
    label = c("a", "b")
  )
  
  sugg <- SuggestedSegments(ae_test, "0000", "msajc003", "NewTestLevel", suggestions_df)
  sugg <- assess(sugg, verbose = FALSE)
  
  expect_true(sugg@assessment_results$info$level_needs_creation)
  
  # Prepare with force=TRUE to skip prompts
  updated_config <- prepare(sugg, force = TRUE, verbose = FALSE)
  
  # Check that level was added to config
  level_names <- sapply(updated_config$levelDefinitions, function(x) x$name)
  expect_true("NewTestLevel" %in% level_names)
  
  # Clean up
  unlink(test_path, recursive = TRUE)
})

test_that("Minimum duration filtering works", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  # Mix of short and long suggestions
  mixed_df <- data.frame(
    start_time = c(0.1, 0.2, 0.5),
    end_time = c(0.105, 0.3, 0.6),  # First is only 5ms
    label = c("short", "long", "long")
  )
  
  sugg <- SuggestedSegments(
    ae, "0000", "msajc003", "Phonetic", mixed_df,
    min_duration = 0.01  # 10ms minimum
  )
  
  sugg <- assess(sugg, verbose = FALSE)
  
  # Should warn about short suggestion
  expect_true(length(sugg@assessment_results$warnings) > 0)
  expect_true(any(grepl("shorter than minimum", sugg@assessment_results$warnings)))
})

test_that("Print methods work for all classes", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5),
    end_time = c(0.3, 0.7),
    label = c("a", "b")
  )
  
  # Test print for SuggestedSegments
  seg_sugg <- SuggestedSegments(ae, "0000", "msajc003", "Phonetic", suggestions_df)
  expect_output(print(seg_sugg), "SuggestedSegments")
  
  # Test print for TranscriptionLog
  log <- TranscriptionLog(ae, "0000", "msajc003", "Phonetic")
  expect_output(summary(log), "Transcription Log")
})

test_that("Empty suggestions are handled correctly", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  # Empty suggestions
  empty_df <- data.frame(
    start_time = numeric(0),
    end_time = numeric(0),
    label = character(0)
  )
  
  sugg <- SuggestedSegments(
    ae, "0000", "msajc003", "Phonetic", empty_df,
    remove_empty = TRUE
  )
  
  sugg <- assess(sugg, verbose = FALSE)
  
  expect_true(sugg@assessment_results$info$empty_suggestions)
  expect_true(length(sugg@assessment_results$warnings) > 0)
})

test_that("Confidence scores work for items", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5, 1.0),
    end_time = c(0.4, 0.8, 1.3),
    label = c("word1", "word2", "word3")
  )
  
  confidence <- c(0.95, 0.87, 0.92)
  
  item_sugg <- SuggestedItems(
    ae, "0000", "msajc003", "Word", suggestions_df,
    confidence_scores = confidence
  )
  
  expect_equal(item_sugg@confidence_scores, confidence)
  expect_true("confidence" %in% names(item_sugg@suggestions))
  expect_equal(item_sugg@suggestions$confidence, confidence)
})

test_that("Phonetic alphabet is tracked for segments", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5),
    end_time = c(0.3, 0.7),
    label = c("ɑ", "i")
  )
  
  seg_sugg <- SuggestedSegments(
    ae, "0000", "msajc003", "Phonetic", suggestions_df,
    phonetic_alphabet = "IPA"
  )
  
  expect_equal(seg_sugg@phonetic_alphabet, "IPA")
  
  # Test with X-SAMPA
  seg_sugg_xsampa <- SuggestedSegments(
    ae, "0000", "msajc003", "Phonetic", suggestions_df,
    phonetic_alphabet = "X-SAMPA"
  )
  
  expect_equal(seg_sugg_xsampa@phonetic_alphabet, "X-SAMPA")
})

test_that("Event categories are tracked", {
  skip_if_not_installed("emuR")
  
  ae_path <- system.file("extdata/emu/DBs/ae", package = "emuR", mustWork = TRUE)
  
  suppressMessages({
    ae <- corpus(ae_path, verbose = FALSE)
  })
  
  suggestions_df <- data.frame(
    start_time = c(0.1, 0.5, 0.9),
    end_time = c(0.1, 0.5, 0.9),  # Events have same start/end
    label = c("H*", "L-", "H%")
  )
  
  event_sugg <- SuggestedEvents(
    ae, "0000", "msajc003", "Tone", suggestions_df,
    event_categories = c("pitch_accent", "phrase_accent", "boundary_tone")
  )
  
  expect_equal(length(event_sugg@event_categories), 3)
  expect_true("pitch_accent" %in% event_sugg@event_categories)
})

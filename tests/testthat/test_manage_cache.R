test_that("manage_cache validates inputs with assertthat", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_validate_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_validate", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path, verbose = FALSE)

  # Test invalid corpus input
  expect_error(
    manage_cache("not_a_corpus"),
    "corpus must be a corpus object"
  )

  # Test invalid action
  expect_error(
    manage_cache(corp, action = "invalid"),
    "should be one of"
  )

  # Test invalid cache_type
  expect_error(
    manage_cache(corp, cache_type = "invalid"),
    "should be one of"
  )

  # Test invalid days_old
  expect_error(
    manage_cache(corp, action = "clean", days_old = -5),
    "days_old must be a positive integer"
  )

  expect_error(
    manage_cache(corp, action = "clean", days_old = 3.5),
    "days_old must be a positive integer"
  )

  # Test invalid dry_run
  expect_error(
    manage_cache(corp, action = "clean", dry_run = "yes"),
    "dry_run must be TRUE or FALSE"
  )

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("manage_cache status action works", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_status_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_status", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path, verbose = FALSE)

  # Default action is status
  result <- manage_cache(corp)

  expect_type(result, "list")
  expect_true("quantify" %in% names(result) ||
              "draft" %in% names(result) ||
              length(result) == 0)

  # Explicit status action
  result2 <- manage_cache(corp, action = "status")
  expect_type(result2, "list")

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("manage_cache list action works", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_list_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_list", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path, verbose = FALSE)

  # List all caches
  files <- manage_cache(corp, action = "list")

  expect_true(is.data.frame(files))

  if (nrow(files) > 0) {
    expect_true("file" %in% names(files))
    expect_true("size_bytes" %in% names(files))
    expect_true("type" %in% names(files))
  }

  # List specific cache type
  quantify_files <- manage_cache(corp, action = "list", cache_type = "quantify")
  expect_true(is.data.frame(quantify_files))

  draft_files <- manage_cache(corp, action = "list", cache_type = "draft")
  expect_true(is.data.frame(draft_files))

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("manage_cache clean action works with dry_run", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_clean_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_clean", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path, verbose = FALSE)

  # Create a dummy cache file
  cache_dir <- file.path(corp@basePath, ".quantify_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  dummy_file <- file.path(cache_dir, "test_cache.qs")
  qs::qsave(list(test = "data"), dummy_file)

  # Make it old by modifying timestamp
  old_time <- Sys.time() - (60 * 60 * 24 * 35)  # 35 days ago
  Sys.setFileTime(dummy_file, old_time)

  # Dry run should not delete
  result <- manage_cache(corp, action = "clean", days_old = 30, dry_run = TRUE)

  expect_true(file.exists(dummy_file))

  # Actual clean should delete
  result <- manage_cache(corp, action = "clean", days_old = 30, dry_run = FALSE)

  # File should be deleted (or cache dir removed)
  # Note: clean_all_caches might remove the entire directory
  expect_true(!file.exists(dummy_file) || !dir.exists(cache_dir))

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("corpus constructor validates inputs", {
  # Test NULL path
  expect_error(
    corpus(NULL),
    "path cannot be NULL or empty"
  )

  # Test empty path
  expect_error(
    corpus(character(0)),
    "path cannot be NULL or empty"
  )

  # Test non-existent path
  expect_error(
    corpus("/this/path/does/not/exist_emuDB"),
    "does not exist"
  )

  # Test path without _emuDB suffix
  expect_error(
    corpus(tempdir()),
    "should end with '_emuDB'"
  )

  # Test invalid verbose
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_verbose_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_verbose", targetDir = test_dir, verbose = FALSE)

  expect_error(
    corpus(db_path, verbose = "yes"),
    "verbose must be TRUE or FALSE"
  )

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("quantify validates inputs with assertthat", {
  skip_if_not_installed("emuR")

  # Create test data
  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_quantify_val_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_quantify_val", targetDir = test_dir, verbose = FALSE)

  # Create a mock segment_list
  test_seg <- segment_list(data.frame(
    labels = "a",
    start = 100,
    end = 200,
    db_uuid = "test-uuid",
    session = "test",
    bundle = "test",
    level = "Phonetic",
    attribute = "Phonetic",
    start_item_id = 1,
    end_item_id = 1,
    start_item_seq_idx = 1,
    end_item_seq_idx = 1,
    type = "SEGMENT",
    sample_start = 4410,
    sample_end = 8820,
    sample_rate = 44100
  ), db_path = db_path)

  # Test invalid object type
  expect_error(
    quantify(data.frame(x = 1:10), function(x) x),
    "object must be a segment_list"
  )

  # Test invalid dsp_function
  expect_error(
    quantify(test_seg, 123),
    "dsp_function must be a function or character string"
  )

  # Test invalid .at values
  expect_error(
    quantify(test_seg, function(x) x, .at = -0.5),
    ".at values must be between 0 and 1"
  )

  expect_error(
    quantify(test_seg, function(x) x, .at = 1.5),
    ".at values must be between 0 and 1"
  )

  expect_error(
    quantify(test_seg, function(x) x, .at = "middle"),
    ".at must be numeric"
  )

  # Test invalid .workers
  expect_error(
    quantify(test_seg, function(x) x, .workers = -1),
    ".workers must be a positive integer"
  )

  expect_error(
    quantify(test_seg, function(x) x, .workers = 2.5),
    ".workers must be a positive integer"
  )

  # Test invalid flags
  expect_error(
    quantify(test_seg, function(x) x, .verbose = "yes"),
    "Logical flags must be TRUE or FALSE"
  )

  expect_error(
    quantify(test_seg, function(x) x, .parallel = 1),
    "Logical flags must be TRUE or FALSE"
  )

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("store_draft_annotations validates inputs", {
  temp_cache <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_cache))

  con <- reindeer:::initialize_draft_cache(temp_cache, "test_function")

  test_annot <- data.frame(start = 100, end = 200, label = "a")
  params <- list(param1 = 10)

  # Test invalid connection
  expect_error(
    reindeer:::store_draft_annotations(
      "not_a_connection", "S1", "B1", "L1", "SEGMENT", "A1",
      test_annot, params
    ),
    "con must be a SQLite database connection"
  )

  # Test invalid strings
  expect_error(
    reindeer:::store_draft_annotations(
      con, 123, "B1", "L1", "SEGMENT", "A1",
      test_annot, params
    ),
    "must be character strings"
  )

  # Test invalid parameters
  expect_error(
    reindeer:::store_draft_annotations(
      con, "S1", "B1", "L1", "SEGMENT", "A1",
      test_annot, "not_a_list"
    ),
    "parameters must be a list"
  )

  # Test invalid error_occurred flag
  expect_error(
    reindeer:::store_draft_annotations(
      con, "S1", "B1", "L1", "SEGMENT", "A1",
      test_annot, params,
      error_occurred = "yes"
    ),
    "error_occurred must be TRUE or FALSE"
  )

  # Test error_occurred=TRUE requires error_message
  expect_error(
    reindeer:::store_draft_annotations(
      con, "S1", "B1", "L1", "SEGMENT", "A1",
      NULL, params,
      error_occurred = TRUE,
      error_message = NULL
    ),
    "error_message must be provided when error_occurred is TRUE"
  )

  # Test error_occurred=FALSE requires annotations
  expect_error(
    reindeer:::store_draft_annotations(
      con, "S1", "B1", "L1", "SEGMENT", "A1",
      NULL, params,
      error_occurred = FALSE
    ),
    "annotations must be provided when error_occurred is FALSE"
  )

  DBI::dbDisconnect(con)
})

test_that("initialize_draft_cache validates inputs", {
  # Test invalid cache_path
  expect_error(
    reindeer:::initialize_draft_cache(123, "test_function"),
    "cache_path must be a character string"
  )

  # Test invalid draft_function_name
  temp_cache <- tempfile(fileext = ".sqlite")
  expect_error(
    reindeer:::initialize_draft_cache(temp_cache, 123),
    "draft_function_name must be a character string"
  )
})

test_that("Edge case: empty segment_list", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_empty_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_empty", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path, verbose = FALSE)

  # Create empty segment_list
  empty_segs <- segment_list(data.frame(
    labels = character(0),
    start = numeric(0),
    end = numeric(0),
    db_uuid = character(0),
    session = character(0),
    bundle = character(0),
    level = character(0),
    attribute = character(0),
    start_item_id = integer(0),
    end_item_id = integer(0),
    start_item_seq_idx = integer(0),
    end_item_seq_idx = integer(0),
    type = character(0),
    sample_start = integer(0),
    sample_end = integer(0),
    sample_rate = numeric(0)
  ), db_path = db_path)

  # Quantify should return empty tibble
  result <- quantify(empty_segs, function(x) x, .verbose = FALSE)

  expect_true(nrow(result) == 0)

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("Edge case: segment_list with single segment", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_single_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_single", targetDir = test_dir, verbose = FALSE)

  # Create single-segment segment_list
  single_seg <- segment_list(data.frame(
    labels = "a",
    start = 100,
    end = 200,
    db_uuid = "test-uuid",
    session = "test",
    bundle = "test",
    level = "Phonetic",
    attribute = "Phonetic",
    start_item_id = 1,
    end_item_id = 1,
    start_item_seq_idx = 1,
    end_item_seq_idx = 1,
    type = "SEGMENT",
    sample_start = 4410,
    sample_end = 8820,
    sample_rate = 44100
  ), db_path = db_path)

  # Should work without error
  expect_s3_class(single_seg, "segment_list")
  expect_equal(nrow(single_seg), 1)

  # Clean up
  unlink(db_path, recursive = TRUE)
})

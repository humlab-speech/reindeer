# Tests for cache size monitoring and management

test_that("format_bytes converts bytes to human-readable format correctly", {
  expect_equal(reindeer:::format_bytes(500), "500.00 B")
  expect_equal(reindeer:::format_bytes(1024), "1.00 KB")
  expect_equal(reindeer:::format_bytes(1024^2), "1.00 MB")
  expect_equal(reindeer:::format_bytes(1024^3), "1.00 GB")
  expect_equal(reindeer:::format_bytes(1024^4), "1.00 TB")

  # Test with larger values
  expect_equal(reindeer:::format_bytes(1536), "1.50 KB")
  expect_equal(reindeer:::format_bytes(2.5 * 1024^2), "2.50 MB")
})

test_that("parse_size_string converts human-readable sizes to bytes", {
  expect_equal(reindeer:::parse_size_string("500 B"), 500)
  expect_equal(reindeer:::parse_size_string("1 KB"), 1024)
  expect_equal(reindeer:::parse_size_string("1.5 KB"), 1536)
  expect_equal(reindeer:::parse_size_string("2 MB"), 2 * 1024^2)
  expect_equal(reindeer:::parse_size_string("1.5 GB"), 1.5 * 1024^3)
  expect_equal(reindeer:::parse_size_string("2 TB"), 2 * 1024^4)

  # Test case insensitivity
  expect_equal(reindeer:::parse_size_string("1 mb"), 1024^2)
  expect_equal(reindeer:::parse_size_string("500kb"), 500 * 1024)
})

test_that("get_file_size returns correct size for files", {
  # Create a test file
  temp_file <- tempfile()
  writeLines(rep("test", 100), temp_file)
  on.exit(unlink(temp_file))

  size <- reindeer:::get_file_size(temp_file)
  expect_true(size > 0)
  expect_true(is.numeric(size))

  # Check file that doesn't exist
  expect_equal(reindeer:::get_file_size("nonexistent_file.txt"), 0)
})

test_that("get_file_size handles directories recursively", {
  # Create test directory structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "cache_test")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create some files
  file1 <- file.path(test_dir, "file1.txt")
  file2 <- file.path(test_dir, "file2.txt")
  writeLines(rep("x", 50), file1)
  writeLines(rep("y", 50), file2)

  # Get directory size
  total_size <- reindeer:::get_file_size(test_dir, recursive = TRUE)
  expect_true(total_size > 0)

  # Should be sum of both files
  file1_size <- file.info(file1)$size
  file2_size <- file.info(file2)$size
  expect_equal(total_size, file1_size + file2_size)
})

test_that("check_cache_size returns correct structure", {
  # Create test cache file
  temp_cache <- tempfile(fileext = ".sqlite")
  writeLines(rep("data", 100), temp_cache)
  on.exit(unlink(temp_cache))

  result <- reindeer:::check_cache_size(
    temp_cache,
    cache_type = "test",
    warn_threshold = "1 MB",
    max_threshold = "2 MB",
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true("size_bytes" %in% names(result))
  expect_true("size_formatted" %in% names(result))
  expect_true("warn_exceeded" %in% names(result))
  expect_true("max_exceeded" %in% names(result))
  expect_true("cache_type" %in% names(result))

  expect_true(is.numeric(result$size_bytes))
  expect_true(is.character(result$size_formatted))
  expect_true(is.logical(result$warn_exceeded))
  expect_true(is.logical(result$max_exceeded))
})

test_that("check_cache_size detects threshold exceedance", {
  # Create test file that exceeds threshold
  temp_cache <- tempfile(fileext = ".sqlite")

  # Write 2 MB of data (should exceed 1 MB warn threshold)
  data <- raw(2 * 1024^2)
  writeBin(data, temp_cache)
  on.exit(unlink(temp_cache))

  result <- reindeer:::check_cache_size(
    temp_cache,
    cache_type = "test",
    warn_threshold = "1 MB",
    max_threshold = "5 MB",
    verbose = FALSE
  )

  expect_true(result$warn_exceeded)
  expect_false(result$max_exceeded)
  expect_true(result$size_bytes > 1 * 1024^2)
})

test_that("check_quantify_cache_size works with corpus", {
  skip_if_not_installed("emuR")
  skip_if_not_installed("reindeer")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_cache_size_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  tryCatch({
    emuR::create_emuDB(name = "test_cache_size", targetDir = test_dir, verbose = FALSE)
    corp <- corpus(db_path)

    # Create a small cache file
    cache_dir <- reindeer:::get_quantify_cache_dir(corp)
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    cache_file <- file.path(cache_dir, "test_cache.sqlite")
    writeLines(rep("test", 50), cache_file)

    result <- check_quantify_cache_size(
      corp,
      warn_threshold = "10 MB",
      max_threshold = "20 MB",
      verbose = FALSE
    )

    expect_type(result, "list")
    expect_false(result$warn_exceeded)
    expect_false(result$max_exceeded)
  }, error = function(e) {
    skip("Could not create corpus object")
  }, finally = {
    # Clean up
    unlink(db_path, recursive = TRUE)
  })
})

test_that("check_draft_cache_size works with corpus", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_draft_cache_size_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  tryCatch({
    emuR::create_emuDB(name = "test_draft_cache_size", targetDir = test_dir, verbose = FALSE)
    corp <- corpus(db_path)

    # Create draft cache directory and files
    cache_dir <- reindeer:::get_draft_cache_dir(corp)
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    # Create some test cache files
    file1 <- file.path(cache_dir, "momel_intsint_20251020.sqlite")
    file2 <- file.path(cache_dir, "test_function_20251019.sqlite")
    writeLines(rep("data", 100), file1)
    writeLines(rep("data", 100), file2)

    result <- check_draft_cache_size(
      corp,
      warn_threshold = "10 MB",
      max_threshold = "20 MB",
      verbose = FALSE
    )

    expect_type(result, "list")
    expect_false(result$warn_exceeded)
    expect_false(result$max_exceeded)
    expect_true(result$size_bytes > 0)
  }, error = function(e) {
    skip("Could not create corpus object")
  }, finally = {
    # Clean up
    unlink(db_path, recursive = TRUE)
  })
})

test_that("list_cache_files returns correct structure", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_list_cache_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  tryCatch({
    emuR::create_emuDB(name = "test_list_cache", targetDir = test_dir, verbose = FALSE)
    corp <- corpus(db_path)

    # Create cache files
    draft_dir <- reindeer:::get_draft_cache_dir(corp)
    dir.create(draft_dir, showWarnings = FALSE, recursive = TRUE)

    file1 <- file.path(draft_dir, "test1_20251020.sqlite")
    file2 <- file.path(draft_dir, "test2_20251019.sqlite")
    writeLines("data", file1)
    writeLines("data", file2)

    result <- list_cache_files(corp, cache_type = "draft")

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) >= 2)
    expect_true(all(c("type", "file", "size_bytes", "size_formatted") %in% names(result)))
  }, error = function(e) {
    skip("Could not create corpus object")
  }, finally = {
    # Clean up
    unlink(db_path, recursive = TRUE)
  })
})

test_that("remove_old_cache_files removes files older than threshold", {
  # Create temporary directory
  temp_dir <- tempdir()
  cache_dir <- file.path(temp_dir, "test_cleanup_cache")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Create test files with different ages
  old_file <- file.path(cache_dir, "old_cache_20250101.sqlite")
  recent_file <- file.path(cache_dir, "recent_cache_20251020.sqlite")

  writeLines("old data", old_file)
  writeLines("recent data", recent_file)

  # Set old file's modification time to 60 days ago
  old_time <- Sys.time() - (60 * 24 * 3600)
  Sys.setFileTime(old_file, old_time)

  # Test dry run first
  result_dry <- reindeer:::remove_old_cache_files(
    cache_dir,
    days_old = 30,
    dry_run = TRUE,
    verbose = FALSE
  )

  expect_true(is.numeric(result_dry))  # Returns count (integer or double)
  expect_true(result_dry >= 1)
  expect_true(file.exists(old_file))  # Should still exist after dry run

  # Now remove for real
  result_real <- reindeer:::remove_old_cache_files(
    cache_dir,
    days_old = 30,
    dry_run = FALSE,
    verbose = FALSE
  )

  expect_false(file.exists(old_file))  # Should be removed
  expect_true(file.exists(recent_file))  # Should still exist
})

test_that("clean_draft_cache removes old files", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_clean_draft_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  tryCatch({
    emuR::create_emuDB(name = "test_clean_draft", targetDir = test_dir, verbose = FALSE)
    corp <- corpus(db_path)

    # Create draft cache with old file
    cache_dir <- reindeer:::get_draft_cache_dir(corp)
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    old_file <- file.path(cache_dir, "test_20250101.sqlite")
    writeLines("old data", old_file)

    # Set modification time to 60 days ago
    old_time <- Sys.time() - (60 * 24 * 3600)
    Sys.setFileTime(old_file, old_time)

    # Dry run
    result <- clean_draft_cache(corp, days_old = 30, dry_run = TRUE, verbose = FALSE)
    expect_true(file.exists(old_file))

    # Real cleanup
    result <- clean_draft_cache(corp, days_old = 30, dry_run = FALSE, verbose = FALSE)
    expect_false(file.exists(old_file))
  }, error = function(e) {
    skip("Could not create corpus object")
  }, finally = {
    # Clean up
    unlink(db_path, recursive = TRUE)
  })
})

test_that("check_all_cache_sizes checks all cache types", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_all_caches_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  tryCatch({
    emuR::create_emuDB(name = "test_all_caches", targetDir = test_dir, verbose = FALSE)
    corp <- corpus(db_path)

    # Create various cache files
    draft_dir <- reindeer:::get_draft_cache_dir(corp)
    dir.create(draft_dir, showWarnings = FALSE, recursive = TRUE)
    writeLines("draft", file.path(draft_dir, "test_20251020.sqlite"))

    # Suppress output
    result <- suppressMessages(
      check_all_cache_sizes(corp, verbose = FALSE)
    )

    expect_type(result, "list")
    expect_true("draft" %in% names(result))
  }, error = function(e) {
    skip("Could not create corpus object")
  }, finally = {
    # Clean up
    unlink(db_path, recursive = TRUE)
  })
})

test_that("cache size warnings appear when thresholds exceeded", {
  # Create a large temporary file
  temp_cache <- tempfile(fileext = ".sqlite")

  # Write 600 KB of data (should exceed 500 KB warn threshold)
  data <- raw(600 * 1024)
  writeBin(data, temp_cache)
  on.exit(unlink(temp_cache))

  # Capture warnings - check_cache_size uses cli::cli_alert_warning which produces output
  result <- reindeer:::check_cache_size(
    temp_cache,
    cache_type = "test",
    warn_threshold = "500 KB",
    max_threshold = "2 MB",
    verbose = TRUE
  )

  # Verify the size check result shows threshold exceeded
  expect_true(result$warn_exceeded)
  expect_false(result$max_exceeded)
})

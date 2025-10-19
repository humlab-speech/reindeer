# Tests for cache serialization functionality

test_that("qs serialization preserves data integrity", {
  skip_if_not_installed("qs")
  
  # Create test data similar to quantify results
  test_data <- data.frame(
    labels = c("a", "e", "i"),
    start = c(100, 200, 300),
    end = c(150, 250, 350),
    F1 = c(700, 500, 300),
    F2 = c(1200, 1800, 2300),
    stringsAsFactors = FALSE
  )
  
  # Serialize with qs
  blob <- qs::qserialize(test_data, preset = "fast")
  
  # Deserialize
  result <- qs::qdeserialize(blob)
  
  # Check integrity
  expect_equal(result, test_data)
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 5)
})

test_that("qs provides better compression than serialize", {
  skip_if_not_installed("qs")
  
  # Create larger test data
  test_data <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000),
    label = sample(letters, 1000, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Compare sizes
  blob_base <- serialize(test_data, NULL)
  blob_qs <- qs::qserialize(test_data, preset = "fast")
  
  # qs should be smaller (typically 20-40% smaller)
  expect_lt(length(blob_qs), length(blob_base))
  
  # Verify compression ratio is reasonable
  ratio <- length(blob_qs) / length(blob_base)
  expect_lt(ratio, 0.95)  # At least 5% savings
})

test_that("cache schema includes format column", {
  # Create temporary cache
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Initialize cache connection
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Check schema
  columns <- DBI::dbListFields(conn, "cache")
  expect_true("format" %in% columns)
  expect_true("result_blob" %in% columns)
  expect_true("cache_key" %in% columns)
})

test_that("cache stores and retrieves qs format correctly", {
  skip_if_not_installed("qs")
  
  # Create temporary cache
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Initialize cache connection
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Test data
  test_data <- list(x = 1:10, y = letters[1:10])
  cache_key <- "test_key_1"
  
  # Store with qs format
  reindeer:::.set_persistent_cache(cache_key, test_data, conn, format = "qs")
  
  # Retrieve
  result <- reindeer:::.get_persistent_cache(cache_key, conn)
  
  # Verify
  expect_equal(result, test_data)
  
  # Check format in database
  format_check <- DBI::dbGetQuery(conn, 
    "SELECT format FROM cache WHERE cache_key = ?",
    params = list(cache_key))
  expect_equal(format_check$format, "qs")
})

test_that("cache stores and retrieves rds format correctly", {
  # Create temporary cache
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Initialize cache connection
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Test data
  test_data <- list(x = 1:10, y = letters[1:10])
  cache_key <- "test_key_2"
  
  # Store with rds format
  reindeer:::.set_persistent_cache(cache_key, test_data, conn, format = "rds")
  
  # Retrieve
  result <- reindeer:::.get_persistent_cache(cache_key, conn)
  
  # Verify
  expect_equal(result, test_data)
  
  # Check format in database
  format_check <- DBI::dbGetQuery(conn, 
    "SELECT format FROM cache WHERE cache_key = ?",
    params = list(cache_key))
  expect_equal(format_check$format, "rds")
})

test_that("cache handles mixed formats (backward compatibility)", {
  skip_if_not_installed("qs")
  
  # Create temporary cache
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Initialize cache connection
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Store with different formats
  test_data_1 <- list(x = 1:5, method = "rds")
  test_data_2 <- list(x = 6:10, method = "qs")
  
  reindeer:::.set_persistent_cache("key_rds", test_data_1, conn, format = "rds")
  reindeer:::.set_persistent_cache("key_qs", test_data_2, conn, format = "qs")
  
  # Retrieve both
  result_1 <- reindeer:::.get_persistent_cache("key_rds", conn)
  result_2 <- reindeer:::.get_persistent_cache("key_qs", conn)
  
  # Verify both work
  expect_equal(result_1, test_data_1)
  expect_equal(result_2, test_data_2)
})

test_that("auto format selection works correctly", {
  # Create temporary cache
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Initialize cache connection
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Test data
  test_data <- data.frame(x = 1:10)
  cache_key <- "test_auto"
  
  # Store with auto format
  reindeer:::.set_persistent_cache(cache_key, test_data, conn, format = "auto")
  
  # Retrieve
  result <- reindeer:::.get_persistent_cache(cache_key, conn)
  
  # Verify
  expect_equal(result, test_data)
  
  # Check that format was selected (qs if available, rds otherwise)
  format_check <- DBI::dbGetQuery(conn, 
    "SELECT format FROM cache WHERE cache_key = ?",
    params = list(cache_key))
  
  if (requireNamespace("qs", quietly = TRUE)) {
    expect_equal(format_check$format, "qs")
  } else {
    expect_equal(format_check$format, "rds")
  }
})

test_that("cache migration adds format column to existing cache", {
  # Create temporary cache with old schema (no format column)
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  cache_file <- file.path(temp_dir, "quantify_cache.sqlite")
  conn_old <- DBI::dbConnect(RSQLite::SQLite(), cache_file)
  
  # Create old schema without format column
  DBI::dbExecute(conn_old, "
    CREATE TABLE cache (
      cache_key TEXT PRIMARY KEY,
      result_blob BLOB,
      created_at INTEGER,
      accessed_at INTEGER,
      size_bytes INTEGER
    )
  ")
  
  DBI::dbDisconnect(conn_old)
  
  # Now use our connection function which should migrate
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Check that format column was added
  columns <- DBI::dbListFields(conn, "cache")
  expect_true("format" %in% columns)
})

test_that("cache fallback works when deserialization fails", {
  skip_if_not_installed("qs")
  
  # Create temporary cache
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Initialize cache connection
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Store valid data with qs
  test_data <- list(x = 1:5)
  reindeer:::.set_persistent_cache("valid_key", test_data, conn, format = "qs")
  
  # Manually corrupt the format marker (but keep valid data)
  # This tests the fallback mechanism
  DBI::dbExecute(conn,
    "UPDATE cache SET format = 'rds' WHERE cache_key = 'valid_key'")
  
  # Should still retrieve successfully via fallback
  result <- reindeer:::.get_persistent_cache("valid_key", conn)
  expect_equal(result, test_data)
})

test_that("cache_summary function works", {
  skip_if_not_installed("qs")
  
  # Create temporary cache with some entries
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Add some test entries
  for (i in 1:5) {
    test_data <- list(x = 1:10, id = i)
    format <- if (i %% 2 == 0) "qs" else "rds"
    reindeer:::.set_persistent_cache(paste0("key_", i), test_data, conn, format = format)
  }
  
  # Get summary (suppressing output)
  result <- suppressMessages(cache_summary(temp_dir))
  
  # Check result structure
  expect_type(result, "list")
  expect_true("total_entries" %in% names(result))
  expect_equal(result$total_entries, 5)
  expect_true("format_distribution" %in% names(result))
})

test_that("clear_cache function works", {
  # Create temporary cache with entries
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  conn <- reindeer:::.get_persistent_cache_connection(temp_dir)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Add test entries
  for (i in 1:3) {
    test_data <- list(x = i)
    reindeer:::.set_persistent_cache(paste0("key_", i), test_data, conn)
  }
  
  # Clear cache
  n_cleared <- suppressMessages(clear_cache(temp_dir))
  
  # Check all entries removed
  expect_equal(n_cleared, 3)
  
  count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as n FROM cache")$n
  expect_equal(count, 0)
})

# Tests for draft annotation cache system

test_that("get_draft_cache_filename removes draft_ prefix and formats correctly", {
  expect_equal(
    reindeer:::get_draft_cache_filename("draft_momel_intsint", as.Date("2025-10-20")),
    "momel_intsint_20251020.sqlite"
  )

  expect_equal(
    reindeer:::get_draft_cache_filename("momel_intsint", as.Date("2025-10-19")),
    "momel_intsint_20251019.sqlite"
  )

  expect_equal(
    reindeer:::get_draft_cache_filename("draft_test_function"),
    sprintf("test_function_%s.sqlite", format(Sys.Date(), "%Y%m%d"))
  )
})

test_that("draft cache directory is created", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_db_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  # Create minimal test corpus
  emuR::create_emuDB(
    name = "test_db",
    targetDir = test_dir,
    verbose = FALSE
  )

  corp <- corpus(db_path)

  cache_dir <- reindeer:::get_draft_cache_dir(corp)

  expect_true(dir.exists(cache_dir))
  expect_true(grepl("\\.draft_cache$", cache_dir))

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("initialize_draft_cache creates proper schema", {
  temp_cache <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_cache))

  con <- reindeer:::initialize_draft_cache(temp_cache, "draft_test")

  # Check tables exist
  tables <- DBI::dbListTables(con)
  expect_true("draft_metadata" %in% tables)
  expect_true("draft_annotations" %in% tables)

  # Check metadata table structure
  metadata_cols <- DBI::dbListFields(con, "draft_metadata")
  expect_true(all(c("draft_function", "created_at", "parameters_json", "n_bundles_completed") %in% metadata_cols))

  # Check annotations table structure
  annot_cols <- DBI::dbListFields(con, "draft_annotations")
  expect_true(all(c("session", "bundle", "level_name", "annotations_blob", "parameters_json", "error_occurred") %in% annot_cols))

  DBI::dbDisconnect(con)
})

test_that("get_draft_cache creates new cache correctly", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_db_cache_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_db_cache", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path)

  params <- list(param1 = 100, param2 = "test")

  cache_info <- reindeer:::get_draft_cache(
    corp,
    "draft_test_function",
    params,
    verbose = FALSE
  )

  expect_true(cache_info$is_new)
  expect_equal(cache_info$n_completed, 0)
  expect_s4_class(cache_info$con, "SQLiteConnection")
  expect_true(file.exists(cache_info$path))

  # Check metadata was inserted
  metadata <- DBI::dbGetQuery(cache_info$con, "SELECT * FROM draft_metadata")
  expect_equal(nrow(metadata), 1)
  expect_equal(metadata$draft_function, "draft_test_function")

  DBI::dbDisconnect(cache_info$con)

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("get_draft_cache resumes from existing cache", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_db_resume_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_db_resume", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path)

  params <- list(param1 = 100)

  # Create initial cache
  cache_info1 <- reindeer:::get_draft_cache(corp, "draft_test", params, verbose = FALSE)

  # Simulate some completed bundles
  DBI::dbExecute(cache_info1$con, "
    UPDATE draft_metadata
    SET n_bundles_completed = 5
    WHERE id = 1")

  DBI::dbDisconnect(cache_info1$con)

  # Get cache again (should resume)
  cache_info2 <- reindeer:::get_draft_cache(corp, "draft_test", params, verbose = FALSE)

  expect_false(cache_info2$is_new)
  expect_equal(cache_info2$n_completed, 5)

  DBI::dbDisconnect(cache_info2$con)

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("force_overwrite removes existing cache", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_db_force_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_db_force", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path)

  params <- list(param1 = 100)

  # Create cache
  cache_info1 <- reindeer:::get_draft_cache(corp, "draft_test", params, verbose = FALSE)
  cache_path <- cache_info1$path
  DBI::dbDisconnect(cache_info1$con)

  expect_true(file.exists(cache_path))

  # Force overwrite
  cache_info2 <- reindeer:::get_draft_cache(
    corp,
    "draft_test",
    params,
    force_overwrite = TRUE,
    verbose = FALSE
  )

  expect_true(cache_info2$is_new)
  expect_equal(cache_info2$n_completed, 0)

  DBI::dbDisconnect(cache_info2$con)

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("is_bundle_cached works correctly", {
  temp_cache <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_cache))

  con <- reindeer:::initialize_draft_cache(temp_cache, "draft_test")

  # Initially not cached
  expect_false(reindeer:::is_bundle_cached(con, "S1", "B1", "Level1", "Attr1"))

  # Store annotation
  test_annot <- data.frame(start = 100, label = "test")
  reindeer:::store_draft_annotations(
    con, "S1", "B1", "Level1", "ITEM", "Attr1",
    test_annot, list(param1 = 10)
  )

  # Now should be cached
  expect_true(reindeer:::is_bundle_cached(con, "S1", "B1", "Level1", "Attr1"))

  # Different bundle not cached
  expect_false(reindeer:::is_bundle_cached(con, "S1", "B2", "Level1", "Attr1"))

  DBI::dbDisconnect(con)
})

test_that("store and retrieve draft annotations", {
  temp_cache <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_cache))

  con <- reindeer:::initialize_draft_cache(temp_cache, "draft_test")

  # Store annotation
  test_annot <- data.frame(
    start = c(100, 200, 300),
    end = c(150, 250, 350),
    label = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  params <- list(windowSize = 20, minF = 60)

  reindeer:::store_draft_annotations(
    con, "Session1", "Bundle1", "Phonetic", "SEGMENT", "Phonetic",
    test_annot, params
  )

  # Retrieve
  retrieved <- reindeer:::retrieve_draft_annotations(con, "Session1", "Bundle1", "Phonetic")

  expect_equal(nrow(retrieved), 1)
  expect_equal(retrieved$session, "Session1")
  expect_equal(retrieved$bundle, "Bundle1")
  expect_false(retrieved$error_occurred)

  # Check annotations were deserialized correctly
  retrieved_annot <- retrieved$annotations[[1]]
  expect_equal(nrow(retrieved_annot), 3)
  expect_equal(retrieved_annot$label, c("a", "b", "c"))

  DBI::dbDisconnect(con)
})

test_that("store draft annotations with error", {
  temp_cache <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_cache))

  con <- reindeer:::initialize_draft_cache(temp_cache, "draft_test")

  # Store error
  reindeer:::store_draft_annotations(
    con, "Session1", "Bundle1", "Phonetic", "SEGMENT", "Phonetic",
    NULL, list(param1 = 10),
    error_occurred = TRUE,
    error_message = "Test error message"
  )

  # Retrieve
  retrieved <- reindeer:::retrieve_draft_annotations(con)

  expect_equal(nrow(retrieved), 1)
  expect_true(retrieved$error_occurred)
  expect_equal(retrieved$error_message, "Test error message")
  expect_true(is.null(retrieved$annotations[[1]]))

  DBI::dbDisconnect(con)
})

test_that("mark_draft_completed updates metadata", {
  temp_cache <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_cache))

  con <- reindeer:::initialize_draft_cache(temp_cache, "draft_test")

  # Insert metadata
  DBI::dbExecute(con, "
    INSERT INTO draft_metadata
      (draft_function, created_at, last_updated, corpus_path, corpus_uuid,
       parameters_json, completed)
    VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(
      "draft_test",
      as.character(Sys.time()),
      as.character(Sys.time()),
      "/tmp/test",
      "uuid123",
      "{}",
      0
    )
  )

  # Mark completed
  reindeer:::mark_draft_completed(con)

  # Check
  metadata <- DBI::dbGetQuery(con, "SELECT completed FROM draft_metadata")
  expect_equal(metadata$completed, 1)

  DBI::dbDisconnect(con)
})

test_that("draft_cache_summary works correctly", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_db_summary_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_db_summary", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path)

  # No cache exists
  summary1 <- draft_cache_summary(corp, "draft_nonexistent")
  expect_false(summary1$exists)

  # Create cache with data
  params <- list(param1 = 100)
  cache_info <- reindeer:::get_draft_cache(corp, "draft_test", params, verbose = FALSE)

  # Add some data
  test_annot <- data.frame(start = 100, label = "a")
  reindeer:::store_draft_annotations(
    cache_info$con, "S1", "B1", "Level1", "ITEM", "Attr1",
    test_annot, params
  )

  reindeer:::mark_draft_completed(cache_info$con)
  DBI::dbDisconnect(cache_info$con)

  # Get summary
  summary2 <- draft_cache_summary(corp, "draft_test")

  expect_true(summary2$exists)
  expect_true(summary2$has_data)
  expect_equal(summary2$draft_function, "draft_test")
  expect_equal(summary2$n_bundles_completed, 1)
  expect_true(summary2$completed)

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("list_draft_caches returns correct information", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_db_list_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_db_list", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path)

  # Create multiple caches
  cache_info1 <- reindeer:::get_draft_cache(corp, "draft_test1", list(), verbose = FALSE)
  DBI::dbDisconnect(cache_info1$con)

  cache_info2 <- reindeer:::get_draft_cache(corp, "draft_test2", list(), verbose = FALSE)
  DBI::dbDisconnect(cache_info2$con)

  # List caches
  cache_list <- list_draft_caches(corp)

  expect_s3_class(cache_list, "data.frame")
  expect_true(nrow(cache_list) >= 2)
  expect_true(all(c("cache_file", "draft_function", "n_completed") %in% names(cache_list)))

  # Clean up
  unlink(db_path, recursive = TRUE)
})

test_that("find_draft_cache_files finds and sorts by date", {
  skip_if_not_installed("emuR")

  test_dir <- tempdir()
  db_path <- file.path(test_dir, "test_db_find_emuDB")

  if (dir.exists(db_path)) {
    unlink(db_path, recursive = TRUE)
  }

  emuR::create_emuDB(name = "test_db_find", targetDir = test_dir, verbose = FALSE)
  corp <- corpus(db_path)

  # Create cache for different dates (simulate by creating files manually)
  cache_dir <- reindeer:::get_draft_cache_dir(corp)

  file1 <- file.path(cache_dir, "test_function_20251018.sqlite")
  file2 <- file.path(cache_dir, "test_function_20251019.sqlite")
  file3 <- file.path(cache_dir, "test_function_20251020.sqlite")

  # Create empty files
  file.create(file1)
  file.create(file2)
  file.create(file3)

  # Find files
  found_files <- reindeer:::find_draft_cache_files(corp, "draft_test_function")

  expect_equal(length(found_files), 3)

  # Should be sorted newest first
  expect_true(grepl("20251020", found_files[1]))
  expect_true(grepl("20251019", found_files[2]))
  expect_true(grepl("20251018", found_files[3]))

  # Clean up
  unlink(db_path, recursive = TRUE)
})

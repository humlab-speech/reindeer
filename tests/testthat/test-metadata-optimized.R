# ==============================================================================
# TESTS FOR OPTIMIZED METADATA OPERATIONS
# ==============================================================================

library(testthat)
library(reindeer)

test_that("Optimized metadata gathering works correctly", {
  skip_on_cran()
  
  # Setup
  if (!dir.exists(file.path(tempdir(), "emuR_demoData"))) {
    emuR::create_emuRdemoData(tempdir())
  }
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  corp <- corpus(ae_path)
  
  # Add test metadata at different levels
  add_metadata(corp, list(Project = "Test", Year = 2024))
  add_metadata(corp, list(SessionType = "Recording"), session = "0000")
  add_metadata(corp, list(Speaker = "TestSpeaker"), session = "0000", bundle = "msajc003")
  
  # Test gathering
  expect_silent(gather_metadata(corp, verbose = FALSE))
  
  # Verify metadata is in cache
  con <- get_connection(corp)
  
  db_meta <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM metadata_database WHERE db_uuid = '%s'",
    corp@config$UUID
  ))
  expect_gt(nrow(db_meta), 0)
  
  sess_meta <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM metadata_session WHERE db_uuid = '%s'",
    corp@config$UUID
  ))
  expect_gt(nrow(sess_meta), 0)
  
  bundle_meta <- DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM metadata_bundle WHERE db_uuid = '%s'",
    corp@config$UUID
  ))
  expect_gt(nrow(bundle_meta), 0)
  
  DBI::dbDisconnect(con)
})

test_that("Optimized metadata retrieval maintains correct precedence", {
  skip_on_cran()
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  corp <- corpus(ae_path)
  
  # Set metadata at all levels
  add_metadata(corp, list(TestField = "database_value"))
  add_metadata(corp, list(TestField = "session_value"), session = "0000")
  add_metadata(corp, list(TestField = "bundle_value"), session = "0000", bundle = "msajc003")
  
  # Gather and retrieve
  gather_metadata(corp, verbose = FALSE)
  metadata <- get_metadata(corp)
  
  # Check precedence: bundle > session > database
  bundle_row <- metadata[metadata$session == "0000" & metadata$bundle == "msajc003", ]
  expect_equal(bundle_row$TestField, "bundle_value")
  
  # Check session-level default applies to other bundles
  other_bundle <- metadata[metadata$session == "0000" & metadata$bundle != "msajc003", ]
  if (nrow(other_bundle) > 0) {
    expect_true(all(other_bundle$TestField == "session_value"))
  }
})

test_that("Optimized metadata retrieval is faster than naive approach", {
  skip_on_cran()
  skip_if_not_installed("bench")
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  corp <- corpus(ae_path)
  
  # Add substantial metadata
  for (i in 1:5) {
    add_metadata(corp, list(
      Field1 = paste0("value", i),
      Field2 = i * 10,
      Field3 = as.logical(i %% 2)
    ), session = "0000", bundle = paste0("msajc", sprintf("%03d", i)))
  }
  
  gather_metadata(corp, verbose = FALSE)
  
  # The optimized version should complete quickly
  timing <- system.time({
    result <- get_metadata(corp)
  })
  
  expect_lt(timing["elapsed"], 1.0)  # Should take less than 1 second
  expect_gt(nrow(result), 0)
  expect_true("Field1" %in% names(result))
})

test_that("Parallel metadata gathering works when available", {
  skip_on_cran()
  skip_if_not_installed("future.apply")
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  corp <- corpus(ae_path)
  
  # Add metadata to multiple bundles
  bundles <- corp[]
  for (i in seq_len(min(10, nrow(bundles)))) {
    add_metadata(
      corp,
      list(TestValue = i),
      session = bundles$session[i],
      bundle = bundles$bundle[i]
    )
  }
  
  # Test parallel gathering
  expect_silent(gather_metadata(corp, verbose = FALSE, parallel = TRUE))
  
  # Verify results
  metadata <- get_metadata(corp)
  expect_true("TestValue" %in% names(metadata))
})

test_that("Metadata export uses optimized queries", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  corp <- corpus(ae_path)
  
  # Add metadata
  add_metadata(corp, list(Age = 30, Gender = "Female"), session = "0000", bundle = "msajc003")
  gather_metadata(corp, verbose = FALSE)
  
  # Export
  temp_file <- tempfile(fileext = ".xlsx")
  
  timing <- system.time({
    export_metadata(corp, temp_file, overwrite = TRUE)
  })
  
  expect_lt(timing["elapsed"], 2.0)  # Should be fast
  expect_true(file.exists(temp_file))
  
  # Verify content
  bundle_data <- openxlsx::read.xlsx(temp_file, sheet = "bundles")
  expect_true("Age" %in% names(bundle_data))
  expect_true("Gender" %in% names(bundle_data))
  
  unlink(temp_file)
})

test_that("Metadata operations handle empty/missing data gracefully", {
  skip_on_cran()
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  corp <- corpus(ae_path)
  
  # Clear all metadata
  con <- get_connection(corp)
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_bundle WHERE db_uuid = '%s'", corp@config$UUID))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_session WHERE db_uuid = '%s'", corp@config$UUID))
  DBI::dbExecute(con, sprintf("DELETE FROM metadata_database WHERE db_uuid = '%s'", corp@config$UUID))
  DBI::dbDisconnect(con)
  
  # Should handle empty metadata
  expect_silent(metadata <- get_metadata(corp))
  expect_equal(ncol(metadata), 2)  # Just session and bundle columns
})

test_that("Bulk operations maintain data integrity", {
  skip_on_cran()
  
  ae_path <- file.path(tempdir(), "emuR_demoData", "ae_emuDB")
  corp <- corpus(ae_path)
  
  # Add metadata to all bundles
  bundles <- corp[]
  
  for (i in seq_len(nrow(bundles))) {
    add_metadata(
      corp,
      list(Index = i, Category = ifelse(i %% 2 == 0, "Even", "Odd")),
      session = bundles$session[i],
      bundle = bundles$bundle[i]
    )
  }
  
  # Gather and retrieve
  gather_metadata(corp, verbose = FALSE)
  metadata <- get_metadata(corp)
  
  # Verify all bundles have metadata
  expect_equal(nrow(metadata), nrow(bundles))
  expect_true(all(!is.na(metadata$Index)))
  expect_true(all(metadata$Category %in% c("Even", "Odd")))
  
  # Verify index matches
  for (i in seq_len(nrow(metadata))) {
    expected_category <- ifelse(metadata$Index[i] %% 2 == 0, "Even", "Odd")
    expect_equal(metadata$Category[i], expected_category)
  }
})

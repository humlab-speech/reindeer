# Test optimized metadata implementation
# This script tests the new metadata caching and retrieval system

library(reindeer)
library(testthat)

test_that("Metadata system initialization works", {
  # Create test database and wrap in corpus
  ae_handle <- create_ae_db(verbose = FALSE)
  ae <- corpus(ae_handle, verbose = FALSE)

  # Test gathering metadata
  expect_no_error(gather_metadata(ae, verbose = FALSE))

  # Test retrieving metadata
  meta <- get_metadata(ae)
  expect_s3_class(meta, "tbl_df")
  expect_true("session" %in% names(meta))
  expect_true("bundle" %in% names(meta))
})

test_that("Metadata inheritance works correctly", {
  ae_handle <- create_ae_db(verbose = FALSE)
  ae <- corpus(ae_handle, verbose = FALSE)

  # Set database-level default
  add_metadata(ae, list(Country = "USA", Language = "English"))

  # Set session-level override
  sessions <- list_sessions_from_cache(get_connection(ae), get_db_uuid(ae))
  if (nrow(sessions) > 0) {
    add_metadata(ae, list(Dialect = "Northern"), session = sessions$name[1])
  }

  # Set bundle-level override
  bundles <- list_bundles_from_cache(get_connection(ae), get_db_uuid(ae))
  if (nrow(bundles) > 0) {
    add_metadata(ae, list(Speaker = "S001"),
                session = bundles$session[1],
                bundle = bundles$name[1])
  }

  # Gather and verify
  gather_metadata(ae, verbose = FALSE)
  meta <- get_metadata(ae)

  # All bundles should have Country from database level
  expect_true(all(!is.na(meta$Country)))
  expect_true(all(meta$Country == "USA"))

  # Session bundles should have Dialect
  session_bundles <- meta[meta$session == sessions$name[1], ]
  expect_true(all(!is.na(session_bundles$Dialect)))

  # Specific bundle should have Speaker
  specific <- meta[meta$session == bundles$session[1] & meta$bundle == bundles$name[1], ]
  expect_equal(specific$Speaker, "S001")
})

test_that("Summary method works", {
  ae_handle <- create_ae_db(verbose = FALSE)
  ae <- corpus(ae_handle, verbose = FALSE)
  gather_metadata(ae, verbose = FALSE)

  expect_no_error(summary(ae))
  expect_output(summary(ae), "Summary of emuDB")
  expect_output(summary(ae), "Level definitions")
})

test_that("Excel export/import works", {
  skip_if_not_installed("openxlsx")

  ae_handle <- create_ae_db(verbose = FALSE)
  ae <- corpus(ae_handle, verbose = FALSE)

  # Add some metadata
  add_metadata(ae, list(Project = "TestProject", Year = 2024))
  gather_metadata(ae, verbose = FALSE)
  
  # Export
  temp_file <- tempfile(fileext = ".xlsx")
  expect_no_error(export_metadata(ae, temp_file, overwrite = TRUE))
  expect_true(file.exists(temp_file))
  
  # Modify and re-import
  meta_df <- openxlsx::read.xlsx(temp_file, sheet = "bundles")
  meta_df$Project <- "ModifiedProject"
  
  wb <- openxlsx::loadWorkbook(temp_file)
  openxlsx::deleteData(wb, "bundles", rows = 1:(nrow(meta_df)+1), cols = 1:ncol(meta_df), gridExpand = TRUE)
  openxlsx::writeData(wb, "bundles", meta_df)
  openxlsx::saveWorkbook(wb, temp_file, overwrite = TRUE)
  
  # Import
  expect_no_error(import_metadata(ae, temp_file))
  
  # Verify
  meta <- get_metadata(ae)
  expect_true(all(meta$Project == "ModifiedProject"))
  
  unlink(temp_file)
})

test_that("Programmatic metadata assignment works", {
  ae_handle <- create_ae_db(verbose = FALSE)
  ae <- corpus(ae_handle, verbose = FALSE)
  bundles <- list_bundles_from_cache(get_connection(ae), get_db_uuid(ae))

  if (nrow(bundles) > 0) {
    # Test assignment syntax
    expect_no_error({
      ae[bundles$session[1], bundles$name[1]] <- list(Age = 25, Sex = "Male")
    })

    gather_metadata(ae, verbose = FALSE)
    meta <- get_metadata(ae)

    specific <- meta[meta$session == bundles$session[1] & meta$bundle == bundles$name[1], ]
    expect_equal(as.numeric(specific$Age), 25)
    expect_equal(specific$Sex, "Male")
  }
})

test_that("Type validation works", {
  ae_handle <- create_ae_db(verbose = FALSE)
  ae <- corpus(ae_handle, verbose = FALSE)
  bundles <- list_bundles_from_cache(get_connection(ae), get_db_uuid(ae))

  if (nrow(bundles) > 0) {
    # Set initial value
    add_metadata(ae, list(Age = 25), session = bundles$session[1], bundle = bundles$name[1])
    gather_metadata(ae, verbose = FALSE)

    # Try to set incompatible type (should warn or error)
    expect_error({
      # This should fail in non-interactive mode
      add_metadata(ae, list(Age = "not a number"),
                  session = bundles$session[1],
                  bundle = bundles$name[1])
    })
  }
})

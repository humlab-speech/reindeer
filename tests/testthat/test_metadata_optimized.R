# ==============================================================================
# Tests for optimized metadata system
# Merged from test_metadata_optimized.R and test-metadata-optimized.R
# ==============================================================================

library(testthat)
library(reindeer)

# ==============================================================================
# Initialization & Gathering
# ==============================================================================

test_that("Metadata system initialization works", {
  ae <- create_isolated_ae_corpus()

  expect_no_error(gather_metadata(ae, verbose = FALSE))

  meta <- get_metadata(ae)
  expect_s3_class(meta, "tbl_df")
  expect_true("session" %in% names(meta))
  expect_true("bundle" %in% names(meta))
})

test_that("Gathered metadata populates SQL cache tables", {
  skip_on_cran()

  ae <- create_isolated_ae_corpus()

  add_metadata(ae, list(Project = "Test", Year = 2024))
  add_metadata(ae, list(SessionType = "Recording"), session = "0000")
  add_metadata(ae, list(Speaker = "TestSpeaker"), session = "0000", bundle = "msajc003")

  expect_silent(gather_metadata(ae, verbose = FALSE))

  con <- get_connection(ae)
  on.exit(DBI::dbDisconnect(con))

  db_meta <- DBI::dbGetQuery(con,
    "SELECT * FROM metadata_database WHERE db_uuid = ?",
    params = list(ae@config$UUID))
  expect_gt(nrow(db_meta), 0)

  sess_meta <- DBI::dbGetQuery(con,
    "SELECT * FROM metadata_session WHERE db_uuid = ?",
    params = list(ae@config$UUID))
  expect_gt(nrow(sess_meta), 0)

  bundle_meta <- DBI::dbGetQuery(con,
    "SELECT * FROM metadata_bundle WHERE db_uuid = ?",
    params = list(ae@config$UUID))
  expect_gt(nrow(bundle_meta), 0)
})

# ==============================================================================
# Inheritance & Precedence
# ==============================================================================

test_that("Metadata inheritance works correctly (3 levels)", {
  ae <- create_isolated_ae_corpus()

  add_metadata(ae, list(Country = "USA", Language = "English"))

  sessions <- list_sessions_from_cache(get_connection(ae), get_db_uuid(ae))
  if (nrow(sessions) > 0) {
    add_metadata(ae, list(Dialect = "Northern"), session = sessions$name[1])
  }

  bundles <- list_bundles_from_cache(get_connection(ae), get_db_uuid(ae))
  if (nrow(bundles) > 0) {
    add_metadata(ae, list(Speaker = "S001"),
                session = bundles$session[1],
                bundle = bundles$name[1])
  }

  gather_metadata(ae, verbose = FALSE)
  meta <- get_metadata(ae)

  # All bundles inherit database-level Country
  expect_true(all(!is.na(meta$Country)))
  expect_true(all(meta$Country == "USA"))

  # Session bundles inherit Dialect
  session_bundles <- meta[meta$session == sessions$name[1], ]
  expect_true(all(!is.na(session_bundles$Dialect)))

  # Specific bundle has Speaker
  specific <- meta[meta$session == bundles$session[1] & meta$bundle == bundles$name[1], ]
  expect_equal(specific$Speaker, "S001")
})

test_that("Precedence: bundle > session > database", {
  skip_on_cran()

  ae <- create_isolated_ae_corpus()

  add_metadata(ae, list(TestField = "database_value"))
  add_metadata(ae, list(TestField = "session_value"), session = "0000")
  add_metadata(ae, list(TestField = "bundle_value"), session = "0000", bundle = "msajc003")

  gather_metadata(ae, verbose = FALSE)
  metadata <- get_metadata(ae)

  bundle_row <- metadata[metadata$session == "0000" & metadata$bundle == "msajc003", ]
  expect_equal(bundle_row$TestField, "bundle_value")

  other_bundle <- metadata[metadata$session == "0000" & metadata$bundle != "msajc003", ]
  if (nrow(other_bundle) > 0) {
    expect_true(all(other_bundle$TestField == "session_value"))
  }
})

# ==============================================================================
# Summary & Display
# ==============================================================================

test_that("Summary method works", {
  ae <- create_isolated_ae_corpus()
  gather_metadata(ae, verbose = FALSE)
  expect_no_error(summary(ae))
})

# ==============================================================================
# Programmatic Assignment
# ==============================================================================

test_that("Programmatic metadata assignment via bracket notation works", {
  ae <- create_isolated_ae_corpus()
  bundles <- list_bundles_from_cache(get_connection(ae), get_db_uuid(ae))

  if (nrow(bundles) > 0) {
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

# ==============================================================================
# Type Validation
# ==============================================================================

test_that("Type changes overwrite the previous value (idempotent upsert)", {
  # Note: real type validation is intentionally NOT enforced. The
  # metadata_fields table records the most recent observed type. Re-applying
  # a metadata key with a different value type overwrites the row instead
  # of erroring (idempotent INSERT OR REPLACE in v0.5.2). A future release
  # may add opt-in strict type checking via an option.
  ae <- create_isolated_ae_corpus()
  bundles <- list_bundles_from_cache(get_connection(ae), get_db_uuid(ae))

  if (nrow(bundles) > 0) {
    add_metadata(ae, list(Age = 25), session = bundles$session[1], bundle = bundles$name[1])
    gather_metadata(ae, verbose = FALSE)

    expect_no_error({
      add_metadata(ae, list(Age = "not a number"),
                  session = bundles$session[1],
                  bundle = bundles$name[1])
    })
  }
})

# ==============================================================================
# Excel Export/Import
# ==============================================================================

test_that("Excel export/import round-trip works", {
  skip_if_not_installed("openxlsx")

  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Project = "TestProject", Year = 2024))
  gather_metadata(ae, verbose = FALSE)

  temp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(temp_file))

  expect_no_error(export_metadata(ae, temp_file, overwrite = TRUE))
  expect_true(file.exists(temp_file))

  # Modify and re-import
  bundles_df  <- openxlsx::read.xlsx(temp_file, sheet = "bundles")
  sessions_df <- openxlsx::read.xlsx(temp_file, sheet = "sessions")
  database_df <- openxlsx::read.xlsx(temp_file, sheet = "database")

  bundles_df$Project <- "ModifiedProject"

  wb2 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb2, "bundles");  openxlsx::writeData(wb2, "bundles",  bundles_df)
  openxlsx::addWorksheet(wb2, "sessions"); openxlsx::writeData(wb2, "sessions", sessions_df)
  openxlsx::addWorksheet(wb2, "database"); openxlsx::writeData(wb2, "database", database_df)
  openxlsx::saveWorkbook(wb2, temp_file, overwrite = TRUE)

  expect_no_error(import_metadata(ae, temp_file))

  meta <- get_metadata(ae)
  expect_true(all(meta$Project == "ModifiedProject"))
})

test_that("Metadata export contains expected columns", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")

  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 30, Gender = "Female"), session = "0000", bundle = "msajc003")
  gather_metadata(ae, verbose = FALSE)

  temp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(temp_file))

  timing <- system.time({ export_metadata(ae, temp_file, overwrite = TRUE) })
  expect_lt(timing["elapsed"], 2.0)
  expect_true(file.exists(temp_file))

  bundle_data <- openxlsx::read.xlsx(temp_file, sheet = "bundles")
  expect_true("Age" %in% names(bundle_data))
  expect_true("Gender" %in% names(bundle_data))
})

# ==============================================================================
# Performance
# ==============================================================================

test_that("Optimized metadata retrieval completes quickly", {
  skip_on_cran()
  skip_if_not_installed("bench")

  ae <- create_isolated_ae_corpus()

  bundle_names <- list_bundles_from_cache(get_connection(ae), get_db_uuid(ae))$name
  bundle_names <- head(bundle_names, 5)

  for (i in seq_along(bundle_names)) {
    add_metadata(ae, list(
      Field1 = paste0("value", i),
      Field2 = i * 10,
      Field3 = as.logical(i %% 2)
    ), session = "0000", bundle = bundle_names[i])
  }
  gather_metadata(ae, verbose = FALSE)

  timing <- system.time({ result <- get_metadata(ae) })
  expect_lt(timing["elapsed"], 1.0)
  expect_gt(nrow(result), 0)
  expect_true("Field1" %in% names(result))
})

# ==============================================================================
# Parallel Gathering
# ==============================================================================

test_that("Parallel metadata gathering works when available", {
  skip_on_cran()
  skip_if_not_installed("future.apply")

  ae <- create_isolated_ae_corpus()
  bundles <- ae[]
  for (i in seq_len(min(10, nrow(bundles)))) {
    add_metadata(ae, list(TestValue = i),
                session = bundles$session[i],
                bundle = bundles$bundle[i])
  }

  expect_silent(gather_metadata(ae, verbose = FALSE, parallel = TRUE))

  metadata <- get_metadata(ae)
  expect_true("TestValue" %in% names(metadata))
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("Empty/missing metadata handled gracefully", {
  skip_on_cran()

  ae <- create_isolated_ae_corpus()

  # Clear all metadata from cache
  con <- get_connection(ae)
  uuid <- ae@config$UUID
  DBI::dbExecute(con, "DELETE FROM metadata_bundle WHERE db_uuid = ?", params = list(uuid))
  DBI::dbExecute(con, "DELETE FROM metadata_session WHERE db_uuid = ?", params = list(uuid))
  DBI::dbExecute(con, "DELETE FROM metadata_database WHERE db_uuid = ?", params = list(uuid))
  DBI::dbDisconnect(con)

  expect_silent(metadata <- get_metadata(ae))
  expect_equal(ncol(metadata), 2)  # Just session and bundle columns
})

# ==============================================================================
# Auto-stub METADATA.json on bundle/session creation (Item 4)
# ==============================================================================

test_that("create_session_and_bundle writes METADATA.json at session level", {
  ae <- create_isolated_ae_corpus()

  reindeer:::create_session_and_bundle(ae, "newsess", "newbndl", verbose = FALSE)

  ses_meta <- file.path(ae@basePath, "newsess_ses", "METADATA.json")
  expect_true(file.exists(ses_meta))

  content <- jsonlite::read_json(ses_meta)
  expect_type(content, "list")
  expect_length(content, 0L)
})

test_that("create_session_and_bundle writes METADATA.json at bundle level", {
  ae <- create_isolated_ae_corpus()

  reindeer:::create_session_and_bundle(ae, "newsess2", "newbndl2", verbose = FALSE)

  bndl_meta <- file.path(
    ae@basePath, "newsess2_ses", "newbndl2_bndl", "METADATA.json"
  )
  expect_true(file.exists(bndl_meta))

  content <- jsonlite::read_json(bndl_meta)
  expect_type(content, "list")
  expect_length(content, 0L)
})

test_that("auto-stub never overwrites existing METADATA.json", {
  ae <- create_isolated_ae_corpus()

  reindeer:::create_session_and_bundle(ae, "presess", "prebndl", verbose = FALSE)
  bndl_meta <- file.path(
    ae@basePath, "presess_ses", "prebndl_bndl", "METADATA.json"
  )

  # Populate with real metadata
  jsonlite::write_json(
    list(participant = list(id = "P999")),
    bndl_meta,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # Re-invoke skeleton write — must NOT overwrite
  result <- reindeer:::.write_metadata_skeleton(bndl_meta, level = "bundle")
  expect_false(result)

  content <- jsonlite::read_json(bndl_meta)
  expect_equal(content$participant$id, "P999")
})

test_that("gather_metadata accepts auto-stubbed bundles without error", {
  ae <- create_isolated_ae_corpus()

  reindeer:::create_session_and_bundle(ae, "stubsess", "stubbndl", verbose = FALSE)

  expect_no_error(gather_metadata(ae, verbose = FALSE))
})

test_that("Bulk operations maintain data integrity", {
  skip_on_cran()

  ae <- create_isolated_ae_corpus()
  bundles <- ae[]

  for (i in seq_len(nrow(bundles))) {
    add_metadata(ae, list(Index = i, Category = ifelse(i %% 2 == 0, "Even", "Odd")),
                session = bundles$session[i],
                bundle = bundles$bundle[i])
  }

  gather_metadata(ae, verbose = FALSE)
  metadata <- get_metadata(ae)

  expect_equal(nrow(metadata), nrow(bundles))
  expect_true(all(!is.na(metadata$Index)))
  expect_true(all(metadata$Category %in% c("Even", "Odd")))

  for (i in seq_len(nrow(metadata))) {
    expected_category <- ifelse(metadata$Index[i] %% 2 == 0, "Even", "Odd")
    expect_equal(metadata$Category[i], expected_category)
  }
})

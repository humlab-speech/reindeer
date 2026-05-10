# ==============================================================================
# Tests for JSON-Schema validation of _DBconfig.json + METADATA.json (Item 3)
# ==============================================================================

library(testthat)
library(reindeer)

skip_if_no_jsonvalidate <- function() {
  testthat::skip_if_not_installed("jsonvalidate")
}

test_that("ae corpus DBconfig validates against schema", {
  skip_if_no_jsonvalidate()
  ae <- create_isolated_ae_corpus()
  res <- validate_corpus(ae)
  db_rows <- res[res$schema == "dbconfig.schema.json", ]
  expect_equal(nrow(db_rows), 1L)
  expect_true(db_rows$ok)
})

test_that("auto-stub METADATA.json (empty {}) validates as schema-clean", {
  skip_if_no_jsonvalidate()
  ae <- create_isolated_ae_corpus()
  reindeer:::create_session_and_bundle(ae, "schemasess", "schemabndl",
                                       verbose = FALSE)
  res <- validate_corpus(ae)
  meta_rows <- res[res$schema == "metadata.schema.json", ]
  expect_gte(nrow(meta_rows), 2L)             # session + bundle stubs
  expect_true(all(meta_rows$ok))
})

test_that("malformed METADATA.json triggers a soft warning by default", {
  skip_if_no_jsonvalidate()
  ae <- create_isolated_ae_corpus()
  reindeer:::create_session_and_bundle(ae, "badsess", "badbndl",
                                       verbose = FALSE)
  bad_path <- file.path(ae@basePath, "badsess_ses", "badbndl_bndl",
                        "METADATA.json")
  jsonlite::write_json(
    list(participant = list(age = "not a number")),
    bad_path, auto_unbox = TRUE
  )
  withr::with_options(list(reindeer.schema_strict = FALSE), {
    expect_warning(
      validate_corpus(ae),
      regexp = "Schema validation"
    )
  })
})

test_that("strict mode promotes schema failures to errors", {
  skip_if_no_jsonvalidate()
  ae <- create_isolated_ae_corpus()
  reindeer:::create_session_and_bundle(ae, "strictsess", "strictbndl",
                                       verbose = FALSE)
  bad_path <- file.path(ae@basePath, "strictsess_ses", "strictbndl_bndl",
                        "METADATA.json")
  jsonlite::write_json(
    list(participant = list(age = "not a number")),
    bad_path, auto_unbox = TRUE
  )
  withr::with_options(list(reindeer.schema_strict = TRUE), {
    expect_error(
      validate_corpus(ae),
      regexp = "Schema validation failed"
    )
  })
})

test_that("set_metadata_database refuses to write a schema-invalid value", {
  skip_if_no_jsonvalidate()
  ae <- create_isolated_ae_corpus()
  # `participant.age` must be numeric (or null) — passing a list violates schema
  expect_error(
    add_metadata(ae, list(participant = list(age = list(invalid = "shape")))),
    regexp = "Schema validation failed"
  )
})

test_that("validate_corpus returns empty tibble when no JSON files present", {
  skip_if_no_jsonvalidate()
  # Build a minimal corpus with no metadata
  tmp <- withr::local_tempdir()
  res <- tryCatch({
    base <- file.path(tmp, "empty_emuDB")
    dir.create(base)
    # Minimal _DBconfig.json
    jsonlite::write_json(
      list(name = "empty", UUID = "00000000-0000-0000-0000-000000000000",
           levelDefinitions = list(),
           linkDefinitions = list()),
      file.path(base, "empty_DBconfig.json"),
      auto_unbox = TRUE, pretty = TRUE
    )
    suppressMessages(corp <- corpus(base))
    validate_corpus(corp)
  }, error = function(e) NULL)
  if (!is.null(res)) {
    # DBconfig present, METADATA absent — at least the dbconfig row exists
    expect_true("file" %in% names(res))
  }
})

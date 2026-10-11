# ============================================================================
# Classed conditions: reindeer_query_error / reindeer_schema_error /
# reindeer_cache_error all inherit from reindeer_error so user code can
# catch them with tryCatch().
# ============================================================================

library(testthat)
library(reindeer)

test_that("query parse failures throw reindeer_query_error", {
  ae <- create_isolated_ae_corpus()
  cnd <- tryCatch(
    query(ae, "this is not valid EQL!@#"),
    reindeer_query_error = function(e) e,
    error = function(e) e
  )
  expect_s3_class(cnd, "reindeer_query_error")
  expect_s3_class(cnd, "reindeer_error")
})

test_that("missing-database errors throw reindeer_query_error", {
  cnd <- tryCatch(
    query("/no/such/path_emuDB", "Phonetic == t"),
    reindeer_query_error = function(e) e,
    error = function(e) e
  )
  expect_s3_class(cnd, "reindeer_query_error")
})

test_that("a single reindeer_error handler catches every reindeer abort", {
  ae <- create_isolated_ae_corpus()
  cnd <- tryCatch(
    query(ae, "totally invalid !!!"),
    reindeer_error = function(e) e,
    error = function(e) e
  )
  expect_s3_class(cnd, "reindeer_error")
})

test_that("corpus(sync_eaf=) warns with class reindeer_deprecated", {
  skip_if_no_emuR()
  p <- create_isolated_ae_db()
  expect_warning(corpus(p, sync_eaf = TRUE), class = "reindeer_deprecated")
})

test_that("auto-CMDI failure warns with a classed condition and keeps the write", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  withr::local_options(reindeer.auto_cmdi = TRUE)
  testthat::local_mocked_bindings(
    describe_corpus = function(...) stop("boom"), .package = "reindeer")
  expect_warning(
    add_metadata(corp, list(Project = "p")),
    class = "reindeer_cmdi_warning")
  expect_equal(jsonlite::read_json(file.path(corp@basePath, "METADATA.json"))$Project, "p")
})

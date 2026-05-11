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

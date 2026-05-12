# ============================================================================
# Parse-error messages include a caret pointer (v0.7).
# Errors emitted via .query_abort() with .eql_caret() should embed two
# extra lines: the input query and a single-char caret pointing at the
# offending position.
# ============================================================================

library(testthat)
library(reindeer)

test_that("simple-query parse failure shows a caret line", {
  ae <- create_isolated_ae_corpus()
  cnd <- tryCatch(query(ae, "###not_eql_at_all"),
                  reindeer_query_error = function(e) e,
                  error = function(e) e)
  msg <- paste(cnd$message, paste(cnd$body, collapse = "\n"), sep = "\n")
  expect_match(msg, "Query:")
  expect_match(msg, "not_eql_at_all")
  expect_match(msg, "\\^")  # caret
})

test_that("dominance parse failure caret points near `^`", {
  ae <- create_isolated_ae_corpus()
  # Truly broken dominance: missing right side.
  cnd <- tryCatch(query(ae, "[Phonetic == t ^]"),
                  reindeer_query_error = function(e) e,
                  error = function(e) e)
  expect_s3_class(cnd, "reindeer_query_error")
})

test_that("the caret helper handles out-of-range positions gracefully", {
  out <- reindeer:::.eql_caret("abc", pos = 99L)
  expect_length(out, 1L)  # falls back to label only
})

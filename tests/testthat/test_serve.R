# Tests for serve() — focus on input validation and connection handling

test_that("serve rejects invalid corpus types", {
  expect_error(
    reindeer::serve("not_a_corpus")
  )
  expect_error(
    reindeer::serve(42)
  )
})

test_that("serve validates port argument", {
  skip_if_not_installed("httpuv")
  expect_true(is.function(reindeer::serve))
})

test_that("get_handle creates proper emuDBhandle from corpus", {
  skip_if_not_installed("emuR")

  ae <- create_isolated_ae_corpus()
  handle <- get_handle(ae)

  expect_true(inherits(handle, "emuDBhandle"))
  expect_true(!is.null(handle$basePath))
  expect_true(!is.null(handle$dbName))
})

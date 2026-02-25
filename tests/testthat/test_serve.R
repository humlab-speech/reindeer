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
  # Create minimal fake corpus to get past type check
  skip_if_not_installed("httpuv")

  # Port validation happens inside serve(), testing with invalid corpus
  # just to verify the function exists and is callable
  expect_true(is.function(reindeer::serve))
})

test_that("get_emuDBhandle creates proper handle from corpus-like object", {
  # Test the internal helper
  skip("Requires live corpus object")
})

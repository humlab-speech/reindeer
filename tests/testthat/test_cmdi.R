# Tests for create_cmdi_metadata

test_that("create_cmdi_metadata rejects invalid corpus types", {
  expect_error(
    reindeer::create_cmdi_metadata("not_a_corpus"),
    "corpus must be"
  )
  expect_error(
    reindeer::create_cmdi_metadata(42),
    "corpus must be"
  )
  expect_error(
    reindeer::create_cmdi_metadata(list(a = 1)),
    "corpus must be"
  )
})

test_that("create_cmdi_metadata accepts emuDBhandle class", {
  # Create a fake emuDBhandle (will fail at config loading, but passes type check)
  fake_handle <- list(
    dbName = "test",
    basePath = tempdir(),
    UUID = "test-uuid"
  )
  class(fake_handle) <- "emuDBhandle"

  # Should pass type check but fail at load_DBconfig
  expect_error(
    reindeer::create_cmdi_metadata(fake_handle, verbose = FALSE),
    "config"
  )
})

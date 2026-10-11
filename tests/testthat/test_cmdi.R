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

test_that(".wav_duration matches wrassp", {
  skip_if_not_installed("wrassp")
  skip_if_no_emuR()
  f <- list.files(create_isolated_ae_db(), "msajc003\\.wav$", recursive = TRUE, full.names = TRUE)[1]
  x <- wrassp::read.AsspDataObj(f)
  expect_equal(reindeer:::.wav_duration(f), attr(x, "endRecord") / attr(x, "sampleRate"),
               tolerance = 1e-3)
})

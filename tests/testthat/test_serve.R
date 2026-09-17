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

test_that("serve() accepts the lazy segment list that query() returns", {
  # The contract is documented in R/reindeer_serve.R:38, the README, and three
  # vignette blocks, none of which execute in CI.
  skip_if_not_installed("emuR")
  skip_if_not_installed("httpuv")

  # serve() resolves an EMU-webApp directory before it starts the server; a
  # stub keeps the test independent of a local webApp checkout.
  webapp <- withr::local_tempdir()
  writeLines("<html><body></body></html>", file.path(webapp, "index.html"))
  withr::local_options(reindeer.emuWebApp.dir = webapp)

  corp <- create_isolated_ae_corpus()
  lazy <- query(corp, "Phoneme =~ .+")
  expect_s7_class(lazy, reindeer::lazy_segment_list)

  withr::defer(httpuv::stopAllServers())

  expect_no_error(
    serve(corp, seglist = lazy, port = httpuv::randomPort(),
          autoOpenURL = "", useViewer = FALSE)
  )
  expect_false(is.null(getOption("reindeer.serve_handle")))
})

test_that("serve() still rejects inputs that are not segment lists", {
  skip_if_not_installed("emuR")
  skip_if_not_installed("httpuv")

  corp <- create_isolated_ae_corpus()
  expect_error(
    serve(corp, seglist = list(session = "0000"), port = httpuv::randomPort(),
          autoOpenURL = "", useViewer = FALSE),
    "segment_list"
  )
})

test_that(".serve_file_response streams exactly the requested range", {
  f <- withr::local_tempfile(fileext = ".wav")
  writeBin(as.raw(1:100), f)

  full <- .serve_file_response(f, "audio/x-wav", NULL)
  expect_equal(full$status, 200L)
  expect_length(full$body, 100)

  # Open-ended ranges are what browsers send when seeking in a media element.
  head_range <- .serve_file_response(f, "audio/x-wav", "bytes=10-")
  expect_equal(head_range$status, 206L)
  expect_length(head_range$body, 90)
  expect_equal(as.integer(head_range$body[1]), 11L)

  mid <- .serve_file_response(f, "audio/x-wav", "bytes=5-9")
  expect_equal(mid$status, 206L)
  expect_length(mid$body, 5)

  # The whole file requested as a range is served whole.
  whole <- .serve_file_response(f, "audio/x-wav", "bytes=0-")
  expect_equal(whole$status, 200L)
  expect_length(whole$body, 100)

  expect_equal(.serve_file_response(f, "audio/x-wav", "bytes=500-600")$status, 416L)
  expect_equal(.serve_file_response(f, "audio/x-wav", "garbage")$status, 416L)
})

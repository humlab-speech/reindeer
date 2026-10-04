# Tests for annotate()/review() and the Artic session plumbing.

stub_artic <- function(features = c("bundleList", "timeAnchors"), envir = parent.frame()) {
  d <- tempfile("artic-stub-")
  dir.create(d, recursive = TRUE)
  writeLines("<html><body>stub</body></html>", file.path(d, "index.html"))
  jsonlite::write_json(
    list(name = "artic", version = "test", protocol = list(version = "0.0.2"),
         features = features),
    file.path(d, "artic-manifest.json"),
    auto_unbox = TRUE
  )
  withr::defer(unlink(d, recursive = TRUE), envir = envir)
  d
}

test_that("find_artic prefers the explicit dir and validates builds", {
  good <- stub_artic()
  bad <- withr::local_tempdir()
  expect_equal(reindeer:::find_artic(appDir = good), good)
  expect_error(reindeer:::find_artic(appDir = bad), class = "reindeer_artic_error")

  # Feature gate: a build without timeAnchors cannot serve a review playlist.
  old <- stub_artic(features = character())
  expect_error(
    reindeer:::find_artic(appDir = old, require = reindeer:::.artic_required_features()),
    class = "reindeer_artic_error"
  )
})

test_that("find_artic reports a missing app", {
  testthat::local_mocked_bindings(
    .artic_candidates = function(appDir = NULL) character(),
    .package = "reindeer"
  )
  expect_error(reindeer:::find_artic(), class = "reindeer_artic_error")
})

test_that("install_artic copies a local build into the cache", {
  good <- stub_artic()
  dest <- tempfile("artic-cache-")
  withr::defer(unlink(dest, recursive = TRUE))
  out <- install_artic(local = good, dest = dest, quiet = TRUE)
  expect_true(file.exists(file.path(out, "index.html")))
  expect_true(file.exists(file.path(out, "artic-manifest.json")))
  expect_error(install_artic(local = good, dest = dest, quiet = TRUE),
               class = "reindeer_artic_error")
})

test_that("build_review_playlist builds ordered anchors with labels", {
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))
  pl <- reindeer:::.build_review_playlist(segs, corpus = corp)

  expected_bundles <- unique(paste(segs$session, segs$bundle))
  got_bundles <- vapply(pl, function(e) paste(e$session, e$name), character(1))
  expect_equal(got_bundles, expected_bundles)
  expect_true(all(vapply(pl, function(e) is.integer(e$timeAnchors$sample_start), logical(1))))
  expect_true("label" %in% names(pl[[1]]$timeAnchors))
  expect_gt(sum(vapply(pl, function(e) nrow(e$timeAnchors), integer(1))), 0L)
})

test_that("build_review_playlist rejects bad input", {
  corp <- create_isolated_ae_corpus()
  expect_error(reindeer:::.build_review_playlist(list(a = 1), corpus = corp),
               class = "reindeer_artic_error")
  expect_error(reindeer:::.build_review_playlist(data.frame(x = 1), corpus = corp),
               class = "reindeer_artic_error")
  expect_error(reindeer:::.build_review_playlist(
    data.frame(session = character(), bundle = character()), corpus = corp
  ), class = "reindeer_artic_error")
})

test_that("edit overlay forces editable, renderable config", {
  corp <- create_isolated_ae_corpus()
  cfg <- load_DBconfig(get_emuDBhandle(corp))
  ov <- reindeer:::.artic_edit_overlay(cfg)

  expect_true(ov$EMUwebAppConfig$restrictions$editItemName)
  expect_true(ov$EMUwebAppConfig$restrictions$editItemSize)
  expect_true(ov$EMUwebAppConfig$activeButtons$saveBundle)
  expect_true(length(ov$EMUwebAppConfig$perspectives[[1]]$levelCanvases$order) > 0L)
  expect_true("OSCI" %in% ov$EMUwebAppConfig$perspectives[[1]]$signalCanvases$order)
})

test_that("annotate() opens a session against a stub dist", {
  skip_if_not_installed("httpuv")
  app <- stub_artic()
  corp <- create_isolated_ae_corpus()
  withr::defer(httpuv::stopAllServers())

  expect_no_error(
    annotate(corp, appDir = app, port = httpuv::randomPort(),
             autoOpenURL = "", useViewer = FALSE)
  )
  expect_false(is.null(getOption("reindeer.serve_handle")))
})

test_that("review() opens a session with a playlist", {
  skip_if_not_installed("httpuv")
  app <- stub_artic()
  corp <- create_isolated_ae_corpus()
  segs <- query(corp, "Phoneme == t")
  withr::defer(httpuv::stopAllServers())

  expect_no_error(
    review(corp, segs, appDir = app, port = httpuv::randomPort(),
           autoOpenURL = "", useViewer = FALSE)
  )
})

test_that("serve() and serve_app() redirect to annotate()/review()", {
  expect_error(serve(42), class = "reindeer_moved_error")
  expect_error(serve_app(42), class = "reindeer_moved_error")
})

test_that("annotate() forwards non-corpus calls to ggplot2", {
  skip_if_not_installed("ggplot2")
  layer <- annotate("text", x = 1, y = 1, label = "a")
  expect_s3_class(layer, "LayerInstance")
})

test_that("guess_mime_type covers Artic asset types", {
  expect_equal(guess_mime_type("a.mjs"), "text/javascript")
  expect_equal(guess_mime_type("a.wasm"), "application/wasm")
  expect_equal(guess_mime_type("a.woff2"), "font/woff2")
  expect_equal(guess_mime_type("a.svg"), "image/svg+xml")
  expect_equal(guess_mime_type("a.wav"), "audio/wav")
})

test_that(".serve_file_response streams exactly the requested range", {
  f <- withr::local_tempfile(fileext = ".wav")
  writeBin(as.raw(1:100), f)

  full <- .serve_file_response(f, "audio/x-wav", NULL)
  expect_equal(full$status, 200L)
  expect_length(full$body, 100)

  head_range <- .serve_file_response(f, "audio/x-wav", "bytes=10-")
  expect_equal(head_range$status, 206L)
  expect_length(head_range$body, 90)
  expect_equal(as.integer(head_range$body[1]), 11L)

  mid <- .serve_file_response(f, "audio/x-wav", "bytes=5-9")
  expect_equal(mid$status, 206L)
  expect_length(mid$body, 5)

  whole <- .serve_file_response(f, "audio/x-wav", "bytes=0-")
  expect_equal(whole$status, 200L)
  expect_length(whole$body, 100)

  expect_equal(.serve_file_response(f, "audio/x-wav", "bytes=500-600")$status, 416L)
  expect_equal(.serve_file_response(f, "audio/x-wav", "garbage")$status, 416L)
})

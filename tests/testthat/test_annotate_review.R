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
    annotate(corp, app_dir = app, port = httpuv::randomPort(),
             auto_open_url = "", use_viewer = FALSE)
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
    review(corp, segs, app_dir = app, port = httpuv::randomPort(),
           auto_open_url = "", use_viewer = FALSE)
  )
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
  # a whole file is handed to httpuv as a file body, never read into R
  expect_equal(full$body$file, normalizePath(f))

  head_range <- .serve_file_response(f, "audio/x-wav", "bytes=10-")
  expect_equal(head_range$status, 206L)
  expect_length(head_range$body, 90)
  expect_equal(as.integer(head_range$body[1]), 11L)

  mid <- .serve_file_response(f, "audio/x-wav", "bytes=5-9")
  expect_equal(mid$status, 206L)
  expect_length(mid$body, 5)

  whole <- .serve_file_response(f, "audio/x-wav", "bytes=0-")
  expect_equal(whole$status, 200L)
  expect_equal(whole$body$file, normalizePath(f))

  expect_equal(.serve_file_response(f, "audio/x-wav", "bytes=500-600")$status, 416L)
  expect_equal(.serve_file_response(f, "audio/x-wav", "garbage")$status, 416L)
})

test_that("Artic helpers: query parsing, safe path parts, annotation indices", {
  q <- reindeer:::.parse_query_string("?session=a%20b&bundle=x")
  expect_equal(q$session, "a b")
  expect_equal(q$bundle, "x")
  expect_equal(reindeer:::.parse_query_string(""), list())

  expect_true(reindeer:::.safe_part("msajc003"))
  expect_false(reindeer:::.safe_part("../x"))
  expect_false(reindeer:::.safe_part("a/b"))
  expect_false(reindeer:::.safe_part(""))
  expect_false(reindeer:::.safe_part(c("a", "b")))

  json <- jsonlite::toJSON(list(
    name = "b", annotates = "b.wav", sampleRate = 16000,
    levels = list(list(name = "W", type = "SEGMENT", items = list(
      list(id = 1, sampleStart = 0, sampleDur = 10, labels = list(list(name = "W", value = "a"))),
      list(id = 2, sampleStart = 10, sampleDur = 10, labels = list(
        list(name = "W", value = "b"), list(name = "X", value = "c"))))))),
    auto_unbox = TRUE)
  p <- reindeer:::parse_annot_json(as.character(json), "u", "s", "b")
  expect_equal(p$items$seq_idx, 1:2)
  expect_equal(p$labels$label_idx, c(1L, 1L, 2L))
})

test_that("annotate(segment_list) opens a review playlist for the segments' corpus", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == t"))
  seen <- NULL
  testthat::local_mocked_bindings(
    .artic_session = function(corpus, ...) { seen <<- list(...); invisible(TRUE) },
    .package = "reindeer")
  suppressMessages(annotate(segs))
  expect_true(!is.null(seen$bundle_entries))
  expect_gt(length(seen$bundle_entries), 0L)
})

test_that("a second annotate() replaces the first server and unload leaves none (F2.5)", {
  skip_if_not_installed("httpuv")
  app <- stub_artic()
  corp <- create_isolated_ae_corpus()
  withr::defer(httpuv::stopAllServers())
  open_it <- function() annotate(corp, app_dir = app, port = httpuv::randomPort(),
                                 auto_open_url = "", use_viewer = FALSE)
  open_it(); open_it()
  expect_length(httpuv::listServers(), 1L)
  reindeer:::.onUnload(NULL)
  expect_length(httpuv::listServers(), 0L)
})

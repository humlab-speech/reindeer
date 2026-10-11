# Corpus connections are memoised in the corpus environment and nothing closed
# them: a session that opens many corpora leaked one SQLite handle each, and
# RSQLite's own finalizer reported it at exit ("call dbDisconnect() when
# finished working with a connection"). A finalizer now closes the handle as
# soon as the corpus object is collected.

test_that("the corpus connection is closed when the corpus is collected", {
  skip_if_no_emuR()

  corp <- create_isolated_ae_corpus()
  con <- get_corpus_connection(corp)
  expect_true(DBI::dbIsValid(con))

  rm(corp)
  invisible(gc())
  invisible(gc())

  expect_false(DBI::dbIsValid(con))
})

test_that("close_connection() is idempotent and leaves the corpus usable", {
  skip_if_no_emuR()

  corp <- create_isolated_ae_corpus()
  expect_true(DBI::dbIsValid(get_corpus_connection(corp)))

  close_connection(corp)
  close_connection(corp)

  # A later call reconnects rather than failing.
  con <- get_corpus_connection(corp)
  expect_true(DBI::dbIsValid(con))
  expect_gt(nrow(DBI::dbGetQuery(con, "SELECT uuid FROM emu_db LIMIT 1")), 0)
})

test_that("session/bundle creation and schema validation keep the shared connection open", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  con <- get_corpus_connection(corp)
  create_session_and_bundle(corp, "tricky'session", "b1", verbose = FALSE)
  expect_true(DBI::dbIsValid(con))
  suppressWarnings(validate_corpus(corp))
  expect_true(DBI::dbIsValid(con))
  s <- list_sessions_from_cache(con, corp@.uuid)$name
  expect_true("tricky'session" %in% s)
})

test_that("navigation reuses the live corpus' connection and pins nothing", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == t"))
  again <- get_corpus_cached(segs)
  expect_identical(again@.connection, corp@.connection)
  expect_true(DBI::dbIsValid(get_corpus_connection(corp)))
  expect_no_error(ascend_to(segs, "Syllable"))
  expect_no_error(scout(segs, 1))

  # No global pin: once the user drops the corpus it can be collected, and a
  # later call simply opens a fresh one.
  path <- corp@basePath
  segs@db_path <- path
  rm(corp, again); gc()
  expect_null(reindeer:::.live_corpus(path))
  expect_no_error(ascend_to(segs, "Syllable"))
})

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

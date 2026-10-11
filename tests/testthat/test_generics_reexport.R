# collect()/glimpse() are the dplyr/pillar generics, re-exported, with methods
# for reindeer classes.

test_that("collect and glimpse are re-exports of dplyr/pillar", {
  expect_identical(reindeer::collect, dplyr::collect)
  expect_identical(reindeer::glimpse, pillar::glimpse)
})

test_that("collect/glimpse work on lazy lists, segment lists and plain tibbles", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  lz <- query(corp, "Phonetic == t")
  segs <- dplyr::collect(lz)
  expect_s7_class(segs, segment_list)
  expect_identical(dplyr::collect(segs), segs)
  tb <- tibble::tibble(a = 1:3)
  expect_identical(dplyr::collect(tb), tb)
  expect_no_error(capture.output(pillar::glimpse(tb)))
  expect_no_error(capture.output(pillar::glimpse(segs)))
  expect_no_error(capture.output(pillar::glimpse(lz)))
  expect_no_error(capture.output(pillar::glimpse(corp)))
})

test_that("print headers have no unrendered glue", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  out <- paste(cli::ansi_strip(capture.output(print(dplyr::collect(query(corp, "Phonetic == t"))),
                                              type = "message")), collapse = "\n")
  out2 <- paste(capture.output(print(dplyr::collect(query(corp, "Phonetic == t")))), collapse = "\n")
  expect_false(grepl("\\{nrow|\\{\\?s", paste(out, out2)))
  expect_true(grepl("17 segments", paste(out, out2)))
})

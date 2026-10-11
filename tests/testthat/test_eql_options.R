# EQL argument handling and small parity fixes (WP3.1).

test_that("single `=` behaves like `==`, quoted alternatives work", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  n <- function(q) nrow(collect(query(corp, q)))
  expect_equal(n("Phonetic = t"), n("Phonetic == t"))
  expect_equal(n("Phonetic == 'm'|'n'"), n("Phonetic == m|n"))
  expect_equal(n("Phonetic == \"m\"|\"n\""), n("Phonetic == m|n"))
  expect_equal(n("Phonetic == 'm'"), n("Phonetic == m"))
})

test_that("sessionPattern/bundlePattern restrict the result (lazy and eager)", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  for (lazy in c(TRUE, FALSE)) {
    none <- as.data.frame(collect(query(corp, "Phonetic == t", sessionPattern = "nomatch", lazy = lazy)))
    expect_equal(nrow(none), 0L)
    one <- as.data.frame(collect(query(corp, "Phonetic == t", bundlePattern = "^msajc003$", lazy = lazy)))
    expect_gt(nrow(one), 0L)
    expect_true(all(one$bundle == "msajc003"))
  }
})

test_that("unsupported or unknown options abort with a classed error", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  expect_error(query(corp, "Phonetic == t", calcTimes = FALSE), class = "reindeer_query_error")
  expect_error(query(corp, "Phonetic == t", resultType = "emusegs"), class = "reindeer_query_error")
  expect_error(query(corp, "Phonetic == t", timeRefSegmentLevel = "Phonetic"),
               class = "reindeer_query_error")
  expect_error(query(corp, "Phonetic == t", bogus = 1), class = "reindeer_query_error")
  expect_no_error(query(corp, "Phonetic == t", calcTimes = TRUE))
})

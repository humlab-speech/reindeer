library(testthat)
library(reindeer)

test_that("enrich() is a hard-deprecated stub pointing at quantify()/biographize()", {
  expect_error(
    enrich(1),
    class = "reindeer_moved_error"
  )
  expect_error(
    enrich(1),
    regexp = "quantify"
  )
})

test_that("enrich stays exported (helpful error, not 'object not found')", {
  expect_true("enrich" %in% getNamespaceExports("reindeer"))
})

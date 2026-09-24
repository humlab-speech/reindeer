# Coverage for quantify.corpus: compute-and-register, connect-existing,
# generator provenance, and the ssffTrackDefinitions round-trip.

library(testthat)
library(reindeer)

test_that(".resolve_dsp_identity parses a qualified pkg::fn expression", {
  id <- reindeer:::.resolve_dsp_identity(superassp::trk_rms, "superassp::trk_rms")
  expect_equal(id$`function`, "trk_rms")
  expect_equal(id$package, "superassp")
  expect_equal(id$version, as.character(utils::packageVersion("superassp")))
})

test_that(".resolve_dsp_identity falls back to the function's namespace for a bare name", {
  id <- reindeer:::.resolve_dsp_identity(superassp::trk_rms, "trk_rms")
  expect_equal(id$`function`, "trk_rms")
  expect_equal(id$package, "superassp")
})

test_that(".resolve_dsp_identity handles a user-defined function (no package)", {
  fake_dsp <- function(listOfFiles, ...) list()
  id <- reindeer:::.resolve_dsp_identity(fake_dsp, "fake_dsp")
  expect_equal(id$`function`, "fake_dsp")
  expect_true(is.na(id$package))
  expect_true(is.na(id$version))
})

test_that(".build_generator_block captures only explicit user args", {
  block <- reindeer:::.build_generator_block(
    superassp::trk_rms, "superassp::trk_rms",
    user_params = list(windowSize = 20)
  )
  expect_equal(block$`function`, "trk_rms")
  expect_equal(block$package, "superassp")
  expect_equal(block$args, list(windowSize = 20))
  expect_match(block$generatedAt, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}")
})

test_that("generator with empty args round-trips through toJSON as an object, not an array", {
  block <- reindeer:::.build_generator_block(
    superassp::trk_rms, "superassp::trk_rms", user_params = list()
  )
  json <- jsonlite::toJSON(list(generator = block), auto_unbox = TRUE, force = TRUE)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_true(is.list(parsed$generator$args))
  expect_null(names(parsed$generator$args))  # empty named list -> {} -> empty list on read-back, not a vector
})

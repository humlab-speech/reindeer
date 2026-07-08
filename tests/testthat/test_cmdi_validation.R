# ==============================================================================
# CMDI Phase 1 — envelope correctness + validation harness.
# Structural validation runs offline; XSD/Schematron layers are separate.
# ==============================================================================

library(testthat)
library(reindeer)

test_that("profile name maps to CCR id, namespace, and XSD url", {
  id <- reindeer:::.cmdi_profile_id("media-corpus")
  expect_equal(id, "clarin.eu:cr1:p_1387365569699")
  expect_match(reindeer:::.cmdi_profile_ns(id),
               "profiles/clarin.eu:cr1:p_1387365569699$")
  expect_match(reindeer:::.cmdi_profile_xsd_url(id), "/xsd$")
  # passthrough of a raw profile id
  expect_equal(reindeer:::.cmdi_profile_id("clarin.eu:cr1:p_999"),
               "clarin.eu:cr1:p_999")
})

test_that("generated CMDI passes structural validation", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = TRUE, verbose = FALSE)

  cmdi <- list.files(outdir, pattern = "_cmdi\\.xml$", full.names = TRUE)[[1]]
  res <- validate_cmdi(cmdi)
  expect_true(res$structural, info = paste(res$problems, collapse = "; "))
})

test_that("CMDI envelope binds the profile namespace + dual schemaLocation + self link", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = TRUE, verbose = FALSE)

  cmdi <- list.files(outdir, pattern = "_cmdi\\.xml$", full.names = TRUE)[[1]]
  x <- paste(readLines(cmdi, warn = FALSE), collapse = "\n")

  expect_match(x, 'xmlns:cmdp="http://www.clarin.eu/cmd/1/profiles/clarin.eu:cr')
  expect_match(x, "profiles/clarin.eu:cr1:p_[0-9]+/xsd")   # profile XSD in schemaLocation
  expect_match(x, "MdSelfLink>urn:uuid:")                   # self link populated
})

test_that("validate_cmdi flags a non-CMDI file", {
  tmp <- withr::local_tempfile(fileext = ".xml")
  writeLines("<foo/>", tmp)

  res <- validate_cmdi(tmp)
  expect_false(res$structural)
  expect_gt(length(res$problems), 0)
})

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

test_that("CMDI Components are profile-conformant (media-corpus tree)", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 30, Gender = "Male"), session = "0000")
  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = TRUE, verbose = FALSE)

  cmdi <- list.files(outdir, pattern = "_cmdi\\.xml$", full.names = TRUE)[[1]]
  doc <- xml2::read_xml(cmdi)
  cmdp <- "http://www.clarin.eu/cmd/1/profiles/clarin.eu:cr1:p_1387365569699"
  nss <- c(cmd = "http://www.clarin.eu/cmd/1", cmdp = cmdp)

  # single profile root component under Components, in the cmdp namespace
  expect_length(
    xml2::xml_find_all(doc, "//cmd:Components/cmdp:media-corpus-profile", ns = nss), 1)
  # required tree present
  expect_length(
    xml2::xml_find_all(doc, "//cmdp:Collection/cmdp:GeneralInfo/cmdp:Name", ns = nss), 1)
  expect_length(
    xml2::xml_find_all(doc, "//cmdp:Collection/cmdp:OriginLocation/cmdp:Location", ns = nss), 1)
  expect_length(
    xml2::xml_find_all(doc, "//cmdp:media-corpus-profile/cmdp:Corpus", ns = nss), 1)
  # speaker count carried in SpeechCorpus
  spk <- xml2::xml_text(xml2::xml_find_first(doc, "//cmdp:NumberOfSpeakers", ns = nss))
  expect_match(spk, "^[0-9]+$")
  # no leftover invented cmd:-namespace component elements
  expect_length(xml2::xml_find_all(doc, "//cmd:Participants", ns = nss), 0)
})

test_that("validate_cmdi flags a non-CMDI file", {
  tmp <- withr::local_tempfile(fileext = ".xml")
  writeLines("<foo/>", tmp)

  res <- validate_cmdi(tmp)
  expect_false(res$structural)
  expect_gt(length(res$problems), 0)
})

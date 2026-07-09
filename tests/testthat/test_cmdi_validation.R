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

test_that("default CMDI is a fully-populated speech-corpus-with-participants record", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Project = "NordicSpeech", Licence = "CC-BY",
                        Language = "Swedish"))
  add_metadata(ae, list(Age = 30, Gender = "Female", Education = "Tertiary"),
               session = "0000")
  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = TRUE, verbose = FALSE)

  cmdi <- list.files(outdir, pattern = "_cmdi\\.xml$", full.names = TRUE)[[1]]
  doc <- xml2::read_xml(cmdi)
  cmdp <- "http://www.clarin.eu/cmd/1/profiles/clarin.eu:cr1:p_1392642184799"
  nss <- c(cmd = "http://www.clarin.eu/cmd/1", cmdp = cmdp)
  x1 <- function(xp) xml2::xml_text(xml2::xml_find_first(doc, xp, ns = nss))

  # correct profile root
  expect_length(
    xml2::xml_find_all(doc, "//cmd:Components/cmdp:SpeechCorpusWithParticipants", ns = nss), 1)
  # required vocab leaves are valid enum values
  expect_equal(x1("//cmdp:GeneralInfo/cmdp:ResourceClass"), "SpeechCorpus")
  expect_equal(x1("//cmdp:SpeechCorpusSpecific/cmdp:Modalities"), "spoken")
  # corpus-level fields mapped to their components
  expect_equal(x1("//cmdp:Access/cmdp:Licence"), "CC-BY")
  expect_equal(x1("//cmdp:Project/cmdp:ProjectName"), "NordicSpeech")
  expect_equal(x1("//cmdp:SubjectLanguage//cmdp:iso-639-3-code"), "swe")
  # annotation tiers mapped from DB config levels
  expect_gt(length(xml2::xml_find_all(doc, "//cmdp:AnnotationTypes/cmdp:AnnotationType", ns = nss)), 0)
  # demographics aggregated from per-bundle metadata
  expect_match(x1("//cmdp:NumberOfSpeakers"), "^[0-9]+$")
  expect_match(x1("//cmdp:AgeDistribution/cmdp:ParticipantMeanAge"), "30")
  # unmapped user field folded into Descriptions
  descr <- xml2::xml_text(xml2::xml_find_all(doc, "//cmdp:GeneralInfo//cmdp:Description", ns = nss))
  expect_true(any(grepl("education: Tertiary", descr)))
})

test_that("media-corpus profile still produces its conformant tree", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = TRUE, verbose = FALSE,
                  profile = "media-corpus")

  cmdi <- list.files(outdir, pattern = "_cmdi\\.xml$", full.names = TRUE)[[1]]
  doc <- xml2::read_xml(cmdi)
  cmdp <- "http://www.clarin.eu/cmd/1/profiles/clarin.eu:cr1:p_1387365569699"
  nss <- c(cmd = "http://www.clarin.eu/cmd/1", cmdp = cmdp)
  expect_length(
    xml2::xml_find_all(doc, "//cmd:Components/cmdp:media-corpus-profile", ns = nss), 1)
})

test_that("validate_cmdi flags a non-CMDI file", {
  tmp <- withr::local_tempfile(fileext = ".xml")
  writeLines("<foo/>", tmp)

  res <- validate_cmdi(tmp)
  expect_false(res$structural)
  expect_gt(length(res$problems), 0)
})

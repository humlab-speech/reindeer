# ==============================================================================
# CMDI Phase 5 — per-session & per-bundle media-session records + collection
# hierarchy. Each EMU session and bundle gets a media-session-profile CMDI with
# per-actor Age/Sex/Education; the corpus record links to them.
# ==============================================================================

library(testthat)
library(reindeer)

ms_ns <- c(cmd  = "http://www.clarin.eu/cmd/1",
           cmdp = "http://www.clarin.eu/cmd/1/profiles/clarin.eu:cr1:p_1336550377513")

test_that("session-cmdi writes session + bundle records in the session dirs", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 30, Gender = "Female", Education = "Tertiary",
                        Language = "Swedish"), session = "0000")
  d <- withr::local_tempdir()
  describe_corpus(ae, output_dir = d, force = TRUE, verbose = FALSE,
                  formats = c("cmdi", "session-cmdi"))

  expect_true(file.exists(file.path(d, "0000_ses", "0000.cmdi.xml")))
  expect_true(file.exists(file.path(d, "0000_ses", "msajc003_bndl", "msajc003.cmdi.xml")))
})

test_that("session record is a media-session profile with actor Age/Sex/Education", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 30, Gender = "Female", Education = "Tertiary"),
               session = "0000")
  p <- withr::local_tempfile(fileext = ".xml")
  reindeer:::create_media_session_cmdi(ae, session = "0000", output_file = p)

  doc <- xml2::read_xml(p)
  x1 <- function(xp) xml2::xml_text(xml2::xml_find_first(doc, xp, ns = ms_ns))
  expect_length(
    xml2::xml_find_all(doc, "//cmd:Components/cmdp:media-session-profile", ns = ms_ns), 1)
  expect_equal(x1("//cmdp:media-session-actor/cmdp:Age"), "30")
  expect_equal(x1("//cmdp:media-session-actor/cmdp:Sex"), "Female")
  expect_equal(x1("//cmdp:media-session-actor/cmdp:Education"), "Tertiary")
  # one media-annotation-bundle per bundle in the session
  expect_gt(length(xml2::xml_find_all(doc, "//cmdp:media-annotation-bundle", ns = ms_ns)), 1)
  expect_true(validate_cmdi(p)$structural)
})

test_that("bundle record references its EAF as a WrittenResource", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 40, Gender = "Male"), session = "0000")
  # place an .eaf next to the bundle audio (as the ELAN autosync would)
  bdir <- file.path(ae@basePath, "0000_ses", "msajc003_bndl")
  writeLines("<ANNOTATION_DOCUMENT/>", file.path(bdir, "msajc003.eaf"))

  p <- withr::local_tempfile(fileext = ".xml")
  reindeer:::create_media_session_cmdi(ae, session = "0000", bundle = "msajc003",
                                       output_file = p)
  doc <- xml2::read_xml(p)
  # exactly one recording, one actor
  expect_length(xml2::xml_find_all(doc, "//cmdp:media-annotation-bundle", ns = ms_ns), 1)
  wr <- xml2::xml_text(xml2::xml_find_first(
    doc, "//cmdp:WrittenResource/cmdp:Name", ns = ms_ns))
  expect_equal(wr, "msajc003.eaf")
  # EAF also appears in the resource proxy list
  x <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(x, "msajc003.eaf")
})

test_that("collection hierarchy links corpus -> session -> bundle", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 30, Gender = "Female"), session = "0000")
  d <- withr::local_tempdir()
  describe_corpus(ae, output_dir = d, force = TRUE, verbose = FALSE,
                  formats = c("cmdi", "session-cmdi"))

  corp <- paste(readLines(file.path(d, "ae_cmdi.xml"), warn = FALSE), collapse = "\n")
  expect_match(corp, "Metadata")
  expect_true(grepl("0000.cmdi.xml", corp, fixed = TRUE))

  ses_path <- file.path(d, "0000_ses", "0000.cmdi.xml")
  ses <- paste(readLines(ses_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("msajc003.cmdi.xml", ses, fixed = TRUE))   # session -> bundle
  expect_true(grepl("ae Speech Corpus", ses, fixed = TRUE))    # names the parent

  expect_true(validate_cmdi(ses_path)$structural)
})

test_that("sex mapping and profile id are correct", {
  expect_equal(reindeer:::.map_sex("Male"), "Male")
  expect_equal(reindeer:::.map_sex("f"), "Female")
  expect_equal(reindeer:::.map_sex("nonbinary"), "Unknown")
  expect_equal(reindeer:::.cmdi_profile_id("media-session"),
               "clarin.eu:cr1:p_1336550377513")
})

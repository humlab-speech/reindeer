# ==============================================================================
# Phase B0 — FAIR coherence: metadata set via the (flat) metadata API must
# actually reach the generated standard artifacts. Before the fix the emitters
# only read legacy nested `.meta_json` participant/project objects and ignored
# METADATA.json fields entirely.
# ==============================================================================

library(testthat)
library(reindeer)

test_that("flat database-level metadata reaches the rendered README", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Funder = "CoherenceFunder"))

  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = TRUE, verbose = FALSE)

  readme <- paste(readLines(file.path(outdir, "README.md"), warn = FALSE),
                  collapse = "\n")
  expect_match(readme, "CoherenceFunder", fixed = TRUE)
})

test_that("flat participant metadata (Age/Gender) reaches the CMDI XML", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 33, Gender = "Female"), session = "0000")

  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = TRUE, verbose = FALSE)

  cmdi <- list.files(outdir, pattern = "_cmdi\\.xml$", full.names = TRUE)
  expect_gt(length(cmdi), 0)
  xml <- paste(readLines(cmdi[[1]], warn = FALSE), collapse = "\n")
  expect_match(xml, "Female")
  expect_match(xml, "33")
})

test_that("participants are built from resolved metadata, not just legacy files", {
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Age = 40, Gender = "Male"), session = "0000")

  parts <- reindeer:::collect_participant_metadata(ae, verbose = FALSE)
  expect_gt(length(parts), 0)
  # lowercase keys, values preserved
  first <- parts[[1]]
  expect_true(!is.null(first$age) || !is.null(first$gender))
})

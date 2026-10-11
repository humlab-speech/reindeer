# ==============================================================================
# Phase 1 — Aim 1 metadata correctness
#   1a. Age/Gender -> DSPP literature norms wired into the quantify/enrich path
#   1b. Legacy <name>.meta_json read when METADATA.json absent (override rule)
# ==============================================================================

library(testthat)
library(reindeer)

# A stub DSP function whose formals name several DSPP columns, so we can test
# derive_dsp_parameters() without pulling in superassp.
fake_dsp <- function(nominalF1 = NA, nominalF2 = NA, nominalF3 = NA,
                     windowSize = NA, maxFormantHz = NA, numFormants = NA) NULL

# ------------------------------------------------------------------------------
# 1a. DSPP norms are actually used (preview == applied)
# ------------------------------------------------------------------------------

test_that("derive_dsp_parameters pulls DSPP norms, not hardcoded constants", {
  dspp <- tibble::as_tibble(reindeer:::dspp_metadataParameters_dt())
  row  <- reindeer:::.lookup_dspp_row(dspp, 30, "Male")

  params <- reindeer:::derive_dsp_parameters(
    fake_dsp, list(Age = 30, Gender = "Male"), character(), list())

  # Every norm column the function accepts comes straight from the DSPP row,
  # so what dsp_parameters() previews is exactly what gets baked in.
  expect_equal(params$nominalF1, row$nominalF1)
  expect_equal(params$windowSize, row$windowSize)
  expect_equal(params$maxFormantHz, row$maxFormantHz)

  # Old code hardcoded 500 for adult males; the table value must differ.
  expect_false(isTRUE(params$nominalF1 == 500))
})

test_that("derive_dsp_parameters is gender- and age-specific", {
  male   <- reindeer:::derive_dsp_parameters(
    fake_dsp, list(Age = 30, Gender = "Male"), character(), list())
  female <- reindeer:::derive_dsp_parameters(
    fake_dsp, list(Age = 30, Gender = "Female"), character(), list())
  expect_false(isTRUE(male$nominalF1 == female$nominalF1))

  child <- reindeer:::derive_dsp_parameters(
    fake_dsp, list(Age = 5, Gender = "Male"), character(), list())
  adult <- reindeer:::derive_dsp_parameters(
    fake_dsp, list(Age = 45, Gender = "Male"), character(), list())
  expect_false(isTRUE(child$nominalF1 == adult$nominalF1))
})

test_that("user-supplied params override derived norms", {
  params <- reindeer:::derive_dsp_parameters(
    fake_dsp, list(Age = 30, Gender = "Male"), character(), list(nominalF1 = 999))
  expect_equal(params$nominalF1, 999)
})

test_that("missing Age/Gender metadata falls back to DSP defaults silently", {
  # No norm block should run; result carries no DSPP columns.
  params <- reindeer:::derive_dsp_parameters(
    fake_dsp, list(Speaker = "X"), character(), list())
  expect_null(params$nominalF1)
})

# ------------------------------------------------------------------------------
# 1b. Legacy .meta_json support with per-level override rule
# ------------------------------------------------------------------------------

test_that("legacy <db>.meta_json is read when METADATA.json is absent", {
  ae <- create_isolated_ae_corpus()
  basePath <- ae@basePath
  db_name <- sub("_emuDB$", "", basename(basePath))

  writeLines('{"LegacyField":"fromLegacy"}',
             file.path(basePath, paste0(db_name, ".meta_json")))

  gather_metadata(ae, verbose = FALSE)
  meta <- get_metadata(ae)

  expect_true("LegacyField" %in% names(meta))
  expect_true(all(meta$LegacyField == "fromLegacy"))
})

test_that("METADATA.json shadows legacy .meta_json at the same level", {
  ae <- create_isolated_ae_corpus()
  basePath <- ae@basePath
  db_name <- sub("_emuDB$", "", basename(basePath))

  writeLines('{"Source":"legacy"}',
             file.path(basePath, paste0(db_name, ".meta_json")))
  writeLines('{"Source":"modern"}',
             file.path(basePath, reindeer:::metadata.filename))

  gather_metadata(ae, verbose = FALSE)
  meta <- get_metadata(ae)

  expect_true("Source" %in% names(meta))
  expect_true(all(meta$Source == "modern"))
})

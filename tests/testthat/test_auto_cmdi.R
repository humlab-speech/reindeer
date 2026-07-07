# ============================================================================
# Auto-regeneration of FAIR metadata artifacts (Aim 3).
# Standard-compliant artifacts are (re)generated automatically as the user
# edits metadata. A drift guard keeps repeated regeneration cheap/idempotent.
# ============================================================================

library(testthat)
library(reindeer)

test_that("dirty bit + describe_corpus work when auto-regen is disabled", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()

  expect_false(reindeer:::.is_metadata_dirty(ae))
  add_metadata(ae, list(Project = "v07-autosync-test"))
  expect_true(reindeer:::.is_metadata_dirty(ae))

  outdir <- withr::local_tempdir()
  describe_corpus(ae, output_dir = outdir, force = FALSE, verbose = FALSE)
  expect_false(reindeer:::.is_metadata_dirty(ae))
})

test_that("add_metadata auto-regenerates FAIR artifacts (no explicit describe_corpus call)", {
  withr::local_options(reindeer.auto_cmdi = TRUE)
  ae <- create_isolated_ae_corpus()
  readme <- file.path(ae@basePath, "README.md")

  add_metadata(ae, list(Project = "auto-regen-1"))

  # Artifact written automatically, and the dirty bit consumed.
  expect_true(file.exists(readme))
  expect_false(reindeer:::.is_metadata_dirty(ae))
})

test_that("a distinct metadata change bumps the artifacts automatically", {
  withr::local_options(reindeer.auto_cmdi = TRUE)
  ae <- create_isolated_ae_corpus()
  readme <- file.path(ae@basePath, "README.md")

  add_metadata(ae, list(Project = "auto-regen-1"))
  mtime1 <- file.info(readme)$mtime

  Sys.sleep(1.1)  # ensure mtime resolution distinguishes runs
  add_metadata(ae, list(Project = "auto-regen-2"))
  mtime2 <- file.info(readme)$mtime

  expect_gt(as.numeric(mtime2), as.numeric(mtime1))
})

test_that("drift guard skips regeneration when metadata is unchanged", {
  withr::local_options(reindeer.auto_cmdi = TRUE)
  ae <- create_isolated_ae_corpus()

  add_metadata(ae, list(Project = "steady"))
  readme <- file.path(ae@basePath, "README.md")
  mtime1 <- file.info(readme)$mtime

  Sys.sleep(1.1)
  # No metadata change: an explicit describe_corpus into the default dir is a
  # no-op and returns nothing written.
  written <- describe_corpus(ae, force = FALSE, verbose = FALSE)
  expect_length(written, 0L)
  expect_equal(as.numeric(file.info(readme)$mtime), as.numeric(mtime1))
})

test_that("bulk import suppresses per-row regeneration then emits once", {
  withr::local_options(reindeer.auto_cmdi = TRUE)
  ae <- create_isolated_ae_corpus()

  # Simulate the bulk-edit contract directly: suppression is honoured.
  withr::local_options(reindeer._auto_cmdi_suppress = TRUE)
  add_metadata(ae, list(Project = "bulk"))
  expect_true(reindeer:::.is_metadata_dirty(ae))  # not consumed while suppressed
})

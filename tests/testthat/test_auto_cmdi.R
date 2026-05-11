# ============================================================================
# Auto-regeneration of FAIR metadata artifacts.
# v0.7.0: add_metadata() flips a dirty bit; describe_corpus() consumes it
# to force rewrite of README/CMDI/DataCite even when files exist.
# ============================================================================

library(testthat)
library(reindeer)

test_that(".mark_metadata_dirty flips the sentinel file", {
  ae <- create_isolated_ae_corpus()
  expect_false(reindeer:::.is_metadata_dirty(ae))
  add_metadata(ae, list(Project = "v07-autosync-test"))
  expect_true(reindeer:::.is_metadata_dirty(ae))
})

test_that("describe_corpus clears the dirty bit after regenerating", {
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Project = "v07-autosync-test"))
  expect_true(reindeer:::.is_metadata_dirty(ae))

  # Run describe_corpus; should regenerate and clear flag.
  outdir <- tempfile("desc_")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  describe_corpus(ae, output_dir = outdir, force = FALSE, verbose = FALSE)

  expect_false(reindeer:::.is_metadata_dirty(ae))
})

test_that("describe_corpus auto-regenerates pre-existing artifacts when dirty", {
  ae <- create_isolated_ae_corpus()
  outdir <- tempfile("desc_")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  # First emission with no dirty bit
  describe_corpus(ae, output_dir = outdir, force = FALSE, verbose = FALSE)
  readme1_path <- file.path(outdir, "README.md")
  expect_true(file.exists(readme1_path))
  mtime1 <- file.info(readme1_path)$mtime

  # Mutate metadata + re-emit; auto-regen should overwrite README despite
  # force = FALSE.
  Sys.sleep(1.1)  # ensure mtime resolution distinguishes runs
  add_metadata(ae, list(Project = "v07-autosync-test"))
  describe_corpus(ae, output_dir = outdir, force = FALSE, verbose = FALSE)
  mtime2 <- file.info(readme1_path)$mtime
  expect_gt(as.numeric(mtime2), as.numeric(mtime1))
})

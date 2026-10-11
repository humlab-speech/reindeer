# ============================================================================
# Auto-regeneration of FAIR metadata artifacts (Aim 3).
# Standard-compliant artifacts are (re)generated automatically as the user
# edits metadata. A drift guard keeps repeated regeneration cheap/idempotent.
# ============================================================================

library(testthat)
library(reindeer)

test_that("describe_corpus emits once, then is a no-op until metadata changes", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Project = "v07-autosync-test"))
  first <- describe_corpus(ae, verbose = FALSE)
  expect_gt(length(first), 0L)
  expect_length(describe_corpus(ae, verbose = FALSE), 0L)
  add_metadata(ae, list(Project = "changed"))
  expect_gt(length(describe_corpus(ae, verbose = FALSE)), 0L)
})

test_that("add_metadata auto-regenerates FAIR artifacts (no explicit describe_corpus call)", {
  withr::local_options(reindeer.auto_cmdi = TRUE)
  ae <- create_isolated_ae_corpus()
  readme <- file.path(ae@basePath, "README.md")

  add_metadata(ae, list(Project = "auto-regen-1"))

  expect_true(file.exists(readme))
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
  expect_false(file.exists(file.path(ae@basePath, "README.md")))  # suppressed: nothing emitted
})

test_that("writing identical metadata again rewrites no FAIR file", {
  withr::local_options(reindeer.auto_cmdi = TRUE)
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Project = "same"))
  files <- list.files(ae@basePath, "\\.(xml|md|cff|json)$", full.names = TRUE)
  before <- file.info(files)$mtime
  Sys.sleep(1.1)
  add_metadata(ae, list(Project = "same"))
  expect_equal(file.info(files)$mtime, before)
})


test_that("a hand-written README is never overwritten", {
  withr::local_options(reindeer.auto_cmdi = FALSE)
  ae <- create_isolated_ae_corpus()
  readme <- file.path(ae@basePath, "README.md")
  writeLines("my own notes", readme)
  add_metadata(ae, list(Project = "p"))
  describe_corpus(ae, verbose = FALSE)
  expect_equal(readLines(readme, warn = FALSE), "my own notes")
  expect_true(file.exists(file.path(ae@basePath, "README-generated.md")))
})

test_that("auto-CMDI is on by default and regenerates every owned artifact in place", {
  withr::local_options(reindeer.auto_cmdi = NULL)       # package default
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(Project = "first"))
  base <- ae@basePath
  expect_true(file.exists(file.path(base, "README.md")))
  expect_true(file.exists(file.path(base, "CITATION.cff")))
  before <- file.info(file.path(base, c("README.md", "CITATION.cff", "datacite.json")))$mtime
  Sys.sleep(1.1)
  add_metadata(ae, list(Project = "second"))
  after <- file.info(file.path(base, c("README.md", "CITATION.cff", "datacite.json")))$mtime
  expect_true(all(after > before))
  expect_length(list.files(base, "-generated"), 0L)     # updated in place, no stale twins
})

test_that("a hand-written CITATION.cff is never overwritten", {
  withr::local_options(reindeer.auto_cmdi = NULL)
  ae <- create_isolated_ae_corpus()
  writeLines("# mine", file.path(ae@basePath, "CITATION.cff"))
  add_metadata(ae, list(Project = "p"))
  expect_equal(readLines(file.path(ae@basePath, "CITATION.cff"), warn = FALSE), "# mine")
  expect_true(file.exists(file.path(ae@basePath, "CITATION-generated.cff")))
})

test_that("opening a corpus writes nothing", {
  withr::local_options(reindeer.auto_cmdi = NULL)
  p <- create_isolated_ae_db()
  corpus(p, verbose = FALSE)
  expect_false(file.exists(file.path(p, "README.md")))
})

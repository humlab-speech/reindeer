# ==============================================================================
# Phase 2 — Aim 6: keep the whole pipeline lazy through enrich / biographize
# and nav verbs, until collect().
# ==============================================================================

library(testthat)
library(reindeer)

test_that("biographize is a public export", {
  expect_true("biographize" %in% getNamespaceExports("reindeer"))
})

test_that("biographize on a lazy_segment_list defers (stays lazy)", {
  ae <- create_isolated_ae_corpus()
  lz <- query(ae, "Phonetic == n", lazy = TRUE)

  b <- biographize(lz, ae)

  expect_true(S7::S7_inherits(b, lazy_segment_list))
  expect_false(b@.state$materialized)
  expect_equal(length(b@query_parts$post_transforms), 1L)
  expect_equal(b@query_parts$post_transforms[[1]]$type, "biographize")
})

test_that("enrich(metadata) on a lazy_segment_list defers via biographize", {
  ae <- create_isolated_ae_corpus()
  lz <- query(ae, "Phonetic == n", lazy = TRUE)

  out <- enrich(lz, ae)  # metadata path (default with = "metadata")

  expect_true(S7::S7_inherits(out, lazy_segment_list))
  expect_false(out@.state$materialized)
  expect_equal(out@query_parts$post_transforms[[1]]$type, "biographize")
})

test_that("enrich(.using) on a lazy_segment_list defers via quantify", {
  ae <- create_isolated_ae_corpus()
  lz <- query(ae, "Phonetic == n", lazy = TRUE)

  called <- 0L
  fake_dsp <- function(file, ...) { called <<- called + 1L; list(f0 = 1) }

  out <- enrich(lz, .using = fake_dsp)

  expect_true(S7::S7_inherits(out, lazy_segment_list))
  expect_equal(out@query_parts$post_transforms[[1]]$type, "quantify")
  expect_equal(called, 0L)  # DSP not run until collect()
})

test_that("collect() replays deferred biographize and matches the eager path", {
  ae <- create_isolated_ae_corpus()

  lazy_out  <- collect(biographize(query(ae, "Phonetic == n", lazy = TRUE), ae))
  eager_out <- biographize(query(ae, "Phonetic == n", lazy = FALSE), ae)

  expect_equal(nrow(lazy_out), nrow(eager_out))
  expect_setequal(names(lazy_out), names(eager_out))
})

test_that("nav verbs materialize by default (so loss reporting fires) but defer on collect = FALSE", {
  ae <- create_isolated_ae_corpus()

  # Default: eager, so provenance/loss reporting runs immediately.
  eager <- scout(query(ae, "Phonetic == n", lazy = TRUE), 1)
  expect_true(S7::S7_inherits(eager, segment_list))

  # Opt-in lazy navigation stays deferred.
  lazy <- scout(query(ae, "Phonetic == n", lazy = TRUE), 1, collect = FALSE)
  expect_true(S7::S7_inherits(lazy, lazy_segment_list))
  expect_false(lazy@.state$materialized)
})

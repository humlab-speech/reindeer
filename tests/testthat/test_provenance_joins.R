# ============================================================================
# Provenance tracking across dplyr *_join verbs.
# v0.7.0 added named provenance entries for left/right/inner/full/anti/semi
# joins on segment_list (replacing the generic "dplyr_op" label).
# ============================================================================

library(testthat)
library(reindeer)

test_that("inner_join records a named provenance step", {
  skip_if_not_installed("dplyr")
  ae <- create_isolated_ae_corpus()
  segs <- collect(query(ae, "Phonetic == n"))
  # Build a small lookup that drops every other bundle so we can assert loss.
  bundles <- unique(segs$bundle)
  keep <- bundles[seq_along(bundles) %% 2 == 1]
  lookup <- data.frame(bundle = keep, region = "kept", stringsAsFactors = FALSE)

  withr::with_options(list(reindeer.loss_warn = 1.0), {
    joined <- dplyr::inner_join(segs, lookup, by = "bundle")
  })

  prov <- provenance(joined)
  expect_gt(nrow(prov), nrow(provenance(segs)))
  expect_equal(tail(prov$verb, 1), "inner_join")
})

test_that("anti_join records a named provenance step", {
  skip_if_not_installed("dplyr")
  ae <- create_isolated_ae_corpus()
  segs <- collect(query(ae, "Phonetic == n"))
  bundles <- unique(segs$bundle)
  drop <- data.frame(bundle = bundles[1], stringsAsFactors = FALSE)
  withr::with_options(list(reindeer.loss_warn = 1.0), {
    aj <- dplyr::anti_join(segs, drop, by = "bundle")
  })
  prov <- provenance(aj)
  expect_equal(tail(prov$verb, 1), "anti_join")
})

test_that("join loss above threshold fires a warning", {
  skip_if_not_installed("dplyr")
  ae <- create_isolated_ae_corpus()
  segs <- collect(query(ae, "Phonetic == n"))
  empty <- data.frame(bundle = "no_such_bundle", stringsAsFactors = FALSE)
  withr::with_options(list(reindeer.loss_warn = 0.0), {
    expect_warning(
      dplyr::inner_join(segs, empty, by = "bundle"),
      regexp = "row.*lost|inner_join"
    )
  })
})

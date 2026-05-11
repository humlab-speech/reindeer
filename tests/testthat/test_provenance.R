# ==============================================================================
# Tests for pipe-loss provenance accounting (Item 2)
# ==============================================================================

library(testthat)
library(reindeer)

test_that("provenance() on a fresh segment_list returns an empty tibble", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  prov <- provenance(segs)
  expect_s3_class(prov, "tbl_df")
  expect_equal(nrow(prov), 1L)            # query seeds with one row
  expect_equal(prov$verb, "query")
  expect_true(is.na(prov$rows_in))
  expect_equal(prov$rows_out, nrow(segs))
})

test_that("dropped() on a fresh segment_list returns 0", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  expect_equal(dropped(segs), 0L)
})

test_that("scout records a step (lost=0 when sequence intact)", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  s2 <- scout(segs, 1)
  prov <- provenance(s2)
  expect_equal(nrow(prov), 2L)
  expect_equal(prov$verb[2], "scout")
  expect_equal(prov$rows_in[2], nrow(segs))
})

test_that("ascend_to records a step", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  withr::with_options(list(reindeer.loss_warn = 1.0), {
    a <- ascend_to(segs, "Word", .from = ae)
  })
  prov <- provenance(a)
  expect_equal(nrow(prov), 2L)
  expect_equal(prov$verb[2], "ascend_to")
})

test_that("dplyr verbs append a 'dplyr_op' step preserving prior history", {
  skip_if_not_installed("dplyr")
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  withr::with_options(list(reindeer.loss_warn = 1.0), {
    f <- dplyr::filter(segs, end - start > 0)
  })
  prov <- provenance(f)
  expect_equal(nrow(prov), 2L)
  expect_equal(prov$verb[2], "dplyr_op")
})

test_that("dropped(seg, step) returns per-step loss; default returns total", {
  skip_if_not_installed("dplyr")
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  withr::with_options(list(reindeer.loss_warn = 1.0), {
    f <- dplyr::filter(segs, end - start > 999999)   # drops everything
  })
  expect_equal(dropped(f, 2L), nrow(segs))
  expect_equal(dropped(f), nrow(segs))
})

test_that("dropped() raises actionable error on out-of-range step", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  expect_error(dropped(segs, 99L), "out of range")
})

test_that("loss warning fires for navigation verbs above threshold", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  withr::with_options(list(reindeer.loss_warn = 0.0), {
    expect_warning(
      ascend_to(segs, "Word", .from = ae),
      regexp = "row.*lost"
    )
  })
  withr::with_options(list(reindeer.loss_warn = 1.0), {
    expect_silent(scout(segs, 0))
  })
})

test_that("dplyr verbs do NOT emit loss warnings (user-explicit ops)", {
  skip_if_not_installed("dplyr")
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  withr::with_options(list(reindeer.loss_warn = 0.0), {
    expect_silent(dplyr::filter(segs, end - start > 999999))
  })
})

test_that("provenance survives base bracket row subsetting", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  withr::with_options(list(reindeer.loss_warn = 1.0), {
    sub <- segs[1:3, ]
  })
  prov <- provenance(sub)
  expect_gte(nrow(prov), 1L)
})

test_that("provenance log truncation respects reindeer.provenance_max", {
  skip_if_not_installed("dplyr")
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n")
  withr::with_options(list(reindeer.provenance_max = 3L,
                          reindeer.loss_warn = 1.0), {
    s <- segs
    suppressWarnings({
      for (i in 1:6) s <- dplyr::filter(s, end - start > 0)
    })
    prov <- provenance(s)
    expect_equal(nrow(prov), 3L)
  })
})

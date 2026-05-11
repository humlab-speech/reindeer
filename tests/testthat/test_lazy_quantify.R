# ==============================================================================
# Tests for deferred quantify on lazy_segment_list (Item 1b)
# ==============================================================================

library(testthat)
library(reindeer)

test_that("quantify on a lazy_segment_list defers DSP and returns lazy", {
  ae <- create_isolated_ae_corpus()
  lazy <- ask_for(ae, "Phonetic == n", lazy = TRUE)

  call_count <- 0L
  fake_dsp <- function(file, ...) {
    call_count <<- call_count + 1L
    list(f0 = c(120, 130, 140))
  }

  lazy2 <- quantify(lazy, fake_dsp)

  expect_true(S7::S7_inherits(lazy2, lazy_segment_list))
  expect_false(S7::S7_inherits(lazy2, segment_list))
  expect_equal(length(lazy2@query_parts$post_transforms), 1L)
  expect_equal(lazy2@query_parts$post_transforms[[1]]$type, "quantify")
  # DSP function not called yet
  expect_equal(call_count, 0L)
})

test_that("quantify on lazy validates dsp_function input", {
  ae <- create_isolated_ae_corpus()
  lazy <- ask_for(ae, "Phonetic == n", lazy = TRUE)

  expect_error(quantify(lazy, dsp_function = 42),
               regexp = "dsp_function")
})

test_that("multiple deferred quantify calls accumulate post_transforms", {
  ae <- create_isolated_ae_corpus()
  lazy <- ask_for(ae, "Phonetic == n", lazy = TRUE)
  fake1 <- function(file, ...) list(a = 1)
  fake2 <- function(file, ...) list(b = 2)

  lazy2 <- quantify(lazy, fake1)
  lazy3 <- quantify(lazy2, fake2)

  expect_equal(length(lazy3@query_parts$post_transforms), 2L)
})

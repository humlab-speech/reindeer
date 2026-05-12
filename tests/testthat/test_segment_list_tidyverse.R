# ==============================================================================
# Tests for tidyverse compatibility of segment_list (Item 1)
# ==============================================================================

library(testthat)
library(reindeer)

minimal_seg_df <- function(n = 3) {
  data.frame(
    labels = rep("a", n),
    start = seq(0, by = 100, length.out = n),
    end = seq(50, by = 100, length.out = n),
    db_uuid = "u",
    session = "s",
    bundle = "b",
    start_item_id = seq_len(n),
    end_item_id = seq_len(n),
    level = "L",
    attribute = "A",
    start_item_seq_idx = seq_len(n),
    end_item_seq_idx = seq_len(n),
    type = "SEGMENT",
    sample_start = 0L,
    sample_end = 10L,
    sample_rate = 16000
  )
}

test_that("segment_list inherits tbl_df, tbl, and data.frame", {
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  expect_s3_class(s, "tbl_df")
  expect_s3_class(s, "tbl")
  expect_s3_class(s, "data.frame")
  expect_true(is_segment_list(s))
  expect_identical(s@db_uuid, "u")
  expect_identical(s@db_path, "/tmp/x")
})

test_that("query returns a tbl_df segment_list (via collect)", {
  ae <- create_isolated_ae_corpus()
  # Default is lazy as of v0.7.0; collect() materialises a segment_list.
  segs <- collect(query(ae, "Phonetic == n"))
  expect_s3_class(segs, "tbl_df")
  expect_true(is_segment_list(segs))
})

test_that("base bracket row subset preserves segment_list and properties", {
  s <- segment_list(minimal_seg_df(5), db_uuid = "u", db_path = "/tmp/x")
  b <- s[1:3, ]
  expect_true(is_segment_list(b))
  expect_identical(b@db_uuid, "u")
  expect_identical(b@db_path, "/tmp/x")
  expect_equal(nrow(b), 3L)
})

test_that("base bracket col subset dropping required cols downcasts to tibble", {
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  c <- s[, c("labels", "start")]
  expect_false(is_segment_list(c))
  expect_s3_class(c, "tbl_df")
})

test_that("single-arg bracket selects columns (tibble semantics)", {
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  one <- s[c("labels", "start")]
  expect_equal(ncol(one), 2L)
  expect_s3_class(one, "tbl_df")
  expect_false(is_segment_list(one))
})

test_that("dplyr::filter preserves segment_list and properties", {
  skip_if_not_installed("dplyr")
  s <- segment_list(minimal_seg_df(5), db_uuid = "u", db_path = "/tmp/x")
  f <- dplyr::filter(s, start > 50)
  expect_true(is_segment_list(f))
  expect_identical(f@db_uuid, "u")
  expect_identical(f@db_path, "/tmp/x")
  expect_lt(nrow(f), nrow(s))
})

test_that("dplyr::mutate preserves segment_list and properties", {
  skip_if_not_installed("dplyr")
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  m <- dplyr::mutate(s, dur = end - start)
  expect_true(is_segment_list(m))
  expect_identical(m@db_uuid, "u")
  expect_true("dur" %in% names(m))
})

test_that("dplyr::arrange preserves segment_list and properties", {
  skip_if_not_installed("dplyr")
  s <- segment_list(minimal_seg_df(5), db_uuid = "u", db_path = "/tmp/x")
  a <- dplyr::arrange(s, dplyr::desc(start))
  expect_true(is_segment_list(a))
  expect_identical(a@db_uuid, "u")
  expect_equal(nrow(a), nrow(s))
})

test_that("dplyr::select keeping all required cols preserves segment_list", {
  skip_if_not_installed("dplyr")
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  sel <- dplyr::select(s, dplyr::everything())
  expect_true(is_segment_list(sel))
  expect_identical(sel@db_uuid, "u")
})

test_that("dplyr::select dropping required cols downcasts to tibble", {
  skip_if_not_installed("dplyr")
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  sel <- dplyr::select(s, labels, start)
  expect_false(is_segment_list(sel))
  expect_s3_class(sel, "tbl_df")
})

test_that("group_by + summarise returns plain (grouped) tibble", {
  skip_if_not_installed("dplyr")
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  g <- s |> dplyr::group_by(level) |> dplyr::summarise(n = dplyr::n())
  expect_false(is_segment_list(g))
  expect_s3_class(g, "tbl_df")
})

test_that("extended_segment_list inherits tbl_df and preserves dsp props through dplyr verbs", {
  skip_if_not_installed("dplyr")
  s <- segment_list(minimal_seg_df(), db_uuid = "u", db_path = "/tmp/x")
  esl <- extended_segment_list(
    s, db_uuid = "u", db_path = "/tmp/x",
    dsp_function = "fake", dsp_columns = character(0)
  )
  expect_s3_class(esl, "tbl_df")
  expect_true(is_extended_segment_list(esl))

  m <- dplyr::mutate(esl, foo = 1)
  expect_true(is_extended_segment_list(m))
  expect_identical(m@dsp_function, "fake")
})

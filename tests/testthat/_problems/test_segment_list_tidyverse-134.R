# Extracted from test_segment_list_tidyverse.R:134

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
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

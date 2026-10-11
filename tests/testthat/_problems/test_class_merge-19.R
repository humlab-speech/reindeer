# Extracted from test_class_merge.R:19

# prequel ----------------------------------------------------------------------
fake_track <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
  data.frame(F1 = c(500, 510), F2 = c(1500, 1510))
}

# test -------------------------------------------------------------------------
skip_if_no_emuR()
corp <- create_isolated_ae_corpus()
segs <- collect(query(corp, "Phonetic == t"))
out <- quantify(segs, fake_track)
expect_true(S7::S7_inherits(out, segment_list))
expect_false("reindeer::extended_segment_list" %in% class(out))
expect_true(is_extended_segment_list(out))
expect_equal(out@dsp_columns, c("F1", "F2"))
expect_false(is_extended_segment_list(segs))
m <- dplyr::mutate(out, k = 1)
expect_equal(m@dsp_columns, c("F1", "F2"))

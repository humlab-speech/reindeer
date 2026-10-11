# Extracted from test_class_merge.R:37

# prequel ----------------------------------------------------------------------
fake_track <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
  data.frame(F1 = c(500, 510), F2 = c(1500, 1510))
}

# test -------------------------------------------------------------------------
skip_if_no_emuR()
corp <- create_isolated_ae_corpus()
lz <- quantify(query(corp, "Phonetic == t"), fake_track)
expect_true(S7::S7_inherits(lz, lazy_segment_list))
long <- pivot_tracks_longer(lz, cols = c("F1", "F2"))
expect_true(all(c("track", "value") %in% names(as.data.frame(long))))

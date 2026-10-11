# WP5.1: one segment_list class; lazy plans are first-class.

fake_track <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
  data.frame(F1 = c(500, 510), F2 = c(1500, 1510))
}

test_that("quantify() returns a plain segment_list carrying dsp props", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == t"))
  out <- quantify(segs, fake_track)
  expect_true(S7::S7_inherits(out, segment_list))
  expect_false("reindeer::extended_segment_list" %in% class(out))
  expect_true(is_extended_segment_list(out))
  expect_equal(out@dsp_columns, c("F1", "F2"))
  expect_false(is_extended_segment_list(segs))
  # props survive dplyr verbs
  m <- dplyr::mutate(out, k = 1)
  expect_equal(m@dsp_columns, c("F1", "F2"))
})

test_that("is_segment_list() is TRUE for lazy plans", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  lz <- query(corp, "Phonetic == t")
  expect_true(is_segment_list(lz))
  expect_true(is_segment_list(collect(lz)))
  expect_false(is_segment_list(data.frame()))
})

test_that("pivot_tracks_longer() and autoplot() accept a lazy quantify plan", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  lz <- quantify(query(corp, "Phonetic == t"), fake_track)
  expect_true(S7::S7_inherits(lz, lazy_segment_list))
  long <- pivot_tracks_longer(lz, cols = c("F1", "F2"))
  expect_s3_class(long, "track_long")
  skip_if_not_installed("ggplot2")
  expect_s3_class(ggplot2::autoplot(lz), "ggplot")
})

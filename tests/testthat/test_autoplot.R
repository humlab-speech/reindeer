# Tests for ggplot2 autoplot + helper geoms.

skip_if_no_emuR()
skip_if_not_installed("ggplot2")

make_wide_seg <- function() {
  ae <- create_shared_ae_corpus()
  segs <- collect(query(ae, "Phonetic == V"))
  n <- nrow(segs)
  segs_df <- tibble::as_tibble(.vec_proxy_segment_list(segs))
  rel_times <- c("0", "0.5", "1")
  cols <- c(paste0("F1_", rel_times), paste0("F2_", rel_times))
  for (col in cols) {
    segs_df[[col]] <- runif(n, 300, 2500)
  }
  list(
    seg = segs,
    wide = extended_segment_list(
      segs_df,
      db_uuid = segs@db_uuid,
      db_path = segs@db_path,
      dsp_function = "synthetic",
      dsp_columns = cols
    )
  )
}

test_that("autoplot.reindeer__segment_list returns a ggplot for labels view", {
  fixt <- make_wide_seg()
  p <- ggplot2::autoplot(fixt$seg, type = "labels")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot.reindeer__extended_segment_list draws formants by default", {
  fixt <- make_wide_seg()
  p <- ggplot2::autoplot(fixt$wide, type = "formants")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot type='auto' detects formants from wide F1_/F2_ columns", {
  fixt <- make_wide_seg()
  p <- ggplot2::autoplot(fixt$wide)
  expect_s3_class(p, "ggplot")
})

test_that("autoplot pitch fails when no F0/pitch column present", {
  fixt <- make_wide_seg()
  expect_error(
    ggplot2::autoplot(fixt$wide, type = "pitch"),
    "pitch"
  )
})

test_that("autoplot spectrogram falls back to labels view", {
  fixt <- make_wide_seg()
  expect_message(
    p <- ggplot2::autoplot(fixt$wide, type = "spectrogram"),
    "Spectrogram"
  )
  expect_s3_class(p, "ggplot")
})

test_that("geom_formant_trajectory accepts pivot_tracks_longer output", {
  fixt <- make_wide_seg()
  long <- pivot_tracks_longer(fixt$wide)
  p <- ggplot2::ggplot(long) + geom_formant_trajectory()
  expect_s3_class(p, "ggplot")
})

test_that("geom_pitch_track filters to F0-like tracks", {
  fixt <- make_wide_seg()
  long <- pivot_tracks_longer(fixt$wide)
  # Inject a fake pitch track
  long_pitch <- long
  long_pitch$track <- "F0"
  p <- ggplot2::ggplot(long_pitch) + geom_pitch_track()
  expect_s3_class(p, "ggplot")
})

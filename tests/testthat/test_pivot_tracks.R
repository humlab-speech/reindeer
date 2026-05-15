# Tests for Feature 1: track-aware pivot, nest, tidyselect.
# Use a hand-rolled wide segment_list (no DSP dependency) so these run fast
# and do not require superassp.

skip_if_no_emuR()

make_wide_seg <- function() {
  ae <- create_shared_ae_corpus()
  segs <- collect(query(ae, "Phonetic == V"))
  # Synthetic wide track columns mirroring quantify(.at = c(0, 0.5, 1))
  n <- nrow(segs)
  segs_df <- tibble::as_tibble(.vec_proxy_segment_list(segs))
  rel_times <- c("0", "0.5", "1")
  track_cols_built <- c(paste0("F1_", rel_times), paste0("F2_", rel_times))
  for (col in track_cols_built) {
    segs_df[[col]] <- runif(n)
  }
  list(
    seg = segs,
    wide = extended_segment_list(
      segs_df,
      db_uuid = segs@db_uuid,
      db_path = segs@db_path,
      dsp_function = "synthetic",
      dsp_columns = track_cols_built
    )
  )
}

test_that("track_cols() detects wide-form track columns", {
  fixt <- make_wide_seg()
  hits <- dplyr::select(fixt$wide, track_cols())
  expect_true(all(grepl("^F[12]_", names(hits))))
  expect_equal(ncol(hits), 6)
})

test_that("metadata_cols() excludes required and track columns", {
  fixt <- make_wide_seg()
  meta <- dplyr::select(fixt$wide, metadata_cols())
  # No required or track columns should appear
  expect_false(any(.required_segment_cols() %in% names(meta)))
  expect_false(any(grepl("^F[12]_", names(meta))))
})

test_that("segment_cols() returns only required columns present", {
  fixt <- make_wide_seg()
  s <- dplyr::select(fixt$wide, segment_cols())
  expect_setequal(names(s), .required_segment_cols())
})

test_that("pivot_tracks_longer expands wide -> long", {
  fixt <- make_wide_seg()
  long <- pivot_tracks_longer(fixt$wide)
  expect_s3_class(long, "track_long")
  expect_true(all(c("track", "rel_time", "value") %in% names(long)))
  # 6 wide cols * input rows = expected long rows
  expect_equal(nrow(long), nrow(fixt$wide) * 6)
  expect_setequal(unique(long$track), c("F1", "F2"))
  expect_setequal(unique(long$rel_time), c(0.0, 0.5, 1.0))
})

test_that("pivot_tracks_wider round-trips", {
  fixt <- make_wide_seg()
  long <- pivot_tracks_longer(fixt$wide)
  wide_again <- pivot_tracks_wider(long)
  # All original track columns present after round-trip
  track_names <- paste(
    rep(c("F1", "F2"), each = 3),
    rep(c("0", "0.5", "1"), 2),
    sep = "_"
  )
  # Allow either "F1_0" or "F1_0.0" form due to numeric-to-character coercion
  expanded <- c(track_names,
                paste(rep(c("F1", "F2"), each = 3),
                      rep(c("0.0", "0.5", "1.0"), 2),
                      sep = "_"))
  hits <- intersect(names(wide_again), expanded)
  expect_gte(length(hits), 6)
})

test_that("nest_by_session returns one row per session with segment_list cells", {
  fixt <- make_wide_seg()
  nested <- nest_by_session(fixt$seg)
  expect_true("session" %in% names(nested))
  expect_true("data" %in% names(nested))
  expect_equal(nrow(nested), length(unique(fixt$seg$session)))
  expect_true(all(vapply(nested$data, is_segment_list, logical(1))))
})

test_that("nest_by_bundle preserves segment_list inner cells", {
  fixt <- make_wide_seg()
  nested <- nest_by_bundle(fixt$seg)
  expect_true(all(c("session", "bundle", "data") %in% names(nested)))
  expect_true(all(vapply(nested$data, is_segment_list, logical(1))))
})

test_that("nest preserves db_uuid via attribute and provenance step", {
  fixt <- make_wide_seg()
  nested <- nest_by_session(fixt$seg)
  expect_equal(attr(nested, "db_uuid"), fixt$seg@db_uuid)
  prov <- attr(nested, "reindeer_provenance")
  expect_true(!is.null(prov))
  expect_true(any(grepl("nest_by_session", prov$verb)))
})

test_that("pivot_tracks_longer adds a provenance step", {
  fixt <- make_wide_seg()
  long <- pivot_tracks_longer(fixt$wide)
  prov <- attr(long, "reindeer_provenance")
  expect_true(!is.null(prov))
  expect_true(any(prov$verb == "pivot_tracks_longer"))
})

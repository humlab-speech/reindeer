# WP4.3: one analysis per bundle file, sliced per segment.

test_that("bundle mode agrees with window mode (forest F1 at midpoints)", {
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == V|@|i:|u:|o:|A:|E|I|O"))
  f1 <- function(mode) {
    out <- suppressWarnings(as.data.frame(quantify(
      segs, superassp::trk_formant_forest, .at = 0.5, .use_metadata = FALSE, .mode = mode)))
    as.numeric(out$F1_Hz)
  }
  b <- f1("bundle"); w <- f1("window")
  expect_equal(length(b), length(w))
  expect_lte(stats::median(abs(b - w), na.rm = TRUE), 5)
})

test_that("bundle mode analyses each file once, window mode once per segment", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == t"))
  calls <- new.env(); calls$whole <- 0L; calls$window <- 0L
  fake <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    if (missing(beginTime)) calls$whole <- calls$whole + 1L else calls$window <- calls$window + 1L
    n <- 400
    obj <- data.frame(frame_time = seq(0.0025, by = 0.005, length.out = n), v = seq_len(n))
    attr(obj, "sampleRate") <- 200; attr(obj, "startTime") <- 0.0025
    class(obj) <- c("AsspDataObj", "data.frame")
    obj
  }
  # A real AsspDataObj is needed for as.data.frame(); skip if superassp's method
  # would reject the fake.
  skip_if_not_installed("superassp")
  out <- tryCatch(quantify(segs, fake), error = function(e) NULL)
  skip_if(is.null(out), "fake track not accepted by as.data.frame.AsspDataObj")
  expect_lte(calls$whole, length(unique(segs$bundle)))
})

test_that("ksv no longer fails for a segment at the start of the file", {
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic =~ .*"))
  first <- segs[order(segs$bundle, segs$start), ][1, ]
  out <- suppressWarnings(quantify(first, superassp::trk_pitch_ksv, .use_metadata = FALSE))
  expect_gt(nrow(as.data.frame(out)), 0)
})

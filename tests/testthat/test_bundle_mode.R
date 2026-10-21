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
  skip_if_no_emuR(); skip_if_no_superassp()
  loadNamespace("superassp")  # registers as.data.frame.AsspDataObj
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == t"))
  calls <- new.env()
  fake <- function(listOfFiles, beginTime = NULL, endTime = NULL, toFile = FALSE,
                   verbose = FALSE, ...) {
    if (is.null(beginTime)) calls$whole <- calls$whole + 1L else calls$window <- calls$window + 1L
    obj <- list(v = matrix(seq_len(400), ncol = 1))
    attr(obj, "sampleRate") <- 200; attr(obj, "startTime") <- 0.0025
    class(obj) <- "AsspDataObj"
    obj
  }
  for (mode in c("bundle", "window")) {
    calls$whole <- 0L; calls$window <- 0L
    quantify(segs, fake, .mode = mode, .use_cache = FALSE)
    if (mode == "bundle") {
      expect_equal(calls$whole, length(unique(segs$bundle)))
      expect_equal(calls$window, 0L)
    } else {
      expect_equal(calls$whole, 0L)
      expect_equal(calls$window, nrow(segs))
    }
  }
})

test_that("ksv no longer fails for a segment at the start of the file", {
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic =~ .*"))
  first <- segs[order(segs$bundle, segs$start), ][1, ]
  out <- suppressWarnings(quantify(first, superassp::trk_pitch_ksv, .use_metadata = FALSE))
  expect_gt(nrow(as.data.frame(out)), 0)
})

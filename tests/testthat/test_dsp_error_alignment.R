# A4 / G0.4: a DSP error yields NA rows for the segments it hit, with one
# reindeer_dsp_warning; results for other segments stay on their own rows, and
# no failure is silently re-measured another way.

fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE,
                     verbose = FALSE, fail_at = numeric(), ...) {
  if (any(abs(beginTime - fail_at) < 1e-9)) stop("boom")
  data.frame(marker = beginTime)
}

run_fake <- function(segs, fail_at, use_cache) {
  out <- quantify(segs, fake_dsp, .use_cache = use_cache, fail_at = fail_at)
  as.data.frame(out)
}

test_that("DSP error on one segment gives NA there and does not shift other results", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))[1:12, ]
  starts <- segs$start / 1000

  for (use_cache in c(TRUE, FALSE)) {
    ok <- run_fake(segs, numeric(), use_cache)
    for (bad in c(4L, 12L)) {
      expect_warning(out <- run_fake(segs, starts[bad], use_cache),
                     class = "reindeer_dsp_warning")
      expect_equal(nrow(out), nrow(ok), info = paste(use_cache, bad))
      expect_true(is.na(out$marker[bad]))
      expect_equal(out$marker[-bad], ok$marker[-bad], info = paste(use_cache, bad))
      expect_equal(out$start, ok$start, info = paste(use_cache, bad))
    }
  }
})

test_that("a whole-file DSP error is reported, not silently re-measured per window (R5)", {
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic =~ .*"))
  calls <- new.env(); calls$n <- 0L
  flaky <- function(listOfFiles, ...) {
    calls$n <- calls$n + 1L
    if (calls$n == 2L) stop("boom")
    superassp::trk_formant_forest(listOfFiles, ...)
  }
  ok <- as.data.frame(quantify(segs, superassp::trk_formant_forest, .at = 0.5,
                               .use_metadata = FALSE, .use_cache = FALSE))
  expect_warning(
    out <- as.data.frame(quantify(segs, flaky, .at = 0.5,
                                  .use_metadata = FALSE, .use_cache = FALSE)),
    class = "reindeer_dsp_warning")
  hit <- is.na(out$F1_Hz) & !is.na(ok$F1_Hz)
  expect_gt(sum(hit), 0)
  expect_length(unique(out$bundle[hit]), 1L)
  expect_identical(as.numeric(out$F1_Hz[!hit]), as.numeric(ok$F1_Hz[!hit]))
})

test_that("corpus mode aborts, DBconfig untouched, when the routine writes nothing", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  before <- reindeer:::load_DBconfig(corp)
  silent <- function(listOfFiles, toFile = FALSE, verbose = FALSE, ...) invisible(1L)
  attr(silent, "ext") <- "zz"; attr(silent, "tracks") <- "zz"
  expect_error(quantify(corp, .using = silent, .verbose = FALSE),
               class = "reindeer_dsp_error")
  expect_identical(reindeer:::load_DBconfig(corp)$ssffTrackDefinitions,
                   before$ssffTrackDefinitions)
})

test_that("corpus mode keeps an existing track file when the routine fails", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  fms <- list.files(corp@basePath, "\\.fms$", recursive = TRUE, full.names = TRUE)
  md5 <- tools::md5sum(fms)
  broken <- function(listOfFiles, toFile = FALSE, verbose = FALSE, ...) stop("boom")
  attr(broken, "ext") <- "fms"; attr(broken, "tracks") <- c("F[Hz]", "B[Hz]")
  expect_error(quantify(corp, .using = broken, name = c("Fx", "Bx"), .verbose = FALSE),
               class = "reindeer_dsp_error")
  expect_identical(tools::md5sum(fms), md5)
})

test_that("measurement paths do not turn errors into NULL", {
  fns <- list(reindeer:::.dsp_whole_table, reindeer:::.run_dsp_windows,
              reindeer:::.read_track_table, reindeer:::.discover_written_column_names,
              reindeer:::.quantify_segment_list_by_name)
  for (f in fns) {
    expect_false(any(grepl("function(e) NULL", deparse(f), fixed = TRUE)))
  }
})

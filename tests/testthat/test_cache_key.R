# A5: the persistent quantify cache key must change with the function, the
# signal file and the parameters.

dsp_a <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
  data.frame(v = 1)
}
dsp_b <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
  data.frame(v = 2)
}

cache_setup <- function() {
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))[1:6, ]
  list(corp = corp, segs = segs, dir = tempfile("qcache_"))
}
status <- function(x) unique(as.data.frame(x)$.cache_status)

test_that("replacing the signal file invalidates cached results", {
  skip_if_no_emuR()
  fx <- cache_setup()
  q <- function() quantify(fx$segs, dsp_a, .use_cache = TRUE, .cache_dir = fx$dir)
  expect_equal(status(q()), "miss")
  expect_equal(status(q()), "hit")
  wavs <- list.files(fx$corp@basePath, "\\.wav$", recursive = TRUE, full.names = TRUE)
  Sys.setFileTime(wavs, Sys.time() + 5)
  expect_equal(status(q()), "miss")
})

test_that("a different function under the same name misses", {
  skip_if_no_emuR()
  fx <- cache_setup()
  f <- dsp_a
  r1 <- quantify(fx$segs, f, .use_cache = TRUE, .cache_dir = fx$dir)
  f <- dsp_b
  r2 <- quantify(fx$segs, f, .use_cache = TRUE, .cache_dir = fx$dir)
  expect_equal(status(r2), "miss")
  expect_equal(unique(as.data.frame(r2)$v), 2)
})

test_that("the same function under another name hits", {
  skip_if_no_emuR()
  fx <- cache_setup()
  quantify(fx$segs, dsp_a, .use_cache = TRUE, .cache_dir = fx$dir)
  g <- dsp_a
  r <- quantify(fx$segs, g, .use_cache = TRUE, .cache_dir = fx$dir)
  expect_equal(status(r), "hit")
})

test_that("bundle-table cache: warm re-run hits, and changing .at or the segment set still hits", {
  skip_if_no_emuR(); skip_if_no_superassp()
  fx <- cache_setup()
  segs <- collect(query(fx$corp, "Phonetic =~ .*"))
  q <- function(s, ...) quantify(s, superassp::trk_formant_forest, .cache_dir = fx$dir,
                                 .use_metadata = FALSE, ...)
  expect_equal(status(suppressWarnings(q(segs, .at = 0.5))), "miss")
  expect_equal(status(suppressWarnings(q(segs, .at = 0.5))), "hit")
  expect_equal(status(suppressWarnings(q(segs, .at = 0.25))), "hit")
  expect_equal(status(suppressWarnings(q(segs[1:10, ], .at = 0.5))), "hit")
})

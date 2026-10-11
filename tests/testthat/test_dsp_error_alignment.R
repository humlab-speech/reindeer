# A4: a DSP error on one segment must yield NA for that segment only; results
# for other segments must not shift onto the wrong rows.

fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE,
                     verbose = FALSE, fail_at = numeric(), ...) {
  if (any(abs(beginTime - fail_at) < 1e-9)) stop("boom")
  data.frame(marker = beginTime)
}

run_fake <- function(segs, fail_at, use_cache) {
  out <- quantify(segs, fake_dsp, .use_cache = use_cache, fail_at = fail_at)
  as.data.frame(out)
}

test_that("DSP error on one segment does not shift other results", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))[1:12, ]
  starts <- segs$start / 1000

  for (use_cache in c(TRUE, FALSE)) {
    ok <- run_fake(segs, numeric(), use_cache)
    for (bad in c(4L, 12L)) {
      out <- run_fake(segs, starts[bad], use_cache)
      expect_equal(nrow(out), nrow(ok) - 1L, info = paste(use_cache, bad))
      # Every surviving row keeps its own marker.
      expect_equal(out$marker, ok$marker[-bad], info = paste(use_cache, bad))
      expect_equal(out$start, ok$start[-bad], info = paste(use_cache, bad))
    }
  }
})

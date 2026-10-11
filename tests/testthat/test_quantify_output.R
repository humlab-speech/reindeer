# Quantify output contract: input row order, no internal columns.

tone <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
  data.frame(val = beginTime * 1000)
}
internal_cols <- c("seg_params", "signal_file", "file_exists", "cache_key",
                   "cached_result", "file_group_id", "seg_params_digest",
                   "result", ".row_id")

test_that("output keeps input order and exposes no internal columns", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  all_segs <- collect(query(corp, "Phoneme =~ .+"))
  # Shuffle so that input order differs from session/bundle/start order.
  set.seed(1)
  for (n in c(10L, 60L, 150L)) {
    if (nrow(all_segs) < n) next
    segs <- all_segs[sample(nrow(all_segs), n), ]
    for (use_cache in c(FALSE, TRUE)) {
      dir <- tempfile("qc_")
      for (pass in 1:2) {          # cold, then warm
        out <- as.data.frame(quantify(segs, tone, .use_cache = use_cache,
                                      .cache_dir = dir))
        expect_equal(out$start, segs$start, info = paste(n, use_cache, pass))
        expect_equal(out$start_item_id, segs$start_item_id)
        expect_length(intersect(names(out), internal_cols), 0L)
        expect_true(all(names(segs) %in% names(out)))
      }
    }
  }
})

test_that(".at on a routine that returns a plain table is an error, not ignored", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))[1:5, ]
  expect_error(quantify(segs, tone, .at = 0.5), class = "reindeer_quantify_error")
})

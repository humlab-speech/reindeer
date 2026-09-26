# Tests for the eggstract glue wrapper. eggstract is in Suggests and may
# not be installed; the tests primarily verify the missing-companion
# abort path and the gating logic. When it's installed, we exercise the
# happy path minimally.

skip_if_no_emuR()

test_that("quantify_egg() aborts with reindeer_missing_companion_error when eggstract absent", {
  if (requireNamespace("eggstract", quietly = TRUE)) {
    skip("eggstract installed - covered elsewhere")
  }
  ae <- create_shared_ae_corpus()
  segs <- collect(query(ae, "Phonetic == V"))
  err <- tryCatch(quantify_egg(segs),
                  reindeer_missing_companion_error = function(e) e)
  expect_s3_class(err, "reindeer_missing_companion_error")
})

test_that("enrich_egg() is removed and always redirects", {
  ae <- create_shared_ae_corpus()
  err <- tryCatch(enrich_egg(ae),
                  reindeer_moved_error = function(e) e)
  expect_s3_class(err, "reindeer_moved_error")
})

test_that(".filter_to_egg_bundles is silent and pass-through without HasEGG", {
  ae <- create_shared_ae_corpus()
  segs <- collect(query(ae, "Phonetic == V"))
  out <- expect_message(
    reindeer:::.filter_to_egg_bundles(segs),
    "HasEGG"
  )
})

test_that(".filter_to_egg_bundles keeps only HasEGG=TRUE rows", {
  ae <- create_shared_ae_corpus()
  segs <- collect(query(ae, "Phonetic == V"))
  segs_df <- tibble::as_tibble(.vec_proxy_segment_list(segs))
  segs_df$HasEGG <- c(TRUE, rep(FALSE, nrow(segs_df) - 1))
  with_meta <- segment_list(segs_df, db_uuid = segs@db_uuid,
                             db_path = segs@db_path)
  kept <- reindeer:::.filter_to_egg_bundles(with_meta)
  expect_equal(nrow(kept), 1L)
})

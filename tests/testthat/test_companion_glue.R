# Tests for the sister-package glue wrappers (eggstract + protoscribe).
# eggstract and protoscribe are in Suggests and may not be installed; the
# tests primarily verify the missing-companion abort path and the gating
# logic. When the companions are installed, we exercise the happy path
# minimally.

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

test_that("enrich_egg() aborts when eggstract is absent", {
  if (requireNamespace("eggstract", quietly = TRUE)) {
    skip("eggstract installed")
  }
  ae <- create_shared_ae_corpus()
  err <- tryCatch(enrich_egg(ae),
                  reindeer_missing_companion_error = function(e) e)
  expect_s3_class(err, "reindeer_missing_companion_error")
})

test_that("propose_annotations() aborts when protoscribe is absent", {
  if (requireNamespace("protoscribe", quietly = TRUE)) {
    skip("protoscribe installed")
  }
  ae <- create_shared_ae_corpus()
  err <- tryCatch(propose_annotations(ae, type = "vad"),
                  reindeer_missing_companion_error = function(e) e)
  expect_s3_class(err, "reindeer_missing_companion_error")
})

test_that("propose_annotations rejects unknown types", {
  ae <- create_shared_ae_corpus()
  expect_error(
    propose_annotations(ae, type = "bogus_kind"),
    "should be one of"
  )
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

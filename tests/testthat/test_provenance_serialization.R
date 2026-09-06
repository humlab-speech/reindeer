# ============================================================================
# Provenance attributes must survive serialisation round-trips so that
# segment_lists saved to disk (saveRDS / qs2::qs_save) keep their full
# pipeline history. The provenance tibble is stored on
# attr(seg, "reindeer_provenance").
# ============================================================================

library(testthat)
library(reindeer)

test_that("saveRDS / readRDS preserves provenance attribute", {
  ae <- create_isolated_ae_corpus()
  segs <- collect(query(ae, "Phonetic == n"))
  prov_before <- provenance(segs)

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(segs, tf)
  segs2 <- readRDS(tf)
  prov_after <- provenance(segs2)

  expect_s3_class(prov_after, "tbl_df")
  expect_equal(nrow(prov_after), nrow(prov_before))
  expect_equal(prov_after$verb, prov_before$verb)
})

test_that("saveRDS preserves provenance across a navigation pipeline", {
  ae <- create_isolated_ae_corpus()
  withr::with_options(list(reindeer.loss_warn = 1.0), {
    segs <- query(ae, "Phonetic == n") |> scout(1)
  })
  prov_before <- provenance(segs)

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(segs, tf)
  segs2 <- readRDS(tf)
  prov_after <- provenance(segs2)

  expect_equal(prov_after$verb, prov_before$verb)
  expect_equal(prov_after$rows_out, prov_before$rows_out)
})

test_that("qs2::qs_save round-trip preserves provenance attribute", {
  skip_if_not_installed("qs2")
  ae <- create_isolated_ae_corpus()
  segs <- collect(query(ae, "Phonetic == n"))
  prov_before <- provenance(segs)

  tf <- tempfile(fileext = ".qs2")
  on.exit(unlink(tf), add = TRUE)
  qs2::qs_save(segs, tf)
  segs2 <- qs2::qs_read(tf)
  prov_after <- provenance(segs2)

  expect_equal(prov_after$verb, prov_before$verb)
  expect_equal(prov_after$rows_out, prov_before$rows_out)
})

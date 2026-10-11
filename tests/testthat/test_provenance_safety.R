# Provenance and dplyr-hook edge cases (WP2.7).

prov_fixture <- function() {
  corp <- create_isolated_ae_corpus()
  list(corp = corp, segs = collect(query(corp, "Phonetic == n")))
}

test_that("row growth is recorded as rows_added, never as negative loss", {
  skip_if_no_emuR()
  fx <- prov_fixture()
  lookup <- data.frame(bundle = unique(fx$segs$bundle))
  lookup <- rbind(lookup, lookup)             # duplicate keys: join multiplies rows
  lookup$tag <- seq_len(nrow(lookup))
  grown <- dplyr::left_join(fx$segs, lookup, by = "bundle", relationship = "many-to-many")
  step <- tail(provenance(grown), 1)
  expect_gt(step$rows_out, step$rows_in)
  expect_equal(step$rows_lost, 0L)
  expect_equal(step$rows_added, step$rows_out - step$rows_in)
  expect_gte(dropped(grown), 0)
})

test_that("a join that creates NA keys downcasts to a plain tibble", {
  skip_if_no_emuR()
  fx <- prov_fixture()
  extra <- data.frame(bundle = c(unique(fx$segs$bundle), "orphan"), tag = 1)
  out <- dplyr::full_join(fx$segs, extra, by = "bundle")
  expect_false(S7::S7_inherits(out, segment_list))
  expect_s3_class(out, "tbl_df")
})

test_that("renaming a required column downcasts to a plain tibble", {
  skip_if_no_emuR()
  fx <- prov_fixture()
  out <- dplyr::rename(fx$segs, lab = labels)
  expect_false(S7::S7_inherits(out, segment_list))
})

test_that("group_by keeps the provenance log", {
  skip_if_no_emuR()
  fx <- prov_fixture()
  filtered <- dplyr::filter(fx$segs, bundle == bundle[1])
  grouped <- dplyr::group_by(filtered, level)
  expect_gte(nrow(provenance(grouped)), nrow(provenance(filtered)))
})

test_that("bind_rows of two segment lists stays a segment_list", {
  skip("WP5.1: vctrs hands restore() a ptype with NA names for the S7-backed class; fixed by the class merge")
  skip_if_no_emuR()
  fx <- prov_fixture()
  both <- dplyr::bind_rows(fx$segs, fx$segs)
  expect_equal(nrow(both), 2 * nrow(fx$segs))
  expect_true(S7::S7_inherits(both, segment_list))
})

test_that("SQL-side navigation records a provenance step", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  out <- collect(scout(query(corp, "Phonetic == n"), 1, collect = FALSE))
  expect_true("scout" %in% provenance(out)$verb)
})

# A1: lazy plans must not share state, steps run in declaration order, and
# lazy ascend/descend follow the link direction (from_id = parent).

marker_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE,
                       verbose = FALSE, ...) {
  data.frame(marker = 7)
}

lazy_fixture <- function() {
  corp <- create_isolated_ae_corpus()
  list(corp = corp, lz = query(corp, "Phonetic == t"))
}

test_that("deriving a plan does not alter or reuse the parent's result", {
  skip_if_no_emuR()
  fx <- lazy_fixture()
  base <- collect(fx$lz)                      # materializes the parent
  child <- quantify(fx$lz, marker_dsp)
  out <- collect(child)
  expect_true("marker" %in% names(out))
  expect_false("marker" %in% names(collect(fx$lz)))
  expect_equal(nrow(out), nrow(base))
})

test_that("parent materialized after child still stays clean", {
  skip_if_no_emuR()
  fx <- lazy_fixture()
  child <- quantify(fx$lz, marker_dsp)
  collect(child)
  expect_false("marker" %in% names(collect(fx$lz)))
})

test_that("nrow -> quantify -> biographize -> collect keeps every step", {
  skip_if_no_emuR()
  fx <- lazy_fixture()
  add_metadata(fx$corp, list(Gender = "f"))
  n <- nrow(fx$lz)
  out <- collect(biographize(quantify(fx$lz, marker_dsp), fx$corp))
  expect_equal(nrow(as.data.frame(out)), n)
  expect_true(all(c("marker", "Gender") %in% names(out)))
})

test_that("quantify() |> scout() measures the input segments", {
  skip_if_no_emuR()
  fx <- lazy_fixture()
  n_in <- nrow(collect(fx$lz))
  calls <- new.env()
  calls$n <- 0L
  counting <- function(listOfFiles, beginTime, endTime, toFile = FALSE,
                       verbose = FALSE, ...) {
    # The executor may first try a whole-file call (no window); count windows.
    if (!missing(beginTime)) calls$n <- calls$n + 1L
    data.frame(marker = 1)
  }
  out <- collect(scout(quantify(fx$lz, counting), 1, collect = FALSE))
  expect_equal(calls$n, n_in)
})

test_that("lazy ascend/descend follow link direction", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  up <- collect(ascend_to(query(corp, "Phoneme == t"), "Syllable", collect = FALSE))
  expect_gt(nrow(up), 0)
  expect_equal(unique(up$level), "Syllable")
  down <- collect(descend_to(query(corp, "Syllable == S"), "Phoneme", collect = FALSE))
  expect_gt(nrow(down), 0)
  expect_equal(unique(down$level), "Phoneme")
})

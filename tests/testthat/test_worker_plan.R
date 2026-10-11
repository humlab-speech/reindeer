# reindeer must never touch the caller's future::plan(); the plan decides
# parallelism (default sequential).

test_that("quantify() leaves future::plan() untouched", {
  skip_if_no_emuR()
  skip_if_no_superassp()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))[1:30, ]
  before <- future::plan()
  quantify(segs, superassp::trk_rms, .at = 0.5)
  gather_metadata(corp, verbose = FALSE)
  expect_identical(class(future::plan()), class(before))
})

test_that("removed parallel arguments warn and are ignored", {
  skip_if_no_emuR()
  skip_if_no_superassp()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))[1:5, ]
  expect_warning(quantify(segs, superassp::trk_rms, .at = 0.5, .workers = 2),
                 class = "reindeer_deprecated")
})

test_that("quantify(corpus, .using=) works under multisession", {
  skip_if_no_emuR()
  skip_if_no_superassp()
  skip_if_not(workers_can_load_reindeer(),
              "reindeer is not installed for parallel workers")
  corp <- create_isolated_ae_corpus()
  old <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(old), add = TRUE)
  expect_no_error(quantify(corp, .using = superassp::trk_rms, .verbose = FALSE,
                           .use_cache = TRUE))
})

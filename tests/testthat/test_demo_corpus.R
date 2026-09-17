# The bundled demo corpus is the fixture that examples and vignettes run on
# (see the remediation plan, WP5.1). It must work offline, with no emuR and no
# network, and it must expose the columns the docs describe.

test_that("demo_corpus() works offline and supports the documented workflow", {
  corp <- demo_corpus()
  expect_s7_class(corp, reindeer::corpus)

  segs <- collect(query(corp, "Phoneme =~ .+"))
  expect_gt(nrow(segs), 100)
  expect_true(all(c("labels", "start", "end", "session", "bundle") %in%
                    names(segs)))

  # The readme's five-minute workflow: query -> collect -> group by label
  summarised <- segs |>
    as.data.frame() |>
    dplyr::group_by(labels) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  expect_gt(nrow(summarised), 1)

  md <- get_metadata(corp)
  expect_gt(nrow(md), 0)

  params <- dsp_parameters(corpus_obj = corp)
  expect_gt(nrow(params), 0)
})

test_that("demo_corpus() is cheap on repeat calls", {
  t1 <- Sys.time()
  demo_corpus()
  first <- as.numeric(Sys.time() - t1, units = "secs")
  t2 <- Sys.time()
  demo_corpus()
  second <- as.numeric(Sys.time() - t2, units = "secs")

  # First call unpacks and caches; later calls must not repeat that work.
  expect_lt(second, max(1, first))
})

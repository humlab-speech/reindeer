# The 21-100 segment band with `.parallel = TRUE` (the default) was broken:
# .process_parallel_io() flattened its results with a double unlist(), which
# turns each per-segment tibble into its columns, and the caller's rbindlist()
# then failed with "Item 1 of input is not a data.frame, data.table or list".
#
# No test covered the band, so the defect shipped. These tests pin both the
# pure flattening step (runs everywhere) and the public call (needs an installed
# package so that parallel workers can load it).

test_that("segment result flattening keeps data frames intact", {
  # PENDING WP1.10 - red until the flattening step is factored out of
  # .process_parallel_io() and stops recursing into the data frames.

  skip("PENDING WP1.10 - see dev/REINDEER_REMEDIATION_PLAN.md")
  a <- data.frame(f0 = 1:2, intensity = c(60, 61))
  b <- data.frame(f0 = 3L, intensity = 62)
  c <- data.frame(f0 = 4L, intensity = 63)

  # One element per file group, each holding a list of per-segment frames,
  # with NULLs where a file or a segment failed.
  results <- list(list(a, b), NULL, list(c), list(NULL, a))

  flat <- reindeer:::.flatten_segment_results(results)

  expect_type(flat, "list")
  expect_length(flat, 5)
  expect_true(all(vapply(flat, is.data.frame, logical(1))))
  expect_setequal(vapply(flat, nrow, integer(1)), c(2L, 1L, 1L, 2L))

  # And the caller's binding step must accept the result as-is.
  bound <- data.table::rbindlist(flat, fill = TRUE)
  expect_equal(nrow(bound), 5)
})

test_that("quantify() works in the 21-100 segment band", {
  # PENDING WP1.10 - the default parallel executor for this band errors.

  skip("PENDING WP1.10 - see dev/REINDEER_REMEDIATION_PLAN.md")
  skip_if_no_emuR()
  skip_if_no_superassp()
  skip_if_not(workers_can_load_reindeer(),
              "reindeer is not installed for parallel workers")

  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))[1:50, ]
  expect_equal(nrow(segs), 50)

  out <- quantify(segs, superassp::trk_rms, .at = 0.5,
                  .parallel = TRUE, .verbose = FALSE)
  expect_equal(nrow(out), 50)
  expect_true("RMS_dB" %in% names(out))
})

# Coverage for segment_list construction, the eager DSP path, and
# quantify()'s interaction with metadata + parallelism. Fixtures come
# from helper-corpus.R: get_shared_ae_path() returns a path to the ae
# demo database (created once per test session), create_shared_ae_corpus()
# wraps it in a corpus(), and create_isolated_ae_corpus() gives a
# fresh copy for tests that mutate metadata.

test_that("segment_list class validation works", {
  skip_if_not_installed("emuR")
  skip_if_not_installed("S7")

  ae_path <- get_shared_ae_path()
  ae_db <- emuR::load_emuDB(ae_path, verbose = FALSE)
  on.exit(DBI::dbDisconnect(ae_db$connection))

  query_result <- emuR::query(ae_db, "Phonetic == n", resultType = "tibble")

  seg_list <- as_segment_list(query_result,
                              db_uuid = ae_db$UUID,
                              db_path = ae_path)

  expect_true(is_segment_list(seg_list))
  expect_equal(seg_list@db_uuid, ae_db$UUID)
  expect_equal(seg_list@db_path, ae_path)

  required_cols <- c("labels", "start", "end", "db_uuid", "session",
                     "bundle", "start_item_id", "end_item_id", "level",
                     "attribute", "start_item_seq_idx", "end_item_seq_idx",
                     "type", "sample_start", "sample_end", "sample_rate")
  expect_true(all(required_cols %in% names(seg_list)))
})

test_that("segment_list can be created from emuR::query result", {
  skip_if_not_installed("emuR")

  ae_path <- get_shared_ae_path()
  ae_db <- emuR::load_emuDB(ae_path, verbose = FALSE)
  on.exit(DBI::dbDisconnect(ae_db$connection))

  query_result <- emuR::query(ae_db, "Phonetic == t", resultType = "tibble")

  seg_list <- as_segment_list(query_result,
                              db_uuid = ae_db$UUID,
                              db_path = ae_path)

  expect_equal(nrow(seg_list), nrow(query_result))
  expect_equal(seg_list$labels, query_result$labels)
  expect_equal(seg_list$start, query_result$start)
  expect_equal(seg_list$end, query_result$end)
})

test_that("query returns segment_list", {
  skip_if_not_installed("emuR")

  ae_path <- get_shared_ae_path()
  seg_list <- query(ae_path, "Phonetic == t", lazy = FALSE)

  expect_true(is_segment_list(seg_list))
  expect_gt(nrow(seg_list), 0)
  expect_true(all(seg_list$labels == "t"))
})

test_that("quantify works with fake DSP function", {
  skip_if_not_installed("emuR")

  corp <- create_shared_ae_corpus()
  ae_path <- get_shared_ae_path()

  seg_list <- query(corp, "Phonetic == n", lazy = FALSE)
  seg_list@db_path <- ae_path

  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(
      f0_mean = 120.5,
      f0_sd = 15.2,
      intensity = 70.3
    )
  }

  result <- quantify(seg_list, fake_dsp, .parallel = FALSE, .verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true("labels" %in% names(result))
  expect_true("start" %in% names(result))
  expect_true("end" %in% names(result))
  expect_true("f0_mean" %in% names(result))
  expect_true("f0_sd" %in% names(result))
  expect_true("intensity" %in% names(result))
  expect_equal(unique(result$f0_mean), 120.5)
})

test_that("quantify works with .at parameter for time points", {
  skip_if_not_installed("emuR")

  corp <- create_shared_ae_corpus()
  ae_path <- get_shared_ae_path()
  seg_list <- query(corp, "Phonetic == n", lazy = FALSE)
  seg_list@db_path <- ae_path

  fake_dsp_track <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    n_frames <- 10
    obj <- data.frame(
      f1 = seq(500, 600, length.out = n_frames),
      f2 = seq(1500, 1600, length.out = n_frames),
      f3 = seq(2500, 2600, length.out = n_frames)
    )
    class(obj) <- c("AsspDataObj", "data.frame")
    obj
  }

  result <- quantify(seg_list, fake_dsp_track,
                     .at = c(0.25, 0.5, 0.75),
                     .parallel = FALSE,
                     .verbose = FALSE)

  n_segs <- nrow(seg_list)
  expect_equal(nrow(result), n_segs * 3)
  expect_true(".time_point" %in% names(result))
  expect_setequal(unique(result$.time_point), c(0.25, 0.5, 0.75))
})

test_that("quantify handles empty segment list", {
  skip_if_not_installed("emuR")

  corp <- create_shared_ae_corpus()
  ae_path <- get_shared_ae_path()
  seg_list <- query(corp, "Phonetic == zzz", lazy = FALSE)
  seg_list@db_path <- ae_path

  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(value = 1)
  }

  result <- quantify(seg_list, fake_dsp, .parallel = FALSE, .verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("quantify error handling works", {
  skip_if_not_installed("emuR")

  corp <- create_shared_ae_corpus()
  ae_path <- get_shared_ae_path()
  seg_list <- query(corp, "Phonetic == n", lazy = FALSE)
  seg_list@db_path <- ae_path

  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(value = 1)
  }

  expect_error(
    quantify(seg_list, fake_dsp, .at = c(-0.5, 0.5)),
    ".at.*between 0 and 1"
  )
  expect_error(
    quantify(seg_list, fake_dsp, .at = c(0.5, 1.5)),
    ".at.*between 0 and 1"
  )
})

test_that("quantify with metadata derivation works", {
  skip_if_not_installed("emuR")

  # Mutates metadata — needs an isolated corpus.
  corp <- create_isolated_ae_corpus()
  ae_path <- corp@basePath

  corp["0000", "msajc003"] <- list(Gender = "Male", Age = 25)

  seg_list <- query(corp, "Phonetic == n", lazy = FALSE)
  seg_list@db_path <- ae_path

  fake_dsp_with_params <- function(listOfFiles, beginTime, endTime,
                                   Gender = "Unknown", Age = 0,
                                   toFile = FALSE, verbose = FALSE, ...) {
    data.frame(
      gender_used = Gender,
      age_used = Age,
      value = 1
    )
  }

  result <- quantify(seg_list, fake_dsp_with_params,
                     .use_metadata = TRUE,
                     .parallel = FALSE,
                     .verbose = FALSE)

  expect_true("gender_used" %in% names(result))
  expect_true("age_used" %in% names(result))

  msajc003_rows <- result[result$bundle == "msajc003", ]
  if (nrow(msajc003_rows) > 0) {
    expect_true(any(msajc003_rows$gender_used == "Male"))
    expect_true(any(msajc003_rows$age_used == 25))
  }
})

test_that("quantify parallel processing works", {
  skip_if_not_installed("emuR")
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")

  corp <- create_shared_ae_corpus()
  ae_path <- get_shared_ae_path()
  seg_list <- query(corp, "Phonetic == n", lazy = FALSE)
  seg_list@db_path <- ae_path

  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    Sys.sleep(0.01)
    data.frame(value = runif(1))
  }

  result_parallel <- quantify(seg_list, fake_dsp,
                              .parallel = TRUE,
                              .workers = 2,
                              .verbose = FALSE)
  result_sequential <- quantify(seg_list, fake_dsp,
                                .parallel = FALSE,
                                .verbose = FALSE)

  expect_gt(nrow(result_parallel), 0)
  expect_gt(nrow(result_sequential), 0)
  expect_equal(nrow(result_parallel), nrow(result_sequential))
})

test_that("segment_list print and summary methods work", {
  skip_if_not_installed("emuR")

  ae_path <- get_shared_ae_path()
  seg_list <- query(ae_path, "Phonetic == t", lazy = FALSE)

  # cli output flows to stderr via message(); capture both streams and
  # check the headers the methods are guaranteed to emit. Substring
  # matches keep the test resilient to cli's bold/colour escapes.
  print_out <- paste(capture.output(print(seg_list), type = "message"),
                     collapse = "\n")
  expect_match(print_out, "segment_list", fixed = TRUE)

  summary_out <- paste(capture.output(summary(seg_list), type = "message"),
                       collapse = "\n")
  expect_match(summary_out, "Segment List Summary", fixed = TRUE)
  expect_match(summary_out, "Database", fixed = TRUE)
})

test_that("quantify preserves segment ordering", {
  skip_if_not_installed("emuR")

  corp <- create_shared_ae_corpus()
  ae_path <- get_shared_ae_path()
  seg_list <- query(corp, "Phonetic =~ .*", lazy = FALSE)
  seg_list@db_path <- ae_path

  seg_list_subset <- seg_list[1:5, ]

  fake_dsp <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(value = runif(1))
  }

  result <- quantify(as_segment_list(seg_list_subset,
                                     db_uuid = seg_list@db_uuid,
                                     db_path = seg_list@db_path),
                     fake_dsp,
                     .parallel = FALSE,
                     .verbose = FALSE)

  expect_equal(result$start_item_id, seg_list_subset$start_item_id)
  expect_equal(result$labels, seg_list_subset$labels)
})

test_that("quantify handles list output from DSP", {
  skip_if_not_installed("emuR")

  corp <- create_shared_ae_corpus()
  ae_path <- get_shared_ae_path()
  seg_list <- query(corp, "Phonetic == n", lazy = FALSE)
  seg_list@db_path <- ae_path

  fake_dsp_list <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    list(
      f0 = c(120, 125, 130),
      intensity = c(70, 72, 71),
      quality = "good"
    )
  }

  result <- quantify(seg_list, fake_dsp_list,
                     .parallel = FALSE,
                     .verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("f0|intensity|quality", names(result))))
})

test_that("quantify(segs, character_name) reads back a registered, already-computed track", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
           .verbose = FALSE, .parallel = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  result <- quantify(segs, "RMS", .at = 0.5, .verbose = FALSE, .parallel = FALSE)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("^RMS", names(result))))
})

test_that("quantify(segs, character vector) reads back multiple registered tracks", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
           .verbose = FALSE, .parallel = FALSE)
  quantify(ae, .using = wrassp::ksvF0, name = "F0", fileExtension = "f0",
           .verbose = FALSE, .parallel = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  result <- quantify(segs, c("RMS", "F0"), .at = 0.5, .verbose = FALSE, .parallel = FALSE)

  expect_true(any(grepl("^RMS", names(result))))
  expect_true(any(grepl("^F0", names(result))))
})

test_that("quantify(segs, character_name) errors on an unregistered track", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  expect_error(
    quantify(segs, "NoSuchTrack", .verbose = FALSE, .parallel = FALSE),
    class = "reindeer_error"
  )
})

test_that("quantify(segs, dsp_function) rejects mixed character/non-character input", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  expect_error(
    quantify(segs, list("RMS", wrassp::rmsana), .verbose = FALSE),
    class = "reindeer_error"
  )
})

test_that("quantify(segs, character_name) falls back to compute when the on-the-fly recipe is registered but file is missing", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, name = "rms_recipe", fileExtension = "rms",
           generator = list(`function` = "rmsana", package = "wrassp",
                            version = as.character(utils::packageVersion("wrassp"))))
  # No files were actually written (connect-existing, documentation-only) —
  # the character path must recompute via the stored recipe rather than error.
  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  result <- quantify(segs, "rms_recipe", .at = 0.5, .verbose = FALSE, .parallel = FALSE)
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # Guard against the fallback branch silently returning the same
  # arbitrary-in-file row for every segment (e.g. treating a relative
  # time point as a fraction of the whole recording instead of indexing
  # by the segment's own absolute start time). Comparing across the full
  # `segs` set isn't a sharp enough test here: cross-bundle content
  # variation alone would produce multiple unique values even under the
  # bug (each bundle is a different recording), masking a same-bundle
  # regression. Narrow it to two segments known to share one bundle —
  # that's where the bug's effect (identical value regardless of
  # in-bundle position) would actually show up.
  same_bundle <- segs[segs$bundle == segs$bundle[1], ][1:2, ]
  same_bundle@db_path <- ae@basePath
  result2 <- quantify(same_bundle, "rms_recipe", .at = 0.5, .verbose = FALSE, .parallel = FALSE)
  expect_true(nrow(result2) < 2 || length(unique(result2$rms_recipe)) > 1)
})

# --- Final-review fix round (2026-09-24 quantify-unification brief) --------

test_that("quantify(segs, character_name) requires .at (Fix 1 / C1)", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
           .verbose = FALSE, .parallel = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath

  expect_error(
    quantify(segs, "RMS", .verbose = FALSE, .parallel = FALSE),
    class = "reindeer_quantify_missing_at_error"
  )

  # And that it still works once .at is supplied.
  expect_no_error(
    quantify(segs, "RMS", .at = 0.5, .verbose = FALSE, .parallel = FALSE)
  )
})

test_that("quantify(segs, character_name) rejects DSP-style extra arguments (Fix 6A / I4)", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
           .verbose = FALSE, .parallel = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath

  expect_error(
    quantify(segs, "RMS", .at = 0.5, windowSize = 20, .verbose = FALSE, .parallel = FALSE),
    class = "reindeer_quantify_unused_arg_error"
  )
})

test_that("quantify(segs, character_name) fallback honours from/index instead of always taking the first element (Fix 2 / C2)", {
  skip_if_not_installed("superassp")

  # superassp::trk_formant_forest is a real, resolvable (get() from its own
  # namespace) DSP function that returns a named list (`F[Hz]`, `B[Hz]`) with
  # sampleRate/startTime attributes on the returned object itself -- exactly
  # what the fallback-to-compute branch needs, and it lets us prove the
  # fix without inventing a fixture that .quantify_segment_list_by_name()
  # could not actually resolve via generator$package/generator$function
  # (a locally-defined fake function has no namespace to `get()` it from).
  ae <- create_isolated_ae_corpus()
  quantify(ae, name = "B", from = "B[Hz]", index = 1L, fileExtension = "nonexistent_ext",
           generator = list(`function` = "trk_formant_forest", package = "superassp"))

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  seg <- as.data.frame(segs)[1, ]

  result <- quantify(segs, "B", .at = 0.5, .verbose = FALSE, .parallel = FALSE)
  expect_true("B" %in% names(result))

  # Independently recompute the same frame both ways and confirm the
  # returned column matches the B[Hz] (bandwidth) element, not the F[Hz]
  # (frequency) element that the pre-fix code would have taken (mat <-
  # computed[[1]]) regardless of `from`.
  wav <- list.files(file.path(ae@basePath, paste0(seg$session, "_ses"),
                              paste0(seg$bundle, "_bndl")),
                    pattern = "\\.wav$", full.names = TRUE)
  computed <- superassp::trk_formant_forest(wav, toFile = FALSE, verbose = FALSE)
  sr <- attr(computed, "sampleRate"); st <- attr(computed, "startTime")
  t_abs <- (seg$start + 0.5 * (seg$end - seg$start)) / 1000
  frame_idx <- max(1L, min(nrow(computed[[1]]), round((t_abs - st) * sr) + 1L))
  expected_B <- computed[["B[Hz]"]][frame_idx, 1]
  expected_F <- computed[["F[Hz]"]][frame_idx, 1]

  got <- result$B[result$session == seg$session & result$bundle == seg$bundle &
                     result$start == seg$start][1]
  expect_equal(got, expected_B)
  expect_false(isTRUE(all.equal(got, expected_F)))
})

test_that("quantify(segs, character_name) fallback aborts when the recomputed result has no sampleRate attribute (Fix 2 / C2)", {
  ae <- create_isolated_ae_corpus()
  # base::list(...) is a real, resolvable, `...`-taking function that
  # returns a plain (attribute-less) list when called the way the
  # fallback branch calls a DSP function -- a minimal stand-in for "a DSP
  # routine whose recomputed result carries no sampleRate attribute",
  # without inventing an unresolvable local fixture (see the sibling test
  # above for why a local closure won't do for this branch).
  quantify(ae, name = "NoRate", fileExtension = "nonexistent_ext2",
           generator = list(`function` = "list", package = "base"))

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath

  # This cli_abort() (like its "no resolvable generator$package" and
  # "cannot resolve pkg::fn" neighbours in the same fallback block) is
  # deliberately unclassed in the brief's snippet -- match on message,
  # not class.
  expect_error(
    quantify(segs, "NoRate", .at = 0.5, .verbose = FALSE, .parallel = FALSE),
    "sampleRate"
  )
})

test_that("quantify(segs, character_name) warns when some segment/time-point combinations produce no measurement (Fix 6B / I4)", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, name = "FallbackWarn", fileExtension = "nonexistent_ext3",
           generator = list(`function` = "rmsana", package = "wrassp",
                            version = as.character(utils::packageVersion("wrassp"))))

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath

  # Graft in one synthetic segment whose bundle doesn't exist on disk, so
  # its generator-fallback recompute can never find a signal file --
  # a deterministic, environment-independent way to force a dropped
  # (segment, time-point) combination without depending on any DSP's
  # toFile=TRUE write actually landing on disk in this environment.
  seg_df <- as.data.frame(segs)[1:3, ]
  fake_row <- seg_df[1, ]
  fake_row$bundle <- "nosuchbundle999"
  fake_row$session <- "nosuchsession999"
  seg_df <- rbind(seg_df, fake_row)
  segs2 <- segment_list(seg_df, db_uuid = segs@db_uuid, db_path = segs@db_path)

  # cli::cli_alert_warning() surfaces as a "message" condition under
  # testthat (it calls message() internally), not a base "warning" --
  # confirmed empirically; expect_warning() does not catch it.
  expect_message(
    result <- quantify(segs2, "FallbackWarn", .at = 0.5, .verbose = FALSE, .parallel = FALSE),
    "segment/time-point combination"
  )
  expect_equal(nrow(result), 3L)
})

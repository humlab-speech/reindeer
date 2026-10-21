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

  result <- quantify(seg_list, fake_dsp, .verbose = FALSE)

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

  skip_if_no_superassp()
  loadNamespace("superassp")  # registers as.data.frame.AsspDataObj
  # Ten frames spanning the analysed window, shaped like a superassp track.
  fake_dsp_track <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    n_frames <- 10
    obj <- list(fm = cbind(seq(500, 600, length.out = n_frames),
                           seq(1500, 1600, length.out = n_frames)))
    attr(obj, "sampleRate") <- n_frames / (endTime - beginTime)
    attr(obj, "startTime") <- beginTime
    class(obj) <- "AsspDataObj"
    obj
  }

  result <- quantify(seg_list, fake_dsp_track,
                     .at = c(0.25, 0.5, 0.75), .mode = "window",
                     .use_cache = FALSE, .verbose = FALSE)

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

  result <- quantify(seg_list, fake_dsp, .verbose = FALSE)

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
                              .workers = 2,
                              .verbose = FALSE)
  result_sequential <- quantify(seg_list, fake_dsp,
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
                     .verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("f0|intensity|quality", names(result))))
})

test_that("quantify(segs, character_name) reads back a registered, already-computed track", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", file_extension = "rms",
           .verbose = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  result <- quantify(segs, "RMS", .at = 0.5, .verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("^RMS", names(result))))
})

test_that("quantify(segs, character vector) reads back multiple registered tracks", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", file_extension = "rms",
           .verbose = FALSE)
  quantify(ae, .using = wrassp::ksvF0, name = "F0", file_extension = "f0",
           .verbose = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  result <- quantify(segs, c("RMS", "F0"), .at = 0.5, .verbose = FALSE)

  expect_true(any(grepl("^RMS", names(result))))
  expect_true(any(grepl("^F0", names(result))))
})

test_that("quantify(segs, character_name) errors on an unregistered track", {
  ae <- create_isolated_ae_corpus()
  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  expect_error(
    quantify(segs, "NoSuchTrack", .verbose = FALSE),
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

test_that("quantify(segs, character_name) aborts when the track file is missing (no recompute)", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, name = "rms_recipe", file_extension = "rms",
           generator = list(`function` = "rmsana", package = "wrassp"))
  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  expect_error(quantify(segs, "rms_recipe", .at = 0.5),
               class = "reindeer_missing_track_file_error")
})

# --- Final-review fix round (2026-09-24 quantify-unification brief) --------

test_that("quantify(segs, character_name) without .at returns every frame", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", file_extension = "rms",
           .verbose = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath

  all <- quantify(segs, "RMS")
  mid <- quantify(segs, "RMS", .at = 0.5)
  expect_gt(nrow(all), nrow(mid))
  expect_false(".time_point" %in% names(all))
  expect_error(quantify(segs, c("RMS", "fm")), class = "reindeer_quantify_error")
})

test_that("quantify(segs, character_name) rejects DSP-style extra arguments (Fix 6A / I4)", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = wrassp::rmsana, name = "RMS", file_extension = "rms",
           .verbose = FALSE)

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath

  expect_error(
    quantify(segs, "RMS", .at = 0.5, windowSize = 20, .verbose = FALSE),
    class = "reindeer_quantify_unused_arg_error"
  )
})

test_that("quantify(segs, character_name) honours from/index", {
  skip_if_no_superassp()
  ae <- create_isolated_ae_corpus()
  quantify(ae, name = "B", from = "bw", index = 1L, file_extension = "fms")

  segs <- query(ae, "Phonetic == n", lazy = FALSE)
  segs@db_path <- ae@basePath
  result <- quantify(segs, "B", .at = 0.5)
  expect_true("B" %in% names(result))

  # The column is the first bandwidth of the .fms file, at the selected frame.
  seg <- as.data.frame(result)[1, ]
  fms <- file.path(ae@basePath, paste0(seg$session, "_ses"), paste0(seg$bundle, "_bndl"),
                   paste0(seg$bundle, ".fms"))
  bw <- superassp::read_track(fms, zero_to_na = FALSE, tracks = "bw")
  k <- round((seg$frame_time - attr(bw, "startTime")) * attr(bw, "sampleRate")) + 1
  expect_equal(as.numeric(seg$B), as.numeric(bw[[1]][k, 1]))
})


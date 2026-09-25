# Coverage for quantify.corpus: compute-and-register, connect-existing,
# generator provenance, and the ssffTrackDefinitions round-trip.

library(testthat)
library(reindeer)

test_that(".resolve_dsp_identity parses a qualified pkg::fn expression", {
  id <- reindeer:::.resolve_dsp_identity(superassp::trk_rms, "superassp::trk_rms")
  expect_equal(id$`function`, "trk_rms")
  expect_equal(id$package, "superassp")
  expect_equal(id$version, as.character(utils::packageVersion("superassp")))
})

test_that(".resolve_dsp_identity falls back to the function's namespace for a bare name", {
  id <- reindeer:::.resolve_dsp_identity(superassp::trk_rms, "trk_rms")
  expect_equal(id$`function`, "trk_rms")
  expect_equal(id$package, "superassp")
})

test_that(".resolve_dsp_identity handles a user-defined function (no package)", {
  fake_dsp <- function(listOfFiles, ...) list()
  id <- reindeer:::.resolve_dsp_identity(fake_dsp, "fake_dsp")
  expect_equal(id$`function`, "fake_dsp")
  expect_true(is.na(id$package))
  expect_true(is.na(id$version))
})

test_that(".build_generator_block captures only explicit user args", {
  block <- reindeer:::.build_generator_block(
    superassp::trk_rms, "superassp::trk_rms",
    user_params = list(windowSize = 20)
  )
  expect_equal(block$`function`, "trk_rms")
  expect_equal(block$package, "superassp")
  expect_equal(block$args, list(windowSize = 20))
  expect_match(block$generatedAt, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}")
})

test_that(".build_generator_block recovers the real exported name behind a locally-renamed function (Fix 7A / I5)", {
  local_fn <- wrassp::rmsana
  block <- reindeer:::.build_generator_block(local_fn, "local_fn", list())
  expect_equal(block$`function`, "rmsana")
  expect_equal(block$package, "wrassp")
})

test_that("generator with empty args round-trips through toJSON as an object, not an array", {
  block <- reindeer:::.build_generator_block(
    superassp::trk_rms, "superassp::trk_rms", user_params = list()
  )
  json <- jsonlite::toJSON(list(generator = block), auto_unbox = TRUE, force = TRUE)
  # Verify the JSON string contains an object {}, not an array []
  expect_match(as.character(json), '"args":\\{\\}')
  # Also verify round-trip produces a list
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_true(is.list(parsed$generator$args))
})

.fake_rms_dsp <- function(listOfFiles, toFile = FALSE, verbose = FALSE, ...) {
  if (isTRUE(toFile)) {
    writeLines("fake-ssff-data", sub("\\.wav$", ".rms", listOfFiles))
    return(invisible(1L))
  }
  data.frame(`RMS[dB]` = 36.9, check.names = FALSE)
}
attr(.fake_rms_dsp, "ext") <- "rms"
attr(.fake_rms_dsp, "tracks") <- "RMS[dB]"
attr(.fake_rms_dsp, "outputType") <- "SSFF"
attr(.fake_rms_dsp, "suggestCaching") <- FALSE

.fake_formant_dsp <- function(listOfFiles, toFile = FALSE, verbose = FALSE, ...) {
  if (isTRUE(toFile)) {
    writeLines("fake-ssff-data", sub("\\.wav$", ".fms", listOfFiles))
    return(invisible(1L))
  }
  list(`F[Hz]` = matrix(500, 1, 4), `B[Hz]` = matrix(80, 1, 4))
}
attr(.fake_formant_dsp, "ext") <- "fms"
attr(.fake_formant_dsp, "tracks") <- c("F[Hz]", "B[Hz]")
attr(.fake_formant_dsp, "outputType") <- "SSFF"
attr(.fake_formant_dsp, "suggestCaching") <- FALSE

test_that("quantify.corpus compute-and-register writes files and registers a track (real DSP)", {
  ae <- create_isolated_ae_corpus()

  result <- quantify(ae, .using = wrassp::rmsana, name = "RMS", fileExtension = "rms",
                     .verbose = FALSE, .parallel = FALSE)

  expect_true(S7::S7_inherits(result, reindeer::corpus))

  cfg <- reindeer:::load_DBconfig(ae)
  track_names <- vapply(cfg$ssffTrackDefinitions, function(t) t$name, character(1))
  expect_true("RMS" %in% track_names)

  entry <- cfg$ssffTrackDefinitions[[which(track_names == "RMS")]]
  expect_equal(entry$fileExtension, "rms")
  expect_equal(entry$columnName, "rms")  # discovered for real via read_track(), not the fallback label
  expect_equal(entry$generator$`function`, "rmsana")
  expect_equal(entry$generator$package, "wrassp")
  expect_type(entry$generator$args, "list")

  rms_files <- list.files(ae@basePath, pattern = "\\.rms$", recursive = TRUE)
  expect_gt(length(rms_files), 0)
})

test_that("quantify.corpus registers one track per DSP output column group", {
  ae <- create_isolated_ae_corpus()

  quantify(ae, .using = .fake_formant_dsp, .verbose = FALSE, .parallel = FALSE)

  cfg <- reindeer:::load_DBconfig(ae)
  track_names <- vapply(cfg$ssffTrackDefinitions, function(t) t$name, character(1))
  expect_true(all(c("F", "B") %in% track_names))
  fms <- vapply(cfg$ssffTrackDefinitions[track_names %in% c("F", "B")],
                function(t) t$fileExtension, character(1))
  expect_true(all(fms == "fms"))
})

test_that("quantify.corpus errors on a name collision without overwrite", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, .using = .fake_rms_dsp, .verbose = FALSE, .parallel = FALSE)

  expect_error(
    quantify(ae, .using = .fake_rms_dsp, .verbose = FALSE, .parallel = FALSE),
    class = "reindeer_error"
  )

  expect_no_error(
    quantify(ae, .using = .fake_rms_dsp, .verbose = FALSE, .parallel = FALSE,
             overwrite = TRUE)
  )
})

test_that("quantify.corpus honours an explicit name for a single-track DSP function", {
  ae <- create_isolated_ae_corpus()

  quantify(ae, .using = .fake_rms_dsp, name = "Intensity",
           .verbose = FALSE, .parallel = FALSE)

  cfg <- reindeer:::load_DBconfig(ae)
  track_names <- vapply(cfg$ssffTrackDefinitions, function(t) t$name, character(1))
  expect_true("Intensity" %in% track_names)
  expect_false("RMS" %in% track_names)
})

test_that("quantify.corpus runs end-to-end with a real superassp function (smoke test)", {
  skip_if_not_installed("superassp")
  ae <- create_isolated_ae_corpus()

  # No file-existence assertion here: real superassp::trk_rms's toFile=TRUE
  # write is unreliable in this environment (see note above this test block).
  # This only proves the real integration path (derive_dsp_parameters() on a
  # real superassp function's formals, real attr() reads, real do.call
  # dispatch) runs to completion and registers something.
  expect_no_error(
    quantify(ae, .using = superassp::trk_rms, .verbose = FALSE, .parallel = FALSE)
  )
  cfg <- reindeer:::load_DBconfig(ae)
  # The ae demo corpus ships two pre-existing ssffTrackDefinitions ("dft",
  # "fm") from emuR's own demo data, so the newly-registered entry is not
  # necessarily at index 1 — locate it by its generator provenance instead
  # of assuming position.
  is_new_entry <- vapply(cfg$ssffTrackDefinitions, function(t) {
    isTRUE(t$generator$`function` == "trk_rms")
  }, logical(1))
  expect_true(any(is_new_entry))
  entry <- cfg$ssffTrackDefinitions[is_new_entry][[1]]
  expect_equal(entry$generator$`function`, "trk_rms")
  expect_equal(entry$generator$package, "superassp")
})

test_that("quantify.corpus connect-existing (fileExtension) registers without computing", {
  ae <- create_isolated_ae_corpus()

  result <- quantify(ae, name = "rms", columnName = "rms", fileExtension = "rms")

  expect_true(S7::S7_inherits(result, reindeer::corpus))
  cfg <- reindeer:::load_DBconfig(ae)
  track_names <- vapply(cfg$ssffTrackDefinitions, function(t) t$name, character(1))
  expect_true("rms" %in% track_names)
  entry <- cfg$ssffTrackDefinitions[[which(track_names == "rms")]]
  expect_null(entry$generator)
  rms_files <- list.files(ae@basePath, pattern = "\\.rms$", recursive = TRUE)
  expect_equal(length(rms_files), 0)  # no computation happened
})

test_that("quantify.corpus connect-existing (from/index) addresses a sub-column", {
  ae <- create_isolated_ae_corpus()

  quantify(ae, name = "F1", from = "F[Hz]", index = 1L,
           columnName = "F[Hz]", fileExtension = "fms")

  cfg <- reindeer:::load_DBconfig(ae)
  track_names <- vapply(cfg$ssffTrackDefinitions, function(t) t$name, character(1))
  entry <- cfg$ssffTrackDefinitions[[which(track_names == "F1")]]
  expect_equal(entry$from, "F[Hz]")
  expect_equal(entry$index, 1L)
})

test_that("quantify.corpus connect-existing requires name", {
  ae <- create_isolated_ae_corpus()
  expect_error(
    quantify(ae, fileExtension = "rms"),
    class = "reindeer_error"
  )
})

test_that("quantify.corpus connect-existing requires fileExtension xor from+index", {
  ae <- create_isolated_ae_corpus()
  expect_error(quantify(ae, name = "rms"), class = "reindeer_error")
  expect_error(
    quantify(ae, name = "F1", from = "F[Hz]"),  # index missing
    class = "reindeer_error"
  )
})

test_that("quantify.corpus connect-existing records an explicit documentation-only generator", {
  ae <- create_isolated_ae_corpus()
  quantify(ae, name = "rms", fileExtension = "rms",
           generator = list(`function` = "external_tool", package = NA, version = "1.0"))

  cfg <- reindeer:::load_DBconfig(ae)
  track_names <- vapply(cfg$ssffTrackDefinitions, function(t) t$name, character(1))
  entry <- cfg$ssffTrackDefinitions[[which(track_names == "rms")]]
  expect_equal(entry$generator$`function`, "external_tool")
})

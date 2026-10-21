# What reindeer relies on from superassp, checked against the installed
# package. CI installs superassp HEAD and runs weekly, so upstream changes fail
# here first. On failure: adapt reindeer, then raise the DESCRIPTION pin.
# (The parameter-name map is checked in test_speaker_norms.R.)

contract_routines <- c("trk_formant_forest", "trk_pitch_ksv", "trk_rms", "trk_dft_spectrum")

contract_wav <- function() {
  d <- tempfile("contract"); dir.create(d)
  src <- list.files(get_shared_ae_path(), "^msajc003\\.wav$", recursive = TRUE, full.names = TRUE)
  file.copy(src, d)
  file.path(d, "msajc003.wav")
}

test_that("toFile = FALSE returns an AsspDataObj with its own time base", {
  skip_if_no_emuR(); skip_if_no_superassp()
  wav <- contract_wav()
  for (fn in contract_routines) {
    obj <- getExportedValue("superassp", fn)(wav, toFile = FALSE, verbose = FALSE)
    expect_s3_class(obj, "AsspDataObj")
    expect_true(is.numeric(attr(obj, "sampleRate")), info = fn)
    expect_true(is.numeric(attr(obj, "startTime")), info = fn)
    expect_true("frame_time" %in% names(as.data.frame(obj)), info = fn)
  }
})

test_that("toFile = TRUE writes <bundle>.<ext> next to the signal", {
  skip_if_no_emuR(); skip_if_no_superassp()
  wav <- contract_wav()
  for (fn in contract_routines) {
    f <- getExportedValue("superassp", fn)
    out <- sub("wav$", attr(f, "ext"), wav)
    f(wav, toFile = TRUE, verbose = FALSE)
    expect_true(file.exists(out), info = fn)
  }
})

test_that("windowed analysis keeps absolute frame times", {
  skip_if_no_emuR(); skip_if_no_superassp()
  obj <- superassp::trk_formant_forest(contract_wav(), beginTime = 0.5, endTime = 0.6,
                                       toFile = FALSE, verbose = FALSE)
  tm <- as.data.frame(obj)$frame_time
  expect_gte(min(tm), 0.5)
  expect_lte(max(tm), 0.6)
})

test_that("read_track(zero_to_na = FALSE, tracks =) returns one track, zeros kept", {
  skip_if_no_emuR(); skip_if_no_superassp()
  fms <- list.files(get_shared_ae_path(), "^msajc003\\.fms$", recursive = TRUE, full.names = TRUE)
  obj <- superassp::read_track(fms, zero_to_na = FALSE, tracks = "fm")
  expect_length(obj, 1L)
  expect_equal(ncol(as.matrix(obj[[1]])), 4L)
  expect_true(any(obj[[1]] == 0))
  expect_equal(attr(obj, "sampleRate"), 200)
  expect_equal(attr(obj, "startTime"), 0.0025)
})

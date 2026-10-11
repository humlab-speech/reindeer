# A2/A3: Age/Gender at any metadata level (and the right Gender coding) must
# reach the DSP routine and change its output.

f1_values <- function(segs, use_metadata) {
  out <- suppressWarnings(as.data.frame(quantify(segs, superassp::trk_formant_forest,
                                                 .at = 0.5, .use_metadata = use_metadata)))
  as.numeric(out$F1_Hz)
}
# Paired comparison on the same segments: mean |dF1| in Hz.
f1_shift <- function(segs) {
  mean(abs(f1_values(segs, TRUE) - f1_values(segs, FALSE)), na.rm = TRUE)
}

vowel_fixture <- function() {
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == V|@|i:|u:|o:|A:|E|I|O"))
  list(corp = corp, segs = segs)
}

test_that("session-level Age/Gender change formant output", {
  skip_if_no_emuR(); skip_if_no_superassp()
  fx <- vowel_fixture()
  for (s in unique(fx$segs$session)) {
    add_metadata(fx$corp, list(Age = 6, Gender = "Female"), session = s)
  }
  expect_gt(f1_shift(fx$segs), 20)
})

test_that("database-level Age/Gender change formant output", {
  skip_if_no_emuR(); skip_if_no_superassp()
  fx <- vowel_fixture()
  add_metadata(fx$corp, list(Age = 6, Gender = "Female"))
  expect_gt(f1_shift(fx$segs), 20)
})

test_that("every mapped routine formal exists in the installed superassp", {
  skip_if_no_superassp()
  m <- reindeer:::.dsp_formal_map
  fns <- grep("^trk_", getNamespaceExports("superassp"), value = TRUE)
  all_formals <- unique(unlist(lapply(fns, function(f) names(formals(getExportedValue("superassp", f))))))
  expect_true(all(m$formal %in% c(all_formals, "gender")))
  expect_true(all(m$dspp %in% names(dspp_metadataParameters_dt())))
})

test_that("derivation maps aliases and gender coding", {
  skip_if_no_superassp()
  md <- list(Age = 30, Gender = "Female")
  p <- derive_dsp_parameters(superassp::trk_pitch_ac, md, c("Gender", "Age"), list())
  expect_true(all(c("minimum_f0", "maximum_f0") %in% names(p)))
  p <- derive_dsp_parameters(superassp::trk_formant_forest, md, c("Gender", "Age"), list())
  expect_equal(p$gender, "u")
  expect_true("nominalF1" %in% names(p))
  p <- derive_dsp_parameters(superassp::trk_formant_burg, md, c("Gender", "Age"), list())
  expect_true(all(c("number_of_formants", "maxHzFormant") %in% names(p)))
  p <- derive_dsp_parameters(superassp::trk_pitch_ksv, list(Gender = "Female"),
                             c("Gender", "Age"), list())
  expect_equal(p$gender, "f")
})

test_that("a routine that cannot receive any norm triggers one warning per call", {
  skip_if_no_emuR()
  fx <- vowel_fixture()
  add_metadata(fx$corp, list(Age = 6, Gender = "Female"))
  deaf <- function(listOfFiles, beginTime, endTime, toFile = FALSE, verbose = FALSE, ...) {
    data.frame(v = 1)
  }
  w <- NULL
  withCallingHandlers(
    quantify(fx$segs, deaf),
    reindeer_metadata_warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  expect_length(w, 1L)
})

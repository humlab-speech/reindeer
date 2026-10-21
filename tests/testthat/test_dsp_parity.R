# DSP parity with emuR: quantify() against emuR::get_trackdata() on ae's "fm"
# track (.fms files made by libassp forest, the routine
# superassp::trk_formant_forest wraps). Goldens come from
# tools/make-trackdata-golden.R. Values and frame times must match exactly.
# A skip names the work package (dev/REINDEER_WAY_FORWARD_3.2.md) that fixes it.

trackdata_golden <- function() {
  readRDS(testthat::test_path("fixtures", "trackdata_golden.rds"))$cases
}

# Character vector of mismatches; character(0) means parity.
dsp_parity_problems <- function(got, exp, value_cols) {
  got <- as.data.frame(got)
  if (nrow(got) != nrow(exp)) return(sprintf("rows %d, emuR %d", nrow(got), nrow(exp)))
  bad <- character()
  if (!identical(paste(got$bundle, got$start), paste(exp$bundle, exp$start))) bad <- "segments"
  if (!isTRUE(all.equal(as.numeric(got$frame_time) * 1000, exp$times_orig, tolerance = 1e-9))) {
    bad <- c(bad, sprintf("frame_time (%d differ)",
                          sum(abs(as.numeric(got$frame_time) * 1000 - exp$times_orig) > 1e-6)))
  }
  for (k in seq_along(value_cols)) {
    if (!identical(as.numeric(got[[value_cols[k]]]), as.numeric(exp[[paste0("T", k)]]))) {
      bad <- c(bad, value_cols[k])
    }
  }
  bad
}

fm_function_mode <- function(corp, .at) {
  segs <- collect(query(corp, "Phonetic =~ .*"))
  suppressWarnings(quantify(segs, superassp::trk_formant_forest, .at = .at,
                            .use_metadata = FALSE, .use_cache = FALSE))
}

fm_track_mode <- function(corp, .at) {
  segs <- collect(query(corp, "Phonetic =~ .*"))
  quantify(segs, "fm", .at = .at)
}

test_that("function mode .at matches get_trackdata(cut =)", {
  skip("G0.2: .at picks round(.at * n_frames), not the frame nearest the cut")
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_shared_ae_corpus()
  g <- trackdata_golden()
  for (cut in c(0.25, 0.5, 0.75)) {
    got <- fm_function_mode(corp, cut)
    expect_identical(dsp_parity_problems(got, g[[paste0("cut_", cut)]], paste0("F", 1:4, "_Hz")),
                     character(), label = paste("cut", cut))
  }
})

test_that("function mode trajectories match get_trackdata()", {
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_shared_ae_corpus()
  got <- fm_function_mode(corp, NULL)
  expect_identical(dsp_parity_problems(got, trackdata_golden()$full, paste0("F", 1:4, "_Hz")),
                   character())
})

test_that("track mode quantify(segs, \"fm\") matches get_trackdata(cut =)", {
  skip("G0.3: track mode loses read_track() errors and needs .at")
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_shared_ae_corpus()
  g <- trackdata_golden()
  for (cut in c(0.25, 0.5, 0.75)) {
    got <- fm_track_mode(corp, cut)
    expect_identical(dsp_parity_problems(got, g[[paste0("cut_", cut)]], paste0("fm", 1:4)),
                     character(), label = paste("cut", cut))
  }
})

test_that("track mode trajectories match get_trackdata()", {
  skip("G0.3: track mode loses read_track() errors and needs .at")
  skip_if_no_emuR(); skip_if_no_superassp()
  corp <- create_shared_ae_corpus()
  got <- fm_track_mode(corp, NULL)
  expect_identical(dsp_parity_problems(got, trackdata_golden()$full, paste0("fm", 1:4)),
                   character())
})

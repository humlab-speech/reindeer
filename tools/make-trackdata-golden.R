# Generate tests/testthat/fixtures/trackdata_golden.rds from emuR (emuR is test-only).
# Run from package root: Rscript tools/make-trackdata-golden.R
#
# Reference for test_dsp_parity.R: emuR::get_trackdata() on the ae demo corpus,
# track "fm" (the .fms files ae ships, made by libassp forest, the routine
# superassp::trk_formant_forest wraps), at three cut points and as full
# trajectories.
suppressMessages({library(emuR)})
d <- tempfile(); dir.create(d)
suppressMessages(create_emuRdemoData(dir = d))
ae <- suppressMessages(load_emuDB(file.path(d, "emuR_demoData", "ae_emuDB"), verbose = FALSE))

eql <- "Phonetic =~ .*"
sl <- emuR::query(ae, eql)
keep <- c("session", "bundle", "start", "end", "times_orig", "T1", "T2", "T3", "T4")
td <- function(cut = NULL) {
  r <- emuR::get_trackdata(ae, sl, "fm", cut = cut, resultType = "tibble", verbose = FALSE)
  as.data.frame(r)[, keep]
}
cases <- list(cut_0.25 = td(0.25), cut_0.5 = td(0.5), cut_0.75 = td(0.75), full = td())
saveRDS(list(emuR_version = as.character(packageVersion("emuR")), eql = eql, track = "fm",
             cases = cases),
        "tests/testthat/fixtures/trackdata_golden.rds", version = 2, compress = "xz")
cat(vapply(cases, nrow, 1L), "\n")

# Whole-file vs windowed DSP at segment midpoints on `ae` (F1.3).
#   Rscript tools/fidelity-whole-file.R   -> prints one row per routine
suppressMessages(devtools::load_all(quiet = TRUE))
corp <- corpus(reindeer:::create_ae_db(), verbose = FALSE)
segs <- collect(query(corp, "Phonetic =~ .+"))
routines <- c("trk_formant_forest", "trk_pitch_ksv", "trk_pitch_rapt",
              "trk_pitch_ac", "trk_spectral_moments", "trk_intensity", "trk_cpps")
res <- lapply(routines, function(r) {
  fn <- getExportedValue("superassp", r)
  run <- function(m) tryCatch(
    quantify(segs, fn, .at = 0.5, .mode = m, .use_cache = FALSE, .verbose = FALSE),
    error = function(e) conditionMessage(e))
  b <- run("bundle"); w <- run("window")
  if (is.character(b) || is.character(w))
    return(data.frame(routine = r, column = NA, n = NA, median_abs_diff = NA,
                      median_rel_diff = NA, note = substr(paste(b[is.character(b)], w[is.character(w)])[1], 1, 60)))
  new <- setdiff(names(b), names(segs)); new <- setdiff(new[vapply(b[new], is.numeric, NA)], c("frame_time", ".time_point"))  # frame_time is file- vs window-relative
  do.call(rbind, lapply(new, function(cn) {
    bv <- as.numeric(b[[cn]]); wv <- as.numeric(w[[cn]]); d <- abs(bv - wv); ok <- is.finite(d) & bv != 0 & wv != 0
    data.frame(routine = r, column = cn, n = sum(ok),
               median_abs_diff = median(d[ok]),
               median_rel_diff = median(d[ok] / pmax(abs(wv[ok]), 1e-9)), note = "")
  }))
})
out <- do.call(rbind, res); print(out, row.names = FALSE)

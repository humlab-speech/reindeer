# Synthetic benchmark corpora: N copies of the `ae` session (7 bundles each).
#   Rscript tools/make-synthetic-corpus.R <n_bundles> <out_dir>
# e.g. 504 -> 72 sessions, 700 -> 100 sessions. Returns <out_dir>/syn<N>_emuDB.
make_synthetic_corpus <- function(n_bundles, out_dir) {
  stopifnot(n_bundles %% 7 == 0)
  src <- reindeer:::create_ae_db()
  ses0 <- list.dirs(src, recursive = FALSE, full.names = TRUE)
  ses0 <- ses0[grepl("_ses$", ses0)][[1]]
  dest <- file.path(out_dir, sprintf("syn%d_emuDB", n_bundles))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(src, pattern = "_DBconfig\\.json$", full.names = TRUE), dest)
  file.rename(file.path(dest, basename(list.files(dest, "_DBconfig", full.names = TRUE))),
              file.path(dest, sprintf("syn%d_DBconfig.json", n_bundles)))
  for (i in seq_len(n_bundles / 7)) {
    file.copy(ses0, dest, recursive = TRUE)
    file.rename(file.path(dest, basename(ses0)), file.path(dest, sprintf("s%03d_ses", i)))
  }
  dest
}

if (sys.nframe() == 0L) {
  a <- commandArgs(TRUE)
  suppressMessages(devtools::load_all(quiet = TRUE))
  print(make_synthetic_corpus(as.integer(a[[1]]), a[[2]]))
}

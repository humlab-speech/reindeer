# Baseline timings for the amendment plan (see BASELINE_2.2.0.md).
#   Rscript benchmarking/baseline.R [dir-with-syn504_emuDB]
a <- commandArgs(TRUE)
t1 <- function(e) unname(system.time(e)["elapsed"])
r <- list()
r$library_load <- t1(suppressMessages(devtools::load_all(quiet = TRUE)))
ae <- reindeer:::create_ae_db()
r$ae_first_open <- t1(c <- corpus(ae, verbose = FALSE))
r$ae_reopen <- t1(corpus(ae, verbose = FALSE))
r$ae_query_t <- t1(s <- reindeer::collect(query(c, "Phonetic == t")))
r$ae_query_dominance <- t1(reindeer::collect(query(c, "[Syllable == S ^ Phonetic == @]")))
if (length(a)) {
  source("tools/make-synthetic-corpus.R")
  p <- file.path(a[[1]], "syn504_emuDB")
  if (!dir.exists(p)) make_synthetic_corpus(504, a[[1]])
  r$syn504_first_open <- t1(c5 <- corpus(p, verbose = FALSE))
  r$syn504_reopen <- t1(corpus(p, verbose = FALSE))
}
print(round(unlist(r), 3))

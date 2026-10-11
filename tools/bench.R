# Reproducible benchmark harness. Each workload runs in a fresh R process under
# /usr/bin/time so wall, CPU-s and peak RSS are comparable across releases.
#   Rscript tools/bench.R [out.md]     # run all, write a markdown table
#   Rscript tools/bench.R --one <name> # internal: run one workload, print a line
# Run on an idle machine (load < 4); the header records the load.
W <- c("library_load", "ae_first_open", "ae_reopen", "ae_query_dominance",
       "syn700_first_open", "syn700_reopen_after_edit", "quantify_253_cold",
       "quantify_253_warm", "scout_25k", "metadata_write_504")

mark <- function() T0 <<- proc.time()
work <- function(name) {
  if (name == "library_load") mark()
  suppressMessages(devtools::load_all(quiet = TRUE))
  source("tools/make-synthetic-corpus.R")
  tmp <- file.path(tempdir(), "bench"); dir.create(tmp, showWarnings = FALSE)
  syn <- function(n) {
    p <- file.path(tmp, sprintf("syn%d_emuDB", n))
    if (!dir.exists(p)) make_synthetic_corpus(n, tmp)
    p
  }
  ae <- function() corpus(reindeer:::create_ae_db(), verbose = FALSE)
  dsp <- function() utils::getFromNamespace(
    if (exists("trk_formant_forest", asNamespace("superassp"))) "trk_formant_forest" else "forest",
    "superassp")
  switch(name,
    library_load = NULL,  # timed from before load_all
    ae_first_open = { p <- reindeer:::create_ae_db(); mark(); corpus(p, verbose = FALSE) },
    ae_reopen = { p <- reindeer:::create_ae_db(); corpus(p, verbose = FALSE); mark(); corpus(p, verbose = FALSE) },
    ae_query_dominance = { c <- ae(); mark(); collect(query(c, "[Syllable == S ^ Phonetic == @]")) },
    syn700_first_open = { p <- syn(700); mark(); corpus(p, verbose = FALSE) },
    syn700_reopen_after_edit = {
      p <- syn(700); corpus(p, verbose = FALSE)
      f <- list.files(p, "_annot\\.json$", recursive = TRUE, full.names = TRUE)[1]
      Sys.sleep(1.1); writeLines(readLines(f), f); mark(); corpus(p, verbose = FALSE)
    },
    quantify_253_cold = { c <- ae(); s <- collect(query(c, "Phonetic =~ .+"))
      f <- dsp(); mark(); quantify(s, f, .use_cache = TRUE, .verbose = FALSE) },
    quantify_253_warm = { c <- ae(); s <- collect(query(c, "Phonetic =~ .+"))
      f <- dsp(); quantify(s, f, .use_cache = TRUE, .verbose = FALSE)
      mark(); quantify(s, f, .use_cache = TRUE, .verbose = FALSE) },
    scout_25k = { c <- corpus(syn(700), verbose = FALSE)
      s <- collect(query(c, "Phonetic =~ .+")); mark(); scout(s, 1) },
    metadata_write_504 = { c <- corpus(syn(504), verbose = FALSE)
      set_metadata(c, list(Age = 30, Gender = "f")); mark(); set_metadata(c, list(Age = 30, Gender = "f")) },
    stop("unknown workload: ", name))
}

a <- commandArgs(TRUE)
if (identical(a[1], "--one")) {
  T0 <- proc.time(); try(work(a[2]), silent = FALSE); t <- proc.time() - T0
  cat(sprintf("RESULT %s %.3f %.3f\n", a[2], t[["elapsed"]], t[["user.self"]] + t[["sys.self"]]))
} else {
  out <- if (length(a)) a[1] else "benchmarking/BASELINE_current.md"
  mac <- Sys.info()[["sysname"]] == "Darwin"
  rows <- vapply(W, function(w) {
    cmd <- if (mac) c("/usr/bin/time", "-l") else c("/usr/bin/time", "-v")
    o <- suppressWarnings(system2(cmd[1], c(cmd[-1], "Rscript", "tools/bench.R", "--one", w),
                                  stdout = TRUE, stderr = TRUE))
    r <- strsplit(sub("^RESULT ", "", grep("^RESULT ", o, value = TRUE)), " ")[[1]]
    rss <- if (mac) as.numeric(sub("^\\s*(\\d+)\\s+maximum resident.*", "\\1",
                                   grep("maximum resident", o, value = TRUE))) / 2^20
           else as.numeric(sub(".*: (\\d+)$", "\\1", grep("Maximum resident", o, value = TRUE))) / 2^10
    if (length(r) < 3) return(sprintf("| `%s` | failed | | |", w))
    sprintf("| `%s` | %s | %s | %.0f |", w, r[2], r[3], rss)
  }, character(1))
  load1 <- tryCatch(trimws(system("uptime", intern = TRUE)), error = function(e) "?")
  hdr <- c(sprintf("# Benchmark, reindeer %s", read.dcf("DESCRIPTION")[, "Version"]),
           "", sprintf("Reproduce: `Rscript tools/bench.R`. sha `%s`; %s; R %s.",
             tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "?"),
             Sys.info()[["machine"]], getRversion()),
           sprintf("Conditions: `%s`", load1), "",
           "| Workload | Wall s | CPU s | Peak RSS MB |", "|---|---|---|---|")
  writeLines(c(hdr, rows), out); cat(readLines(out), sep = "\n")
}

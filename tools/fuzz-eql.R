# Grammar-driven EQL fuzzer: reindeer vs emuR on the `ae` demo database (local).
#   Rscript tools/fuzz-eql.R [n_queries=300] [seed=1]
# Prints mismatches. A confirmed reindeer bug becomes a case in
# tools/make-eql-golden.R; an emuR quirk goes in dev/EQL_FUZZ_TRIAGE.md.
a <- commandArgs(TRUE)
n <- if (length(a) >= 1) as.integer(a[1]) else 300L
set.seed(if (length(a) >= 2) as.integer(a[2]) else 1L)
suppressMessages({ library(emuR); devtools::load_all(quiet = TRUE) })
d <- tempfile(); dir.create(d); suppressMessages(create_emuRdemoData(dir = d))
ae <- suppressMessages(load_emuDB(file.path(d, "emuR_demoData", "ae_emuDB"), verbose = FALSE))
rc <- corpus(file.path(d, "emuR_demoData", "ae_emuDB"), verbose = FALSE)

lv <- c("Phonetic", "Syllable", "Word", "Text", "Foot", "Intonational", "Utterance")
labs <- lapply(setNames(lv, lv), function(l) {
  x <- suppressMessages(emuR::query(ae, paste0(l, " =~ .*"), verbose = FALSE))$labels
  x <- unique(x[nzchar(x)]); x[seq_len(min(5, length(x)))]
})
# hierarchy pairs (parent, child) from the ae schema
pairs <- list(c("Word", "Syllable"), c("Syllable", "Phonetic"), c("Word", "Phonetic"),
              c("Foot", "Syllable"), c("Intonational", "Word"), c("Utterance", "Phonetic"),
              c("Text", "Phonetic"), c("Word", "Text"))
pick <- function(x) x[[sample.int(length(x), 1)]]
simple <- function(l = pick(lv)) {
  if (!length(labs[[l]])) return(sprintf("%s =~ .*", l))   # unlabelled level
  lab <- pick(labs[[l]]); op <- pick(c("==", "!=", "=~"))
  if (op == "=~") sprintf("%s =~ %s", l, if (runif(1) < .5) ".*" else paste0(substr(lab, 1, 1), ".*"))
  else sprintf("%s %s %s", l, op, lab)
}
gen <- function() switch(pick(c("simple", "seq", "dom", "conj", "proj", "num")),
  simple = simple(),
  seq = { l <- pick(lv); sprintf("[%s -> %s]", simple(l), simple(l)) },
  dom = { p <- pick(pairs); sprintf("[%s ^ %s]", simple(p[1]), simple(p[2])) },
  conj = { l <- pick(lv); sprintf("[%s & %s]", simple(l), simple(l)) },
  proj = { p <- pick(pairs); sprintf("[#%s ^ %s]", simple(p[1]), simple(p[2])) },
  num = { p <- pick(pairs); sprintf("[%s =~ .* & Num(%s, %s) %s %d]", p[1], p[1], p[2],
                                    pick(c("==", ">", "<")), sample(1:3, 1)) })
qs <- unique(replicate(n, gen()))
cmp <- function(q) {
  e <- tryCatch(as.data.frame(suppressWarnings(suppressMessages(emuR::query(ae, q, verbose = FALSE)))),
                error = function(e) NULL)
  r <- tryCatch(as.data.frame(collect(query(rc, q))), error = function(e) conditionMessage(e))
  if (is.null(e)) return(if (is.data.frame(r)) "emuR errors, reindeer returns" else NA_character_)
  if (is.character(r)) return(paste("reindeer error:", substr(r, 1, 80)))
  if (nrow(e) != nrow(r)) return(sprintf("rows %d vs %d", nrow(e), nrow(r)))
  if (nrow(e) == 0) return(NA_character_)
  if (!identical(as.character(e$labels), as.character(r$labels))) return("labels")
  if (!isTRUE(all.equal(as.numeric(e$start), as.numeric(r$start), tolerance = 1e-9))) return("start")
  if (!isTRUE(all.equal(as.numeric(e$end), as.numeric(r$end), tolerance = 1e-9))) return("end")
  NA_character_
}
out <- vapply(qs, cmp, "")
bad <- out[!is.na(out)]
cat(sprintf("%d queries, %d mismatches\n", length(qs), length(bad)))
if (length(bad)) print(data.frame(query = names(bad), problem = unname(bad)), right = FALSE, row.names = FALSE)

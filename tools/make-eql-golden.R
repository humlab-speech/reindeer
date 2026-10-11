# Generate tests/testthat/fixtures/eql_golden.rds from emuR (emuR is test-only).
# Run from package root: Rscript tools/make-eql-golden.R
suppressMessages({library(emuR)})
d <- tempfile(); dir.create(d)
suppressMessages(create_emuRdemoData(dir = d))
ae <- suppressMessages(load_emuDB(file.path(d, "emuR_demoData", "ae_emuDB"), verbose = FALSE))

queries <- c(
  # simple
  "Phonetic == t", "Phonetic != t", "Phonetic =~ '[ptk]'", "Phonetic !~ '[ptk]'",
  "Phonetic == t|k|p", "Phonetic == @", "Phonetic == V", "Phonetic == n",
  "Text == the", "Text =~ '.*'", "Syllable == S", "Syllable == W", "Intonational == L",
  "Tone == H*", "Word == C", "Word == F", "Foot == S", "Utterance =~ '.*'",
  "Accent == L+H*", "Accent == H*", "Intermediate =~ '.*'", "Intonational =~ '.*'",
  # sequences
  "[Phonetic == n -> Phonetic == t]", "[Phonetic == m -> Phonetic == @]",
  "[Phonetic == @ -> Phonetic == n]", "[Text == the -> Text =~ '.*']",
  "[Syllable == S -> Syllable == W]", "[Syllable == W -> Syllable == S]",
  "[Phonetic == V -> Phonetic == n]", "[Phonetic =~ '[ptk]' -> Phonetic == @]",
  "[Phonetic == s -> Phonetic == t]", "[Word == C -> Word == F]",
  # dominance
  "[Syllable == S ^ Phonetic == @]", "[Syllable == W ^ Phonetic == @]",
  "[Syllable == S ^ Phonetic == t]", "[Word == C ^ Syllable == S]",
  "[Word == F ^ Syllable == W]", "[Text == the ^ Phonetic == @]",
  "[Text =~ '.*' ^ Phonetic == t]", "[Foot == S ^ Syllable == S]",
  "[Intonational == L ^ Word == C]", "[Utterance =~ '.*' ^ Phonetic == t]",
  "[Phonetic == @ ^ Syllable == W]", "[Phonetic == t ^ Word == C]",
  "[Syllable == S ^ Text == the]",
  # projection
  "[#Syllable == S ^ Phonetic == @]", "[Syllable == S ^ #Phonetic == @]",
  "[#Word == C ^ Syllable == S]", "[Word == C ^ #Syllable == S]",
  "[#Text == the ^ Phonetic == @]", "[Text == the ^ #Phonetic == @]",
  "[#Phonetic == t ^ Word == C]", "[Phonetic == t ^ #Word == C]",
  "[[#Phonetic == n -> Phonetic == t] ^ Syllable == W]",
  # conjunction
  "[Text == the & Word == F]", "[Syllable == S & Text == the]",
  "[Phonetic == @ & Syllable == W]", "[Text =~ '.*' & Word == C]",
  # nested
  "[[Syllable == S ^ Phonetic == @] -> Syllable == W]",
  "[[Word == C ^ Syllable == S] ^ Phonetic == @]",
  "[[Syllable == S -> Syllable == W] ^ Phonetic == @]",
  "[Syllable == S ^ [Phonetic == @ -> Phonetic == n]]",
  # position functions
  "[Syllable == S & Start(Word, Syllable) == 1]",
  "[Syllable == S & End(Word, Syllable) == 1]",
  "[Syllable == W & Medial(Word, Syllable) == 1]",
  "[Syllable =~ .* & Start(Word, Syllable) == 0]",
  "[Phonetic =~ .* & Start(Syllable, Phonetic) == 1]",
  "[Phonetic =~ .* & End(Syllable, Phonetic) == 1]",
  "[Phonetic == t & Start(Word, Phonetic) == 1]",
  "[Word =~ .* & Num(Word, Syllable) == 2]",
  "[Word =~ .* & Num(Word, Syllable) > 1]",
  "[Word =~ .* & Num(Word, Syllable) == 1]",
  "[Syllable =~ .* & Num(Syllable, Phonetic) >= 3]",
  "[Syllable =~ .* & Num(Syllable, Phonetic) < 3]",
  "[Foot =~ .* & Num(Foot, Syllable) == 2]",
  "[Phonetic == n -> Phonetic == t] ",
  # sequence with Start/End functions / num on sequences
  "[#Phonetic == n -> Phonetic == t]",
  "[Phonetic =~ .* -> Phonetic == @]",
  "[Text == a -> Text == the]", "[Text =~ .* -> Text == the]",
  "[Text == the -> Text == the]"
)
queries <- unique(trimws(queries))

res <- lapply(queries, function(q) {
  r <- tryCatch(suppressWarnings(suppressMessages(emuR::query(ae, q, verbose = FALSE))),
                error = function(e) structure(conditionMessage(e), class = "emu_error"))
  if (inherits(r, "emu_error")) return(list(error = unclass(r)))
  df <- as.data.frame(r, stringsAsFactors = FALSE)
  keep <- intersect(c("labels","start","end","start_item_id","end_item_id","level",
                      "attribute","start_item_seq_idx","end_item_seq_idx",
                      "type","sample_start","sample_end","sample_rate","session","bundle"), names(df))
  list(rows = df[, keep, drop = FALSE])
})
names(res) <- queries
saveRDS(list(emuR_version = as.character(packageVersion("emuR")), queries = res),
        "tests/testthat/fixtures/eql_golden.rds", version = 2)
cat(sum(vapply(res, function(x) !is.null(x$rows), TRUE)), "ok /",
    sum(vapply(res, function(x) !is.null(x$error), TRUE)), "emuR-error of", length(res), "\n")

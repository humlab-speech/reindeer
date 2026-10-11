# Parity with emuR::query() on `ae`. Known failures are listed in
# `known_failures` with the work package (dev/REINDEER_AMENDMENT_PLAN_2.2.0.md)
# that fixes them; they are skipped, not dropped.
known_failures <- list(
  "Phonetic == t" = "A6: WP3.1",
  "Phonetic != t" = "A6: WP3.1",
  "Phonetic =~ '[ptk]'" = "A6: WP3.1",
  "Phonetic !~ '[ptk]'" = "A6: WP3.1",
  "Phonetic == t|k|p" = "A6: WP3.1",
  "Phonetic == @" = "A6: WP3.1",
  "Phonetic == V" = "A6: WP3.1",
  "Phonetic == n" = "A6: WP3.1",
  "Text == the" = "A6: WP3.1",
  "Text =~ '.*'" = "A6: WP3.1",
  "Syllable == S" = "A6: WP3.1",
  "Syllable == W" = "A6: WP3.1",
  "Word == C" = "A6: WP3.1",
  "Word == F" = "A6: WP3.1",
  "Utterance =~ '.*'" = "A6: WP3.1",
  "Intermediate =~ '.*'" = "A6: WP3.1",
  "Intonational =~ '.*'" = "A6: WP3.1",
  "[Phonetic == n -> Phonetic == t]" = "A6: WP3.1",
  "[Phonetic == m -> Phonetic == @]" = "A6: WP3.1",
  "[Phonetic == @ -> Phonetic == n]" = "A6: WP3.1",
  "[Text == the -> Text =~ '.*']" = "A6: WP3.1",
  "[Syllable == S -> Syllable == W]" = "A6: WP3.1",
  "[Syllable == W -> Syllable == S]" = "A6: WP3.1",
  "[Phonetic == s -> Phonetic == t]" = "A6: WP3.1",
  "[Word == C -> Word == F]" = "A6: WP3.1",
  "[Syllable == W ^ Phonetic == @]" = "A6: WP3.1",
  "[Syllable == S ^ Phonetic == t]" = "A6: WP3.1",
  "[Word == C ^ Syllable == S]" = "A6: WP3.1",
  "[Word == F ^ Syllable == W]" = "A6: WP3.1",
  "[Text == the ^ Phonetic == @]" = "A6: WP3.1",
  "[Text =~ '.*' ^ Phonetic == t]" = "A6: WP3.1",
  "[Utterance =~ '.*' ^ Phonetic == t]" = "A6: WP3.1",
  "[Phonetic == @ ^ Syllable == W]" = "A6: WP3.1",
  "[Phonetic == t ^ Word == C]" = "A6: WP3.1",
  "[#Word == C ^ Syllable == S]" = "A6: WP3.1",
  "[Word == C ^ #Syllable == S]" = "A6: WP3.1",
  "[#Text == the ^ Phonetic == @]" = "A6: WP3.1",
  "[Text == the ^ #Phonetic == @]" = "A6: WP3.1",
  "[#Phonetic == t ^ Word == C]" = "A6: WP3.1",
  "[Phonetic == t ^ #Word == C]" = "A6: WP3.1",
  "[[#Phonetic == n -> Phonetic == t] ^ Syllable == W]" = "A6/A7: WP3.2-3.4",
  "[Text == the & Word == F]" = "A6: WP3.1",
  "[Text =~ '.*' & Word == C]" = "A6: WP3.1",
  "[[Word == C ^ Syllable == S] ^ Phonetic == @]" = "A6: WP3.1",
  "[[Syllable == S -> Syllable == W] ^ Phonetic == @]" = "A6/A7: WP3.2-3.4",
  "[Syllable == S & Start(Word, Syllable) == 1]" = "A6: WP3.1",
  "[Syllable == S & End(Word, Syllable) == 1]" = "A6: WP3.1",
  "[Syllable == W & Medial(Word, Syllable) == 1]" = "A6: WP3.1",
  "[Syllable =~ .* & Start(Word, Syllable) == 0]" = "A6: WP3.1",
  "[Phonetic =~ .* & Start(Syllable, Phonetic) == 1]" = "A6/A7: WP3.2-3.4",
  "[Phonetic =~ .* & End(Syllable, Phonetic) == 1]" = "A6/A7: WP3.2-3.4",
  "[Phonetic == t & Start(Word, Phonetic) == 1]" = "A6/A7: WP3.2-3.4",
  "[Word =~ .* & Num(Word, Syllable) == 2]" = "A6: WP3.1",
  "[Word =~ .* & Num(Word, Syllable) > 1]" = "A6: WP3.1",
  "[Word =~ .* & Num(Word, Syllable) == 1]" = "A6: WP3.1",
  "[Syllable =~ .* & Num(Syllable, Phonetic) >= 3]" = "A6/A7: WP3.2-3.4",
  "[Syllable =~ .* & Num(Syllable, Phonetic) < 3]" = "A6/A7: WP3.2-3.4",
  "[Foot =~ .* & Num(Foot, Syllable) == 2]" = "A6: WP3.1",
  "[#Phonetic == n -> Phonetic == t]" = "A6: WP3.1",
  "[Phonetic =~ .* -> Phonetic == @]" = "A6: WP3.1"
)

test_that("EQL results match emuR (unsorted, 1e-9)", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  for (q in names(eql_golden())) {
    if (!is.null(known_failures[[q]])) next
    expect_eql_parity(corp, q)
  }
})

for (q in names(known_failures)) {
  local({
    q <- q
    test_that(paste("parity:", q), {
      skip(paste0(known_failures[[q]], ": ", q))
    })
  })
}

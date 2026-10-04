# Parser regressions: operator characters inside label lists, regexes,
# quoted values, and label-group resolution.

test_that("label alternatives inside brackets are not a disjunction", {
  for (q in c("[Phonetic == p|t|k -> Phonetic == H]",
              "[Phonetic == p | t | k -> Phonetic == H]")) {
    p <- parse_eql_query(q)
    expect_equal(p$type, "sequence")
    expect_equal(p$left$alternatives, c("p", "t", "k"))
  }
})

test_that("regex | inside brackets is not a disjunction", {
  p <- parse_eql_query("[Phonetic =~ a|b -> Phonetic == H]")
  expect_equal(p$type, "sequence")
  expect_equal(p$left$value, "a|b")

  p <- parse_eql_query("[Phonetic =~ 'p|t' -> Phonetic == H]")
  expect_equal(p$type, "sequence")
  expect_equal(p$left$value, "p|t")
})

test_that("regex ^ anchor in left dominance operand is not the operator", {
  p <- parse_eql_query("[Phoneme =~ ^a ^ Syllable == S]")
  expect_equal(p$type, "dominance")
  expect_equal(p$left$value, "^a")
  expect_equal(p$right$level, "Syllable")

  p <- parse_eql_query("[Syllable == S ^ Phoneme =~ ^a]")
  expect_equal(p$type, "dominance")
  expect_equal(p$right$value, "^a")
})

test_that("quoted values protect operator characters", {
  p <- parse_eql_query("[Phoneme == n ^ Phonetic == 'a->b']")
  expect_equal(p$type, "dominance")
  expect_equal(p$right$value, "a->b")
})

test_that("& inside a label is not a conjunction", {
  p <- parse_eql_query("[Text == R&B -> Text == x]")
  expect_equal(p$type, "sequence")
  expect_equal(p$left$value, "R&B")
})

test_that("query-level disjunction and conjunction still parse", {
  expect_equal(parse_eql_query("[Phonetic == t | Phonetic == k]")$type, "disjunction")
  expect_equal(parse_eql_query("[Phonetic == t|d | Phonetic == k]")$type, "disjunction")
  expect_equal(parse_eql_query("[Text == x & Accent == S]")$type, "conjunction")
  expect_equal(parse_eql_query("[[Phonetic == t -> Phonetic == H] | Phonetic == k]")$type,
               "disjunction")
})

# --- emuR parity on ae --------------------------------------------------------

ae_parity <- function() {
  skip_if_no_emuR()
  temp_dir <- tempdir()
  if (!dir.exists(file.path(temp_dir, "emuR_demoData"))) {
    emuR::create_emuRdemoData(dir = temp_dir)
  }
  ae_path <- file.path(temp_dir, "emuR_demoData", "ae_emuDB")
  ae <- emuR::load_emuDB(ae_path, verbose = FALSE)
  suppressMessages(emuR::query(ae, "Phonetic == t"))
  list(path = ae_path, db = ae)
}

expect_same_rows <- function(q, s) {
  expect_equal(nrow(query(s$path, q)),
               nrow(suppressWarnings(emuR::query(s$db, q))),
               label = q)
}

test_that("label lists in sequences match emuR", {
  s <- ae_parity()
  expect_same_rows("[Phonetic == p|t|k -> Phonetic == H]", s)
  expect_same_rows("[Phonetic == p | t | k -> Phonetic == H]", s)
})

test_that("label groups resolve like emuR", {
  s <- ae_parity()
  expect_gt(nrow(query(s$path, "Phonetic == stop")), 0)
  expect_same_rows("Phonetic == stop", s)
  expect_same_rows("Phonetic != stop", s)
  expect_same_rows("Phoneme == nasal", s)
  expect_same_rows("Phonetic == stop|nasal", s)
  expect_same_rows("[Phonetic == stop -> Phonetic == H]", s)
  expect_same_rows("[Syllable == S ^ Phoneme == vowel]", s)
})

test_that("regex anchors in dominance give same rows as unanchored-left variant", {
  s <- ae_parity()
  a <- query(s$path, "[Phoneme =~ ^a ^ Syllable == S]")
  b <- query(s$path, "[Syllable == S ^ #Phoneme =~ ^a]")
  expect_equal(nrow(a), nrow(b))
})

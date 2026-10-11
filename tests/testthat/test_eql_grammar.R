# New tokenizer / recursive-descent parser (WP3.3).

test_that("conjunction and disjunction are n-ary and may be unbracketed", {
  p <- parse_eql_query("[Text == x & Word == F & Syllable == W]")
  expect_equal(p$type, "conjunction")
  expect_equal(p$left$type, "conjunction")
  expect_equal(p$right$level, "Syllable")

  p <- parse_eql_query("Text == x & Word == F")
  expect_equal(p$type, "conjunction")

  p <- parse_eql_query("[Phonetic == t | Phonetic == k | Phonetic == p]")
  expect_equal(p$type, "disjunction")
  expect_equal(p$left$type, "disjunction")
})

test_that("nested sequences and projection anywhere parse", {
  p <- parse_eql_query("[[Phonetic == n -> Phonetic == t] -> Phonetic == @]")
  expect_equal(p$type, "sequence")
  expect_equal(p$left$type, "sequence")

  p <- parse_eql_query("[[Syllable == S ^ #Phonetic == @] -> Syllable == W]")
  expect_true(p$left$right$projection)
  expect_true(parse_eql_query("#Phonetic == t")$projection)
  expect_true(parse_eql_query("[Phonetic == n -> #Phonetic == t]")$right$projection)
})

test_that("compact operators without spaces still parse", {
  expect_equal(parse_eql_query("[Phoneme==n->Phoneme==t]")$type, "sequence")
  expect_equal(parse_eql_query("[Phoneme==n^Syllable==S]")$type, "dominance")
})

test_that("precedence: ^ binds tighter than ->, & is weakest", {
  p <- parse_eql_query("[A == a & B == b -> C == c ^ D == d]")
  expect_equal(p$type, "conjunction")
  expect_equal(p$right$type, "sequence")
  expect_equal(p$right$right$type, "dominance")
})

test_that("non-EQL input fails with a caret", {
  chk <- function(q, pattern) {
    cnd <- tryCatch(parse_eql_query(q), error = function(e) e)
    expect_s3_class(cnd, "reindeer_query_error")
    msg <- paste(c(cnd$message, cnd$body), collapse = "\n")
    expect_match(msg, pattern)
    expect_match(msg, "\\^")
  }
  chk("Phonetic == \"t\"", "single quotes")
  chk("[Phonetic == n -> Phonetic == t -> Phonetic == @]", "binary")
  chk("[Syllable == S ^ Word == C ^ Phonetic == @]", "binary")
  chk("[Phonetic == n -> Phonetic == t", "closing bracket")
  chk("Phonetic == n ]", "Unbalanced")
})

test_that("values keep operator characters", {
  expect_equal(parse_eql_query("[Phonetic == p | t | k -> Phonetic == H]")$left$alternatives,
               c("p", "t", "k"))
  expect_equal(parse_eql_query("[Text == R&B -> Text == x]")$left$value, "R&B")
  expect_equal(parse_eql_query("Phonetic =~ [tkp]")$value, "[tkp]")
})

# EQL Comprehensive Test Plan

## Executive Summary

This document provides a comprehensive test plan for the optimized EQL (EMU Query Language) implementation in the reindeer package. It compares current test coverage against the formal EQL specification and identifies missing test cases, edge cases, and niche scenarios.

**References:**
- EQL Manual: https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-querysys.html
- EQL EBNF Grammar: https://ips-lmu.github.io/The-EMU-SDMS-Manual/app-chap-EQL-EBNF.html

---

## Current Test Coverage Analysis

### ✅ Features Currently Tested

1. **Basic Operators**
   - ✅ Equality (`==`)
   - ✅ Inequality (`!=`)
   - ✅ Regex match (`=~`)
   - ⚠️  Regex non-match (`!~`) - partially tested

2. **Sequence Queries**
   - ✅ Basic sequences (`[A -> B]`)
   - ✅ Same-level sequences
   - ⚠️  Projection in sequences (limited)

3. **Dominance Queries**
   - ✅ Basic dominance (`[A ^ B]`)
   - ✅ Multi-level dominance
   - ✅ Projection with dominance

4. **Boolean Operations**
   - ✅ Conjunction (`&`)
   - ⚠️  Disjunction (`|`) - skipped due to emuR parser issues

5. **Functions**
   - ⚠️  `Start()` - tested but not validated against emuR
   - ⚠️  `End()` - tested but not validated against emuR
   - ⚠️  `Medial()` - NOT TESTED
   - ✅ `Num()` with various operators (==, >=, >, <=, !=)

6. **Edge Cases**
   - ✅ Empty results
   - ✅ Case sensitivity
   - ✅ Wildcard patterns
   - ✅ Multiple label matches
   - ✅ Different annotation types (SEGMENT, ITEM, EVENT)

7. **Result Format**
   - ✅ Segment_list object structure
   - ✅ Timing information
   - ✅ Sample information
   - ✅ Result ordering and consistency

### ❌ Features NOT Tested

1. **Operators**
   - ❌ Explicit regex non-match (`!~`) queries
   - ❌ Comparison operators in label queries (>, <, >=, <=)
   - ❌ Alternative operator (`|`) in label alternatives
   - ❌ Single equals (`=`) vs double equals (`==`)

2. **Functions**
   - ❌ `Medial()` function
   - ❌ Position functions with `FALSE` value: `Start(A,B) == FALSE`
   - ❌ Position functions in complex queries
   - ❌ `requery_seq()` and `requery_hier()` (if implemented)

3. **Attributes**
   - ❌ Explicit attribute queries: `Level:Attribute == value`
   - ❌ Queries on non-default attributes
   - ❌ Multiple attributes on same level

4. **Complex Compositions**
   - ❌ Deep nesting: `[[[A] & [B]] -> [C]]`
   - ❌ Combined sequence and dominance with projection
   - ❌ Multiple operators in single query
   - ❌ Nested boolean operations

5. **Special Syntax**
   - ❌ Label alternatives syntax
   - ❌ Time calculation control (`calcTimes` parameter)
   - ❌ NA handling in results

6. **Edge Cases**
   - ❌ Quotes in labels (single vs double)
   - ❌ Escaped special characters in regex
   - ❌ Very long query strings
   - ❌ Unicode characters in labels
   - ❌ Empty string labels
   - ❌ Whitespace handling

---

## Comprehensive Test Suite Plan

### Category 1: Basic Operators (Complete Coverage)

#### 1.1 Equality Operator
```r
test_that("equality operator complete coverage", {
  # Already tested
  expect_query_equivalent("Phonetic == t", ae_path, ae)

  # NEW: With quotes
  expect_query_equivalent("Phonetic == 't'", ae_path, ae)
  expect_query_equivalent('Phonetic == "t"', ae_path, ae)

  # NEW: Single equals (should work identically to ==)
  expect_query_equivalent("Phonetic = t", ae_path, ae)

  # NEW: Empty results
  expect_query_equivalent("Phonetic == ''", ae_path, ae)

  # NEW: Special characters
  expect_query_equivalent("Phonetic == @", ae_path, ae)
})
```

#### 1.2 Inequality Operator
```r
test_that("inequality operator complete coverage", {
  # Already tested
  expect_query_equivalent("Phonetic != t", ae_path, ae)

  # NEW: Ensure != returns complement
  result_eq <- ask_for(ae_path, "Phonetic == t")
  result_neq <- ask_for(ae_path, "Phonetic != t")
  result_all <- ask_for(ae_path, "Phonetic =~ .*")
  expect_equal(nrow(result_eq) + nrow(result_neq), nrow(result_all))
})
```

#### 1.3 Regex Match Operator
```r
test_that("regex match operator complete coverage", {
  # Already tested
  expect_query_equivalent("Phonetic =~ .*", ae_path, ae)
  expect_query_equivalent("Phonetic =~ [tkp]", ae_path, ae)

  # NEW: Complex regex patterns
  expect_query_equivalent("Phonetic =~ ^[AIOUEV]$", ae_path, ae)
  expect_query_equivalent("Word =~ .*ing$", ae_path, ae)
  expect_query_equivalent("Phonetic =~ [^aeiou]", ae_path, ae)

  # NEW: Escaped characters
  expect_query_equivalent("Word =~ \\w+", ae_path, ae)

  # NEW: Case-sensitive matching
  expect_query_equivalent("Phonetic =~ [A-Z]", ae_path, ae)
  expect_query_equivalent("Phonetic =~ [a-z]", ae_path, ae)
})
```

#### 1.4 Regex Non-Match Operator
```r
test_that("regex non-match operator complete coverage", {
  # NEW: Basic non-match
  expect_query_equivalent("Phonetic !~ [tkp]", ae_path, ae)

  # NEW: Should be complement of =~
  result_match <- ask_for(ae_path, "Phonetic =~ [tkp]")
  result_nomatch <- ask_for(ae_path, "Phonetic !~ [tkp]")
  result_all <- ask_for(ae_path, "Phonetic =~ .*")
  expect_equal(nrow(result_match) + nrow(result_nomatch), nrow(result_all))

  # NEW: Complex patterns
  expect_query_equivalent("Word !~ .*ing$", ae_path, ae)
})
```

#### 1.5 Comparison Operators (if supported in label context)
```r
test_that("comparison operators in labels", {
  # NEW: These may not be standard EQL but worth testing
  # Skip if not supported
  skip_if(TRUE, "Comparison operators may not apply to labels")

  # Numeric label comparisons (if database has numeric labels)
  # expect_query_equivalent("NumericLevel > 5", ae_path, ae)
  # expect_query_equivalent("NumericLevel <= 3", ae_path, ae)
})
```

---

### Category 2: Sequence Queries (Extended Coverage)

#### 2.1 Basic Sequences
```r
test_that("sequence queries extended coverage", {
  # Already tested
  expect_query_equivalent("[Phoneme == n -> Phoneme == t]", ae_path, ae)

  # NEW: Sequence with different levels (should fail or be caught)
  expect_error(ask_for(ae_path, "[Phoneme == n -> Phonetic == t]"))

  # NEW: Three-item sequence
  expect_query_equivalent("[Phoneme == n -> Phoneme == t -> Phoneme == s]", ae_path, ae)

  # NEW: Sequence with regex
  expect_query_equivalent("[Phoneme =~ [nm] -> Phoneme == t]", ae_path, ae)

  # NEW: Sequence with inequality
  expect_query_equivalent("[Phoneme != n -> Phoneme == t]", ae_path, ae)
})
```

#### 2.2 Projection in Sequences
```r
test_that("projection in sequences comprehensive", {
  # Already tested (basic)
  expect_query_equivalent("[#Phoneme == n -> Phoneme == t]", ae_path, ae)
  expect_query_equivalent("[Phoneme == n -> #Phoneme == t]", ae_path, ae)

  # NEW: Verify projection returns correct side
  left_proj <- ask_for(ae_path, "[#Phoneme == n -> Phoneme == t]")
  right_proj <- ask_for(ae_path, "[Phoneme == n -> #Phoneme == t]")

  # Labels should differ
  expect_true(all(left_proj$labels == "n"))
  expect_true(all(right_proj$labels == "t"))

  # NEW: Multiple # should error
  expect_error(ask_for(ae_path, "[#Phoneme == n -> #Phoneme == t]"))
})
```

#### 2.3 Nested Sequences
```r
test_that("nested sequence queries", {
  # NEW: Sequence within sequence
  expect_query_equivalent("[[Phoneme == n -> Phoneme == t] -> Phoneme == s]", ae_path, ae)

  # NEW: Sequence with dominance inside
  result <- ask_for(ae_path, "[[Syllable == S ^ Phoneme == n] -> Phoneme == t]")
  expect_true(nrow(result) >= 0)  # Should execute without error
})
```

---

### Category 3: Dominance Queries (Extended Coverage)

#### 3.1 Basic Dominance
```r
test_that("dominance queries extended coverage", {
  # Already tested
  expect_query_equivalent("[Syllable == S ^ Phoneme == n]", ae_path, ae)

  # NEW: Reverse dominance (bidirectional)
  result_forward <- ask_for(ae_path, "[Syllable == S ^ Phoneme == n]")
  result_reverse <- ask_for(ae_path, "[Phoneme == n ^ Syllable == S]")
  expect_equal(nrow(result_forward), nrow(result_reverse))

  # NEW: Dominance with regex
  expect_query_equivalent("[Syllable =~ .* ^ Phoneme == n]", ae_path, ae)

  # NEW: Dominance across multiple levels
  expect_query_equivalent("[Word =~ .* ^ Phonetic == t]", ae_path, ae)
})
```

#### 3.2 Projection in Dominance
```r
test_that("projection in dominance comprehensive", {
  # Already tested
  expect_query_equivalent("[#Syllable == S ^ Phoneme == n]", ae_path, ae)
  expect_query_equivalent("[Syllable == S ^ #Phoneme == n]", ae_path, ae)

  # NEW: Verify projection returns correct level
  syl_proj <- ask_for(ae_path, "[#Syllable == S ^ Phoneme == n]")
  phon_proj <- ask_for(ae_path, "[Syllable == S ^ #Phoneme == n]")

  expect_equal(unique(syl_proj$level), "Syllable")
  expect_equal(unique(phon_proj$level), "Phoneme")
})
```

#### 3.3 Invalid Dominance Relationships
```r
test_that("dominance on non-hierarchical levels errors", {
  # NEW: Dominance requires hierarchical relationship
  # Same level should error
  expect_error(ask_for(ae_path, "[Phoneme == n ^ Phoneme == t]"))

  # NEW: Non-linked levels should error or return empty
  # (depends on database structure)
})
```

---

### Category 4: Boolean Operations (Complete Coverage)

#### 4.1 Conjunction
```r
test_that("conjunction complete coverage", {
  # Already tested (basic)
  expect_query_equivalent("[Phonetic == t & Phonetic == t]", ae_path, ae)

  # NEW: Conjunction on different attributes of same level
  expect_query_equivalent("[Text == always & Accent == S]", ae_path, ae)

  # NEW: Multiple conjunctions
  expect_query_equivalent("[Phonetic == t & Phonetic == t & Phonetic == t]", ae_path, ae)

  # NEW: Conjunction with different operators
  result <- ask_for(ae_path, "[Phonetic == t & Phonetic =~ [tk]]")
  expect_true(nrow(result) >= 0)

  # NEW: Impossible conjunction (should return empty)
  result <- ask_for(ae_path, "[Phonetic == t & Phonetic == k]")
  expect_equal(nrow(result), 0)
})
```

#### 4.2 Disjunction
```r
test_that("disjunction complete coverage", {
  # Currently skipped - need to implement

  # NEW: Basic disjunction
  expect_query_equivalent("[Phonetic == t | Phonetic == k]", ae_path, ae)

  # NEW: Multiple disjunctions
  expect_query_equivalent("[Phonetic == t | Phonetic == k | Phonetic == p]", ae_path, ae)

  # NEW: Should be union of individual queries
  result_t <- ask_for(ae_path, "Phonetic == t")
  result_k <- ask_for(ae_path, "Phonetic == k")
  result_union <- ask_for(ae_path, "[Phonetic == t | Phonetic == k]")
  expect_equal(nrow(result_union), nrow(result_t) + nrow(result_k))

  # NEW: Disjunction with regex
  result <- ask_for(ae_path, "[Phonetic =~ [tk] | Phonetic == p]")
  expect_gt(nrow(result), 0)
})
```

#### 4.3 Mixed Boolean Operations
```r
test_that("mixed conjunction and disjunction", {
  # NEW: Precedence testing
  # [A & B | C] should be (A & B) | C
  result1 <- ask_for(ae_path, "[[Phonetic == t & Phonetic =~ .*] | Phonetic == k]")
  expect_true(nrow(result1) >= 0)

  # NEW: Explicit grouping
  result2 <- ask_for(ae_path, "[Phonetic == t & [Phonetic =~ .* | Phonetic == k]]")
  expect_true(nrow(result2) >= 0)
})
```

---

### Category 5: Position Functions (Complete Coverage)

#### 5.1 Start Function
```r
test_that("Start function complete coverage", {
  # Partially tested - now validate against emuR
  expect_query_equivalent("Start(Syllable, Phoneme) == 1", ae_path, ae)

  # NEW: Start with FALSE
  expect_query_equivalent("Start(Syllable, Phoneme) == 0", ae_path, ae)
  expect_query_equivalent("Start(Syllable, Phoneme) == FALSE", ae_path, ae)

  # NEW: Start with different levels
  expect_query_equivalent("Start(Word, Syllable) == 1", ae_path, ae)
  expect_query_equivalent("Start(Word, Phoneme) == 1", ae_path, ae)

  # NEW: Start in complex query
  result <- ask_for(ae_path, "[Start(Syllable, Phoneme) == 1 & Phoneme == n]")
  expect_true(nrow(result) >= 0)
})
```

#### 5.2 End Function
```r
test_that("End function complete coverage", {
  # Partially tested - now validate against emuR
  expect_query_equivalent("End(Syllable, Phoneme) == 1", ae_path, ae)

  # NEW: End with FALSE
  expect_query_equivalent("End(Syllable, Phoneme) == 0", ae_path, ae)
  expect_query_equivalent("End(Syllable, Phoneme) == FALSE", ae_path, ae)

  # NEW: End with different levels
  expect_query_equivalent("End(Word, Syllable) == 1", ae_path, ae)

  # NEW: Combination of Start and End (should be mutually exclusive)
  start_result <- ask_for(ae_path, "Start(Syllable, Phoneme) == 1")
  end_result <- ask_for(ae_path, "End(Syllable, Phoneme) == 1")
  # No phoneme should be both start and end (unless syllable has 1 phoneme)
})
```

#### 5.3 Medial Function
```r
test_that("Medial function complete coverage", {
  # NEW: Not currently tested
  expect_query_equivalent("Medial(Syllable, Phoneme) == 1", ae_path, ae)
  expect_query_equivalent("Medial(Syllable, Phoneme) == TRUE", ae_path, ae)

  # NEW: Medial FALSE (should be Start OR End)
  expect_query_equivalent("Medial(Syllable, Phoneme) == 0", ae_path, ae)

  # NEW: Verify mutual exclusivity
  start_result <- ask_for(ae_path, "Start(Syllable, Phoneme) == 1")
  medial_result <- ask_for(ae_path, "Medial(Syllable, Phoneme) == 1")
  end_result <- ask_for(ae_path, "End(Syllable, Phoneme) == 1")

  # No overlap between categories
  # (some phonemes might be in none if syllable has only 1 phoneme)
})
```

#### 5.4 Position Functions in Complex Queries
```r
test_that("position functions in complex queries", {
  # NEW: Position + dominance
  result <- ask_for(ae_path, "[[Syllable == S ^ Start(Syllable, Phoneme) == 1] & Phoneme == n]")
  expect_true(nrow(result) >= 0)

  # NEW: Position + sequence
  result <- ask_for(ae_path, "[Start(Syllable, Phoneme) == 1 -> End(Syllable, Phoneme) == 1]")
  expect_true(nrow(result) >= 0)
})
```

---

### Category 6: Count Function (Extended Coverage)

#### 6.1 Num with All Operators
```r
test_that("Num function with all comparison operators", {
  # Already tested: ==, >=
  expect_query_equivalent("Num(Syllable, Phoneme) == 2", ae_path, ae)
  expect_query_equivalent("Num(Syllable, Phoneme) >= 3", ae_path, ae)

  # NEW: Less than operators
  expect_query_equivalent("Num(Syllable, Phoneme) < 3", ae_path, ae)
  expect_query_equivalent("Num(Syllable, Phoneme) <= 2", ae_path, ae)

  # Already tested: !=, >
  expect_query_equivalent("Num(Syllable, Phoneme) != 1", ae_path, ae)
  expect_query_equivalent("Num(Syllable, Phoneme) > 2", ae_path, ae)

  # NEW: Edge values
  expect_query_equivalent("Num(Syllable, Phoneme) == 0", ae_path, ae)  # Should be empty
  expect_query_equivalent("Num(Syllable, Phoneme) >= 1", ae_path, ae)  # All syllables
})
```

#### 6.2 Num with Different Level Pairs
```r
test_that("Num function with various hierarchies", {
  # Already tested
  expect_query_equivalent("Num(Word, Syllable) >= 2", ae_path, ae)

  # NEW: Different combinations
  expect_query_equivalent("Num(Word, Phoneme) >= 5", ae_path, ae)
  expect_query_equivalent("Num(Utterance, Word) > 1", ae_path, ae)

  # NEW: Deep hierarchy
  # (depends on database structure)
})
```

#### 6.3 Num in Complex Queries
```r
test_that("Num in complex query contexts", {
  # NEW: Num + projection
  result <- ask_for(ae_path, "[#Num(Syllable, Phoneme) >= 3 ^ Phoneme == n]")
  expect_true(nrow(result) >= 0)

  # NEW: Num + boolean
  result <- ask_for(ae_path, "[Num(Syllable, Phoneme) == 3 & Syllable == S]")
  expect_true(nrow(result) >= 0)
})
```

---

### Category 7: Attributes (Complete Coverage)

#### 7.1 Explicit Attribute Queries
```r
test_that("explicit attribute queries", {
  # NEW: Level:Attribute syntax
  expect_query_equivalent("Word:Text == always", ae_path, ae)
  expect_query_equivalent("Word:Accent == S", ae_path, ae)

  # NEW: Default attribute vs explicit
  result_implicit <- ask_for(ae_path, "Word == always")
  result_explicit <- ask_for(ae_path, "Word:Text == always")
  expect_equal(nrow(result_implicit), nrow(result_explicit))

  # NEW: Non-default attribute
  result <- ask_for(ae_path, "Word:Accent =~ .*")
  expect_gt(nrow(result), 0)
})
```

#### 7.2 Multiple Attributes via Conjunction
```r
test_that("multiple attributes on same level", {
  # NEW: Multiple attributes via &
  expect_query_equivalent("[Word:Text == always & Word:Accent == S]", ae_path, ae)

  # NEW: Different values on different attributes
  result <- ask_for(ae_path, "[Word:Text =~ a.* & Word:Accent == S]")
  expect_true(nrow(result) >= 0)
})
```

---

### Category 8: Complex Nested Queries

#### 8.1 Deep Nesting
```r
test_that("deeply nested queries", {
  # NEW: Three levels of nesting
  query <- "[[[Syllable == S ^ Phoneme == n] & Start(Syllable, Phoneme) == 1] -> Phoneme == t]"
  result <- ask_for(ae_path, query)
  expect_true(nrow(result) >= 0)

  # NEW: Complex example from manual
  query <- "[[[Num(Text, Syllable) == 3] ^ [Phoneme == @ ^ Start(Word, Syllable) == 1]] -> #Text == his]"
  result <- ask_for(ae_path, query)
  expect_true(nrow(result) >= 0)
})
```

#### 8.2 All Operators Combined
```r
test_that("queries combining all operator types", {
  # NEW: Sequence + Dominance + Conjunction + Position + Count
  query <- "[[Num(Syllable, Phoneme) >= 3 & Start(Word, Syllable) == 1] ^ #Phoneme == n -> Phoneme == t]"
  result <- ask_for(ae_path, query)
  expect_true(nrow(result) >= 0)

  # NEW: With projection at different positions
  query <- "[Syllable == S ^ [#Phoneme == n -> Phoneme == t]]"
  result <- ask_for(ae_path, query)
  expect_true(nrow(result) >= 0)
})
```

---

### Category 9: Edge Cases and Error Handling

#### 9.1 Quote Handling
```r
test_that("quote handling in queries", {
  # NEW: Single quotes
  expect_query_equivalent("Phonetic == 't'", ae_path, ae)

  # NEW: Double quotes
  expect_query_equivalent('Phonetic == "t"', ae_path, ae)

  # NEW: Mixed quotes (should be same result)
  result_single <- ask_for(ae_path, "Phonetic == 't'")
  result_double <- ask_for(ae_path, 'Phonetic == "t"')
  expect_equal(nrow(result_single), nrow(result_double))

  # NEW: Quotes in label itself (if exists in data)
  # expect_query_equivalent("Word == \"can't\"", ae_path, ae)
})
```

#### 9.2 Whitespace Handling
```r
test_that("whitespace handling", {
  # NEW: Extra spaces
  result_compact <- ask_for(ae_path, "Phonetic==t")
  result_spaced <- ask_for(ae_path, "Phonetic  ==  t")
  expect_equal(nrow(result_compact), nrow(result_spaced))

  # NEW: Whitespace in brackets
  result1 <- ask_for(ae_path, "[Phoneme==n->Phoneme==t]")
  result2 <- ask_for(ae_path, "[ Phoneme == n -> Phoneme == t ]")
  expect_equal(nrow(result1), nrow(result2))
})
```

#### 9.3 Special Characters
```r
test_that("special characters in labels", {
  # NEW: Regex metacharacters as literals
  # If database has labels with special chars
  # expect_query_equivalent("Phonetic == .", ae_path, ae)  # Literal dot
  # expect_query_equivalent("Phonetic == *", ae_path, ae)  # Literal asterisk

  # NEW: Unicode characters (if data contains them)
  # skip("Unicode testing requires special test data")
})
```

#### 9.4 Invalid Query Syntax
```r
test_that("invalid queries produce informative errors", {
  # NEW: Unclosed bracket
  expect_error(ask_for(ae_path, "[Phoneme == n"))

  # NEW: Invalid operator
  expect_error(ask_for(ae_path, "Phoneme === n"))

  # NEW: Missing operand
  expect_error(ask_for(ae_path, "Phoneme =="))

  # NEW: Multiple # in same query
  expect_error(ask_for(ae_path, "[#Phoneme == n -> #Phoneme == t]"))

  # NEW: Invalid function name
  expect_error(ask_for(ae_path, "Invalid(Syllable, Phoneme) == 1"))

  # NEW: Wrong number of function arguments
  expect_error(ask_for(ae_path, "Start(Syllable) == 1"))
  expect_error(ask_for(ae_path, "Num(Syllable, Phoneme, Extra) == 1"))
})
```

#### 9.5 Boundary Values
```r
test_that("boundary value handling", {
  # NEW: Empty string label
  result <- ask_for(ae_path, "Phonetic == ''")
  expect_equal(nrow(result), 0)

  # NEW: Very long label
  long_label <- paste(rep("a", 1000), collapse = "")
  result <- ask_for(ae_path, sprintf("Phonetic == %s", long_label))
  expect_equal(nrow(result), 0)

  # NEW: Num with 0
  expect_query_equivalent("Num(Syllable, Phoneme) == 0", ae_path, ae)

  # NEW: Num with very large number
  expect_query_equivalent("Num(Syllable, Phoneme) < 1000", ae_path, ae)
})
```

---

### Category 10: Performance and Correctness

#### 10.1 Result Equivalence
```r
test_that("optimized implementation matches emuR exactly", {
  # Sample of queries that should match emuR exactly
  queries <- c(
    "Phonetic == t",
    "Phoneme =~ [nmt]",
    "[Syllable == S ^ Phoneme == n]",
    "[Phoneme == n -> Phoneme == t]",
    "Num(Syllable, Phoneme) >= 3",
    "[Word =~ .* & Accent == S]"
  )

  for (q in queries) {
    result_opt <- ask_for(ae_path, q)
    result_emuR <- query(ae, q)

    # Check row count
    expect_equal(nrow(result_opt), nrow(result_emuR),
                label = sprintf("Row count for: %s", q))

    # Check that same bundles are returned
    expect_setequal(result_opt$bundle, result_emuR$bundle,
                   label = sprintf("Bundles for: %s", q))
  }
})
```

#### 10.2 Performance Benchmarks
```r
test_that("performance characteristics maintained", {
  skip_if_not_installed("bench")

  # Test various query types for performance
  simple_query <- "Phonetic == t"
  complex_query <- "[[Num(Syllable, Phoneme) >= 3 ^ Phoneme == n] -> Phoneme == t]"

  bm_simple <- bench::mark(
    emuR = query(ae, simple_query),
    optimized = ask_for(ae_path, simple_query),
    iterations = 20,
    check = FALSE
  )

  bm_complex <- bench::mark(
    emuR = query(ae, complex_query),
    optimized = ask_for(ae_path, complex_query),
    iterations = 10,
    check = FALSE
  )

  # Log performance ratios
  message(sprintf("Simple query speedup: %.2fx",
                 bm_simple$median[1] / bm_simple$median[2]))
  message(sprintf("Complex query speedup: %.2fx",
                 bm_complex$median[1] / bm_complex$median[2]))
})
```

---

## Missing Feature Summary

### Critical Missing Tests (High Priority)

1. **`Medial()` function** - Completely untested
2. **Regex non-match (`!~`)** - Minimal coverage
3. **Disjunction (`|`)** - Skipped due to parser issues
4. **Explicit attributes (`Level:Attribute`)** - Not tested
5. **Position functions with `FALSE`** - Not tested
6. **Deep nesting (3+ levels)** - Not comprehensively tested

### Important Edge Cases (Medium Priority)

7. **Quote handling** - Single vs double quotes
8. **Escaped characters in regex** - Not tested
9. **Whitespace normalization** - Not tested
10. **Invalid syntax error messages** - Not comprehensively tested
11. **Boundary values** - Empty strings, very long strings, numeric extremes

### Advanced Features (Lower Priority)

12. **`requery_seq()` and `requery_hier()`** - If implemented
13. **`calcTimes` parameter** - Time calculation control
14. **Label alternatives syntax** - If supported
15. **Unicode in labels** - If relevant to use cases
16. **Complex mixed boolean operations** - Precedence testing

---

## Niche Query Scenarios

### Scenario 1: Multi-Word Phrases
```r
# Find three-syllable words with schwa in first syllable followed by "his"
query <- "[[[Num(Text, Syllable) == 3] ^ [Phoneme == @ ^ Start(Word, Syllable) == 1]] -> #Text == his]"
```

### Scenario 2: Syllable Boundaries
```r
# Find sequences where syllable-final consonant precedes syllable-initial consonant
query <- "[End(Syllable, Phoneme) == 1 -> Start(Syllable, Phoneme) == 1]"
```

### Scenario 3: Stress Patterns
```r
# Find words with strong syllable followed by weak syllable
query <- "[Word =~ .* ^ [Syllable == S -> Syllable == W]]"
```

### Scenario 4: Minimum Syllable Complexity
```r
# Find stressed syllables with at least 3 phonemes
query <- "[Syllable == S & Num(Syllable, Phoneme) >= 3]"
```

### Scenario 5: Onset/Coda Constraints
```r
# Find syllables where onset is voiceless plosive and coda is nasal
query <- "[Syllable =~ .* ^ [Start(Syllable, Phoneme) =~ [tkp] & End(Syllable, Phoneme) =~ [nm]]]"
```

### Scenario 6: Prosodic Position
```r
# Find phonemes in utterance-initial words
query <- "[[Start(Utterance, Word) == 1 ^ Phoneme =~ .*]]"
```

### Scenario 7: Tone Sequences
```r
# Find H* tone followed by L- tone
query <- "[Tone == H* -> Tone == L-]"
```

### Scenario 8: Word-Level Patterns
```r
# Find polysyllabic words ending in specific phoneme
query <- "[[Num(Word, Syllable) > 1] ^ End(Word, Phoneme) == n]"
```

---

## Implementation Recommendations

### Phase 1: Fill Critical Gaps
1. Implement `Medial()` function tests
2. Add comprehensive `!~` operator tests
3. Fix and enable `|` disjunction tests
4. Add `Level:Attribute` syntax tests

### Phase 2: Edge Case Coverage
1. Add quote handling tests
2. Add whitespace normalization tests
3. Add error message validation tests
4. Add boundary value tests

### Phase 3: Advanced Features
1. Test deep nesting (3+ levels)
2. Test complex boolean combinations
3. Add niche scenario tests
4. Add performance regression tests

### Phase 4: Documentation
1. Document tested vs untested features
2. Add examples for all query types
3. Create user guide with common patterns
4. Document known limitations

---

## Test Data Requirements

The current test suite uses the `ae` demo database. For comprehensive testing, consider:

1. **Extended demo data** with:
   - Labels containing special characters
   - Multiple attributes per level
   - Deep hierarchies (4+ levels)
   - Various annotation types (SEGMENT, ITEM, EVENT)
   - Tone annotations

2. **Synthetic test data** for edge cases:
   - Empty labels
   - Very long labels
   - Unicode characters
   - Numeric labels

3. **Performance test data**:
   - Large database (1000+ bundles)
   - Complex hierarchies
   - Many overlapping annotations

---

## Conclusion

Current test coverage is **approximately 60-70%** of the full EQL specification. The implementation handles most common use cases well, but several important features remain untested:

**Strengths:**
- Good coverage of basic operators
- Sequence and dominance queries well-tested
- `Num()` function comprehensively tested
- Result format validation solid

**Weaknesses:**
- `Medial()` function not tested
- Disjunction (`|`) disabled
- Attribute syntax not tested
- Many edge cases uncovered
- Limited error handling validation

**Priority actions:**
1. Implement `Medial()` tests
2. Fix/enable disjunction tests
3. Add attribute syntax tests
4. Expand error handling tests
5. Add niche scenario tests

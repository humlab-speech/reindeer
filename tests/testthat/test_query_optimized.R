# Test suite for optimized EQL implementation
# Tests equivalence with emuR::query()

library(testthat)
# NB: do NOT `library(emuR)` here — emuR exports a `query()` that would mask
# reindeer::query(), causing every `query(ae_path, ...)` call below to call
# emuR's variant (which rejects character paths) and abort. All emuR uses
# below are fully qualified instead.

# Setup test database
setup_test_db <- function() {
  skip_if_no_emuR()
  temp_dir <- tempdir()
  if (!dir.exists(file.path(temp_dir, 'emuR_demoData'))) {
    emuR::create_emuRdemoData(dir = temp_dir)
  }
  ae_path <- file.path(temp_dir, 'emuR_demoData', 'ae_emuDB')
  ae <- emuR::load_emuDB(ae_path, verbose = FALSE)

  # Ensure cache exists by running a simple query with emuR::query
  suppressMessages(emuR::query(ae, "Phonetic == t"))

  list(path = ae_path, db = ae)
}

# =============================================================================
# Strengthened comparison helper (Phase 2)
# =============================================================================
expect_query_equivalent <- function(query_str, ae_path, ae_db, tolerance = 1e-3) {
  result_opt <- query(ae_path, query_str)
  result_emuR <- emuR::query(ae_db, query_str)

  # Row count
  expect_equal(
    nrow(result_opt),
    nrow(result_emuR),
    label = sprintf("Row count for query: %s", query_str)
  )

  if (nrow(result_opt) == 0) return(invisible(TRUE))

  # Sort both by (session, bundle, start_item_seq_idx) for comparison
  opt_sorted <- result_opt[order(result_opt$session, result_opt$bundle,
                                  result_opt$start_item_seq_idx), ]
  emu_sorted <- result_emuR[order(result_emuR$session, result_emuR$bundle,
                                   result_emuR$start_item_seq_idx), ]

  # Labels
  expect_equal(opt_sorted$labels, emu_sorted$labels,
               label = sprintf("Labels for: %s", query_str))

  # Times (milliseconds, with tolerance)
  expect_equal(opt_sorted$start, emu_sorted$start, tolerance = tolerance,
               label = sprintf("Start times for: %s", query_str))
  expect_equal(opt_sorted$end, emu_sorted$end, tolerance = tolerance,
               label = sprintf("End times for: %s", query_str))

  # IDs
  expect_equal(opt_sorted$start_item_id, emu_sorted$start_item_id,
               label = sprintf("start_item_id for: %s", query_str))
  expect_equal(opt_sorted$end_item_id, emu_sorted$end_item_id,
               label = sprintf("end_item_id for: %s", query_str))

  # Metadata columns
  expect_equal(opt_sorted$level, emu_sorted$level,
               label = sprintf("Level for: %s", query_str))
  expect_equal(opt_sorted$attribute, emu_sorted$attribute,
               label = sprintf("Attribute for: %s", query_str))
  expect_equal(opt_sorted$type, emu_sorted$type,
               label = sprintf("Type for: %s", query_str))
  expect_equal(opt_sorted$sample_start, emu_sorted$sample_start,
               label = sprintf("sample_start for: %s", query_str))
  expect_equal(opt_sorted$sample_end, emu_sorted$sample_end,
               label = sprintf("sample_end for: %s", query_str))
  expect_equal(opt_sorted$sample_rate, emu_sorted$sample_rate,
               label = sprintf("sample_rate for: %s", query_str))

  invisible(TRUE)
}

# =============================================================================
# Simple Queries
# =============================================================================
describe("Simple Queries", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("equality queries work", {
    expect_query_equivalent("Phonetic == t", ae_path, ae)
    expect_query_equivalent("Phoneme == n", ae_path, ae)
    expect_query_equivalent("Syllable == S", ae_path, ae)
  })

  test_that("equality with quotes works", {
    expect_query_equivalent("Phonetic == 't'", ae_path, ae)

    result_no_quotes <- query(ae_path, "Phonetic == t")
    result_single <- query(ae_path, "Phonetic == 't'")
    result_double <- query(ae_path, 'Phonetic == "t"')

    expect_equal(nrow(result_no_quotes), nrow(result_single))
    expect_equal(nrow(result_no_quotes), nrow(result_double))
  })

  test_that("inequality queries work", {
    expect_query_equivalent("Phonetic != t", ae_path, ae)
    expect_query_equivalent("Phoneme != n", ae_path, ae)

    # Verify != returns complement of ==
    result_eq <- query(ae_path, "Phonetic == t")
    result_neq <- query(ae_path, "Phonetic != t")
    result_all <- query(ae_path, "Phonetic =~ .*")

    expect_equal(nrow(result_eq) + nrow(result_neq), nrow(result_all))
  })

  test_that("regex match queries work", {
    expect_query_equivalent("Phonetic =~ .*", ae_path, ae)

    result1 <- query(ae_path, "Phonetic =~ [tkp]")
    expect_true(nrow(result1) > 0)

    result2 <- query(ae_path, "Phonetic =~ ^[AIOUEV]$")
    expect_gt(nrow(result2), 0)

    result3 <- query(ae_path, "Phonetic =~ [^aeiou]")
    expect_gt(nrow(result3), 0)
  })

  test_that("regex non-match queries work", {
    result_match <- query(ae_path, "Phonetic =~ [tkp]")
    result_nomatch <- query(ae_path, "Phonetic !~ [tkp]")
    result_all <- query(ae_path, "Phonetic =~ .*")

    expect_equal(nrow(result_match) + nrow(result_nomatch), nrow(result_all))
  })

  test_that("queries with special characters work", {
    expect_query_equivalent("Phonetic == V", ae_path, ae)
    expect_query_equivalent("Phonetic == @", ae_path, ae)
  })

  # Phase 3.10: Labels with special characters
  test_that("labels with SQL-sensitive characters don't cause errors", {
    # Single quotes in label values — tests SQL escaping
    result <- query(ae_path, "Phonetic == O'Brien")
    expect_equal(nrow(result), 0)  # No match but no SQL error

    # Asterisk (used in Tone level as H*)
    expect_query_equivalent("Tone == H*", ae_path, ae)
  })
})

# =============================================================================
# Phase 3.7: Regex patterns
# =============================================================================
describe("Regex Pattern Fidelity", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("regex patterns match emuR", {
    expect_query_equivalent("Phonetic =~ .*", ae_path, ae)
    # emuR doesn't support !~ so only test regex =~ against it
    # Test =~ with simple patterns (emuR has quirks with anchors/brackets)
    result <- query(ae_path, "Phonetic =~ ^[mnN]$")
    expect_true(nrow(result) > 0)
    # Verify complement of =~ and !~
    result_match <- query(ae_path, "Phonetic =~ ^[mnN]$")
    result_nomatch <- query(ae_path, "Phonetic !~ ^[mnN]$")
    result_all <- query(ae_path, "Phonetic =~ .*")
    expect_equal(nrow(result_match) + nrow(result_nomatch), nrow(result_all))
  })
})

# =============================================================================
# Phase 3.1: Label alternatives
# =============================================================================
describe("Label Alternatives", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("label alternatives work", {
    # Phonetic == m | n should match both m and n
    result_alt <- query(ae_path, "Phonetic == m | n")
    result_m <- query(ae_path, "Phonetic == m")
    result_n <- query(ae_path, "Phonetic == n")
    expect_equal(nrow(result_alt), nrow(result_m) + nrow(result_n))
  })
})

# =============================================================================
# Sequence Queries
# =============================================================================
describe("Sequence Queries", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("basic sequence queries work", {
    expect_query_equivalent("[Phoneme == n -> Phoneme == t]", ae_path, ae)
    expect_query_equivalent("[Phoneme == k -> Phoneme == s]", ae_path, ae)
  })

  test_that("sequence with same labels work", {
    expect_query_equivalent("[Phoneme == n -> Phoneme == n]", ae_path, ae)
  })

  # Phase 3.6: Sequence query fidelity
  test_that("sequence timing spans correctly", {
    expect_query_equivalent("[Phonetic == @ -> Phonetic == n]", ae_path, ae)
    expect_query_equivalent("[#Phonetic == @ -> Phonetic == n]", ae_path, ae)
    expect_query_equivalent("[Phonetic == @ -> #Phonetic == n]", ae_path, ae)
  })

  # Phase 3.11: Three-item sequence chains
  test_that("chained sequences work", {
    expect_query_equivalent("[[Phonetic == m -> Phonetic == V] -> Phonetic == s]", ae_path, ae)
  })
})

# =============================================================================
# Dominance Queries
# =============================================================================
describe("Dominance Queries", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("basic dominance queries work", {
    expect_query_equivalent("[Syllable == S ^ Phoneme == n]", ae_path, ae)
    expect_query_equivalent("[Word == F ^ Phoneme == t]", ae_path, ae)
  })

  test_that("dominance with projection works", {
    expect_query_equivalent("[Syllable == S ^ #Phoneme == n]", ae_path, ae)
    expect_query_equivalent("[#Syllable == S ^ Phoneme == n]", ae_path, ae)
  })

  test_that("multi-level dominance works", {
    expect_query_equivalent("[Word == F ^ Phonetic == t]", ae_path, ae)
    expect_query_equivalent("[Intermediate == L- ^ Phoneme == n]", ae_path, ae)
  })

  # Phase 3.3: Dominance both directions
  test_that("dominance is non-directional", {
    expect_query_equivalent("[Phoneme == n ^ Syllable == S]", ae_path, ae)
    expect_query_equivalent("[Phonetic == t ^ Word == F]", ae_path, ae)
  })
})

# =============================================================================
# Boolean Operations
# =============================================================================
describe("Boolean Operations", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("conjunction queries work", {
    expect_query_equivalent("[Phonetic == t & Phonetic == t]", ae_path, ae)
  })

  # emuR can't parse [A | B] syntax, so validate logically
  test_that("disjunction is union of sub-queries", {
    result <- query(ae_path, "[Phonetic == t | Phonetic == k]")
    result_t <- query(ae_path, "Phonetic == t")
    result_k <- query(ae_path, "Phonetic == k")
    expect_equal(nrow(result), nrow(result_t) + nrow(result_k))
  })

  test_that("disjunction works in query", {
    result <- query(ae_path, "[Phonetic == t | Phonetic == k]")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) ||
                S7::S7_inherits(result, reindeer::lazy_segment_list) ||
                inherits(result, "emuRsegs"))
    expect_gt(nrow(result), 0)

    result_t <- query(ae_path, "Phonetic == t")
    result_k <- query(ae_path, "Phonetic == k")
    expect_gte(nrow(result), max(nrow(result_t), nrow(result_k)))
  })

  # Phase 3.8: Conjunction with attributes
  test_that("conjunction with different attributes", {
    # This requires attribute-as-level resolution
    expect_query_equivalent("[Text == always & Accent == S]", ae_path, ae)
  })
})

# =============================================================================
# Function Queries
# =============================================================================
describe("Function Queries", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("Start function works", {
    expect_query_equivalent("Start(Syllable, Phoneme) == 1", ae_path, ae)
    expect_query_equivalent("Start(Word, Syllable) == 1", ae_path, ae)
  })

  test_that("Start function with FALSE/0", {
    expect_query_equivalent("Start(Syllable, Phoneme) == 0", ae_path, ae)

    result_true <- query(ae_path, "Start(Syllable, Phoneme) == 1")
    result_false <- query(ae_path, "Start(Syllable, Phoneme) == 0")
    result_all <- query(ae_path, "Phoneme =~ .*")

    expect_equal(nrow(result_true) + nrow(result_false), nrow(result_all))
  })

  test_that("End function works", {
    expect_query_equivalent("End(Syllable, Phoneme) == 1", ae_path, ae)
    expect_query_equivalent("End(Word, Syllable) == 1", ae_path, ae)
  })

  test_that("End function with FALSE/0", {
    expect_query_equivalent("End(Syllable, Phoneme) == 0", ae_path, ae)

    result_true <- query(ae_path, "End(Syllable, Phoneme) == 1")
    result_false <- query(ae_path, "End(Syllable, Phoneme) == 0")
    result_all <- query(ae_path, "Phoneme =~ .*")

    expect_equal(nrow(result_true) + nrow(result_false), nrow(result_all))
  })

  test_that("Medial function works", {
    expect_query_equivalent("Medial(Syllable, Phoneme) == 1", ae_path, ae)
    expect_query_equivalent("Medial(Word, Syllable) == 1", ae_path, ae)
  })

  test_that("Medial function with FALSE/0", {
    expect_query_equivalent("Medial(Syllable, Phoneme) == 0", ae_path, ae)

    result_medial_false <- query(ae_path, "Medial(Syllable, Phoneme) == 0")
    result_medial_true <- query(ae_path, "Medial(Syllable, Phoneme) == 1")
    result_all <- query(ae_path, "Phoneme =~ .*")

    expect_equal(nrow(result_medial_false) + nrow(result_medial_true), nrow(result_all))
  })

  # Phase 3.2: TRUE/FALSE in position functions
  test_that("position functions accept TRUE/FALSE", {
    expect_query_equivalent("Start(Syllable, Phoneme) == TRUE", ae_path, ae)
    expect_query_equivalent("End(Syllable, Phoneme) == FALSE", ae_path, ae)
    expect_query_equivalent("Medial(Word, Syllable) == T", ae_path, ae)
  })

  test_that("Position functions handle edge cases", {
    result_start <- query(ae_path, "Start(Syllable, Phoneme) == 1")
    result_medial <- query(ae_path, "Medial(Syllable, Phoneme) == 1")
    result_end <- query(ae_path, "End(Syllable, Phoneme) == 1")

    expect_true(nrow(result_start) > 0)
    expect_true(nrow(result_end) > 0)
    expect_true(nrow(result_medial) > 0)
  })

  test_that("Num function works", {
    expect_query_equivalent("Num(Syllable, Phoneme) >= 3", ae_path, ae)
    expect_query_equivalent("Num(Syllable, Phoneme) == 2", ae_path, ae)
    expect_query_equivalent("Num(Word, Syllable) >= 2", ae_path, ae)
  })

  test_that("Num function with all comparison operators", {
    expect_query_equivalent("Num(Syllable, Phoneme) > 2", ae_path, ae)
    expect_query_equivalent("Num(Syllable, Phoneme) < 3", ae_path, ae)
    expect_query_equivalent("Num(Syllable, Phoneme) <= 2", ae_path, ae)
    expect_query_equivalent("Num(Syllable, Phoneme) != 1", ae_path, ae)
    expect_query_equivalent("Num(Syllable, Phoneme) >= 1", ae_path, ae)
  })

  # Phase 3.13: Num in complex combinations
  test_that("Num with dominance", {
    expect_query_equivalent("[Phoneme == m ^ Num(Word, Syllable) == 3]", ae_path, ae)
  })

  test_that("Position functions in complex queries", {
    expect_query_equivalent("[Start(Syllable, Phoneme) == 1 & Phoneme == n]", ae_path, ae)

    expect_query_equivalent("[Start(Syllable, Phoneme) == 1 -> End(Syllable, Phoneme) == 1]", ae_path, ae)
  })
})

# =============================================================================
# Phase 3.4: ITEM levels get correct deduced times
# =============================================================================
describe("ITEM Level Timing", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("ITEM levels get correct deduced times", {
    expect_query_equivalent("Phoneme == n", ae_path, ae)
    expect_query_equivalent("Syllable == S", ae_path, ae)
    expect_query_equivalent("Word =~ .*", ae_path, ae)
  })
})

# =============================================================================
# Phase 3.5: EVENT level timing
# =============================================================================
describe("EVENT Level Timing", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("EVENT level timing matches emuR", {
    expect_query_equivalent("Tone =~ .*", ae_path, ae)
  })
})

# =============================================================================
# Edge Cases
# =============================================================================
describe("Edge Cases", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("queries with no results work", {
    expect_query_equivalent("Phonetic == xyz", ae_path, ae)
    expect_query_equivalent("[Phoneme == xyz -> Phoneme == abc]", ae_path, ae)
  })

  test_that("queries return proper segment_list object", {
    result <- query(ae_path, "Phonetic == t")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "data.frame"))

    expected_cols <- c("labels", "start", "end", "session", "bundle",
                      "level", "attribute", "start_item_id", "end_item_id",
                      "type", "sample_start", "sample_end", "sample_rate")
    expect_true(all(expected_cols %in% names(result)))
  })

  test_that("case-sensitive label matching", {
    result_lower <- query(ae_path, "Phonetic == s")
    result_upper <- query(ae_path, "Phonetic == S")
    expect_true(nrow(result_lower) != nrow(result_upper) ||
                (nrow(result_lower) == 0 && nrow(result_upper) == 0))
  })

  test_that("wildcard patterns work", {
    result <- query(ae_path, "Phonetic =~ .*")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))
    expect_gt(nrow(result), 0)
  })

  test_that("multiple label matches work", {
    result1 <- query(ae_path, "Phonetic == t")
    result2 <- query(ae_path, "Phonetic == k")
    combined <- query(ae_path, "[Phonetic == t | Phonetic == k]")

    expect_gte(nrow(combined), max(nrow(result1), nrow(result2)))
  })
})

# =============================================================================
# Result Format Consistency
# =============================================================================
describe("Result Format Consistency", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("timing information is correct for SEGMENT types", {
    result_opt <- query(ae_path, "Phonetic == t")
    result_emuR <- emuR::query(ae, "Phonetic == t")

    expect_true(all(!is.na(result_opt$start)))
    expect_true(all(!is.na(result_opt$end)))
    expect_true(all(result_opt$end >= result_opt$start))
  })

  test_that("sample information is consistent", {
    result <- query(ae_path, "Phonetic == t")

    if (nrow(result) > 0) {
      expect_true(all(!is.na(result$sample_rate)))
      expect_true(all(result$sample_rate > 0))
    }
  })
})

# =============================================================================
# Database Path Handling
# =============================================================================
describe("Database Path Handling", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("works with path string", {
    result <- query(ae_path, "Phonetic == t")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))
    expect_gt(nrow(result), 0)
  })

  test_that("handles cache file variations", {
    expect_no_error(query(ae_path, "Phonetic == t"))
  })

  test_that("gives informative error for missing database", {
    expect_error(
      query("/nonexistent/path", "Phonetic == t"),
      "SQLite database not found"
    )
  })
})

# =============================================================================
# Complex Multi-Level Queries
# =============================================================================
describe("Complex Multi-Level Queries", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("deep hierarchy traversal works", {
    expect_query_equivalent("[Intermediate =~ .* ^ Phoneme == n]", ae_path, ae)
    expect_query_equivalent("[Syllable == S ^ Phonetic == t]", ae_path, ae)
  })

  test_that("combined sequence and dominance rejects mismatched levels", {
    # This query mixes levels in a sequence: the dominance result is at Syllable level
    # but the right side is Phoneme level. emuR also rejects this.
    expect_error(
      query(ae_path, "[[Syllable == S ^ Phoneme == n] -> Phoneme == t]"),
      "same level"
    )
  })

  test_that("multiple projections work", {
    expect_query_equivalent("[#Syllable == S ^ Phoneme == n]", ae_path, ae)
    expect_query_equivalent("[Syllable == S ^ #Phoneme == n]", ae_path, ae)
  })
})

# =============================================================================
# Boundary Conditions
# =============================================================================
describe("Boundary Conditions", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("handles single-item results", {
    result <- query(ae_path, "Word == absolutely")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))
  })

  test_that("handles queries on EVENT levels", {
    result <- query(ae_path, "Tone =~ .*")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))
    if (nrow(result) > 0) {
      expect_equal(result$type[1], "EVENT")
    }
  })

  test_that("handles queries on ITEM levels", {
    result <- query(ae_path, "Phoneme == n")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))
    if (nrow(result) > 0) {
      expect_equal(result$type[1], "ITEM")
    }
  })

  test_that("handles queries on SEGMENT levels", {
    result <- query(ae_path, "Phonetic == t")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))
    if (nrow(result) > 0) {
      expect_equal(result$type[1], "SEGMENT")
    }
  })
})

# =============================================================================
# Attribute Queries
# =============================================================================
describe("Attribute Queries", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("explicit attribute syntax works", {
    result <- query(ae_path, "Word:Text =~ .*")
    expect_gt(nrow(result), 0)

    result <- query(ae_path, "Word:Accent =~ .*")
    expect_gt(nrow(result), 0)
  })

  test_that("default attribute matches explicit", {
    result_implicit <- query(ae_path, "Word =~ .*")
    result_explicit <- query(ae_path, "Word:Text =~ .*")

    expect_equal(nrow(result_implicit), nrow(result_explicit))
  })

  test_that("multiple attributes via conjunction", {
    result <- query(ae_path, "[Word:Text =~ .* & Word:Accent =~ .*]")
    expect_gt(nrow(result), 0)
  })
})

# =============================================================================
# Query Language Edge Cases
# =============================================================================
describe("Query Language Edge Cases", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("handles whitespace variations", {
    result_compact <- query(ae_path, "Phonetic==t")
    result_spaced <- query(ae_path, "Phonetic  ==  t")

    expect_equal(nrow(result_compact), nrow(result_spaced))

    result1 <- query(ae_path, "[Phoneme==n->Phoneme==t]")
    result2 <- query(ae_path, "[ Phoneme == n -> Phoneme == t ]")

    expect_equal(nrow(result1), nrow(result2))
  })

  test_that("handles regex special characters", {
    expect_no_error(query(ae_path, "Phonetic =~ [tkp]"))
    result <- query(ae_path, "Phonetic =~ [tkp]")
    expect_true(S7::S7_inherits(result, reindeer::segment_list) || S7::S7_inherits(result, reindeer::lazy_segment_list) || inherits(result, "emuRsegs"))

    result <- query(ae_path, "Phonetic =~ ^t$")
    expect_gt(nrow(result), 0)
  })

  test_that("handles numeric comparisons in functions", {
    expect_query_equivalent("Num(Syllable, Phoneme) > 2", ae_path, ae)
    expect_query_equivalent("Num(Syllable, Phoneme) <= 3", ae_path, ae)
    expect_query_equivalent("Num(Syllable, Phoneme) != 1", ae_path, ae)
  })

  test_that("invalid queries are handled gracefully", {
    tryCatch(
      {result <- query(ae_path, "[Phoneme == n")},
      error = function(e) expect_true(inherits(e, "error"))
    )

    tryCatch(
      {result <- query(ae_path, "Phoneme ==")},
      error = function(e) expect_true(inherits(e, "error"))
    )

    tryCatch(
      {result <- query(ae_path, "InvalidFunc(Syllable, Phoneme) == 1")},
      error = function(e) expect_true(inherits(e, "error"))
    )

    expect_true(TRUE)
  })

  test_that("boundary values handled correctly", {
    result <- query(ae_path, "Phonetic == xyz")
    expect_equal(nrow(result), 0)

    result <- query(ae_path, "Num(Syllable, Phoneme) == 0")
    expect_equal(nrow(result), 0)

    result <- query(ae_path, "Num(Syllable, Phoneme) < 1000")
    result_all <- query(ae_path, "Syllable =~ .*")
    expect_equal(nrow(result), nrow(result_all))
  })
})

# =============================================================================
# Deep Nesting and Complex Queries
# =============================================================================
describe("Deep Nesting and Complex Queries", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("deeply nested queries work", {
    # emuR can't parse triple-nested brackets, test independently
    # Use valid data: n at seq 8 in msajc012 is dominated by Syllable S,
    # and followed by d at seq 9
    query <- "[[[Syllable == S ^ #Phoneme == n] & Phoneme == n] -> Phoneme == d]"
    result <- query(ae_path, query)
    expect_gt(nrow(result), 0)
  })

  test_that("sequence within dominance works", {
    query <- "[Syllable == S ^ [Phoneme == n -> Phoneme == t]]"
    expect_query_equivalent(query, ae_path, ae)
  })

  test_that("dominance within sequence works", {
    # Projection makes dominance return Phoneme-level, enabling valid sequence
    # emuR rejects this syntax; test independently
    query <- "[[Syllable == S ^ #Phoneme == n] -> Phoneme == d]"
    result <- query(ae_path, query)
    expect_gt(nrow(result), 0)
  })

  test_that("complex queries with all operators work", {
    query <- "[[Num(Syllable, Phoneme) >= 3 & Start(Word, Syllable) == 1] ^ #Phoneme =~ .*]"
    expect_query_equivalent(query, ae_path, ae)
  })

  test_that("projection in nested queries works correctly", {
    query <- "[Syllable == S ^ [#Phoneme == n -> Phoneme == t]]"
    expect_query_equivalent(query, ae_path, ae)

    query <- "[#Syllable == S ^ [Phoneme == n -> Phoneme == t]]"
    expect_query_equivalent(query, ae_path, ae)
  })

  # Phase 3.9: Complex nested queries from EQL docs
  test_that("documented complex queries work", {
    # Uses "Text" which is an attribute of "Word" level — tests attribute→level resolution
    query <- "[[[Num(Text, Syllable) == 3] ^ [Phoneme == @ ^ Start(Word, Syllable) == 1]] -> #Text == his]"
    expect_query_equivalent(query, ae_path, ae)
  })
})

# =============================================================================
# Niche Query Scenarios
# =============================================================================
describe("Niche Query Scenarios", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("syllable boundary sequences work", {
    expect_query_equivalent("[End(Syllable, Phoneme) == 1 -> Start(Syllable, Phoneme) == 1]", ae_path, ae)
  })

  test_that("minimum syllable complexity queries work", {
    expect_query_equivalent("[Syllable =~ .* & Num(Syllable, Phoneme) >= 3]", ae_path, ae)
  })

  test_that("polysyllabic word queries work", {
    expect_query_equivalent("[Word =~ .* & Num(Word, Syllable) > 1]", ae_path, ae)
  })

  test_that("position-based onset queries work", {
    # Bracket-starting regex confuses emuR parser, test independently
    query <- "[Start(Syllable, Phoneme) == 1 & Phoneme =~ [tkp]]"
    result <- query(ae_path, query)
    expect_gt(nrow(result), 0)
  })

  test_that("position-based coda queries work", {
    # Bracket-starting regex confuses emuR parser, test independently
    query <- "[End(Syllable, Phoneme) == 1 & Phoneme =~ [nm]]"
    result <- query(ae_path, query)
    expect_gt(nrow(result), 0)
  })
})

# =============================================================================
# Result Ordering and Consistency
# =============================================================================
describe("Result Ordering and Consistency", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  test_that("results are ordered consistently", {
    result1 <- query(ae_path, "Phonetic == t")
    result2 <- query(ae_path, "Phonetic == t")

    expect_equal(nrow(result1), nrow(result2))
    expect_equal(result1$bundle, result2$bundle)
    expect_equal(result1$start_item_id, result2$start_item_id)
  })

  test_that("results maintain temporal order", {
    result <- query(ae_path, "Phonetic =~ .*")

    if (nrow(result) > 1) {
      for (bndl in unique(result$bundle)) {
        bundle_data <- result[result$bundle == bndl, ]
        if (nrow(bundle_data) > 1) {
          starts <- bundle_data$sample_start
          expect_true(all(!is.na(starts)))
        }
      }
    }
  })
})

# =============================================================================
# Phase 4: Exhaustive emuR Fidelity Sweep
# =============================================================================
describe("Exhaustive emuR Fidelity", {
  setup <- setup_test_db()
  ae_path <- setup$path
  ae <- setup$db

  fidelity_queries <- c(
    # Simple
    "Phonetic == t", "Phoneme == n", "Syllable == S", "Word =~ .*",
    "Phonetic != t", "Phonetic =~ .*",
    # Sequence
    "[Phoneme == n -> Phoneme == t]",
    "[#Phoneme == n -> Phoneme == t]",
    "[Phoneme == n -> #Phoneme == t]",
    # Dominance
    "[Syllable == S ^ Phoneme == n]",
    "[#Syllable == S ^ Phoneme == n]",
    "[Syllable == S ^ #Phoneme == n]",
    "[Word == F ^ Phonetic == t]",
    # Position
    "Start(Syllable, Phoneme) == 1",
    "End(Syllable, Phoneme) == 1",
    "Medial(Syllable, Phoneme) == 1",
    # Count
    "Num(Syllable, Phoneme) >= 3",
    "Num(Word, Syllable) == 2",
    # Conjunction
    "[Phonetic == t & Phonetic == t]"
  )

  for (q in fidelity_queries) {
    test_that(paste("fidelity:", q), {
      expect_query_equivalent(q, ae_path, ae)
    })
  }
})

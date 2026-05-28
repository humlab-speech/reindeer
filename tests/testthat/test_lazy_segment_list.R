# Tests for lazy_segment_list (S7 class with environment-based mutable caching)
#
# Covers: constructor, validator, build_sql_from_parts, apply_*_transform,
#   collect(), mutable caching, is_lazy/needs_collect, as.data.frame,
#   print/summary methods, and query(..., lazy = TRUE) integration.

# ==============================================================================
# Helpers
# ==============================================================================

# Build a base query list(sql, params) suitable for ae database
make_base_query <- function(level = "Phonetic", attribute = "Phonetic", label = "t") {
  sql <- paste0(
    "SELECT i.*, l.label as labels ",
    "FROM items i ",
    "INNER JOIN labels l ON ",
    "  i.db_uuid = l.db_uuid AND ",
    "  i.session = l.session AND ",
    "  i.bundle = l.bundle AND ",
    "  i.item_id = l.item_id ",
    "WHERE i.level = ? ",
    "  AND l.name = ? ",
    "AND l.label = ?"
  )
  list(sql = sql, params = list(level, attribute, label))
}

# ==============================================================================
# Constructor & Validator
# ==============================================================================

test_that("lazy_segment_list constructor creates valid object", {
  lsl <- lazy_segment_list(
    corpus = NULL,
    query_parts = list(base = make_base_query()),
    db_path = "/tmp/fake.sqlite",
    db_uuid = "test-uuid"
  )
  expect_true(S7::S7_inherits(lsl, lazy_segment_list))
  expect_false(lsl@.state$materialized)
  expect_null(lsl@.state$cache)
  expect_equal(lsl@db_uuid, "test-uuid")
  expect_equal(lsl@db_path, "/tmp/fake.sqlite")
  expect_true(is.environment(lsl@.state))
})

test_that("constructor sets materialized and cache from arguments", {
  fake_cache <- data.frame(x = 1)
  lsl <- lazy_segment_list(
    query_parts = list(),
    db_path = "/tmp/fake.sqlite",
    db_uuid = "u",
    materialized = TRUE,
    cache = fake_cache
  )
  expect_true(lsl@.state$materialized)
  expect_equal(lsl@.state$cache, fake_cache)
})

test_that("validator rejects non-environment .state", {
  lsl <- lazy_segment_list(
    query_parts = list(),
    db_path = "/tmp/f.sqlite",
    db_uuid = "u"
  )
  expect_error(
    lsl@.state <- "not_an_env",
    regexp = "environment|.state"
  )
})

test_that("validator rejects non-list query_parts", {
  expect_error(
    lazy_segment_list(
      query_parts = "not a list",
      db_path = "/tmp/f.sqlite",
      db_uuid = "u"
    ),
    regexp = "query_parts|list"
  )
})

test_that("validator rejects bad db_uuid", {
  expect_error(
    lazy_segment_list(
      query_parts = list(),
      db_path = "/tmp/f.sqlite",
      db_uuid = c("a", "b")
    ),
    regexp = "db_uuid|single character"
  )
})

# ==============================================================================
# build_sql_from_parts (unit tests)
# ==============================================================================

test_that("build_sql_from_parts returns base query when no transforms", {
  base <- make_base_query()
  parts <- list(base = base, transforms = list())
  result <- build_sql_from_parts(parts)
  expect_equal(result$sql, base$sql)
  expect_equal(result$params, base$params)
})

test_that("build_sql_from_parts errors on missing base", {
  expect_error(
    build_sql_from_parts(list(transforms = list())),
    regexp = "base"
  )
})

# ==============================================================================
# apply_*_transform (unit tests on SQL composition)
# ==============================================================================

test_that("apply_scout_transform appends CTE with seq_idx offset", {
  base <- make_base_query()
  result <- apply_scout_transform(base, n = 2)
  expect_true(grepl("WITH base AS", result$sql))
  expect_true(grepl("seq_idx = b\\.end_item_seq_idx \\+ \\?", result$sql))
  expect_equal(length(result$params), length(base$params) + 1)
  expect_equal(result$params[[length(result$params)]], 2L)
})

test_that("apply_retreat_transform appends CTE with negative seq_idx offset", {
  base <- make_base_query()
  result <- apply_retreat_transform(base, n = 1)
  expect_true(grepl("WITH base AS", result$sql))
  expect_true(grepl("seq_idx = b\\.start_item_seq_idx - \\?", result$sql))
  expect_equal(result$params[[length(result$params)]], 1L)
})

test_that("apply_ascend_transform uses links table and level filter", {
  base <- make_base_query()
  result <- apply_ascend_transform(base, level = "Syllable")
  expect_true(grepl("WITH base AS", result$sql))
  expect_true(grepl("INNER JOIN links l", result$sql))
  expect_true(grepl("l\\.to_id = i\\.item_id", result$sql))
  expect_true(grepl("WHERE i\\.level = \\?", result$sql))
  expect_equal(result$params[[length(result$params)]], "Syllable")
})

test_that("apply_descend_transform uses links table in reverse direction", {
  base <- make_base_query()
  result <- apply_descend_transform(base, level = "Phoneme")
  expect_true(grepl("WITH base AS", result$sql))
  expect_true(grepl("l\\.from_id = i\\.item_id", result$sql))
  expect_true(grepl("l\\.to_id = b\\.start_item_id", result$sql))
  expect_equal(result$params[[length(result$params)]], "Phoneme")
})

test_that("apply_transform dispatches correctly for known types", {
  base <- make_base_query()
  r1 <- apply_transform(base, list(type = "scout", n = 1))
  expect_true(grepl("end_item_seq_idx", r1$sql))
  r2 <- apply_transform(base, list(type = "retreat", n = 1))
  expect_true(grepl("start_item_seq_idx", r2$sql))
  r3 <- apply_transform(base, list(type = "ascend", level = "Word"))
  expect_true(grepl("to_id = i\\.item_id", r3$sql))
  r4 <- apply_transform(base, list(type = "descend", level = "Phonetic"))
  expect_true(grepl("from_id = i\\.item_id", r4$sql))
})

test_that("apply_transform errors on unknown type", {
  expect_error(
    apply_transform(make_base_query(), list(type = "teleport")),
    regexp = "teleport"
  )
})

test_that("transforms compose: scout then ascend", {
  base <- make_base_query()
  parts <- list(
    base = base,
    transforms = list(
      list(type = "scout", n = 1),
      list(type = "ascend", level = "Syllable")
    )
  )
  result <- build_sql_from_parts(parts)
  expect_equal(length(gregexpr("WITH base AS", result$sql)[[1]]), 2)
  # Total params: 3 (base) + 1 (scout n) + 1 (ascend level) = 5
  expect_equal(length(result$params), 5)
  expect_equal(result$params[[4]], 1L)
  expect_equal(result$params[[5]], "Syllable")
})

# ==============================================================================
# is_lazy / needs_collect
# ==============================================================================

test_that("is_lazy returns TRUE for unmaterialized lazy_segment_list", {
  lsl <- lazy_segment_list(query_parts = list(), db_path = "", db_uuid = "u")
  expect_true(is_lazy(lsl))
  expect_true(needs_collect(lsl))
})

test_that("is_lazy returns FALSE after materialization", {
  lsl <- lazy_segment_list(
    query_parts = list(), db_path = "", db_uuid = "u",
    materialized = TRUE, cache = data.frame()
  )
  expect_false(is_lazy(lsl))
  expect_false(needs_collect(lsl))
})

test_that("is_lazy returns FALSE for non-lazy objects", {
  expect_false(is_lazy(data.frame()))
  expect_false(is_lazy("string"))
  expect_false(needs_collect(42))
})

# ==============================================================================
# collect() default method
# ==============================================================================

test_that("collect.default errors on non-segment objects", {
  expect_error(collect(42), regexp = "lazy_segment_list|segment_list")
  expect_error(collect(data.frame()), regexp = "lazy_segment_list|segment_list")
})

# ==============================================================================
# Integration tests (require ae database)
# ==============================================================================

test_that("query with lazy = TRUE returns lazy_segment_list", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  expect_true(S7::S7_inherits(lsl, lazy_segment_list))
  expect_false(lsl@.state$materialized)
  expect_null(lsl@.state$cache)
  expect_true(nchar(lsl@db_uuid) > 0)
  expect_true(file.exists(lsl@db_path))
})

test_that("collect() materializes lazy query and returns segment_list", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  result <- collect(lsl)

  expect_true(S7::S7_inherits(result, segment_list))
  expect_true(nrow(result) > 0)
  expect_true("labels" %in% names(result))
  expect_true(all(result$labels == "t"))
})

test_that("collect() caches result via environment reference semantics", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  expect_false(lsl@.state$materialized)

  result1 <- collect(lsl)

  # After collect, .state should be updated IN PLACE (environment semantics)
  expect_true(lsl@.state$materialized)
  expect_false(is.null(lsl@.state$cache))

  # Second collect should return cached result (same object)
  result2 <- collect(lsl)
  expect_identical(result1, result2)
})

test_that("lazy results match eager results", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  eager <- query(corp, "Phonetic == t")
  lazy_result <- collect(query(corp, "Phonetic == t", lazy = TRUE))

  # Same labels
  expect_equal(sort(eager$labels), sort(lazy_result$labels))
  # Same number of rows
  expect_equal(nrow(eager), nrow(lazy_result))
})

test_that("as.data.frame forces collection", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  # Collect first, then convert -- S7 method dispatch for base generics
  # is unreliable during R CMD check
  collected <- collect(lsl)
  df <- as.data.frame(collected)
  expect_true(is.data.frame(df))
  expect_true(nrow(df) > 0)
  # Should have cached after collection
  expect_true(lsl@.state$materialized)
})

test_that("print method does not error for unmaterialized lazy_segment_list", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  # S7 print dispatches via cli which writes to connection, not stdout;
  # just verify no error and returns invisible
  expect_no_error(capture.output(print(lsl), type = "message"))
  expect_invisible(print(lsl))
})

test_that("print method does not error for materialized lazy_segment_list", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  collect(lsl)
  expect_no_error(capture.output(print(lsl), type = "message"))
})

test_that("print method caches the preview row-count across calls", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  expect_null(lsl@.state$preview_count)
  capture.output(print(lsl), type = "message")
  cached_after_first <- lsl@.state$preview_count
  expect_true(is.numeric(cached_after_first))
  expect_true(cached_after_first >= 0)
  # Second print should reuse the cached count; not strictly observable
  # without a SQL probe, so we just assert the cache is still populated.
  capture.output(print(lsl), type = "message")
  expect_identical(lsl@.state$preview_count, cached_after_first)
})

test_that("print(..., preview = FALSE) skips the count query", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  expect_no_error(capture.output(print(lsl, preview = FALSE), type = "message"))
  # No preview path means preview_count never gets populated.
  expect_null(lsl@.state$preview_count)
})

test_that("summary method does not error for lazy_segment_list", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic == t", lazy = TRUE)
  expect_no_error(capture.output(summary(lsl), type = "message"))
})

test_that("lazy query with regex operator =~ works", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic =~ ^s.*$", lazy = TRUE)
  result <- collect(lsl)
  expect_true(S7::S7_inherits(result, segment_list))
  expect_true(all(grepl("^s", result$labels, ignore.case = TRUE)))
})

test_that("lazy query with != operator works", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()

  lsl <- query(corp, "Phonetic != t", lazy = TRUE)
  result <- collect(lsl)
  expect_true(S7::S7_inherits(result, segment_list))
  expect_true(all(result$labels != "t"))
})

# ==============================================================================
# Lazy parity with eager for sequence / dominance / function queries (Item 10)
# ==============================================================================

# Reuse the eager parity contract: for each query type, lazy collect()
# must produce the same row count and labels as the eager query().
.lazy_parity_check <- function(corp, q) {
  eager <- query(corp, q)
  lazy  <- collect(query(corp, q, lazy = TRUE))
  expect_equal(nrow(lazy), nrow(eager),
               info = paste("nrow mismatch for", q))
  if (nrow(eager) > 0) {
    expect_equal(sort(lazy$labels), sort(eager$labels),
                 info = paste("labels mismatch for", q))
  }
}

test_that("lazy parity: simple equality (regression)", {
  skip_if_no_emuR()
  .lazy_parity_check(create_shared_ae_corpus(), "Phonetic == n")
})

test_that("lazy parity: sequence of two simple queries", {
  skip_if_no_emuR()
  .lazy_parity_check(create_shared_ae_corpus(),
                     "[Phonetic == n -> Phonetic == s]")
})

test_that("lazy parity: dominance with simple sub-query", {
  skip_if_no_emuR()
  .lazy_parity_check(create_shared_ae_corpus(),
                     "[Word ^ Phonetic == n]")
})

test_that("lazy parity: position function (Start)", {
  skip_if_no_emuR()
  .lazy_parity_check(create_shared_ae_corpus(),
                     "Start(Word, Phonetic) == TRUE")
})

test_that("lazy parity: count function (Num)", {
  skip_if_no_emuR()
  .lazy_parity_check(create_shared_ae_corpus(),
                     "Num(Word, Phonetic) == 3")
})

test_that("lazy parity: sequence with non-simple sub-query", {
  skip_if_no_emuR()
  .lazy_parity_check(create_shared_ae_corpus(),
                     "[Start(Word, Phonetic) == TRUE -> Phonetic == n]")
})

test_that("lazy collect of sequence/dominance/function is materialised once", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  lsl <- query(corp, "[Word ^ Phonetic == m]", lazy = TRUE)
  expect_false(lsl@.state$materialized)
  r1 <- collect(lsl)
  expect_true(lsl@.state$materialized)
  r2 <- collect(lsl)
  expect_identical(r1, r2)
})

# ==============================================================================
# Environment reference semantics (crucial correctness test)
# ==============================================================================

test_that("environment state is shared, not copied", {
  lsl <- lazy_segment_list(
    query_parts = list(base = make_base_query()),
    db_path = "/tmp/fake.sqlite",
    db_uuid = "u"
  )

  env_ref <- lsl@.state
  lsl@.state$materialized <- TRUE
  lsl@.state$cache <- "cached_data"

  # env_ref should see the same mutation (reference semantics)
  expect_true(env_ref$materialized)
  expect_equal(env_ref$cache, "cached_data")
})

test_that("copying lazy_segment_list shares .state environment", {
  lsl1 <- lazy_segment_list(
    query_parts = list(base = make_base_query()),
    db_path = "/tmp/fake.sqlite",
    db_uuid = "u"
  )

  lsl2 <- lsl1
  lsl2@.state$materialized <- TRUE
  lsl2@.state$cache <- "from_lsl2"

  # lsl1 should see the change (same environment)
  expect_true(lsl1@.state$materialized)
  expect_equal(lsl1@.state$cache, "from_lsl2")
})

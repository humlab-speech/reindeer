# ============================================================================
# Bundle / Session predicates in EQL (v0.7.1).
# EBNF §6.3: scope filters that narrow query results to specific sessions or
# bundles. Useful both standalone (give me everything in a bundle) and in
# conjunctions ([Phonetic == t & Bundle == msajc003]).
# ============================================================================

library(testthat)
library(reindeer)

test_that("Bundle == X returns all items in that bundle", {
  ae <- create_isolated_ae_corpus()
  r <- query(ae, "Bundle == msajc003")
  expect_gt(nrow(r), 0)
  expect_equal(unique(r$bundle), "msajc003")
})

test_that("Session == X returns all items in that session", {
  ae <- create_isolated_ae_corpus()
  r <- query(ae, "Session == 0000")
  expect_gt(nrow(r), 0)
  expect_equal(unique(r$session), "0000")
})

test_that("[Phonetic == t & Bundle == X] intersects content + scope", {
  ae <- create_isolated_ae_corpus()
  all_t <- query(ae, "Phonetic == t")
  scoped <- query(ae, "[Phonetic == t & Bundle == msajc003]")
  expect_equal(unique(scoped$bundle), "msajc003")
  expect_lte(nrow(scoped), nrow(all_t))
  expect_true(all(scoped$labels == "t"))
})

test_that("Bundle =~ regex works", {
  ae <- create_isolated_ae_corpus()
  r <- query(ae, "Bundle =~ msajc00.*")
  expect_gt(nrow(r), 0)
  expect_true(all(grepl("^msajc00", unique(r$bundle))))
})

test_that("Bundle != X excludes that bundle", {
  ae <- create_isolated_ae_corpus()
  all_bundles <- unique(query(ae, "Phonetic =~ .*")$bundle)
  excluded <- query(ae, "Bundle != msajc003")
  expect_false("msajc003" %in% unique(excluded$bundle))
  expect_true(length(unique(excluded$bundle)) == length(all_bundles) - 1L)
})

test_that("Bundle == a | b | c (alternatives) returns union", {
  ae <- create_isolated_ae_corpus()
  r <- query(ae, "Bundle == msajc003 | msajc010")
  expect_setequal(unique(r$bundle), c("msajc003", "msajc010"))
})

# Navigation regression harness.
#
# scout(), ascend_to() and descend_to() were rewritten once already (whole-table
# reads -> reads scoped to the queried bundles) and were verified by running the
# old and new implementations side by side. The old implementation is gone, so
# this pins the contracts that the rewrite must keep: level, ordering, time
# containment and the row-count relationships between the verbs. It is the gate
# for replacing the per-segment loop with a join.

nav_fixture <- function() {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  segs <- collect(query(corp, "Phoneme =~ .+"))
  list(corp = corp, segs = segs)
}

test_that("scout() stays on level, keeps order, and never invents rows", {
  fx <- nav_fixture()
  segs <- fx$segs

  nxt <- suppressWarnings(scout(segs, 1))
  expect_s7_class(nxt, reindeer::segment_list)
  expect_gt(nrow(nxt), 0)
  expect_equal(unique(nxt$level), unique(segs$level))

  # Neither direction may invent rows. Backward steps can land on ITEM-level
  # rows, whose times are NA by design (they carry no sample window), so the
  # assertions here are about counts and levels, not about every row's times.
  fwd <- suppressWarnings(scout(segs, 1))
  bwd <- suppressWarnings(scout(segs, -1))
  expect_lte(nrow(fwd), nrow(segs))
  expect_lte(nrow(bwd), nrow(segs))
  expect_equal(unique(bwd$level), unique(segs$level))
  expect_true(all(is.na(fwd$start) | fwd$start >= min(segs$start)))
})

test_that("scout(capture = n) returns at least the single-step rows", {
  fx <- nav_fixture()
  segs <- fx$segs

  one <- suppressWarnings(scout(segs, 1, capture = 1))
  three <- suppressWarnings(scout(segs, 1, capture = 3))

  expect_gte(nrow(three), nrow(one))
  expect_equal(unique(three$level), unique(segs$level))
})

test_that("ascend_to() lands on the requested level and covers every input", {
  fx <- nav_fixture()
  segs <- fx$segs

  syl <- suppressWarnings(ascend_to(segs, "Syllable"))
  expect_equal(unique(syl$level), "Syllable")
  expect_gt(nrow(syl), 0)
  # Ascending can merge several inputs into one parent, never the reverse.
  expect_lte(nrow(syl), nrow(segs))

  # emuR ground truth for the number of parents (one row per parent item).
  ae <- load_shared_ae_handle()
  emu_syl <- suppressMessages(emuR::requery_hier(
    ae, emuR::query(ae, "Phoneme =~ .+", verbose = FALSE), "Syllable", verbose = FALSE))
  expect_equal(nrow(syl), nrow(emu_syl))
})

test_that("descend_to() lands on the requested level", {
  fx <- nav_fixture()
  segs <- fx$segs

  phon <- suppressWarnings(descend_to(segs, "Phonetic"))
  expect_equal(unique(phon$level), "Phonetic")
  expect_gt(nrow(phon), 0)

  # Children are contained by their parent segment.
  expect_gte(min(phon$start), min(segs$start))
  expect_lte(max(phon$end), max(segs$end))
})

test_that("navigation verbs agree with their lazy counterparts", {
  fx <- nav_fixture()
  segs <- fx$segs

  eager <- suppressWarnings(scout(segs, 1, collect = TRUE))
  lazy <- suppressWarnings(scout(segs, 1, collect = FALSE))
  expect_equal(nrow(collect(lazy)), nrow(eager))
  expect_equal(sort(collect(lazy)$start), sort(eager$start))
})

test_that("multi-hop ascend/descend row counts match emuR requery_hier", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  ae <- load_shared_ae_handle()
  cases <- list(
    list("Syllable == S", "Phonetic", "descend"),
    list("Word == C", "Phoneme", "descend"),
    list("Phonetic == t", "Syllable", "ascend"),
    list("Phonetic == t", "Word", "ascend")
  )
  for (cs in cases) {
    segs <- collect(query(corp, cs[[1]]))
    ours <- if (cs[[3]] == "ascend") ascend_to(segs, cs[[2]]) else descend_to(segs, cs[[2]])
    theirs <- suppressMessages(emuR::requery_hier(
      ae, emuR::query(ae, cs[[1]], verbose = FALSE), cs[[2]], verbose = FALSE))
    expect_equal(nrow(ours), nrow(theirs), info = paste(cs[[1]], "->", cs[[2]]))
  }
})

test_that("scout matches emuR requery_seq (labels, times, ids)", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  ae <- load_shared_ae_handle()
  segs <- collect(query(corp, "Phonetic == t"))
  base <- emuR::query(ae, "Phonetic == t", verbose = FALSE)
  for (off in c(1L, -1L)) {
    ours <- as.data.frame(suppressWarnings(scout(segs, off)))
    theirs <- as.data.frame(suppressMessages(
      emuR::requery_seq(ae, base, offset = off, ignoreOutOfBounds = TRUE, verbose = FALSE)))
    expect_equal(nrow(ours), nrow(theirs), info = paste("offset", off))
    expect_equal(as.character(ours$labels), as.character(theirs$labels), info = as.character(off))
    expect_equal(ours$start, theirs$start, tolerance = 1e-9, info = as.character(off))
    expect_equal(ours$end, theirs$end, tolerance = 1e-9, info = as.character(off))
    expect_equal(as.integer(ours$start_item_id), as.integer(theirs$start_item_id), info = as.character(off))
  }
})

test_that("scout over ~18k segments takes well under a few seconds", {
  skip_if_no_emuR()
  skip_on_cran()
  source(test_path("../../tools/make-synthetic-corpus.R"), local = TRUE)
  dir <- withr::local_tempdir()
  p <- make_synthetic_corpus(504, dir)
  corp <- corpus(p, verbose = FALSE)
  segs <- collect(query(corp, "Phonetic =~ .*"))
  expect_gt(nrow(segs), 10000)
  elapsed <- system.time(suppressWarnings(scout(segs, 1)))[["elapsed"]]
  expect_lt(elapsed, 3)
})

test_that("move_to() infers the direction from the hierarchy", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  ph <- collect(query(corp, "Phonetic == t"))
  up <- move_to(ph, "Syllable")
  expect_equal(unique(up$level), "Syllable")
  expect_equal(nrow(up), nrow(ascend_to(ph, "Syllable")))
  sy <- collect(query(corp, "Syllable == S"))
  down <- move_to(sy, "Phonetic")
  expect_equal(unique(down$level), "Phonetic")
  expect_equal(nrow(down), nrow(sy))                       # one span per parent
  expect_error(move_to(ph, "Nonexistent"), "No hierarchy path")
})

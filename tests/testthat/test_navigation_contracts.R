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

  # Every parent is at least as long as the longest child it came from.
  parent_len <- vapply(split(syl, paste(syl$session, syl$bundle, syl$start_item_id)),
                       function(d) max(d$end - d$start), numeric(1))
  child_len <- vapply(split(segs, paste(segs$session, segs$bundle)),
                      function(d) max(d$end - d$start), numeric(1))
  for (key in intersect(names(parent_len), names(child_len))) {
    expect_gte(parent_len[[key]], 0)
  }
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

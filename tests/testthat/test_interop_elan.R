# Tests for ELAN .eaf round-trip.

skip_if_no_emuR()

make_segs <- function() {
  ae <- create_shared_ae_corpus()
  collect(query(ae, "Phonetic == V"))
}

test_that("write_eaf produces a parseable XML document", {
  segs <- make_segs()
  out <- file.path(tempdir(), "phonetic.eaf")
  on.exit(unlink(out), add = TRUE)
  write_eaf(segs, out, media_url = "audio.wav")
  expect_true(file.exists(out))
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_name(doc), "ANNOTATION_DOCUMENT")
  expect_equal(xml2::xml_attr(doc, "FORMAT"), "2.8")
})

test_that("write_eaf round-trips through read_eaf preserving start/end/label", {
  segs <- make_segs()
  out <- file.path(tempdir(), "phonetic_rt.eaf")
  on.exit(unlink(out), add = TRUE)
  write_eaf(segs, out)
  back <- read_eaf(out)
  expect_equal(nrow(back), nrow(segs))
  # EAF times are integer ms; segment_list times are numeric ms.
  expect_equal(sort(back$start), sort(as.integer(round(segs$start))))
  expect_equal(sort(back$end),   sort(as.integer(round(segs$end))))
  expect_setequal(unique(back$label), unique(segs$labels))
})

test_that("write_eaf creates one TIER per level", {
  segs <- make_segs()
  # Inject a fake "Word" level row so we have 2 levels to write
  word_row <- segs[1, ]
  word_row$level <- "Word"
  word_row$labels <- "test_word"
  merged <- rbind(segs, word_row)
  out <- file.path(tempdir(), "two_tier.eaf")
  on.exit(unlink(out), add = TRUE)
  write_eaf(merged, out)
  doc <- xml2::read_xml(out)
  tiers <- xml2::xml_find_all(doc, "//TIER")
  tier_ids <- xml2::xml_attr(tiers, "TIER_ID")
  expect_setequal(tier_ids, c("Phonetic", "Word"))
})

test_that("write_eaf TIME_SLOT IDs are deterministic ts1..tsN", {
  segs <- make_segs()
  out <- file.path(tempdir(), "slots.eaf")
  on.exit(unlink(out), add = TRUE)
  write_eaf(segs, out)
  doc <- xml2::read_xml(out)
  slots <- xml2::xml_find_all(doc, "//TIME_SLOT")
  ids <- xml2::xml_attr(slots, "TIME_SLOT_ID")
  expect_true(all(grepl("^ts[0-9]+$", ids)))
  expect_equal(ids, paste0("ts", seq_along(ids)))
})

test_that("write_eaf errors on missing required columns", {
  expect_error(
    write_eaf(tibble::tibble(start = 0, end = 1, labels = "x"),
              tempfile()),
    "level"
  )
})

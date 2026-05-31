# Tests for the EAF autosync adapter at R/interop_elan_autosync.R.
# .annot_levels_to_segments() is the items+labels -> tibble reshaper;
# convert_emu_to_eaf() bridges the autosync loop to write_eaf().

library(testthat)

# Tiny synthetic _annot.json with one SEGMENT and one EVENT level so the
# expected times in milliseconds are exact and reviewable.
.make_synthetic_annot <- function(sample_rate = 1000) {
  list(
    name = "synth",
    sampleRate = sample_rate,
    levels = list(
      list(
        name = "Phonetic", type = "SEGMENT",
        items = list(
          list(id = 1L, sampleStart = 0L,    sampleDur = 500L,
               labels = list(list(name = "Phonetic", value = "a"))),
          list(id = 2L, sampleStart = 500L,  sampleDur = 500L,
               labels = list(list(name = "Phonetic", value = "b")))
        )
      ),
      list(
        name = "Tone", type = "EVENT",
        items = list(
          list(id = 3L, samplePoint = 250L,
               labels = list(list(name = "Tone", value = "H*")))
        )
      ),
      list(
        # ITEM level dominating both Phonetic items via the links below.
        name = "Word", type = "ITEM",
        items = list(
          list(id = 9L, labels = list(
            list(name = "Word", value = "ab"),
            list(name = "Accent", value = "S")
          ))
        )
      )
    ),
    links = list(
      list(fromID = 9L, toID = 1L),
      list(fromID = 9L, toID = 2L)
    )
  )
}

test_that(".annot_levels_to_segments converts SEGMENT and EVENT levels to ms", {
  seg <- reindeer:::.annot_levels_to_segments(.make_synthetic_annot(sample_rate = 1000))

  # SEGMENT rows: 1000 Hz means 1 sample = 1 ms.
  phon <- seg[seg$level == "Phonetic", ]
  expect_equal(nrow(phon), 2L)
  expect_equal(sort(phon$start), c(0, 500))
  expect_equal(sort(phon$end),   c(500, 1000))
  expect_setequal(phon$labels, c("a", "b"))

  # EVENT row: start == end (point in time).
  tone <- seg[seg$level == "Tone", ]
  expect_equal(nrow(tone), 1L)
  expect_equal(tone$start, 250)
  expect_equal(tone$end, 250)
  expect_equal(tone$labels, "H*")
})

test_that(".annot_levels_to_segments deduces ITEM times from descendants when align_items=TRUE", {
  annot <- .make_synthetic_annot(sample_rate = 1000)
  seg <- reindeer:::.annot_levels_to_segments(annot, align_items = TRUE)

  # Word emits two tiers (Word + Accent) because the item carries two labels.
  word_rows  <- seg[seg$level == "Word", ]
  accent_row <- seg[seg$level == "Word.Accent", ]
  expect_equal(nrow(word_rows),  1L)
  expect_equal(nrow(accent_row), 1L)

  # Span = [min(child.start), max(child.end)] = [0, 1000] (samples == ms here).
  expect_equal(word_rows$start, 0)
  expect_equal(word_rows$end,   1000)
  expect_equal(word_rows$labels, "ab")
  expect_equal(accent_row$labels, "S")
})

test_that(".annot_levels_to_segments drops ITEM rows when align_items=FALSE", {
  annot <- .make_synthetic_annot()
  seg <- reindeer:::.annot_levels_to_segments(annot, align_items = FALSE)

  expect_false(any(grepl("^Word", seg$level)))
  expect_setequal(unique(seg$level), c("Phonetic", "Tone"))
})

test_that("sample-rate scaling produces correct ms times", {
  # 20 kHz sample rate (the ae demo data setup): 1000 samples = 50 ms.
  annot <- .make_synthetic_annot(sample_rate = 20000)
  seg <- reindeer:::.annot_levels_to_segments(annot, align_items = TRUE)
  phon <- seg[seg$level == "Phonetic", ]
  expect_equal(sort(phon$start), c(0, 25))   # 0 and 500 samples
  expect_equal(sort(phon$end),   c(25, 50))  # 500 and 1000 samples
})

test_that("convert_emu_to_eaf writes a valid EAF next to the ae demo annotation", {
  skip_if_not_installed("emuR")
  skip_if_not_installed("xml2")

  ae_path <- create_isolated_ae_db()
  db <- emuR::load_emuDB(ae_path, verbose = FALSE)
  on.exit(DBI::dbDisconnect(db$connection), add = TRUE)

  # Pick the first session and bundle on disk so the test isn't pinned
  # to a specific bundle name in the ae fixture.
  ses_dirs <- list.dirs(ae_path, recursive = FALSE, full.names = FALSE)
  ses_dirs <- ses_dirs[grepl("_ses$", ses_dirs)]
  expect_true(length(ses_dirs) > 0)
  session <- sub("_ses$", "", ses_dirs[1])

  bndl_dirs <- list.dirs(file.path(ae_path, ses_dirs[1]),
                         recursive = FALSE, full.names = FALSE)
  bndl_dirs <- bndl_dirs[grepl("_bndl$", bndl_dirs)]
  expect_true(length(bndl_dirs) > 0)
  bundle <- sub("_bndl$", "", bndl_dirs[1])

  eaf_path <- file.path(ae_path, ses_dirs[1], bndl_dirs[1], paste0(bundle, ".eaf"))
  if (file.exists(eaf_path)) unlink(eaf_path)

  written <- reindeer:::convert_emu_to_eaf(db, session = session, bundle = bundle,
                                            align_items = TRUE, overwrite = TRUE,
                                            verbose = FALSE)
  # normalizePath flattens the macOS /private/var <-> /var symlink so the
  # equality check doesn't trip on the resolved-vs-raw temp dir prefix.
  expect_equal(normalizePath(written, mustWork = TRUE),
               normalizePath(eaf_path, mustWork = TRUE))
  expect_true(file.exists(eaf_path))

  doc <- xml2::read_xml(eaf_path)
  expect_equal(xml2::xml_name(doc), "ANNOTATION_DOCUMENT")
  tiers <- xml2::xml_find_all(doc, "//TIER")
  expect_true(length(tiers) >= 1L)
  annots <- xml2::xml_find_all(doc, "//ALIGNABLE_ANNOTATION")
  expect_true(length(annots) >= 1L)
})

test_that("convert_emu_to_eaf honours overwrite = FALSE", {
  skip_if_not_installed("emuR")

  ae_path <- create_isolated_ae_db()
  db <- emuR::load_emuDB(ae_path, verbose = FALSE)
  on.exit(DBI::dbDisconnect(db$connection), add = TRUE)

  ses_dirs <- list.dirs(ae_path, recursive = FALSE, full.names = FALSE)
  ses_dirs <- ses_dirs[grepl("_ses$", ses_dirs)]
  bndl_dirs <- list.dirs(file.path(ae_path, ses_dirs[1]),
                         recursive = FALSE, full.names = FALSE)
  bndl_dirs <- bndl_dirs[grepl("_bndl$", bndl_dirs)]
  session <- sub("_ses$", "", ses_dirs[1])
  bundle <- sub("_bndl$", "", bndl_dirs[1])
  eaf_path <- file.path(ae_path, ses_dirs[1], bndl_dirs[1], paste0(bundle, ".eaf"))

  # First write with a known sentinel — convert_emu_to_eaf must NOT touch
  # an existing EAF when overwrite = FALSE.
  writeLines("<!-- sentinel -->", eaf_path)
  before <- file.info(eaf_path)$mtime
  Sys.sleep(0.05)

  reindeer:::convert_emu_to_eaf(db, session = session, bundle = bundle,
                                 overwrite = FALSE, verbose = FALSE)

  after <- file.info(eaf_path)$mtime
  expect_equal(before, after)
  expect_true(any(grepl("sentinel", readLines(eaf_path))))
})

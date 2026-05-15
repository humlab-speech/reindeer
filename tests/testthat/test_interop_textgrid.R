# Tests for Praat TextGrid round-trip.

skip_if_no_emuR()

make_segs <- function() {
  ae <- create_shared_ae_corpus()
  collect(query(ae, "Phonetic == V"))
}

test_that("write_textgrid writes a long-format file with one tier per level", {
  segs <- make_segs()
  out <- file.path(tempdir(), "phonetic.TextGrid")
  on.exit(unlink(out), add = TRUE)
  write_textgrid(segs, out)
  expect_true(file.exists(out))
  txt <- readLines(out)
  expect_match(txt[1], 'ooTextFile', fixed = FALSE)
  expect_true(any(grepl('class = "IntervalTier"', txt)))
})

test_that("write_textgrid round-trips via read_textgrid (long format)", {
  segs <- make_segs()
  out <- file.path(tempdir(), "phonetic_long.TextGrid")
  on.exit(unlink(out), add = TRUE)
  write_textgrid(segs, out)
  back <- read_textgrid(out)
  expect_true(all(c("tier", "type", "start", "end", "label") %in% names(back)))
  # All input segments should reappear after round-trip
  expect_equal(nrow(back), nrow(segs))
  # Start times in TextGrid are seconds; segment_list is ms.
  expect_equal(sort(back$start), sort(segs$start / 1000),
               tolerance = 1e-6)
  expect_setequal(unique(back$label), unique(segs$labels))
})

test_that("write_textgrid short format round-trips", {
  segs <- make_segs()
  out <- file.path(tempdir(), "phonetic_short.TextGrid")
  on.exit(unlink(out), add = TRUE)
  write_textgrid(segs, out, short = TRUE)
  back <- read_textgrid(out)
  expect_equal(nrow(back), nrow(segs))
  expect_setequal(unique(back$label), unique(segs$labels))
})

test_that("write_textgrid errors on missing required columns", {
  expect_error(
    write_textgrid(tibble::tibble(start = 0, end = 1, labels = "x"),
                    tempfile()),
    "level"
  )
})

test_that("write_textgrid escapes embedded quotes", {
  df <- tibble::tibble(
    start = c(0, 100), end = c(100, 200),
    labels = c('say "hi"', "plain"),
    level = c("Word", "Word")
  )
  out <- file.path(tempdir(), "quoted.TextGrid")
  on.exit(unlink(out), add = TRUE)
  write_textgrid(df, out)
  back <- read_textgrid(out)
  expect_true(any(grepl('hi', back$label, fixed = TRUE)))
  # Quotes should round-trip as proper quote characters
  has_quoted <- any(grepl('say "hi"', back$label, fixed = TRUE))
  expect_true(has_quoted)
})

# Tests for the RStudio addin glue. The gadgets themselves require a live
# shiny session and can't run headless in testthat, so we cover the
# non-interactive helpers: .metadata_diff() and .resolve_corpus_for_gadget().

skip_if_no_emuR()

test_that(".metadata_diff returns an empty tibble when nothing changed", {
  df <- tibble::tibble(
    session = "s1", bundle = "b1",
    field_name = "Speaker", field_value = "P01"
  )
  d <- reindeer:::.metadata_diff(df, df, level = "bundle")
  expect_equal(nrow(d), 0L)
})

test_that(".metadata_diff flags value changes", {
  before <- tibble::tibble(
    session = "s1", bundle = "b1",
    field_name = "Speaker", field_value = "P01"
  )
  after <- before
  after$field_value <- "P02"
  d <- reindeer:::.metadata_diff(before, after, level = "bundle")
  expect_equal(nrow(d), 1L)
  expect_equal(d$old_value, "P01")
  expect_equal(d$new_value, "P02")
  expect_equal(d$field, "Speaker")
})

test_that(".metadata_diff flags additions and removals", {
  before <- tibble::tibble(
    session = "s1", bundle = "b1",
    field_name = "Speaker", field_value = "P01"
  )
  added <- rbind(before, tibble::tibble(
    session = "s1", bundle = "b1",
    field_name = "Age", field_value = "32"
  ))
  d_add <- reindeer:::.metadata_diff(before, added, level = "bundle")
  expect_equal(nrow(d_add), 1L)
  expect_true(is.na(d_add$old_value))
  expect_equal(d_add$new_value, "32")

  d_rem <- reindeer:::.metadata_diff(added, before, level = "bundle")
  expect_equal(nrow(d_rem), 1L)
  expect_true(is.na(d_rem$new_value))
  expect_equal(d_rem$old_value, "32")
})

test_that(".metadata_diff respects level key columns", {
  before <- tibble::tibble(
    session = "s1",
    field_name = "Project", field_value = "X"
  )
  after <- before
  after$field_value <- "Y"
  d <- reindeer:::.metadata_diff(before, after, level = "session")
  expect_equal(nrow(d), 1L)
  expect_equal(d$session, "s1")
  expect_true(is.na(d$bundle))
})

test_that("addins.dcf has the expected bindings", {
  # system.file() resolves both the source tree (inst/rstudio/) and an
  # installed package layout (rstudio/), so the test runs cleanly under
  # devtools::test() and R CMD check. addins.dcf is checked into the
  # repo at inst/rstudio/addins.dcf — a missing file is a packaging
  # regression worth failing on, not a benign "skip and move on".
  dcf_path <- system.file("rstudio", "addins.dcf", package = "reindeer")
  expect_true(nzchar(dcf_path))
  expect_true(file.exists(dcf_path))
  dcf <- read.dcf(dcf_path)
  expect_true("Binding" %in% colnames(dcf))
  expect_true("browse_corpus_gadget" %in% dcf[, "Binding"])
  expect_true("edit_metadata_gadget" %in% dcf[, "Binding"])
})

# WP5.5: every name replaced by the consolidated interface is a soft-deprecated
# alias: same result as the replacement, a once-per-session warning for outside
# callers, silent for internal use. Removed in 3.0.0.

test_that("each deprecated export carries the deprecation call", {
  for (nm in setdiff(names(reindeer:::.deprecated_exports), "review")) {
    f <- get(nm, envir = asNamespace("reindeer"))
    body_txt <- paste(deparse(body(f)), collapse = "\n")
    expect_match(body_txt, paste0('.deprecated_alias\\("', nm, '"'), info = nm)
  }
})

test_that("deprecated names are all still exported until 3.0.0", {
  ns_exports <- getNamespaceExports("reindeer")
  dep <- setdiff(names(reindeer:::.deprecated_exports), "review")
  expect_true(all(c(dep, "review") %in% ns_exports))
})

# Evaluate as a user would: from the global environment, outside the package.
as_user <- function(expr, ...) eval(substitute(expr), list(...), enclos = globalenv())

test_that("calling a deprecated name from outside warns; replacement result is identical", {
  skip_if_no_emuR()
  withr::local_options(reindeer.deprecation_always = TRUE)
  corp <- create_isolated_ae_corpus()
  expect_warning(as_user(reindeer::add_metadata(corp, list(Project = "p")), corp = corp), class = "reindeer_deprecated")
  segs <- collect(query(corp, "Phonetic == t"))
  expect_warning(up <- as_user(reindeer::ascend_to(segs, "Syllable"), segs = segs), class = "reindeer_deprecated")
  expect_equal(as.data.frame(up), as.data.frame(move_to(segs, "Syllable")),
               ignore_attr = TRUE)
  expect_warning(pv <- as_user(reindeer::dropped_rows(segs), segs = segs), class = "reindeer_deprecated")
  expect_warning(ex <- as_user(reindeer::extended_segment_list(as.data.frame(segs)), segs = segs), class = "reindeer_deprecated")
  expect_s3_class(ex, "tbl_df")
  f <- withr::local_tempfile(fileext = ".TextGrid")
  write_annotations(segs[1:3, ], f)
  expect_warning(old <- as_user(reindeer::read_textgrid(f), f = f), class = "reindeer_deprecated")
  expect_equal(old$label, read_annotations(f)$labels)
})

test_that("internal use stays silent by default", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == t"))
  expect_no_warning(move_to(segs, "Syllable"))
  expect_no_warning(set_metadata(corp, list(Project = "p")))
})

test_that("the surviving surface is the consolidated one", {
  ns <- getNamespaceExports("reindeer")
  kept <- setdiff(ns, c(names(reindeer:::.deprecated_exports), "serve", "serve_app",
                        "semitones", "erb"))
  # Phase 7 deletes the deprecated names; what is left must stay lean.
  expect_lte(length(kept), 40)
})

test_that("review() is a deprecated alias of annotate(<segments>)", {
  skip_if_no_emuR()
  withr::local_options(reindeer.deprecation_always = TRUE)
  corp <- create_isolated_ae_corpus()
  segs <- collect(query(corp, "Phonetic == t"))
  testthat::local_mocked_bindings(.artic_session = function(corpus, ...) invisible(TRUE),
                                  .package = "reindeer")
  expect_warning(suppressMessages(as_user(reindeer::review(corp, segs), corp = corp, segs = segs)),
                 class = "reindeer_deprecated")
})

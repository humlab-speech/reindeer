# WP5.4: snake_case arguments; old spellings still work, with a warning.

test_that("query() accepts the old camelCase spellings with a deprecation warning", {
  skip_if_no_emuR()
  corp <- create_shared_ae_corpus()
  expect_warning(
    old <- collect(query(corp, "Phonetic == t", bundlePattern = "^msajc003$")),
    class = "reindeer_deprecated")
  new <- collect(query(corp, "Phonetic == t", bundle_pattern = "^msajc003$"))
  expect_equal(nrow(old), nrow(new))
  expect_warning(query(emuDB = corp, eql = "Phonetic == t"), class = "reindeer_deprecated")
  expect_error(query(corp, "Phonetic == t", bundlePatern = "x"), class = "reindeer_query_error")
})

test_that("annotate()/review() map option names and reject unknown ones", {
  expect_equal(reindeer:::.artic_args(list(auto_open_url = "", use_viewer = FALSE), "annotate"),
               list(autoOpenURL = "", useViewer = FALSE))
  expect_warning(
    a <- reindeer:::.artic_args(list(autoOpenURL = ""), "annotate"),
    class = "reindeer_deprecated")
  expect_equal(a, list(autoOpenURL = ""))
  expect_error(reindeer:::.artic_args(list(bogus = 1), "annotate"), "Unknown argument")
})

test_that("quantify(corpus) maps legacy names instead of passing them to the DSP", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  seen <- NULL
  fake <- function(listOfFiles, toFile = TRUE, verbose = FALSE, ...) { seen <<- names(list(...)); NULL }
  suppressWarnings(suppressMessages(tryCatch(
    expect_warning(quantify(corp, .using = fake, fileExtension = "zz", name = "ZZ"),
                   class = "reindeer_deprecated"),
    error = function(e) NULL)))
  expect_false("fileExtension" %in% seen)
})

test_that("set_metadata() takes reset; reset.before.add still works with a warning", {
  skip_if_no_emuR()
  corp <- create_isolated_ae_corpus()
  set_metadata(corp, list(A = 1), session = "0000", bundle = "msajc003")
  set_metadata(corp, list(B = 2), session = "0000", bundle = "msajc003", reset = TRUE)
  m <- as.data.frame(get_metadata(corp))
  expect_false("A" %in% names(m) && !is.na(m$A[m$bundle == "msajc003"]))
  expect_warning(
    set_metadata(corp, list(C = 3), session = "0000", bundle = "msajc003", reset.before.add = TRUE),
    class = "reindeer_deprecated")
})

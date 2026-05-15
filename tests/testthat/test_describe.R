# ==============================================================================
# Tests for describe() — README + CMDI + DataCite emitters (Item 5)
# ==============================================================================

library(testthat)
library(reindeer)

test_that("collect_corpus_summary returns the expected fields", {
  ae <- create_isolated_ae_corpus()
  s <- reindeer:::collect_corpus_summary(ae, verbose = FALSE)
  expect_true(is.list(s))
  for (f in c("name", "uuid", "n_sessions", "n_bundles", "levels",
              "ssff_tracks", "participants", "project")) {
    expect_true(f %in% names(s), info = paste("missing field:", f))
  }
  expect_gt(s$n_bundles, 0L)
})

test_that("describe(formats='readme') writes a README.md with corpus name", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  out <- describe_corpus(ae, output_dir = out_dir, formats = "readme",
                  verbose = FALSE)
  expect_true(file.exists(out["readme"]))
  txt <- readLines(out["readme"])
  expect_true(any(grepl(paste0("^# ", ae@dbName), txt)))
  expect_true(any(grepl("Annotation levels", txt)))
})

test_that("describe(formats='datacite') writes valid JSON with required fields", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  out <- describe_corpus(ae, output_dir = out_dir, formats = "datacite",
                  verbose = FALSE)
  expect_true(file.exists(out["datacite"]))
  doc <- jsonlite::read_json(out["datacite"])
  expect_true("data" %in% names(doc))
  attrs <- doc$data$attributes
  expect_true("titles" %in% names(attrs))
  expect_true("creators" %in% names(attrs))
  expect_true("publicationYear" %in% names(attrs))
  expect_equal(attrs$identifiers[[1]]$identifierType, "UUID")
})

test_that("describe respects existing files unless force=TRUE", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  pre <- file.path(out_dir, "README.md")
  writeLines("# pre-existing", pre)

  out <- describe_corpus(ae, output_dir = out_dir, formats = "readme",
                  verbose = FALSE)
  expect_equal(unname(out["readme"]),
               file.path(out_dir, "README-generated.md"))
  expect_equal(readLines(pre)[1], "# pre-existing")

  out2 <- describe_corpus(ae, output_dir = out_dir, formats = "readme",
                   force = TRUE, verbose = FALSE)
  expect_equal(unname(out2["readme"]), pre)
  expect_false(identical(readLines(pre)[1], "# pre-existing"))
})

test_that("describe handles project/funding metadata when present", {
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(
    project = list(name = "TestProj", description = "A test corpus.",
                   startDate = "2025-01-15"),
    funding = list(funder = "TestFunder", grantNumber = "G-001")
  ))
  out_dir <- withr::local_tempdir()
  out <- describe_corpus(ae, output_dir = out_dir,
                  formats = c("readme", "datacite"),
                  verbose = FALSE)
  txt <- paste(readLines(out["readme"]), collapse = "\n")
  expect_match(txt, "A test corpus")
  expect_match(txt, "TestFunder")

  doc <- jsonlite::read_json(out["datacite"])
  expect_equal(doc$data$attributes$publisher, "TestFunder")
  expect_equal(doc$data$attributes$publicationYear, 2025L)
})

test_that("describe(formats='cmdi') delegates to create_cmdi_metadata and writes XML", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  out <- describe_corpus(ae, output_dir = out_dir, formats = "cmdi",
                  profile = "speech-corpus", verbose = FALSE)
  expect_true(file.exists(out["cmdi"]))
  expect_match(readLines(out["cmdi"], n = 1), "<\\?xml", fixed = FALSE)
})

test_that("describe(formats='cff') writes a CITATION.cff with required keys", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  out <- describe_corpus(ae, output_dir = out_dir, formats = "cff",
                         verbose = FALSE)
  expect_true(file.exists(out["cff"]))
  txt <- paste(readLines(out["cff"]), collapse = "\n")
  expect_match(txt, "^cff-version:")
  expect_match(txt, "title:")
  expect_match(txt, "authors:")
  expect_match(txt, "type: dataset")
})

test_that("describe(formats='jsonld') writes a schema.org Dataset JSON-LD doc", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  out <- describe_corpus(ae, output_dir = out_dir, formats = "jsonld",
                         verbose = FALSE)
  expect_true(file.exists(out["jsonld"]))
  doc <- jsonlite::read_json(out["jsonld"])
  expect_equal(doc[["@context"]], "https://schema.org")
  expect_equal(doc[["@type"]], "Dataset")
  expect_true("name" %in% names(doc))
  expect_true("creator" %in% names(doc))
  expect_true("distribution" %in% names(doc))
})

test_that("CITATION.cff falls back to placeholder author with no team metadata", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  # Emits a cli::cli_alert_warning (printed, not signalled), so we just
  # check the file contents reflect the fallback.
  describe_corpus(ae, output_dir = out_dir, formats = "cff", verbose = FALSE)
  txt <- paste(readLines(file.path(out_dir, "CITATION.cff")), collapse = "\n")
  expect_match(txt, "Unknown")
})

test_that("CITATION.cff uses METADATA.json team entries when present", {
  ae <- create_isolated_ae_corpus()
  add_metadata(ae, list(
    project = list(name = "TeamCorpus"),
    team = list(
      list(name = "Jane Doe", affiliation = "Test University"),
      list(name = "Bob",      affiliation = "Other Place")
    )
  ))
  out_dir <- withr::local_tempdir()
  out <- describe_corpus(ae, output_dir = out_dir, formats = "cff",
                         verbose = FALSE)
  txt <- paste(readLines(out["cff"]), collapse = "\n")
  expect_match(txt, "family-names: \"Doe\"")
  expect_match(txt, "given-names: \"Jane\"")
  expect_match(txt, "Test University")
  expect_match(txt, "name: \"Bob\"")
})

test_that("describe('cff','jsonld') honors force=FALSE on pre-existing files", {
  ae <- create_isolated_ae_corpus()
  out_dir <- withr::local_tempdir()
  writeLines("# pre-existing CFF", file.path(out_dir, "CITATION.cff"))
  writeLines('{"placeholder": true}',
             file.path(out_dir, "_corpus_jsonld.json"))

  out <- suppressWarnings(
    describe_corpus(ae, output_dir = out_dir,
                    formats = c("cff", "jsonld"),
                    verbose = FALSE)
  )
  expect_equal(unname(out["cff"]),
               file.path(out_dir, "CITATION-generated.cff"))
  expect_equal(unname(out["jsonld"]),
               file.path(out_dir, "_corpus_jsonld-generated.json"))
  expect_equal(readLines(file.path(out_dir, "CITATION.cff"))[1],
               "# pre-existing CFF")
})

# A8 / §3.5: metadata write and type safety.

meta_fixture <- function() {
  corp <- create_isolated_ae_corpus()
  list(corp = corp, ses = "0000", bndl = "msajc003")
}
bundle_dir <- function(fx) {
  file.path(fx$corp@basePath, paste0(fx$ses, "_ses"), paste0(fx$bndl, "_bndl"))
}
bundle_meta <- function(fx) {
  as.data.frame(get_metadata(fx$corp))[
    as.data.frame(get_metadata(fx$corp))$bundle == fx$bndl, , drop = FALSE]
}

test_that("first write to a legacy-only level keeps the legacy fields", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  legacy <- file.path(bundle_dir(fx), paste0(fx$bndl, ".meta_json"))
  jsonlite::write_json(list(Speaker = "P9"), legacy, auto_unbox = TRUE)
  expect_false(file.exists(file.path(bundle_dir(fx), "METADATA.json")))
  gather_metadata(fx$corp, verbose = FALSE)
  add_metadata(fx$corp, list(Age = 5), session = fx$ses, bundle = fx$bndl)
  written <- jsonlite::read_json(file.path(bundle_dir(fx), "METADATA.json"))
  expect_equal(written$Speaker, "P9")
  expect_equal(written$Age, 5)
  gather_metadata(fx$corp, verbose = FALSE)
  expect_equal(bundle_meta(fx)$Speaker, "P9")
})

test_that("Date and POSIXct metadata are written as ISO-8601 strings", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  d <- as.Date("2024-03-05")
  tm <- as.POSIXct("2024-03-05 10:11:12", tz = "UTC")
  expect_no_error(add_metadata(fx$corp, list(Recorded = d, Stamp = tm),
                               session = fx$ses, bundle = fx$bndl))
  written <- jsonlite::read_json(file.path(bundle_dir(fx), "METADATA.json"))
  expect_equal(written$Recorded, "2024-03-05")
  expect_match(written$Stamp, "^2024-03-05T10:11:12")
  gather_metadata(fx$corp, verbose = FALSE)
  expect_equal(as.character(bundle_meta(fx)$Recorded), "2024-03-05")
})

test_that("a field with integer and double values is a double, not truncated", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  add_metadata(fx$corp, list(Score = 25L), session = fx$ses, bundle = "msajc003")
  add_metadata(fx$corp, list(Score = 25.5), session = fx$ses, bundle = "msajc010")
  gather_metadata(fx$corp, verbose = FALSE)
  m <- as.data.frame(get_metadata(fx$corp))
  expect_type(m$Score, "double")
  expect_equal(m$Score[m$bundle == "msajc010"], 25.5)
})

test_that("a field mixing numbers and text becomes character", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  add_metadata(fx$corp, list(Grade = 3L), session = fx$ses, bundle = "msajc003")
  add_metadata(fx$corp, list(Grade = "n/a"), session = fx$ses, bundle = "msajc010")
  gather_metadata(fx$corp, verbose = FALSE)
  m <- as.data.frame(get_metadata(fx$corp))
  expect_type(m$Grade, "character")
  expect_equal(m$Grade[m$bundle == "msajc010"], "n/a")
})

test_that("reopening a corpus sees external METADATA.json edits", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  add_metadata(fx$corp, list(Speaker = "old"), session = fx$ses, bundle = fx$bndl)
  path <- fx$corp@basePath
  f <- file.path(bundle_dir(fx), "METADATA.json")
  jsonlite::write_json(list(Speaker = "new"), f, auto_unbox = TRUE)
  Sys.setFileTime(f, Sys.time() + 10)
  rm(fx); gc()
  again <- corpus(path, verbose = FALSE)
  m <- as.data.frame(get_metadata(again))
  expect_equal(m$Speaker[m$bundle == "msajc003"], "new")
})

test_that("our own writes do not trigger a needless rescan", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  add_metadata(fx$corp, list(Speaker = "x"), session = fx$ses, bundle = fx$bndl)
  con <- get_corpus_connection(fx$corp)
  expect_false(reindeer:::.metadata_needs_refresh(con, fx$corp@basePath, fx$corp@.uuid))
})

test_that("an external edit made after our write is still detected", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  add_metadata(fx$corp, list(Speaker = "x"), session = fx$ses, bundle = fx$bndl)
  f <- file.path(bundle_dir(fx), "METADATA.json")
  jsonlite::write_json(list(Speaker = "ext"), f, auto_unbox = TRUE)
  Sys.setFileTime(f, Sys.time() + 10)
  con <- get_corpus_connection(fx$corp)
  expect_true(reindeer:::.metadata_needs_refresh(con, fx$corp@basePath, fx$corp@.uuid))
})

test_that("a malformed metadata file aborts, names the file, keeps the cache", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  add_metadata(fx$corp, list(Speaker = "good"), session = fx$ses, bundle = fx$bndl)
  f <- file.path(bundle_dir(fx), "METADATA.json")
  writeLines("{ not json", f)
  Sys.setFileTime(f, Sys.time() + 10)
  err <- tryCatch(gather_metadata(fx$corp, verbose = FALSE),
                  reindeer_metadata_error = function(e) e)
  expect_s3_class(err, "reindeer_metadata_error")
  expect_match(conditionMessage(err), "METADATA.json")
  m <- as.data.frame(get_metadata(fx$corp))
  expect_equal(m$Speaker[m$bundle == fx$bndl], "good")
})

test_that("reset.before.add refreshes the cache, not only the file", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  add_metadata(fx$corp, list(Old = "yes"), session = fx$ses, bundle = fx$bndl)
  add_metadata(fx$corp, list(New = "yes"), session = fx$ses, bundle = fx$bndl,
               reset.before.add = TRUE)
  m <- as.data.frame(get_metadata(fx$corp))
  row <- m[m$bundle == fx$bndl, ]
  expect_false("Old" %in% names(m) && !is.na(row$Old))
  expect_equal(row$New, "yes")
})

test_that("bracket selectors match whole names and accept empty assignment", {
  skip_if_no_emuR()
  fx <- meta_fixture()
  # Add a second session whose name starts with the first's.
  extra <- file.path(fx$corp@basePath, "00001_ses")
  dir.create(file.path(extra, "x_bndl"), recursive = TRUE)
  expect_equal(unique(reindeer:::get_metadata_for_patterns(
    fx$corp, reindeer:::.full_match("0000"), ".*")$session), "0000")
  expect_no_error(fx$corp["0000", ] <- list())
  fx$corp["0000", ] <- list(Tag = "t")
  m <- as.data.frame(get_metadata(fx$corp))
  expect_true(all(m$Tag[m$session == "0000"] == "t"))
})

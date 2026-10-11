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

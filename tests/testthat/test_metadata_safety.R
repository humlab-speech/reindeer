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
